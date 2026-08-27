# Chronological committed-capital ledger. Segment P&L is marked at the final
# accepted midpoint of each session, with executable entry/exit prices and
# explicit fees on their event dates.
mab_decode_testing_session_dates <- function(value, expected_n) {
  parts <- strsplit(as.character(value), ";", fixed = TRUE)[[1L]]
  dates <- as.Date(parts[nzchar(parts)])
  mab_assert(!anyNA(dates) && !anyDuplicated(dates) &&
               length(dates) == as.integer(expected_n),
             "Frozen testing-session identity is malformed or incomplete.")
  dates
}

mab_segment_daily_pnl <- function(segment, monetary_data) {
  leg <- segment$leg[[1L]]
  contract_col <- paste0(leg, "_contract"); midpoint_col <- paste0(leg, "_price")
  rows <- monetary_data$timestamp >= segment$entry_timestamp[[1L]] &
    monetary_data$timestamp <= segment$exit_timestamp[[1L]] &
    as.character(monetary_data[[contract_col]]) == as.character(segment$raw_contract[[1L]]) &
    is.finite(monetary_data[[midpoint_col]])
  z <- monetary_data[rows, c("timestamp", midpoint_col), drop = FALSE]
  if (!nrow(z)) stop("A realised segment has no admissible daily mark path.", call. = FALSE)
  z$session_date <- as.Date(z$timestamp, tz = "Europe/London")
  z <- z[!duplicated(z$session_date, fromLast = TRUE), , drop = FALSE]
  z$mark <- z[[midpoint_col]]
  exit_date <- as.Date(segment$exit_timestamp[[1L]], tz = "Europe/London")
  z$mark[z$session_date == exit_date] <- segment$exit_price_displayed[[1L]]
  previous <- c(segment$entry_price_displayed[[1L]], head(z$mark, -1L))
  gross_native <- segment$signed_quantity[[1L]] * (z$mark - previous) *
    segment$point_value_native_per_displayed_point[[1L]]
  # Existing trade accounting converts the segment at its causal exit BFIX.
  pnl <- gross_native * segment$pnl_fx_rate_usd_per_native[[1L]]
  fees <- numeric(nrow(z))
  entry_hit <- which(z$session_date == as.Date(segment$entry_timestamp[[1L]], tz = "Europe/London"))
  exit_hit <- which(z$session_date == exit_date)
  if (length(entry_hit)) fees[entry_hit[[1L]]] <- fees[entry_hit[[1L]]] + segment$entry_fee_usd[[1L]]
  if (length(exit_hit)) fees[tail(exit_hit, 1L)] <- fees[tail(exit_hit, 1L)] + segment$exit_fee_usd[[1L]]
  data.frame(session_date = z$session_date, net_usd_pnl = pnl - fees,
             segment_id = segment$segment_id[[1L]], stringsAsFactors = FALSE)
}

mab_build_committed_capital_ledger <- function(results, strategies, monetary_data) {
  mab_assert(length(results) == nrow(strategies) && length(monetary_data) == nrow(strategies),
             "Backtest results, strategies and monetary paths must align.")
  pnl_rows <- capital_rows <- list(); p <- c <- 0L
  for (i in seq_along(results)) {
    strategy <- strategies[i, , drop = FALSE]
    result <- results[[i]]
    dates <- sort(unique(as.Date(monetary_data[[i]]$timestamp, tz = "Europe/London")))
    dates <- dates[dates >= as.Date(strategy$testing_start) & dates <= as.Date(strategy$testing_end)]
    if (length(dates)) {
      c <- c + 1L
      capital_rows[[c]] <- data.frame(
        session_date = dates, model_label = strategy$model_label,
        pair_id = strategy$pair_id, committed_capital_usd = strategy$pair_sleeve_usd,
        stringsAsFactors = FALSE
      )
    }
    if (nrow(result$segments)) for (j in seq_len(nrow(result$segments))) {
      p <- p + 1L
      x <- mab_segment_daily_pnl(result$segments[j, , drop = FALSE], monetary_data[[i]])
      x$model_label <- strategy$model_label; x$pair_id <- strategy$pair_id
      pnl_rows[[p]] <- x
    }
  }
  capital <- mab_bind_rows(capital_rows); pnl <- mab_bind_rows(pnl_rows)
  cap <- aggregate(committed_capital_usd ~ session_date + model_label, capital, sum)
  if (nrow(pnl)) net <- aggregate(net_usd_pnl ~ session_date + model_label, pnl, sum)
  else net <- transform(cap[c("session_date", "model_label")], net_usd_pnl = 0)
  ledger <- merge(cap, net, by = c("session_date", "model_label"), all.x = TRUE)
  ledger$net_usd_pnl[is.na(ledger$net_usd_pnl)] <- 0
  ledger$return_committed <- ledger$net_usd_pnl / ledger$committed_capital_usd
  ledger <- ledger[order(ledger$model_label, ledger$session_date), , drop = FALSE]
  rownames(ledger) <- NULL
  list(ledger = ledger, segment_daily_pnl = pnl, capital_schedule = capital)
}

# Production ledger: capital is a property of the formation-stage selected
# schedule, not of downstream model/threshold availability or realised entry.
mab_build_selected_schedule_ledger <- function(results, strategies, monetary_data,
                                               selected_schedule, model_routes,
                                               prepared_pair_series) {
  selected <- selected_schedule[selected_schedule$selected %in% TRUE, , drop = FALSE]
  mab_assert(all(model_routes$selection_preserved %in% TRUE),
             "A model route attempted to alter the selected schedule.")
  strategy_key <- if (nrow(strategies)) paste(
    strategies$model_label, strategies$pair_id, as.Date(strategies$formation_endpoint)
  ) else character()
  tradeable_key <- with(
    model_routes[model_routes$route_status == "TRADEABLE", , drop = FALSE],
    paste(model_label, pair_id, as.Date(endpoint_session_date))
  )
  mab_assert(setequal(strategy_key, tradeable_key),
             "TRADEABLE routes and executable strategy specifications differ.")
  pnl_rows <- list(); p <- 0L
  for (i in seq_along(results)) {
    result <- results[[i]]; strategy <- strategies[i, , drop = FALSE]
    if (nrow(result$segments)) for (j in seq_len(nrow(result$segments))) {
      p <- p + 1L
      z <- mab_segment_daily_pnl(result$segments[j, , drop = FALSE], monetary_data[[i]])
      z$model_label <- strategy$model_label; z$pair_id <- strategy$pair_id
      pnl_rows[[p]] <- z
    }
  }
  pnl <- mab_bind_rows(pnl_rows)
  capital_rows <- vector("list", nrow(model_routes))
  selected_key <- paste(selected$pair_id, as.Date(selected$endpoint_session_date))
  for (i in seq_len(nrow(model_routes))) {
    route <- model_routes[i, , drop = FALSE]
    hit <- match(
      paste(route$pair_id, as.Date(route$endpoint_session_date)), selected_key
    )
    mab_assert(!is.na(hit), "Route lacks its formation-selected schedule row.")
    schedule <- selected[hit, , drop = FALSE]
    path <- prepared_pair_series[[as.character(route$pair_id)]]
    mab_assert(!is.null(path), paste0("Missing prepared pair path: ", route$pair_id))
    dates <- mab_decode_testing_session_dates(
      schedule$testing_session_dates[[1L]], schedule$testing_sessions[[1L]]
    )
    available_dates <- unique(as.Date(path$timestamp, tz = "Europe/London"))
    mab_assert(all(dates %in% available_dates),
               "A frozen testing session is absent from the prepared pair path.")
    capital_rows[[i]] <- data.frame(
      session_date = dates, model_label = route$model_label,
      pair_id = route$pair_id,
      route_status = route$route_status,
      committed_capital_usd = route$selected_pair_sleeve_usd,
      stringsAsFactors = FALSE
    )
  }
  capital <- mab_bind_rows(capital_rows)
  cap <- aggregate(committed_capital_usd ~ session_date + model_label, capital, sum)
  if (nrow(pnl)) net <- aggregate(net_usd_pnl ~ session_date + model_label, pnl, sum)
  else net <- transform(cap[c("session_date", "model_label")], net_usd_pnl = 0)
  ledger <- merge(cap, net, by = c("session_date", "model_label"), all.x = TRUE)
  ledger$net_usd_pnl[is.na(ledger$net_usd_pnl)] <- 0
  ledger$return_committed <- ledger$net_usd_pnl / ledger$committed_capital_usd
  ledger <- ledger[order(ledger$model_label, ledger$session_date), , drop = FALSE]
  rownames(ledger) <- NULL
  list(ledger = ledger, segment_daily_pnl = pnl, capital_schedule = capital,
       model_routes = model_routes)
}
