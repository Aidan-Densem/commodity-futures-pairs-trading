# Validated V2 practical quote cleaner. Statistical midpoint validity remains a
# separate baseline field in market_data.R. All thresholds below are calibrated
# on formation rows and then frozen for testing.

v2_relative_spread <- function(bid, ask) {
  mid <- (as.numeric(bid) + as.numeric(ask)) / 2
  (as.numeric(ask) - as.numeric(bid)) / mid
}

v2_robust_sigma <- function(x) {
  x <- as.numeric(x[is.finite(x)])
  if (length(x) < 2L) return(NA_real_)
  1.4826 * stats::mad(x, center = stats::median(x), constant = 1, na.rm = TRUE)
}

v2_safe_quantile <- function(x, probability) {
  x <- as.numeric(x[is.finite(x)])
  if (!length(x)) return(NA_real_)
  as.numeric(stats::quantile(x, probability, names = FALSE, type = 8, na.rm = TRUE))
}

calibrate_execution_quote_leg <- function(formation, tick_size,
                                          config = production_config$quote_quality) {
  required <- c("timestamp", "bid", "ask", "close")
  missing <- setdiff(required, names(formation))
  if (length(missing)) stop("Quote calibration lacks: ", paste(missing, collapse = ", "), call. = FALSE)
  elementary <- is.finite(formation$bid) & is.finite(formation$ask) &
    formation$bid > 0 & formation$ask > 0 & formation$bid <= formation$ask
  mid <- (formation$bid + formation$ask) / 2
  elementary <- elementary & is.finite(mid) & mid > 0
  relative <- v2_relative_spread(formation$bid, formation$ask)
  usable <- relative[elementary & is.finite(relative) & relative > 0]
  minimum <- config$minimum_clean_formation_quotes
  if (length(usable) < minimum) stop("Insufficient formation quotes for V2 calibration.", call. = FALSE)
  log_width <- log(usable)
  width_sigma <- v2_robust_sigma(log_width)
  robust_envelope <- exp(stats::median(log_width) + config$log_spread_mad_multiplier *
                           ifelse(is.finite(width_sigma), width_sigma, 0))
  median_multiple <- config$spread_median_multiple * stats::median(usable)
  empirical <- v2_safe_quantile(usable, config$spread_empirical_quantile)
  tick_floor <- if (is.finite(tick_size) && tick_size > 0) {
    config$tick_floor_multiple * tick_size / stats::median(mid[elementary])
  } else 0
  relative_cutoff <- max(tick_floor, empirical, min(robust_envelope, median_multiple), na.rm = TRUE)
  preliminary <- elementary & is.finite(relative) & relative <= relative_cutoff
  accepted_mid <- mid[preliminary]
  returns <- abs(diff(log(accepted_mid)))
  return_sigma <- v2_robust_sigma(returns)
  price_cutoff <- max(
    config$price_relative_floor,
    config$price_return_mad_multiplier * ifelse(is.finite(return_sigma), return_sigma, 0),
    config$price_quantile_multiplier * v2_safe_quantile(returns, config$price_return_quantile),
    na.rm = TRUE
  )
  close_mid <- abs(log(as.numeric(formation$close) / mid))
  cm <- close_mid[preliminary & is.finite(formation$close) & formation$close > 0 & is.finite(close_mid)]
  cm_sigma <- v2_robust_sigma(cm)
  close_mid_cutoff <- max(
    v2_safe_quantile(cm, config$close_mid_empirical_quantile),
    stats::median(cm, na.rm = TRUE) + config$close_mid_mad_multiplier * ifelse(is.finite(cm_sigma), cm_sigma, 0),
    relative_cutoff / 2, na.rm = TRUE
  )
  list(
    relative_spread_cutoff = relative_cutoff,
    price_log_move_cutoff = price_cutoff,
    close_mid_consistency_cutoff = close_mid_cutoff,
    local_reference_observations = as.integer(config$local_reference_observations),
    local_reference_minimum = as.integer(config$local_reference_minimum),
    quote_quality_version = config$version,
    calibration_start = min(as.POSIXct(formation$timestamp, tz = "Europe/London")),
    calibration_end = max(as.POSIXct(formation$timestamp, tz = "Europe/London")),
    formation_only = TRUE
  )
}

assess_execution_quote_leg <- function(data, frozen_contract,
                                       prior_clean_mid = numeric()) {
  required <- c("timestamp", "bid", "ask", "close")
  missing <- setdiff(required, names(data))
  if (length(missing)) stop("Quote assessment lacks: ", paste(missing, collapse = ", "), call. = FALSE)
  n <- nrow(data); mid <- (data$bid + data$ask) / 2
  relative <- v2_relative_spread(data$bid, data$ask)
  clean <- rep(TRUE, n); reason <- vector("list", n)
  history <- prior_clean_mid[is.finite(prior_clean_mid) & prior_clean_mid > 0]
  reject <- function(i, code) { clean[[i]] <<- FALSE; reason[[i]] <<- c(reason[[i]], code) }
  for (i in seq_len(n)) {
    if (!is.finite(data$bid[[i]])) reject(i, "NONFINITE_BID")
    if (!is.finite(data$ask[[i]])) reject(i, "NONFINITE_ASK")
    if (is.finite(data$bid[[i]]) && data$bid[[i]] <= 0) reject(i, "NONPOSITIVE_BID")
    if (is.finite(data$ask[[i]]) && data$ask[[i]] <= 0) reject(i, "NONPOSITIVE_ASK")
    if (is.finite(data$bid[[i]]) && is.finite(data$ask[[i]]) && data$bid[[i]] > data$ask[[i]]) reject(i, "CROSSED_QUOTE")
    if (!is.finite(mid[[i]]) || mid[[i]] <= 0) next
    if (clean[[i]] && (!is.finite(relative[[i]]) || relative[[i]] > frozen_contract$relative_spread_cutoff)) {
      reject(i, "RELATIVE_SPREAD_OUTLIER")
    }
    if (clean[[i]] && length(history) >= frozen_contract$local_reference_minimum) {
      reference <- stats::median(tail(history, frozen_contract$local_reference_observations))
      if (abs(log(mid[[i]] / reference)) > frozen_contract$price_log_move_cutoff) {
        reject(i, "MIDQUOTE_LEVEL_OUTLIER")
      }
    }
    if (clean[[i]] && (!is.finite(data$close[[i]]) || data$close[[i]] <= 0 ||
                       abs(log(data$close[[i]] / mid[[i]])) > frozen_contract$close_mid_consistency_cutoff)) {
      reject(i, "CLOSE_MID_INCONSISTENT")
    }
    if (clean[[i]]) history <- c(history, mid[[i]])
  }
  data.frame(
    timestamp = as.POSIXct(data$timestamp, tz = "Europe/London"), quote_mid = mid,
    relative_spread = relative, execution_quote_clean = clean,
    execution_quote_reason = vapply(reason, function(z) if (length(z)) paste(unique(z), collapse = ";") else "ACCEPTED", character(1L)),
    quote_quality_version = frozen_contract$quote_quality_version,
    stringsAsFactors = FALSE
  )
}

apply_frozen_pair_quote_cleaner <- function(synchronised, y_contract, x_contract,
                                            sample = c("formation", "testing"),
                                            formation_history = NULL) {
  sample <- match.arg(sample)
  leg_frame <- function(z, leg) data.frame(
    timestamp = z$timestamp, bid = z[[paste0("bid_", leg)]], ask = z[[paste0("ask_", leg)]],
    close = z[[paste0("close_", leg)]], stringsAsFactors = FALSE
  )
  prior_y <- prior_x <- numeric()
  if (!is.null(formation_history)) {
    fy <- assess_execution_quote_leg(leg_frame(formation_history, "y"), y_contract)
    fx <- assess_execution_quote_leg(leg_frame(formation_history, "x"), x_contract)
    prior_y <- fy$quote_mid[fy$execution_quote_clean]
    prior_x <- fx$quote_mid[fx$execution_quote_clean]
  }
  y <- assess_execution_quote_leg(leg_frame(synchronised, "y"), y_contract, prior_y)
  x <- assess_execution_quote_leg(leg_frame(synchronised, "x"), x_contract, prior_x)
  synchronised$execution_quote_clean_y <- y$execution_quote_clean
  synchronised$execution_quote_clean_x <- x$execution_quote_clean
  synchronised$execution_quote_clean <- y$execution_quote_clean & x$execution_quote_clean &
    synchronised$raw_simultaneous_opportunity
  synchronised$execution_quote_reason <- ifelse(
    synchronised$execution_quote_clean, "ACCEPTED",
    paste0("Y:", y$execution_quote_reason, "|X:", x$execution_quote_reason)
  )
  attr(synchronised, "cleaner_sample") <- sample
  synchronised
}

quote_feasibility_summary <- function(formation, config = production_config$quote_rule) {
  required <- c("raw_simultaneous_opportunity", "execution_quote_clean", "calendar_session_date")
  missing <- setdiff(required, names(formation))
  if (length(missing)) stop("Quote feasibility input lacks: ", paste(missing, collapse = ", "), call. = FALSE)
  opportunities <- formation$raw_simultaneous_opportunity %in% TRUE
  clean <- opportunities & formation$execution_quote_clean %in% TRUE
  n_opp <- sum(opportunities); n_clean <- sum(clean)
  counts <- table(formation$calendar_session_date[clean])
  sessions_60 <- sum(counts >= 60L)
  share <- if (n_opp > 0L) n_clean / n_opp else NA_real_
  data.frame(
    N_opp = n_opp, N_clean = n_clean, clean_share_of_raw_two_leg_opportunities = share,
    sessions_with_at_least_60_clean = sessions_60,
    final_quote_feasibility_pass = n_clean >= config$minimum_clean_events &&
      sessions_60 >= config$minimum_sessions_with_60_clean &&
      is.finite(share) && share >= config$minimum_conditional_clean_share,
    stringsAsFactors = FALSE
  )
}
