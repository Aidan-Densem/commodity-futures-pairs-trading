# Exact listed-contract construction under the submitted synchronous convention:
# both legs advance five weekdays before the earlier last-tradeable date.

subtract_weekdays <- function(date, n = 5L) {
  value <- as.Date(date); remaining <- as.integer(n)
  if (is.na(value) || remaining < 0L) stop("Invalid weekday offset.", call. = FALSE)
  while (remaining > 0L) {
    value <- value - 1L
    weekday <- as.POSIXlt(value)$wday
    if (!weekday %in% c(0L, 6L)) remaining <- remaining - 1L
  }
  value
}

build_synchronous_pair_roll_schedule <- function(lifecycle, pair_id, y_generic, x_generic,
                                                  start_date, end_date,
                                                  roll_weekdays = 5L) {
  life <- validate_contract_lifecycle(lifecycle)
  y <- life[toupper(life$generic) == toupper(y_generic), , drop = FALSE]
  x <- life[toupper(life$generic) == toupper(x_generic), , drop = FALSE]
  if (!nrow(y) || !nrow(x)) stop("Both lifecycle chains are required.", call. = FALSE)
  start_date <- as.Date(start_date); end_date <- as.Date(end_date)
  first_usable <- function(chain) {
    individual_roll <- as.Date(vapply(seq_len(nrow(chain)), function(i) as.character(
      subtract_weekdays(chain$last_trade_date[[i]], roll_weekdays)
    ), character(1L)))
    hit <- which(chain$first_trade_date <= start_date & individual_roll > start_date)
    if (!length(hit)) hit <- which(individual_roll > start_date)
    if (!length(hit)) stop("No lifecycle contract covers the requested start.", call. = FALSE)
    hit[[1L]]
  }
  iy <- first_usable(y); ix <- first_usable(x); from <- start_date
  rows <- list(); k <- 0L
  while (from <= end_date && iy <= nrow(y) && ix <= nrow(x)) {
    earlier_last <- min(y$last_trade_date[[iy]], x$last_trade_date[[ix]])
    roll_date <- subtract_weekdays(earlier_last, roll_weekdays)
    until <- min(roll_date, end_date + 1L)
    if (until <= from) stop("The synchronous lifecycle schedule is non-increasing.", call. = FALSE)
    k <- k + 1L
    rows[[k]] <- data.frame(
      pair_id = pair_id, effective_from = from, effective_until = until,
      y_contract = y$contract[[iy]], x_contract = x$contract[[ix]],
      y_delivery_month = y$delivery_month[[iy]], x_delivery_month = x$delivery_month[[ix]],
      y_last_trade_date = y$last_trade_date[[iy]], x_last_trade_date = x$last_trade_date[[ix]],
      roll_date = roll_date, roll_decision_basis = "earlier_last_trade_minus_5_weekdays",
      causal_reference_only = TRUE, stringsAsFactors = FALSE
    )
    from <- until; iy <- iy + 1L; ix <- ix + 1L
    if (iy <= nrow(y) && y$first_trade_date[[iy]] > from) stop("Incoming Y contract was not yet listed at roll.", call. = FALSE)
    if (ix <= nrow(x) && x$first_trade_date[[ix]] > from) stop("Incoming X contract was not yet listed at roll.", call. = FALSE)
  }
  if (!k) stop("No synchronous pair schedule could be constructed.", call. = FALSE)
  do.call(rbind, rows)
}

construct_active_exact_pair_series <- function(exact_quotes, schedule,
                                               y_generic, x_generic,
                                               timezone = "Europe/London") {
  quotes <- validate_quote_table(exact_quotes, timezone)
  validate_synchronous_exact_contract_schedule(schedule)
  rows <- vector("list", nrow(schedule))
  for (i in seq_len(nrow(schedule))) {
    start <- as.Date(schedule$effective_from[[i]]); end <- as.Date(schedule$effective_until[[i]])
    date <- as.Date(quotes$timestamp, tz = timezone)
    y <- quotes[date >= start & date < end & toupper(quotes$generic) == toupper(y_generic) &
                  quotes$contract == schedule$y_contract[[i]], , drop = FALSE]
    x <- quotes[date >= start & date < end & toupper(quotes$generic) == toupper(x_generic) &
                  quotes$contract == schedule$x_contract[[i]], , drop = FALSE]
    z <- synchronise_quote_legs(y, x, timezone)
    if (nrow(z)) {
      z$pair_id <- schedule$pair_id[[i]]
      z$schedule_segment <- i
      rows[[i]] <- z
    }
  }
  rows <- rows[vapply(rows, is.data.frame, logical(1L))]
  if (!length(rows)) return(data.frame())
  out <- do.call(rbind, rows); out <- out[order(out$timestamp), , drop = FALSE]
  out$opportunity_index <- seq_len(nrow(out))
  out$roll_boundary <- c(TRUE, out$schedule_segment[-1L] != out$schedule_segment[-nrow(out)])
  rownames(out) <- NULL
  out
}
