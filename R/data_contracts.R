required_quote_columns <- function() c(
  "timestamp", "generic", "contract", "bid", "ask", "close"
)

validate_quote_table <- function(x, timezone = "Europe/London") {
  stopifnot(is.data.frame(x))
  missing <- setdiff(required_quote_columns(), names(x))
  if (length(missing)) stop(
    "Quote table is missing: ", paste(missing, collapse = ", "),
    call. = FALSE
  )
  timestamp <- as.POSIXct(x$timestamp, tz = timezone)
  if (anyNA(timestamp)) stop("Quote timestamps must be parseable.", call. = FALSE)
  x$timestamp <- timestamp
  x
}

# Central schema mapping for the report-conformant active-market clock.
# Increment fields lead into each row. Rejected/intervening rows and structural
# boundaries are represented before any statistical subset is taken.
resolve_active_time_increments <- function(formation, accepted_rows = seq_len(nrow(formation))) {
  stopifnot(is.data.frame(formation), length(accepted_rows) > 0L)
  accepted_rows <- as.integer(accepted_rows)
  dat <- formation[accepted_rows, , drop = FALSE]
  if (length(accepted_rows) > 1L && any(diff(accepted_rows) != 1L) &&
      !"transition_valid" %in% names(dat)) {
    stop("A non-consecutive subset requires explicit transition_valid boundaries.", call. = FALSE)
  }
  backward <- intersect(c("active_dt_minutes", "Active_Dt_Backward_Minutes"), names(dat))
  cumulative <- intersect(c("active_time_minutes", "Active_Time_Minutes",
                            "Window_Active_Time_Minutes"), names(dat))
  if (length(backward)) {
    dt <- as.numeric(dat[[backward[[1L]]]])
  } else if (length(cumulative)) {
    clock <- as.numeric(dat[[cumulative[[1L]]]])
    dt <- c(0, diff(clock))
  } else if ("Active_Dt_Minutes" %in% names(dat)) {
    forward <- as.numeric(dat$Active_Dt_Minutes)
    dt <- c(0, head(forward, -1L))
  } else if ("timestamp" %in% names(dat)) {
    # Synthetic/sample interface only. Production preparation writes an
    # explicit active-clock field before this function is called.
    stamp <- as.POSIXct(dat$timestamp, tz = "Europe/London")
    dt <- c(0, as.numeric(diff(stamp), units = "mins"))
  } else {
    stop("No recognised active-market time field is present.", call. = FALSE)
  }
  if (length(dt) != nrow(dat) || any(!is.finite(dt)) || any(dt < 0)) {
    stop("Active-market increments must be finite and non-negative.",
         call. = FALSE)
  }
  if ("transition_valid" %in% names(dat)) {
    invalid <- !(dat$transition_valid %in% TRUE)
    dt[invalid] <- 0
  }
  dt
}

required_lifecycle_columns <- function() c(
  "generic", "contract", "delivery_month", "first_trade_date", "last_trade_date"
)

validate_contract_lifecycle <- function(x) {
  stopifnot(is.data.frame(x))
  missing <- setdiff(required_lifecycle_columns(), names(x))
  if (length(missing)) stop("Lifecycle metadata lack: ", paste(missing, collapse = ", "), call. = FALSE)
  x$first_trade_date <- as.Date(x$first_trade_date)
  x$last_trade_date <- as.Date(x$last_trade_date)
  x$delivery_month <- as.Date(paste0(substr(as.character(x$delivery_month), 1L, 7L), "-01"))
  if (anyNA(x[c("first_trade_date", "last_trade_date", "delivery_month")]) ||
      any(x$last_trade_date <= x$first_trade_date) || anyDuplicated(x[c("generic", "contract")])) {
    stop("Lifecycle dates/identifiers are incomplete or inconsistent.", call. = FALSE)
  }
  x[order(toupper(x$generic), x$delivery_month), , drop = FALSE]
}

required_session_interval_columns <- function() c(
  "contract", "session_id", "open_timestamp", "close_timestamp", "admissible"
)

validate_session_intervals <- function(x, timezone = "Europe/London") {
  stopifnot(is.data.frame(x))
  missing <- setdiff(required_session_interval_columns(), names(x))
  if (length(missing)) stop("Session intervals lack: ", paste(missing, collapse = ", "), call. = FALSE)
  x$open_timestamp <- as.POSIXct(x$open_timestamp, tz = timezone)
  x$close_timestamp <- as.POSIXct(x$close_timestamp, tz = timezone)
  x$admissible <- x$admissible %in% TRUE
  if (anyNA(x[c("open_timestamp", "close_timestamp")]) ||
      any(x$close_timestamp <= x$open_timestamp) || any(!nzchar(as.character(x$session_id)))) {
    stop("Session intervals are incomplete or non-positive.", call. = FALSE)
  }
  x[order(x$contract, x$open_timestamp), , drop = FALSE]
}

validate_strategy_spec <- function(x) {
  required <- c(
    "pair_id", "formation_endpoint", "testing_start", "testing_end",
    "upper_entry", "lower_entry", "upper_exit", "lower_exit",
    "alpha", "beta", "formation_centre", "pair_sleeve_usd", "model_label"
  )
  stopifnot(is.data.frame(x), nrow(x) == 1L)
  missing <- setdiff(required, names(x))
  if (length(missing)) stop(
    "Strategy specification is missing: ", paste(missing, collapse = ", "),
    call. = FALSE
  )
  numeric_fields <- c(
    "upper_entry", "lower_entry", "upper_exit", "lower_exit",
    "alpha", "beta", "formation_centre", "pair_sleeve_usd"
  )
  if (!all(vapply(x[numeric_fields], function(z) is.numeric(z) && is.finite(z), logical(1L)))) {
    stop("Strategy numeric fields must be finite.", call. = FALSE)
  }
  if (!(x$lower_entry < x$upper_entry)) stop("Entry thresholds are unordered.", call. = FALSE)
  x
}
