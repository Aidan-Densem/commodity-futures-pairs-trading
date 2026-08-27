encode_session_dates <- function(dates) {
  paste(format(as.Date(dates), "%Y-%m-%d"), collapse = ";")
}

decode_session_dates <- function(value, expected_n = NULL) {
  parts <- strsplit(as.character(value), ";", fixed = TRUE)[[1L]]
  dates <- as.Date(parts[nzchar(parts)])
  if (anyNA(dates) || anyDuplicated(dates)) stop(
    "Frozen testing-session dates are malformed or duplicated.", call. = FALSE
  )
  if (!is.null(expected_n) && length(dates) != as.integer(expected_n)) stop(
    "Frozen testing-session date count differs from its window contract.", call. = FALSE
  )
  dates
}

build_pair_rolling_session_windows <- function(pair_quotes,
                                               formation_sessions = 20L,
                                               testing_sessions = 10L,
                                               step_sessions = 1L) {
  required <- c("pair_id", "calendar_session_date", "timestamp", "statistical_quote_valid")
  missing <- setdiff(required, names(pair_quotes))
  if (length(missing)) stop("Pair window input lacks: ", paste(missing, collapse = ", "), call. = FALSE)
  pair_ids <- unique(as.character(pair_quotes$pair_id))
  if (length(pair_ids) != 1L) stop("Build windows one synchronised pair at a time.", call. = FALSE)
  usable <- pair_quotes$statistical_quote_valid %in% TRUE & !is.na(pair_quotes$calendar_session_date)
  ordered <- pair_quotes[usable, , drop = FALSE]
  ordered <- ordered[order(ordered$timestamp), , drop = FALSE]
  sessions <- unique(as.character(ordered$calendar_session_date))
  need <- as.integer(formation_sessions + testing_sessions)
  if (length(sessions) < need) return(data.frame())
  endpoints <- seq.int(as.integer(formation_sessions), length(sessions) - as.integer(testing_sessions),
                       by = as.integer(step_sessions))
  do.call(rbind, lapply(endpoints, function(e) {
    formation <- sessions[seq.int(e - formation_sessions + 1L, e)]
    testing <- sessions[seq.int(e + 1L, e + testing_sessions)]
    formation_rows <- which(as.character(ordered$calendar_session_date) %in% formation)
    testing_rows <- which(as.character(ordered$calendar_session_date) %in% testing)
    endpoint_time <- max(ordered$timestamp[as.character(ordered$calendar_session_date) == sessions[[e]]])
    data.frame(
      pair_id = pair_ids[[1L]],
      endpoint_id = sprintf("endpoint_%s", format(as.Date(endpoint_time), "%Y%m%d")),
      endpoint_session_id = sessions[[e]], endpoint_session_date = as.Date(sessions[[e]]),
      formation_start = min(ordered$timestamp[formation_rows]),
      formation_end = max(ordered$timestamp[formation_rows]),
      testing_start = min(ordered$timestamp[testing_rows]),
      testing_end = max(ordered$timestamp[testing_rows]),
      formation_session_ids = paste(formation, collapse = ";"),
      testing_session_ids = paste(testing, collapse = ";"),
      testing_session_dates = encode_session_dates(testing),
      formation_sessions = formation_sessions, testing_sessions = testing_sessions,
      step_sessions = step_sessions, stringsAsFactors = FALSE
    )
  }))
}

build_rolling_session_windows <- function(pair_quotes, formation_sessions = 20L,
                                          testing_sessions = 10L, step_sessions = 1L) {
  if (!"pair_id" %in% names(pair_quotes)) stop("pair_id is required; global calendars are prohibited.", call. = FALSE)
  parts <- split(pair_quotes, pair_quotes$pair_id, drop = TRUE)
  rows <- lapply(parts, build_pair_rolling_session_windows,
                 formation_sessions, testing_sessions, step_sessions)
  rows <- rows[vapply(rows, function(x) is.data.frame(x) && nrow(x), logical(1L))]
  if (length(rows)) do.call(rbind, rows) else data.frame()
}

validate_synchronous_exact_contract_schedule <- function(schedule) {
  required <- c("pair_id", "effective_from", "effective_until", "y_contract",
                "x_contract", "y_last_trade_date", "x_last_trade_date", "roll_date")
  missing <- setdiff(required, names(schedule))
  if (length(missing)) stop("Exact-contract schedule lacks: ", paste(missing, collapse = ", "), call. = FALSE)
  if (any(as.Date(schedule$effective_until) <= as.Date(schedule$effective_from)) ||
      anyDuplicated(schedule[c("pair_id", "effective_from")])) {
    stop("The synchronous exact-contract schedule has overlapping/non-positive keys.", call. = FALSE)
  }
  invisible(schedule)
}
