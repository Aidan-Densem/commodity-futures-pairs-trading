# Active time is the measure of the intersection of the two exact contracts'
# admissible session intervals. No elapsed-gap cutoff or wall-clock proxy is
# used. Calendars are explicit external reference inputs.

joint_admissible_intervals <- function(calendar_y, calendar_x, contract_y, contract_x,
                                       timezone = "Europe/London") {
  y <- validate_session_intervals(calendar_y, timezone)
  x <- validate_session_intervals(calendar_x, timezone)
  y <- y[y$contract == contract_y & y$admissible, , drop = FALSE]
  x <- x[x$contract == contract_x & x$admissible, , drop = FALSE]
  if (!nrow(y) || !nrow(x)) return(data.frame())
  rows <- vector("list", nrow(y) * nrow(x)); k <- 0L
  for (i in seq_len(nrow(y))) for (j in seq_len(nrow(x))) {
    start <- max(y$open_timestamp[[i]], x$open_timestamp[[j]])
    end <- min(y$close_timestamp[[i]], x$close_timestamp[[j]])
    if (end > start) {
      k <- k + 1L
      rows[[k]] <- data.frame(
        open_timestamp = as.POSIXct(start, origin = "1970-01-01", tz = timezone),
        close_timestamp = as.POSIXct(end, origin = "1970-01-01", tz = timezone),
        admissible_interval_id = paste(y$session_id[[i]], x$session_id[[j]], sep = "::"),
        stringsAsFactors = FALSE
      )
    }
  }
  if (!k) return(data.frame())
  do.call(rbind, rows[seq_len(k)])
}

active_minutes_between <- function(timestamp_prev, timestamp_curr,
                                   calendar_y, calendar_x, contract_y, contract_x,
                                   timezone = "Europe/London") {
  left <- as.POSIXct(timestamp_prev, tz = timezone)
  right <- as.POSIXct(timestamp_curr, tz = timezone)
  if (length(left) != 1L || length(right) != 1L || is.na(left) || is.na(right) || right <= left) {
    stop("Active-time endpoints must be two ordered finite timestamps.", call. = FALSE)
  }
  joint <- joint_admissible_intervals(calendar_y, calendar_x, contract_y, contract_x, timezone)
  if (!nrow(joint)) return(0)
  starts <- pmax(as.numeric(joint$open_timestamp), as.numeric(left))
  ends <- pmin(as.numeric(joint$close_timestamp), as.numeric(right))
  sum(pmax(ends - starts, 0)) / 60
}

admissible_interval_at <- function(timestamp, joint_intervals, timezone = "Europe/London") {
  stamp <- as.POSIXct(timestamp, tz = timezone)
  vapply(as.numeric(stamp), function(value) {
    hit <- which(as.numeric(joint_intervals$open_timestamp) <= value &
                   value < as.numeric(joint_intervals$close_timestamp))
    if (length(hit) == 1L) as.character(joint_intervals$admissible_interval_id[[hit]]) else NA_character_
  }, character(1L))
}

add_pair_active_clock <- function(x, calendar_y, calendar_x,
                                  timezone = "Europe/London") {
  required <- c("timestamp", "contract_y", "contract_x", "opportunity_index",
                "statistical_quote_valid")
  missing <- setdiff(required, names(x))
  if (length(missing)) stop("Pair clock input lacks: ", paste(missing, collapse = ", "), call. = FALSE)
  x <- x[order(as.POSIXct(x$timestamp, tz = timezone), x$opportunity_index), , drop = FALSE]
  n <- nrow(x)
  # Exchange admissibility and the dissertation's statistical session are two
  # different identities.  The former may change across a scheduled closure;
  # the latter is simply the London calendar date used by the 20:10 design and
  # the 18/20 quote-feasibility rule.
  x$admissible_interval_id <- NA_character_
  x$calendar_session_date <- as.Date(x$timestamp, tz = timezone)
  keys <- unique(paste(x$contract_y, x$contract_x, sep = "|"))
  for (key in keys) {
    hit <- which(paste(x$contract_y, x$contract_x, sep = "|") == key)
    joint <- joint_admissible_intervals(
      calendar_y, calendar_x, x$contract_y[[hit[[1L]]]], x$contract_x[[hit[[1L]]]], timezone
    )
    if (nrow(joint)) x$admissible_interval_id[hit] <- admissible_interval_at(x$timestamp[hit], joint, timezone)
  }
  x$transition_valid <- FALSE
  x$active_dt_minutes <- 0
  if (n > 1L) for (i in 2:n) {
    consecutive <- x$opportunity_index[[i]] == x$opportunity_index[[i - 1L]] + 1L
    same_contracts <- identical(as.character(x$contract_y[[i]]), as.character(x$contract_y[[i - 1L]])) &&
      identical(as.character(x$contract_x[[i]]), as.character(x$contract_x[[i - 1L]]))
    endpoints_admissible <- !is.na(x$admissible_interval_id[[i]]) &&
      !is.na(x$admissible_interval_id[[i - 1L]])
    accepted <- isTRUE(x$statistical_quote_valid[[i]]) && isTRUE(x$statistical_quote_valid[[i - 1L]])
    explicit_boundary <- if ("roll_boundary" %in% names(x)) {
      isTRUE(x$roll_boundary[[i]])
    } else if ("structural_boundary" %in% names(x)) {
      isTRUE(x$structural_boundary[[i]])
    } else FALSE
    candidate <- consecutive && same_contracts && endpoints_admissible && accepted && !explicit_boundary
    if (candidate) x$active_dt_minutes[[i]] <- active_minutes_between(
      x$timestamp[[i - 1L]], x$timestamp[[i]], calendar_y, calendar_x,
      x$contract_y[[i]], x$contract_x[[i]], timezone
    )
    x$transition_valid[[i]] <- candidate && is.finite(x$active_dt_minutes[[i]]) &&
      x$active_dt_minutes[[i]] > 0
  }
  # A new segment starts at a row whose incoming transition is unusable.
  # This annotation is created on the raw synchronised topology before any
  # statistical or execution-quality rows are removed.
  boundary <- !x$transition_valid
  if (n) boundary[[1L]] <- TRUE
  x$structural_segment_id <- cumsum(boundary)
  x$Active_Dt_Backward_Minutes <- x$active_dt_minutes
  x$Active_Time_Minutes <- cumsum(x$active_dt_minutes)
  x
}

# Compatibility name retained for callers, but production must supply explicit
# calendars. A missing calendar is now an error rather than a gap-cutoff proxy.
add_empirical_active_clock <- function(x, timezone = "Europe/London",
                                       calendar_y = NULL, calendar_x = NULL) {
  if (is.null(calendar_y) || is.null(calendar_x)) stop(
    "Explicit exact-contract session calendars are required for active time.", call. = FALSE
  )
  add_pair_active_clock(x, calendar_y, calendar_x, timezone)
}

validate_segment_safe_transitions <- function(x) {
  if ("transition_valid" %in% names(x)) return(x$transition_valid %in% TRUE)
  n <- nrow(x); if (!n) return(logical())
  valid <- c(FALSE, rep(TRUE, n - 1L))
  if ("structural_segment_id" %in% names(x) && n > 1L) {
    valid[-1L] <- x$structural_segment_id[-1L] == x$structural_segment_id[-n]
  }
  if ("accepted_adjacency" %in% names(x)) valid <- valid & (x$accepted_adjacency %in% TRUE)
  valid
}
