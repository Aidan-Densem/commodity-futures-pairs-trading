MODEL_ROUTE_STATES <- c(
  "TRADEABLE", "MODEL_NO_TRADE", "THRESHOLD_UNAVAILABLE", "MODEL_UNAVAILABLE"
)

normalise_threshold_route_table <- function(threshold_results) {
  if (is.null(threshold_results)) return(data.frame())
  if (is.data.frame(threshold_results)) return(threshold_results)
  rows <- lapply(threshold_results, function(x) {
    if (is.data.frame(x)) x else if (!is.null(x$selected)) x$selected else NULL
  })
  rows <- rows[vapply(rows, is.data.frame, logical(1L))]
  if (length(rows)) do.call(rbind, rows) else data.frame()
}

build_model_route_manifest <- function(selected_schedule, model_label,
                                       threshold_results = NULL,
                                       model_availability = NULL,
                                       pair_sleeve_usd = 200000) {
  selected <- selected_schedule[selected_schedule$selected %in% TRUE, , drop = FALSE]
  if (!nrow(selected)) return(data.frame())
  required_schedule <- c(
    "endpoint_id", "endpoint_session_date", "pair_id", "primary_rank",
    "testing_session_dates", "testing_sessions"
  )
  missing_schedule <- setdiff(required_schedule, names(selected))
  if (length(missing_schedule)) stop(
    "Selected schedule lacks frozen testing-session identity: ",
    paste(missing_schedule, collapse = ", "), call. = FALSE
  )
  out <- selected[required_schedule]
  out$model_label <- model_label
  out$selection_status <- "SELECTED"
  out$model_fit_status <- "AVAILABLE"
  out$threshold_status <- "UNAVAILABLE"
  out$route_status <- "THRESHOLD_UNAVAILABLE"
  out$route_reason <- "no_threshold_result"
  out$selected_pair_sleeve_usd <- pair_sleeve_usd
  out$selection_preserved <- TRUE
  key <- paste(out$pair_id, as.Date(out$endpoint_session_date))
  if (!is.null(model_availability)) {
    mkey <- paste(model_availability$pair_id, as.Date(model_availability$endpoint_session_date))
    hit <- match(key, mkey)
    unavailable <- is.na(hit) | !(model_availability$model_available[hit] %in% TRUE)
    out$route_status[unavailable] <- "MODEL_UNAVAILABLE"
    out$model_fit_status[unavailable] <- "UNAVAILABLE"
    out$threshold_status[unavailable] <- "NOT_ATTEMPTED_MODEL_UNAVAILABLE"
    out$route_reason[unavailable] <- ifelse(
      is.na(hit[unavailable]), "no_model_result",
      as.character(model_availability$model_reason[hit[unavailable]])
    )
  } else unavailable <- rep(FALSE, nrow(out))
  tab <- normalise_threshold_route_table(threshold_results)
  if (nrow(tab)) {
    pair_col <- if ("pair_id" %in% names(tab)) "pair_id" else "Pair"
    date_col <- if ("endpoint_session_date" %in% names(tab)) "endpoint_session_date" else "Session_Date"
    hit <- match(key, paste(tab[[pair_col]], as.Date(tab[[date_col]])))
    has <- !is.na(hit) & !unavailable
    state <- if ("route_status" %in% names(tab)) as.character(tab$route_status[hit[has]]) else {
      ifelse(tab$strategy_available[hit[has]] %in% TRUE, "TRADEABLE", "MODEL_NO_TRADE")
    }
    state[!state %in% MODEL_ROUTE_STATES] <- "THRESHOLD_UNAVAILABLE"
    out$route_status[has] <- state
    out$threshold_status[has] <- ifelse(
      state == "TRADEABLE", "AVAILABLE_POSITIVE_OBJECTIVE",
      ifelse(state == "MODEL_NO_TRADE", "NONPOSITIVE_OUTSIDE_OPTION", "UNAVAILABLE")
    )
    out$route_reason[has] <- ifelse(
      state == "TRADEABLE", "positive_optimised_objective",
      ifelse(state == "MODEL_NO_TRADE", "outside_option_dominates", "threshold_not_available")
    )
  }
  if (any(!out$route_status %in% MODEL_ROUTE_STATES)) stop("Invalid model route state.", call. = FALSE)
  out$strategy_available <- out$route_status == "TRADEABLE"
  out$reason_code <- out$route_reason
  out
}

validate_model_route_manifest <- function(routes, selected_schedule, models) {
  selected <- selected_schedule[selected_schedule$selected %in% TRUE, , drop = FALSE]
  expected <- expand.grid(
    selected_row = seq_len(nrow(selected)), model_label = models,
    stringsAsFactors = FALSE
  )
  observed <- paste(routes$pair_id, as.Date(routes$endpoint_session_date), routes$model_label)
  target <- paste(
    selected$pair_id[expected$selected_row],
    as.Date(selected$endpoint_session_date[expected$selected_row]),
    expected$model_label
  )
  if (nrow(routes) != nrow(expected) || !setequal(observed, target) ||
      any(!routes$route_status %in% MODEL_ROUTE_STATES) ||
      any(!(routes$selection_preserved %in% TRUE))) {
    stop("Route manifest does not account for every selected pair-window-model.", call. = FALSE)
  }
  invisible(routes)
}
