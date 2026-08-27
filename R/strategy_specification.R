build_strategy_specifications <- function(threshold_results, selected_schedule,
                                          model_label, pair_sleeve_usd = 200000) {
  selected <- selected_schedule[selected_schedule$selected %in% TRUE, , drop = FALSE]
  if (!nrow(selected)) return(data.frame())
  threshold_table <- if (is.data.frame(threshold_results)) threshold_results else {
    do.call(rbind, lapply(threshold_results, function(x) if (is.null(x$selected)) x else x$selected))
  }
  if (!nrow(threshold_table)) return(data.frame())
  if ("strategy_available" %in% names(threshold_table)) {
    threshold_table <- threshold_table[threshold_table$strategy_available %in% TRUE, , drop = FALSE]
  }
  if (!nrow(threshold_table)) return(data.frame())
  threshold_pair <- if ("pair_id" %in% names(threshold_table)) "pair_id" else "Pair"
  threshold_date <- if ("endpoint_session_date" %in% names(threshold_table)) {
    "endpoint_session_date"
  } else "Session_Date"
  key <- paste(selected$pair_id, as.Date(selected$endpoint_session_date))
  hit <- match(key, paste(threshold_table[[threshold_pair]], as.Date(threshold_table[[threshold_date]])))
  keep <- !is.na(hit)
  selected <- selected[keep, , drop = FALSE]; hit <- hit[keep]
  if (!nrow(selected)) return(data.frame())
  t <- threshold_table[hit, , drop = FALSE]
  required_selected <- c(
    "alpha", "beta", "formation_centre", "testing_start", "testing_end",
    "testing_session_dates", "testing_sessions"
  )
  missing <- setdiff(required_selected, names(selected))
  if (length(missing)) stop("Selected schedule lacks frozen strategy metadata: ",
                            paste(missing, collapse = ", "), call. = FALSE)
  out <- data.frame(
    pair_id = selected$pair_id,
    formation_endpoint = as.Date(selected$endpoint_session_date),
    testing_start = as.POSIXct(selected$testing_start, tz = "Europe/London"),
    testing_end = as.POSIXct(selected$testing_end, tz = "Europe/London"),
    upper_entry = as.numeric(t$upper_entry), lower_entry = as.numeric(t$lower_entry),
    upper_exit = as.numeric(t$upper_exit), lower_exit = as.numeric(t$lower_exit),
    alpha = as.numeric(selected$alpha), beta = as.numeric(selected$beta),
    formation_centre = as.numeric(selected$formation_centre),
    testing_session_dates = as.character(selected$testing_session_dates),
    testing_sessions = as.integer(selected$testing_sessions),
    pair_sleeve_usd = pair_sleeve_usd, model_label = model_label,
    route_status = "TRADEABLE",
    stringsAsFactors = FALSE
  )
  invisible(lapply(seq_len(nrow(out)), function(i) validate_strategy_spec(out[i, , drop = FALSE])))
  out
}
