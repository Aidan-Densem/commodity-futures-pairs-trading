ou_gh_formation_scale <- function(x) {
  values <- as.numeric(x)
  values <- values[is.finite(values)]
  ou_gh_assert(length(values) >= 2L, "Formation spread has too few finite values.")
  centre <- stats::median(values)
  scale <- 1.4826 * stats::median(abs(values - centre))
  source <- "formation_median_and_mad"
  warning <- NA_character_
  if (!is.finite(scale) || scale <= 0) {
    scale <- stats::sd(values)
    source <- "formation_sd_fallback"
    warning <- "MAD was invalid; formation SD used."
  }
  ou_gh_assert(is.finite(scale) && scale > 0, "Formation scale is invalid.")
  list(centre = centre, scale = scale, source = source, warning = warning,
    n_valid = length(values), formation_only = TRUE)
}

ou_gh_load_pair_window <- function(task_row) {
  ou_gh_assert(nrow(task_row) == 1L, "Exactly one task row is required.")
  path <- task_row$spread_object_path_or_identifier[[1L]]
  ou_gh_assert(file.exists(path), paste("Missing pair-window cache:", path))
  expected <- as.character(task_row$spread_object_sha256[[1L]])
  if (nzchar(expected)) {
    ou_gh_assert(identical(ou_gh_sha256(path), expected),
      "Pair-window cache hash differs from the authoritative manifest.")
  }
  cache <- readRDS(path)
  windows <- cache$windows %||% cache
  ou_gh_assert(is.data.frame(windows$data) && is.list(windows$windows),
    "Saved pair-window cache is malformed.")
  hit <- which(vapply(windows$windows, function(value) {
    identical(as.character(value$window_name),
      as.character(task_row$window_identifier[[1L]]))
  }, logical(1L)))
  ou_gh_assert(length(hit) == 1L, "Window identifier does not resolve uniquely.")
  list(windows = windows, definition = windows$windows[[hit]], position = hit,
    cache_path = path, cache_hash = expected)
}

ou_gh_prepare_selected_formation <- function(
    task_row,
    minimum_transitions = 100L,
    target_active_dt = 1,
    active_dt_tolerance = 1e-8,
    maximum_calendar_gap_minutes = NULL,
    resolved_window = NULL
) {
  resolved <- resolved_window %||% ou_gh_load_pair_window(task_row)
  data <- resolved$windows$data
  definition <- resolved$definition
  rows <- sort(unique(as.integer(definition$estimation_rows)))
  testing_rows <- sort(unique(as.integer(definition$testing_rows)))
  ou_gh_assert(length(rows) > minimum_transitions, "Formation row count is too small.")
  ou_gh_assert(length(testing_rows) > 0L && max(rows) < min(testing_rows),
    "Formation and testing rows overlap or are unresolved.")
  y_col <- if ("y_price_column" %in% names(task_row)) {
    as.character(task_row$y_price_column[[1L]])
  } else paste0(task_row$y_id[[1L]], "_Midpoint")
  x_col <- if ("x_price_column" %in% names(task_row)) {
    as.character(task_row$x_price_column[[1L]])
  } else paste0(task_row$x_id[[1L]], "_Midpoint")
  ou_gh_assert(!grepl("close", y_col, ignore.case = TRUE) &&
                 !grepl("close", x_col, ignore.case = TRUE),
               "Production OU-GH formation must use accepted quote midpoints, not Close.")
  required <- c("Dates", "Active_Time_Minutes", "Transition_Valid",
                "Structural_Segment_ID", y_col, x_col)
  ou_gh_assert(all(required %in% names(data)), paste(
    "Pair cache lacks:", paste(setdiff(required, names(data)), collapse = ", ")
  ))
  selected <- data[rows, , drop = FALSE]
  y <- as.numeric(selected[[y_col]])
  x <- as.numeric(selected[[x_col]])
  spread <- rep(NA_real_, length(rows))
  valid_price <- is.finite(y) & is.finite(x) & y > 0 & x > 0
  formation_centre <- if ("formation_centre" %in% names(task_row)) {
    as.numeric(task_row$formation_centre[[1L]])
  } else 0
  spread[valid_price] <- log(y[valid_price]) - (
    task_row$alpha[[1L]] + task_row$beta[[1L]] * log(x[valid_price])
  ) - formation_centre
  states <- data.frame(
    global_row = rows,
    timestamp = ou_gh_time(selected$Dates),
    active_time = as.numeric(selected$Active_Time_Minutes),
    transition_valid = as.logical(selected$Transition_Valid),
    structural_segment_id = as.integer(selected$Structural_Segment_ID),
    spread = spread,
    structural_exclusion = if ("Structural_Exclusion" %in% names(selected)) {
      as.logical(selected$Structural_Exclusion)
    } else FALSE,
    roll_from_previous = if ("Roll_Transition_From_Previous" %in% names(selected)) {
      as.logical(selected$Roll_Transition_From_Previous)
    } else FALSE,
    roll_to_next = if ("Roll_Transition_To_Next" %in% names(selected)) {
      as.logical(selected$Roll_Transition_To_Next)
    } else FALSE,
    stringsAsFactors = FALSE
  )
  ou_gh_assert(max(states$timestamp, na.rm = TRUE) <= task_row$Formation_End[[1L]],
    "Resolved formation states extend past Formation_End.")
  ou_gh_assert(max(states$timestamp, na.rm = TRUE) < task_row$Testing_Start[[1L]],
    "Testing timestamps entered the formation object.")
  previous <- seq_len(nrow(states) - 1L)
  current <- previous + 1L
  calendar_dt <- as.numeric(difftime(
    states$timestamp[current], states$timestamp[previous], units = "mins"
  ))
  active_dt <- states$active_time[current] - states$active_time[previous]
  flags <- data.frame(
    nonfinite = !is.finite(states$spread[previous]) |
      !is.finite(states$spread[current]) |
      !is.finite(calendar_dt) | !is.finite(active_dt),
    nonconsecutive_source_row =
      states$global_row[current] != states$global_row[previous] + 1L,
    nonpositive_calendar_dt = calendar_dt <= 0,
    nonpositive_active_dt = active_dt <= 0,
    upstream_transition_invalid = !(states$transition_valid[current] %in% TRUE),
    active_dt_not_target = abs(active_dt - target_active_dt) > active_dt_tolerance,
    structural_exclusion = states$structural_exclusion[previous] |
      states$structural_exclusion[current],
    roll_boundary = states$roll_to_next[previous] |
      states$roll_from_previous[current],
    stringsAsFactors = FALSE
  )
  flags[] <- lapply(flags, function(value) {
    value[is.na(value)] <- TRUE
    value
  })
  accepted <- !Reduce(`|`, flags)
  segment <- ifelse(accepted, states$structural_segment_id[current], NA_integer_)
  audit <- data.frame(
    transition_index = previous,
    previous_global_row = states$global_row[previous],
    current_global_row = states$global_row[current],
    previous_timestamp = states$timestamp[previous],
    current_timestamp = states$timestamp[current],
    calendar_dt_minutes = calendar_dt,
    scheduled_closure_span_diagnostic = calendar_dt > 5,
    active_dt = active_dt,
    x_previous = states$spread[previous],
    x_current = states$spread[current],
    flags,
    accepted = accepted,
    segment_id = segment,
    stringsAsFactors = FALSE
  )
  keep <- which(audit$accepted)
  transitions <- data.frame(
    pair_id = task_row$Pair[[1L]],
    task_key = task_row$task_key[[1L]],
    segment_id = audit$segment_id[keep],
    x_previous = audit$x_previous[keep],
    x_current = audit$x_current[keep],
    previous_timestamp = audit$previous_timestamp[keep],
    current_timestamp = audit$current_timestamp[keep],
    delta = audit$active_dt[keep],
    previous_global_row = audit$previous_global_row[keep],
    current_global_row = audit$current_global_row[keep],
    stringsAsFactors = FALSE
  )
  ou_gh_assert(nrow(transitions) >= minimum_transitions,
    "Insufficient accepted formation transitions.")
  state_rows <- sort(unique(c(
    match(transitions$previous_global_row, states$global_row),
    match(transitions$current_global_row, states$global_row)
  )))
  scaling <- ou_gh_formation_scale(states$spread[state_rows])
  scaled <- transitions
  scaled$x_previous <- (scaled$x_previous - scaling$centre) / scaling$scale
  scaled$x_current <- (scaled$x_current - scaling$centre) / scaling$scale
  segment_lengths <- as.integer(table(transitions$segment_id))
  formation_hash <- ou_gh_hash_object(list(
    task_key = task_row$task_key[[1L]], alpha = task_row$alpha[[1L]],
    beta = task_row$beta[[1L]], states = states[, c(
      "global_row", "timestamp", "active_time", "spread"
    )], transitions = transitions, scaling = scaling
  ))
  list(
    task = task_row,
    formation_only = TRUE,
    testing_rows_loaded = FALSE,
    window_definition = definition,
    formation_states = states,
    transition_audit = audit,
    transitions = transitions,
    scaled_transitions = scaled,
    scaling = scaling,
    formation_hash = formation_hash,
    n_transitions = nrow(transitions),
    n_segments = length(unique(transitions$segment_id)),
    n_formation_rows = length(rows),
    minimum_segment_length = min(segment_lengths),
    median_segment_length = stats::median(segment_lengths),
    maximum_segment_length = max(segment_lengths),
    active_span_minutes = diff(range(states$active_time, na.rm = TRUE)),
    segment_lengths = segment_lengths,
    formation_endpoint = max(states$timestamp[is.finite(states$spread)]),
    formation_endpoint_spread = tail(states$spread[is.finite(states$spread)], 1L),
    rejected_transitions = sum(!audit$accepted),
    rejection_counts = colSums(flags)
  )
}

ou_gh_split_formation <- function(transitions, training_fraction = 0.75) {
  needed <- c("segment_id", "x_previous", "x_current", "delta")
  ou_gh_assert(all(needed %in% names(transitions)),
    "Transitions do not satisfy the formation-split contract.")
  segment_ids <- unique(transitions$segment_id)
  if (length(segment_ids) >= 2L) {
    n_training <- min(length(segment_ids) - 1L,
      max(1L, floor(training_fraction * length(segment_ids))))
    training_ids <- segment_ids[seq_len(n_training)]
    training <- transitions[transitions$segment_id %in% training_ids, , drop = FALSE]
    validation <- transitions[!transitions$segment_id %in% training_ids, , drop = FALSE]
    rule <- "complete_segments_chronological_last_block_validation"
  } else {
    n <- nrow(transitions)
    cut <- min(n - 2L, max(2L, floor(training_fraction * n)))
    training <- transitions[seq_len(cut), , drop = FALSE]
    validation <- transitions[seq.int(cut + 1L, n), , drop = FALSE]
    rule <- "single_segment_contiguous_tail_validation"
  }
  ou_gh_assert(nrow(training) >= 100L && nrow(validation) >= 50L,
    "Insufficient transitions for formation-block selection.")
  list(
    training = training, validation = validation, rule = rule,
    training_fraction_realised = nrow(training) / nrow(transitions),
    n_training = nrow(training), n_validation = nrow(validation),
    training_segments = unique(training$segment_id),
    validation_segments = unique(validation$segment_id),
    split_hash = ou_gh_hash_object(list(
      rule = rule,
      training_rows = training[, c("previous_global_row", "current_global_row")],
      validation_rows = validation[, c("previous_global_row", "current_global_row")]
    ))
  )
}

ou_gh_build_horizon_pairs <- function(transitions, horizons) {
  horizons <- sort(unique(as.integer(horizons)))
  ou_gh_assert(all(horizons >= 1L), "Horizons must be positive integers.")
  output <- list()
  counter <- 0L
  for (segment_id in unique(transitions$segment_id)) {
    one <- transitions[transitions$segment_id == segment_id, , drop = FALSE]
    one <- one[order(one$previous_timestamp), , drop = FALSE]
    values <- c(one$x_previous[[1L]], one$x_current)
    timestamps <- c(one$previous_timestamp[[1L]], one$current_timestamp)
    rows <- c(one$previous_global_row[[1L]], one$current_global_row)
    for (horizon in horizons) {
      if (length(values) <= horizon) next
      starts <- seq_len(length(values) - horizon)
      ends <- starts + horizon
      counter <- counter + 1L
      output[[counter]] <- data.frame(
        segment_id = segment_id,
        horizon = horizon,
        x_previous = values[starts],
        x_current = values[ends],
        previous_timestamp = timestamps[starts],
        current_timestamp = timestamps[ends],
        previous_global_row = rows[starts],
        current_global_row = rows[ends],
        stringsAsFactors = FALSE
      )
    }
  }
  if (!length(output)) return(data.frame())
  do.call(rbind, output)
}
