gh_decode_session_dates <- function(value, expected_n) {
  parts <- strsplit(as.character(value), ";", fixed = TRUE)[[1L]]
  dates <- as.Date(parts[nzchar(parts)])
  if (anyNA(dates) || anyDuplicated(dates) ||
      length(dates) != as.integer(expected_n)) stop(
    "Frozen testing-session dates violate the GH task contract.", call. = FALSE
  )
  dates
}

build_gh_task_manifest <- function(selected_schedule, prepared_pair_series,
                                   cache_dir,
                                   gh_mode = c("STRICT_INTERIOR", "FULL_FAMILY")) {
  gh_mode <- match.arg(gh_mode)
  selected <- selected_schedule[selected_schedule$selected %in% TRUE, , drop = FALSE]
  required <- c("pair_id", "endpoint_id", "endpoint_session_date", "primary_rank",
                "alpha", "beta", "formation_centre", "formation_start",
                "testing_start", "testing_end", "testing_session_dates",
                "testing_sessions", "y_generic", "x_generic")
  missing <- setdiff(required, names(selected))
  if (length(missing)) stop("Selected schedule lacks GHI task fields: ",
                            paste(missing, collapse = ", "), call. = FALSE)
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  manifest <- vector("list", nrow(selected))
  for (i in seq_len(nrow(selected))) {
    row <- selected[i, , drop = FALSE]
    sync <- prepared_pair_series[[as.character(row$pair_id)]]
    if (is.null(sync)) stop("Missing prepared pair path for ", row$pair_id, call. = FALSE)
    use <- sync$timestamp >= as.POSIXct(row$formation_start, tz = "Europe/London") &
      sync$timestamp <= as.POSIXct(row$testing_end, tz = "Europe/London")
    sync <- sync[use, , drop = FALSE]
    n <- nrow(sync)
    roll <- sync$roll_boundary %in% TRUE
    dat <- data.frame(
      Dates = sync$timestamp, Active_Time_Minutes = sync$Active_Time_Minutes,
      Y_Midpoint = sync$midpoint_y, X_Midpoint = sync$midpoint_x,
      Structural_Exclusion = !(sync$statistical_quote_valid %in% TRUE),
      Structural_Segment_ID = sync$structural_segment_id,
      Transition_Valid = sync$transition_valid %in% TRUE,
      Roll_Transition_From_Previous = roll,
      Roll_Transition_To_Next = c(tail(roll, -1L), FALSE),
      stringsAsFactors = FALSE
    )
    formation_rows <- which(as.Date(dat$Dates) >= as.Date(row$formation_start) &
                              as.Date(dat$Dates) <= as.Date(row$endpoint_session_date))
    frozen_testing_dates <- gh_decode_session_dates(
      row$testing_session_dates[[1L]], row$testing_sessions[[1L]]
    )
    testing_rows <- which(as.Date(dat$Dates) %in% frozen_testing_dates)
    window_name <- paste0("window_", format(as.Date(row$endpoint_session_date), "%Y%m%d"))
    cache <- list(windows = list(data = dat, windows = setNames(list(list(
      window_name = window_name, estimation_rows = formation_rows,
      testing_rows = testing_rows
    )), window_name)))
    key <- paste(row$endpoint_id, sprintf("rank_%02d", row$primary_rank), row$pair_id, sep = "__")
    path <- file.path(cache_dir, paste0(gsub("[^A-Za-z0-9_.-]", "_", key), ".rds"))
    saveRDS(cache, path, version = 3)
    manifest[[i]] <- data.frame(
      task_key = key, Pair = row$pair_id, Session_Date = as.Date(row$endpoint_session_date),
      window_identifier = window_name,
      endpoint_id = row$endpoint_id, official_rank = row$primary_rank,
      Formation_Start = as.POSIXct(min(dat$Dates[formation_rows]), tz = "Europe/London"),
      Formation_End = as.POSIXct(max(dat$Dates[formation_rows]), tz = "Europe/London"),
      Testing_Start = as.POSIXct(min(dat$Dates[testing_rows]), tz = "Europe/London"),
      Testing_End = as.POSIXct(max(dat$Dates[testing_rows]), tz = "Europe/London"),
      y_id = "Y", x_id = "X", y_price_column = "Y_Midpoint", x_price_column = "X_Midpoint",
      alpha = row$alpha, beta = row$beta, formation_centre = row$formation_centre,
      spread_object_path_or_identifier = normalizePath(path, winslash = "/", mustWork = TRUE),
      spread_object_sha256 = unname(tools::sha256sum(path)),
      trade_flag = TRUE,
      gh_mode = gh_mode,
      operational_classification = if (gh_mode == "STRICT_INTERIOR") {
        "strict_interior_GHI"
      } else "full_family_OU_GH",
      testing_session_dates = row$testing_session_dates,
      testing_sessions = row$testing_sessions,
      source_manifest_hash = NA_character_, stringsAsFactors = FALSE
    )
  }
  out <- do.call(rbind, manifest)
  hash <- if (requireNamespace("digest", quietly = TRUE)) digest::digest(out, algo = "sha256") else NA_character_
  out$source_manifest_hash <- hash
  out
}

build_strict_interior_gh_task_manifest <- function(selected_schedule,
                                                    prepared_pair_series,
                                                    cache_dir) {
  build_gh_task_manifest(
    selected_schedule, prepared_pair_series, cache_dir,
    gh_mode = "STRICT_INTERIOR"
  )
}

build_full_family_gh_task_manifest <- function(selected_schedule,
                                                prepared_pair_series,
                                                cache_dir) {
  build_gh_task_manifest(
    selected_schedule, prepared_pair_series, cache_dir,
    gh_mode = "FULL_FAMILY"
  )
}
