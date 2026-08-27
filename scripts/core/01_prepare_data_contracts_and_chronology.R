#!/usr/bin/env Rscript
source(file.path("R", "io_helpers.R")); source(file.path("R", "data_contracts.R"))
source(file.path("R", "public_input_paths.R"))
source(file.path("R", "market_data.R")); source(file.path("R", "active_market_clock.R"))
source(file.path("R", "rolling_windows.R")); source(file.path("R", "exact_contract_roll.R"))
source(file.path("config", "production_config.R"))

prepare_analysis_data_main <- function() {
  repo_assert(identical(Sys.getenv("ALLOW_FULL_EXACT_CONTRACT_PREPARATION"), "TRUE"),
              paste(
                "Set ALLOW_FULL_EXACT_CONTRACT_PREPARATION=TRUE to authorise",
                "the empirical exact-contract/session reconstruction."
              ))
  root <- repo_external_data_root(TRUE)
  quotes <- utils::read.csv(file.path(root, "market_quotes.csv"), stringsAsFactors = FALSE,
                            check.names = FALSE)
  quotes <- prepare_accepted_quotes(quotes)
  pairs <- utils::read.csv(repo_candidate_pairs_path(root), stringsAsFactors = FALSE)
  lifecycle <- utils::read.csv(file.path(root, "contract_lifecycle.csv"), stringsAsFactors = FALSE)
  intervals <- utils::read.csv(file.path(root, "session_intervals.csv"), stringsAsFactors = FALSE)
  lifecycle <- validate_contract_lifecycle(lifecycle)
  intervals <- validate_session_intervals(intervals, production_config$timezone)
  date_range <- range(as.Date(quotes$timestamp, tz = production_config$timezone))
  pair_series <- setNames(vector("list", nrow(pairs)), pairs$pair_id)
  schedules <- windows <- vector("list", nrow(pairs))
  for (i in seq_len(nrow(pairs))) {
    row <- pairs[i, , drop = FALSE]
    schedule <- build_synchronous_pair_roll_schedule(
      lifecycle, row$pair_id, row$y_generic, row$x_generic,
      date_range[[1L]], date_range[[2L]], 5L
    )
    series <- construct_active_exact_pair_series(
      quotes, schedule, row$y_generic, row$x_generic, production_config$timezone
    )
    if (!nrow(series)) next
    series <- add_pair_active_clock(
      series, intervals, intervals, production_config$timezone
    )
    series$y_generic <- row$y_generic; series$x_generic <- row$x_generic
    pair_series[[row$pair_id]] <- series
    schedules[[i]] <- schedule
    windows[[i]] <- build_pair_rolling_session_windows(
      series, production_config$formation_sessions,
      production_config$testing_sessions, production_config$step_sessions
    )
  }
  pair_series <- pair_series[vapply(pair_series, is.data.frame, logical(1L))]
  schedules <- schedules[vapply(schedules, function(x) is.data.frame(x) && nrow(x), logical(1L))]
  windows <- windows[vapply(windows, function(x) is.data.frame(x) && nrow(x), logical(1L))]
  schedule <- if (length(schedules)) do.call(rbind, schedules) else data.frame()
  windows <- if (length(windows)) do.call(rbind, windows) else data.frame()
  repo_assert(length(pair_series) > 0L && nrow(windows) > 0L,
              "Exact-contract preparation produced no pair windows.")
  repo_atomic_rds(quotes, repo_path("output", "prepared", "accepted_quotes.rds"))
  repo_atomic_rds(pair_series, repo_path("output", "prepared", "exact_pair_series.rds"))
  repo_atomic_csv(windows, repo_path("output", "prepared", "rolling_windows.csv"))
  repo_atomic_csv(schedule, repo_path("output", "prepared", "exact_contract_schedule.csv"))
  repo_atomic_csv(intervals, repo_path("output", "prepared", "session_intervals.csv"))
  invisible(list(quotes = quotes, pair_series = pair_series, windows = windows,
                 exact_contract_schedule = schedule, session_intervals = intervals))
}
if (sys.nframe() == 0L) prepare_analysis_data_main()
