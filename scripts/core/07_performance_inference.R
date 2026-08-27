#!/usr/bin/env Rscript
source(file.path("R", "io_helpers.R")); source(file.path("R", "performance_statistics.R"))
source(file.path("config", "production_config.R"))

performance_statistics_main <- function() {
  repo_assert(identical(Sys.getenv("ALLOW_FULL_PERFORMANCE_INFERENCE"), "TRUE"),
              paste(
                "Set ALLOW_FULL_PERFORMANCE_INFERENCE=TRUE to authorise",
                "the full HAC/bootstrap performance stage."
              ))
  daily <- utils::read.csv(repo_path("output", "backtest", "daily_returns.csv"), stringsAsFactors = FALSE)
  repo_assert(all(c("session_date", "model_label", "return_committed") %in% names(daily)),
              "Daily ledger lacks the performance interface.")
  result <- performance_analysis(daily, production_config$performance)
  root <- repo_path("output", "performance")
  repo_atomic_csv(result$summary, file.path(root, "performance_summary.csv"))
  repo_atomic_csv(result$newey_west_sensitivity, file.path(root, "newey_west_sensitivity.csv"))
  repo_atomic_csv(result$pairwise_hac_return_differences,
                  file.path(root, "pairwise_hac_return_differences.csv"))
  repo_atomic_csv(result$bootstrap_model_intervals, file.path(root, "bootstrap_model_intervals.csv"))
  repo_atomic_csv(result$pairwise_sharpe_contrasts, file.path(root, "pairwise_sharpe_contrasts.csv"))
  repo_atomic_rds(result, file.path(root, "performance_inference.rds")); invisible(result)
}
if (sys.nframe() == 0L) performance_statistics_main()
