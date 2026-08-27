#!/usr/bin/env Rscript
source(file.path("R", "io_helpers.R"))
source(file.path("R", "data_contracts.R"))
source(file.path("R", "spread_construction.R"))
source(file.path(
  "R", "analyses", "conditional_dynamics_diagnostics.R"
))

run_optional_conditional_dynamics <- function() {
  repo_assert(
    identical(Sys.getenv("ALLOW_OPTIONAL_CONDITIONAL_DYNAMICS"), "TRUE"),
    paste(
      "Set ALLOW_OPTIONAL_CONDITIONAL_DYNAMICS=TRUE to authorise this",
      "standalone formation-only descriptive analysis."
    )
  )
  selected <- utils::read.csv(
    repo_path("output", "ranking", "selected_schedule.csv"),
    stringsAsFactors = FALSE
  )
  paths <- readRDS(repo_path("output", "prepared", "exact_pair_series.rds"))
  result <- build_conditional_dynamics_diagnostics(selected, paths)
  output <- repo_path("output", "optional", "conditional_dynamics")
  dir.create(output, recursive = TRUE, showWarnings = FALSE)
  repo_atomic_rds(result, file.path(output, "conditional_dynamics.rds"))
  repo_atomic_csv(
    result$window_diagnostics, file.path(output, "window_diagnostics.csv")
  )
  repo_atomic_csv(
    result$descriptive_summary, file.path(output, "diagnostic_summary.csv")
  )
  invisible(result)
}

if (sys.nframe() == 0L) run_optional_conditional_dynamics()
