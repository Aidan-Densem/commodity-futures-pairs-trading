#!/usr/bin/env Rscript
source(file.path("R", "io_helpers.R"))
source(file.path("R", "alternatives", "full_family_ou_gh", "repository_adapter.R"))
source(file.path(
  "R", "alternatives", "full_family_ou_gh", "validation",
  "full_family_known_truth_validation.R"
))

run_optional_known_truth_validation <- function() {
  repo_assert(
    identical(Sys.getenv("ALLOW_EXPENSIVE_FULL_FAMILY_KNOWN_TRUTH"), "TRUE"),
    paste(
      "Set ALLOW_EXPENSIVE_FULL_FAMILY_KNOWN_TRUTH=TRUE to authorise the",
      "standalone 18-truth, five-replication ADEMP validation."
    )
  )
  environment <- full_family_gh_environment()
  result <- run_full_family_known_truth_validation(environment)
  output <- repo_path("output", "optional", "full_family_known_truth")
  dir.create(output, recursive = TRUE, showWarnings = FALSE)
  repo_atomic_rds(result, file.path(output, "known_truth_validation.rds"))
  repo_atomic_csv(result$design, file.path(output, "known_truth_design.csv"))
  repo_atomic_csv(
    result$case_results, file.path(output, "known_truth_case_results.csv")
  )
  repo_atomic_csv(
    result$recovery_summary, file.path(output, "known_truth_recovery_summary.csv")
  )
  invisible(result)
}

if (sys.nframe() == 0L) run_optional_known_truth_validation()
