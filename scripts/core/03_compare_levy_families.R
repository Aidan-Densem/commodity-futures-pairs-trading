#!/usr/bin/env Rscript

source(file.path("R", "io_helpers.R"))
source(file.path("R", "levy_exact_input.R"))

levy_screen_main <- function(command = c("prepare", "audit", "smoke", "fit", "validate", "aggregate"),
                             workers = 4L) {
  command <- match.arg(command)
  if (command == "prepare") {
    repo_assert(identical(Sys.getenv("ALLOW_LEVY_INPUT_CONSTRUCTION"), "TRUE"),
                paste(
                  "Set ALLOW_LEVY_INPUT_CONSTRUCTION=TRUE to authorise the",
                  "selected-sample transition materialisation."
                ))
    selected <- utils::read.csv(repo_path("output", "ranking", "selected_schedule.csv"),
                                stringsAsFactors = FALSE)
    pair_series <- readRDS(repo_path("output", "prepared", "exact_pair_series.rds"))
    return(invisible(build_exact_transition_likelihood_inputs(selected, pair_series)))
  }
  python <- Sys.getenv("PYTHON", unset = Sys.which("python3"))
  repo_assert(nzchar(python), "Set PYTHON to the documented Python 3.9 environment.")
  if (command == "fit") repo_assert(
    identical(Sys.getenv("ALLOW_EXPENSIVE_LEVY_SCREEN"), "TRUE"),
    "Set ALLOW_EXPENSIVE_LEVY_SCREEN=TRUE to authorise the selected-sample likelihood census."
  )
  status <- system2(python, c(repo_path("python", "exact_transition_engine.py"),
                              "--command", command, "--workers", as.integer(workers)))
  repo_assert(identical(status, 0L), paste("Exact-transition stage failed:", command))
  if (command == "aggregate") {
    input <- repo_path("output", "exact_transition_likelihood_run", "exact_transition_likelihood",
                       "per_window_exact_likelihood_results.csv")
    output <- repo_path("output", "levy_model_selection")
    status <- system2(python, c(repo_path("python", "model_selection.py"),
                                "--input", input, "--output-dir", output))
    repo_assert(identical(status, 0L), "cAIC/cBIC aggregation failed.")
  }
  invisible(TRUE)
}

if (sys.nframe() == 0L) levy_screen_main("audit")
