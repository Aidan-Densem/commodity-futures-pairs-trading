#!/usr/bin/env Rscript

# Public orchestration in the order of the dissertation methodology.
# This launcher is definition-only unless run_pipeline_main() is called or the
# file is executed directly with the explicit master authority switch enabled.

run_pipeline_main <- function(workers = 4L) {
  if (!identical(Sys.getenv("ALLOW_EXPENSIVE_PIPELINE"), "TRUE")) stop(
    "Set ALLOW_EXPENSIVE_PIPELINE=TRUE only after reading docs/REPRODUCIBILITY.md.",
    call. = FALSE
  )
  stages <- c(
    "00_validate_inputs.R",
    "01_prepare_data_contracts_and_chronology.R",
    "02_construct_spreads_gaussian_economics_and_select_pairs.R",
    "03_compare_levy_families.R",
    "04_fit_strict_interior_ou_gh.R",
    "05_calibrate_trading_thresholds.R",
    "06_run_exact_contract_backtest.R",
    "07_performance_inference.R"
  )
  for (file in stages) {
    sys.source(file.path("scripts", "core", file), envir = .GlobalEnv)
  }

  validate_inputs_main(TRUE)
  Sys.setenv(ALLOW_FULL_EXACT_CONTRACT_PREPARATION = "TRUE")
  prepare_analysis_data_main()
  Sys.setenv(ALLOW_EXPENSIVE_RANKING = "TRUE")
  dynamic_pair_ranking_main()
  Sys.setenv(ALLOW_LEVY_INPUT_CONSTRUCTION = "TRUE")
  levy_screen_main("prepare")
  levy_screen_main("audit")
  Sys.setenv(ALLOW_EXPENSIVE_LEVY_SCREEN = "TRUE")
  levy_screen_main("fit", workers)
  levy_screen_main("validate")
  levy_screen_main("aggregate")
  Sys.setenv(ALLOW_EXPENSIVE_GHI_FIT = "TRUE")
  fit_production_models_main(workers)
  Sys.setenv(
    ALLOW_EXPENSIVE_THRESHOLDS = "TRUE",
    ALLOW_EXPENSIVE_COMPLETE_EPISODE_THRESHOLDS = "TRUE"
  )
  calibrate_trading_thresholds_main("all")
  Sys.setenv(ALLOW_FULL_EMPIRICAL_BACKTEST = "TRUE")
  run_backtest_main()
  Sys.setenv(ALLOW_FULL_PERFORMANCE_INFERENCE = "TRUE")
  performance_statistics_main()
  invisible(TRUE)
}

if (sys.nframe() == 0L) run_pipeline_main()
