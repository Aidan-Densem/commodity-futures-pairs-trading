root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
if (!file.exists(file.path(root, "config", "production_config.R"))) {
  stop("Run this script from the repository root.", call. = FALSE)
}
Sys.setenv(ALLOW_EXPENSIVE_PIPELINE = "", ALLOW_EXPENSIVE_RANKING = "",
           ALLOW_EXPENSIVE_LEVY_SCREEN = "", ALLOW_EXPENSIVE_GHI_FIT = "",
           ALLOW_EXPENSIVE_THRESHOLDS = "", ALLOW_FULL_EMPIRICAL_BACKTEST = "",
           ALLOW_FULL_EXACT_CONTRACT_PREPARATION = "",
           ALLOW_LEVY_INPUT_CONSTRUCTION = "",
           ALLOW_FULL_PERFORMANCE_INFERENCE = "",
           ALLOW_OPTIONAL_CONDITIONAL_DYNAMICS = "",
           ALLOW_EXPENSIVE_FULL_FAMILY_GH_FIT = "",
           ALLOW_EXPENSIVE_FULL_FAMILY_GH_THRESHOLDS = "",
           ALLOW_EXPENSIVE_COMPLETE_EPISODE_THRESHOLDS = "",
           ALLOW_EXPENSIVE_FULL_FAMILY_KNOWN_TRUTH = "")
for (file in c(
  "io_helpers.R", "data_contracts.R", "market_data.R", "active_market_clock.R",
  "rolling_windows.R", "spread_construction.R", "kalman_hedge.R",
  "gaussian_ou_estimation.R", "dynamic_ranking.R", "performance_statistics.R",
  "backtest_engine.R", "exact_contract_roll.R", "quote_quality_v2.R",
  "prospective_cost.R", "model_route_manifest.R", "v2_common.R",
  "threshold_mc.R", "levy_exact_input.R"
)) source(file.path(root, "R", file))
source(file.path(root, "R", "formation_candidates.R"))
source(file.path(root, "R", "gh_branch_contract.R"))
source(file.path(root, "R", "alternatives", "full_family_ou_gh",
                 "repository_adapter.R"))
source(file.path(root, "R", "ou_gh_task_construction.R"))
source(file.path(root, "config", "alternatives", "full_family_ou_gh",
                 "full_family_gh_contract.R"))
load_common_backtest_engine(.GlobalEnv)
source(file.path(root, "R", "strategy_specification.R"))
source(file.path(root, "tests", "smoke", "helpers.R"))

tests <- c(
  "test_midprice_spread.R",
  "test_kalman_report_contract.R",
  "test_dynamic_ranking_report_contract.R",
  "test_model_agnostic_backtest.R",
  "test_fee_contract.R",
  "test_strategy_and_ledger.R",
  "test_explicit_roll_accounting.R",
  "test_public_ancillary_contracts.R",
  "test_exact_gaussian_ou.R",
  "test_statistical_contracts.R",
  "test_quote_cleaning_formula_parity.R",
  "test_quote_cleaning_python_contract.R",
  "test_complete_episode_threshold_mc.R",
  "test_source_load_config.R",
  "test_core_contracts.R",
  "test_data_spine_route_and_inference_contracts.R",
  "test_active_time_roll_and_gh_branch_contracts.R",
  "test_full_family_duration_router_and_capital_contracts.R"
)
for (test in tests) {
  cat("Running ", test, "...\n", sep = "")
  sys.source(file.path(root, "tests", "smoke", test), envir = .GlobalEnv)
}
cat("ALL_SMOKE_TESTS_PASS\n")
