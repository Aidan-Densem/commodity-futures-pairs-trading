r_files <- list.files(repo_path(), pattern = "[.]R$", recursive = TRUE, full.names = TRUE)
smoke_expect(all(vapply(r_files, function(path) !inherits(try(parse(path), silent = TRUE), "try-error"), logical(1L))),
             "At least one repository R source does not parse")
source(repo_path("config", "production_config.R"), local = TRUE)
smoke_equal(production_config$explicit_fees$brokerage_usd_per_contract_side, 1,
            message = "Configuration brokerage benchmark changed")
smoke_expect(isFALSE(production_config$explicit_fees$apply_regulatory_fees) &&
               isFALSE(production_config$explicit_fees$apply_transaction_taxes),
             "Configuration enables excluded fees")
script_text <- paste(readLines(repo_path(
  "scripts", "core", "03_compare_levy_families.R"
), warn = FALSE), collapse = "\n")
smoke_expect(!grepl("ldsf_run_|reconstruct|composite", script_text, ignore.case = TRUE),
             "Default Levy entry still exposes reconstruction/composite selection")
pipeline_text <- paste(readLines(repo_path("scripts", "run_pipeline.R"), warn = FALSE), collapse = "\n")
smoke_expect(grepl("ALLOW_EXPENSIVE_PIPELINE", pipeline_text, fixed = TRUE),
             "Master production guard is absent")
producers <- c(
  "exact_pair_series.rds" = "scripts/core/01_prepare_data_contracts_and_chronology.R",
  "formation_candidate_metrics.csv" = "scripts/core/02_construct_spreads_gaussian_economics_and_select_pairs.R",
  "ou_gh_task_manifest.rds" = "scripts/core/04_fit_strict_interior_ou_gh.R",
  "strategy_specs.rds" = "scripts/core/05_calibrate_trading_thresholds.R",
  "model_route_manifest.csv" = "scripts/core/05_calibrate_trading_thresholds.R",
  "daily_returns.csv" = "scripts/core/06_run_exact_contract_backtest.R"
)
smoke_expect(all(vapply(names(producers), function(object) {
  any(grepl(object, readLines(repo_path(producers[[object]]), warn = FALSE), fixed = TRUE))
}, logical(1L))), "An internal pipeline object has no producing stage")

gh_task_text <- paste(readLines(
  repo_path("R", "threshold_task_construction.R"), warn = FALSE
), collapse = "\n")
gh_threshold_text <- paste(readLines(
  repo_path("scripts", "core", "internal", "complete_episode_threshold_stage.R"),
  warn = FALSE
), collapse = "\n")
smoke_expect(
  grepl("task_construction_failure", gh_task_text, fixed = TRUE) &&
    grepl("THRESHOLD_UNAVAILABLE", gh_threshold_text, fixed = TRUE),
  "A GH simulator/threshold-task construction failure can still abort routing"
)

roll_fixture <- data.frame(
  timestamp = as.POSIXct(c("2025-01-01", "2025-01-02"), tz = "Europe/London"),
  global_row_index = 1:2, spread = c(1, 9), y_contract = c("Y1", "Y2"),
  x_contract = c("X1", "X2"), stringsAsFactors = FALSE)
roll_result <- mab_adjust_signal_spread(roll_fixture)
smoke_equal(roll_result$data$adjusted_signal_spread, roll_fixture$spread,
            message = "Undocumented mechanical roll offset remains active")
smoke_expect(roll_result$signal_roll_adjustment_status == "NO_ADJUSTMENT_DEFINED" &&
               roll_result$signal_roll_transition_rule_status == "NO_SIGNAL_TRANSITION_DEFINED",
             "Signal roll status remains implicit")
cat("SOURCE_LOAD_CONFIG_AND_LEVY_ROUTE_PASS\n")
