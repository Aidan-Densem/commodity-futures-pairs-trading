#!/usr/bin/env Rscript
source(file.path("R", "io_helpers.R")); source(file.path("R", "data_contracts.R"))
source(file.path("R", "public_input_paths.R"))
source(file.path("R", "market_data.R")); source(file.path("R", "spread_construction.R"))
source(file.path("R", "backtest_engine.R")); source(file.path("config", "production_config.R"))
source(file.path("R", "quote_quality_v2.R")); source(file.path("R", "formation_candidates.R"))
source(file.path("R", "prospective_cost.R")); source(file.path("R", "model_route_manifest.R"))

run_backtest_main <- function() {
  repo_assert(identical(Sys.getenv("ALLOW_FULL_EMPIRICAL_BACKTEST"), "TRUE"),
              "Set ALLOW_FULL_EMPIRICAL_BACKTEST=TRUE to authorise the empirical backtest.")
  root <- repo_external_data_root(TRUE); load_common_backtest_engine(.GlobalEnv)
  strategies <- readRDS(repo_path("output", "thresholds", "strategy_specs.rds"))
  pair_series <- readRDS(repo_path("output", "prepared", "exact_pair_series.rds"))
  ranking <- utils::read.csv(
    repo_path("output", "ranking", "selected_schedule.csv"), stringsAsFactors = FALSE
  )
  routes <- utils::read.csv(
    repo_path("output", "thresholds", "model_route_manifest.csv"), stringsAsFactors = FALSE
  )
  validate_model_route_manifest(routes, ranking, unique(routes$model_label))
  specs <- mab_read_contract_specs(file.path(root, "contract_specs.csv"))
  bfix <- mab_read_bfix(file.path(root, "bfix.xlsx"))
  fees <- mab_fee_configuration(
    1, mab_read_fee_table(repo_fee_schedule_path(root)), TRUE, TRUE, FALSE
  )
  results <- monetary_paths <- vector("list", nrow(strategies))
  for (i in seq_len(nrow(strategies))) {
    strategy <- validate_strategy_spec(strategies[i, , drop = FALSE])
    hit <- which(ranking$selected %in% TRUE & ranking$pair_id == strategy$pair_id &
                   as.Date(ranking$endpoint_session_date) == as.Date(strategy$formation_endpoint))
    repo_assert(length(hit) == 1L, "Strategy lacks one selected schedule row.")
    schedule <- ranking[hit, , drop = FALSE]
    pair <- list(
      pair_id = schedule$pair_id[[1L]], y_generic = schedule$y_generic[[1L]],
      x_generic = schedule$x_generic[[1L]]
    )
    path <- pair_series[[as.character(strategy$pair_id)]]
    formation <- path[
      path$timestamp >= as.POSIXct(schedule$formation_start, tz = production_config$timezone) &
        path$timestamp <= as.POSIXct(schedule$formation_end, tz = production_config$timezone),
      , drop = FALSE
    ]
    testing_dates <- mab_decode_testing_session_dates(
      schedule$testing_session_dates[[1L]], schedule$testing_sessions[[1L]]
    )
    testing <- path[
      as.Date(path$timestamp, tz = production_config$timezone) %in% testing_dates,
      , drop = FALSE
    ]
    repo_assert(
      identical(sort(unique(as.Date(testing$timestamp, tz = production_config$timezone))),
                sort(testing_dates)),
      "Backtest path does not contain exactly the frozen testing sessions."
    )
    cleaner <- calibrate_pair_execution_cleaner(
      formation, pair, specs, production_config
    )
    testing <- apply_frozen_pair_quote_cleaner(
      testing, cleaner$y_contract, cleaner$x_contract,
      sample = "testing", formation_history = formation
    )
    monetary <- prepare_midpoint_monetary_data(testing, strategy)
    results[[i]] <- backtest_monetary_spread_threshold_strategy(
      monetary, strategy_threshold_table(strategy), specs, bfix, fees,
      y_generic = if (grepl("1$", pair$y_generic)) pair$y_generic else paste0(pair$y_generic, "1"),
      x_generic = if (grepl("1$", pair$x_generic)) pair$x_generic else paste0(pair$x_generic, "1"),
      pair_committed_capital_usd = strategy$pair_sleeve_usd,
      scenario_id = "monetary_realistic_explicit_roll", include_path = FALSE,
      notional_overshoot_tolerance = production_config$integer_sizing$maximum_gross_notional_overshoot,
      max_normalised_hedge_error = production_config$integer_sizing$maximum_normalised_hedge_error,
      verbose = FALSE
    )
    results[[i]]$settings$model_label <- strategy$model_label
    monetary_paths[[i]] <- monetary
  }
  ledger <- mab_build_selected_schedule_ledger(
    results, strategies, monetary_paths, ranking, routes, pair_series
  )
  repo_atomic_rds(results, repo_path("output", "backtest", "realised_bidask_backtest.rds"))
  repo_atomic_rds(ledger, repo_path("output", "backtest", "daily_committed_capital_ledger.rds"))
  repo_atomic_csv(ledger$ledger, repo_path("output", "backtest", "daily_returns.csv"))
  invisible(list(results = results, ledger = ledger))
}
if (sys.nframe() == 0L) run_backtest_main()
