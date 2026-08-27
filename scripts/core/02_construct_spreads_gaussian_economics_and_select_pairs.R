#!/usr/bin/env Rscript
source(file.path("R", "io_helpers.R")); source(file.path("R", "data_contracts.R"))
source(file.path("R", "public_input_paths.R"))
source(file.path("R", "market_data.R")); source(file.path("R", "active_market_clock.R"))
source(file.path("R", "spread_construction.R")); source(file.path("R", "kalman_hedge.R"))
source(file.path("R", "gaussian_ou_estimation.R")); source(file.path("R", "dynamic_ranking.R"))
source(file.path("R", "backtest_engine.R")); source(file.path("R", "formation_candidates.R"))
source(file.path("R", "quote_quality_v2.R")); source(file.path("R", "prospective_cost.R"))
source(file.path("config", "production_config.R"))

dynamic_pair_ranking_main <- function() {
  repo_assert(identical(Sys.getenv("ALLOW_EXPENSIVE_RANKING"), "TRUE"),
              "Set ALLOW_EXPENSIVE_RANKING=TRUE to authorise the rolling candidate census.")
  root <- repo_external_data_root(TRUE); load_common_backtest_engine(.GlobalEnv)
  pair_series <- readRDS(repo_path("output", "prepared", "exact_pair_series.rds"))
  windows <- utils::read.csv(repo_path("output", "prepared", "rolling_windows.csv"), stringsAsFactors = FALSE)
  pairs <- utils::read.csv(repo_candidate_pairs_path(root), stringsAsFactors = FALSE)
  specs <- mab_read_contract_specs(file.path(root, "contract_specs.csv"))
  bfix <- mab_read_bfix(file.path(root, "bfix.xlsx"))
  fees <- mab_fee_configuration(
    1, mab_read_fee_table(repo_fee_schedule_path(root)), TRUE, TRUE, FALSE
  )
  candidates <- build_formation_candidate_metrics(windows, pairs, pair_series, specs, bfix, fees,
                                                   production_config)
  ranking <- rank_adf05_top_two(candidates, production_config$pair_sleeve_usd)
  repo_atomic_csv(candidates, repo_path("output", "ranking", "formation_candidate_metrics.csv"))
  repo_atomic_csv(ranking, repo_path("output", "ranking", "selected_schedule.csv"))
  invisible(ranking)
}
if (sys.nframe() == 0L) dynamic_pair_ranking_main()
