source_files <- c(
  "io_helpers.R", "data_contracts.R", "market_data.R",
  "spread_construction.R", "kalman_hedge.R", "dynamic_ranking.R",
  file.path("alternatives", "finite_horizon_mc", "gaussian_finite_horizon.R"),
  "threshold_objective.R",
  "performance_statistics.R", "backtest_engine.R"
)
for (file in source_files) {
  parsed <- parse(repo_path("R", file))
  smoke_expect(length(parsed) > 0L, paste("Could not parse", file))
}
source(repo_path("config", "production_config.R"), local = TRUE)
source(repo_path("config", "contracts_v2.R"), local = TRUE)
source(repo_path("R", "v2_common.R"), local = TRUE)
source(repo_path("R", "data_contracts.R"), local = TRUE)
source(repo_path("R", "dynamic_ranking.R"), local = TRUE)
source(repo_path("R", "threshold_objective.R"), local = TRUE)
source(repo_path("R", "alternatives", "finite_horizon_mc",
                 "gaussian_finite_horizon.R"), local = TRUE)
smoke_expect(production_config$formation_sessions == 20L, "Formation length changed")
smoke_expect(production_config$testing_sessions == 10L, "Testing length changed")
smoke_expect(production_config$pair_sleeve_usd == 200000, "Sleeve capital changed")
smoke_expect(production_config$maximum_pairs_per_endpoint == 2L, "Breadth changed")
smoke_expect(production_config$threshold_mc$final_paths == 10000L, "MC budget changed")

specification <- data.frame(
  pair_id = "Y_X", formation_endpoint = as.Date("2025-12-15"),
  testing_start = as.POSIXct("2025-12-16", tz = "Europe/London"),
  testing_end = as.POSIXct("2025-12-26", tz = "Europe/London"),
  upper_entry = 1, lower_entry = -1, upper_exit = 0, lower_exit = 0,
  alpha = 0, beta = 1, formation_centre = 0,
  pair_sleeve_usd = 200000, model_label = "G1"
)
smoke_expect(nrow(validate_strategy_spec(specification)) == 1L,
  "Standard strategy-spec validation failed")

candidates <- data.frame(
  endpoint_id = c("e1", "e1", "e1", "e2"),
  endpoint_session_date = as.Date(c("2025-12-15", "2025-12-15", "2025-12-15", "2025-12-16")),
  pair_id = c("A_B", "A_C", "B_C", "A_B"),
  half_life_sessions = c(1, 2, 3, 2),
  cost_adjusted_opportunity = c(3, 2, 1, 2),
  robust_spread_scale = c(3, 2, 1, 2),
  v2_cost_primary = c(1, 1, 1, 1),
  technical_object_valid = TRUE, whole_contract_implementable = TRUE,
  v2_cost_status_valid = TRUE, final_quote_feasibility_pass = TRUE,
  adf05_pass = TRUE
)
ranking <- rank_adf05_top_two(candidates)
smoke_expect(sum(ranking$selected[ranking$endpoint_id == "e1"]) == 2L,
  "Top-two breadth contract failed")
smoke_expect(sum(ranking$selected[ranking$endpoint_id == "e2"]) == 1L,
  "One-eligible-pair variable-breadth contract failed")

candidate <- data.frame(candidate_id = "forced", d_plus = 1, d_minus = 1,
                        c_plus = 0, c_minus = 0)
forced <- v2_path_payoff(c(0, 2, 3), 0:2, candidate, 0, .2, .2)
smoke_expect(isTRUE(forced$forced_terminal_close), "Terminal liquidation was not applied")
smoke_equal(forced$total_reward, -1.2, message = "Terminal liquidation payoff changed")
no_entry <- v2_path_payoff(c(0, .1, -.1), 0:2, candidate, 0, .2, .2)
smoke_equal(no_entry$total_reward, 0, message = "No-entry reward is not zero")

parameters <- v2_gaussian_ou_parameters(0, .01, stationary_sd = .1)
first <- v2_simulate_gaussian_ou_exact(0:4, 0, 10, 1234L, parameters)$paths
second <- v2_simulate_gaussian_ou_exact(0:4, 0, 10, 1234L, parameters)$paths
smoke_equal(first, second, message = "Seeded Gaussian simulation is not reproducible")
cat("CORE_CONTRACTS_PASS\n")
