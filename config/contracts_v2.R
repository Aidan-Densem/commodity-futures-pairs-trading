PRODUCTION_V2_CONTRACT <- list(
  schema_version = "2.0.0-terminal-quote-cost-aligned-portable",
  quote_quality = list(
    version = "quote_quality_v2.0.0-causal-formation-robust",
    log_spread_mad_multiplier = 8,
    spread_median_multiple = 50,
    spread_empirical_quantile = 0.995,
    tick_floor_multiple = 4,
    price_return_mad_multiplier = 15,
    price_return_quantile = 0.999,
    price_quantile_multiplier = 2,
    price_relative_floor = log(1.25),
    local_reference_observations = 60L,
    local_reference_minimum = 20L,
    close_mid_mad_multiplier = 8,
    close_mid_empirical_quantile = 0.995,
    minimum_clean_formation_quotes = 200L,
    terminal_age_floor_minutes = 5,
    terminal_age_cap_minutes = 30,
    terminal_gap_quantile = 0.995,
    invalid_action = "skip_signal_and_fill",
    terminal_policy = "last_clean_same_contract_same_session_within_formation_frozen_age_else_incomplete"
  ),
  cost_proxy = list(
    version = "cost_proxy_v2.0.0-clean-formation-trimmed-mean",
    primary_method = "clean_formation_trimmed_mean_10pct",
    trim_fraction = 0.10,
    sensitivity_methods = c("legacy_median", "clean_median", "clean_q75", "clean_mean"),
    blocked_validation_split = 0.70,
    minimum_pair_window_observations = 200L,
    minimum_root_observations = 500L,
    fallback_hierarchy = c("pair_window", "fail_closed"),
    testing_pnl_used = FALSE
  ),
  terminal = list(
    version = "terminal_policy_v2.0.0-liquidate-at-horizon",
    production_policy = "liquidate_at_horizon",
    objective_version = "full_horizon_repeated_cycle_expected_net_reward_per_active_minute_v2",
    bridge_objective_version = "renewal_cycle_terminal_liquidation_v2",
    starting_state = "flat",
    ordinary_exit = "fitted_centre_crossing",
    repeated_cycles = TRUE,
    no_same_observation_reversal = TRUE,
    no_entry_reward = 0,
    terminal_cost = "liquidation_half_of_formation_roundtrip_proxy",
    observation_step_active_minutes = 1L
  ),
  capital = list(
    selection_rule = "N_w=min(2,E_w)",
    pair_sleeve_usd = 200000,
    fixed_strategy_budget_usd = 400000,
    variable_commitment = TRUE,
    adf_alpha = 0.05
  ),
  production = list(
    coarse_paths = 250L,
    intermediate_paths = 750L,
    final_paths = 10000L,
    path_batch_size = 250L,
    workers = 4L,
    base_seed = 91001L,
    resume = TRUE,
    overwrite = FALSE
  )
)
