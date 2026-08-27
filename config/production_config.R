production_config <- list(
  timezone = "Europe/London",
  formation_sessions = 20L,
  testing_sessions = 10L,
  step_sessions = 1L,
  pair_sleeve_usd = 200000,
  maximum_pairs_per_endpoint = 2L,
  adf_alpha = 0.05,
  quote_rule = list(
    minimum_clean_events = 200L,
    minimum_sessions_with_60_clean = 18L,
    minimum_conditional_clean_share = 0.90
  ),
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
    minimum_clean_formation_quotes = 200L
  ),
  cost_proxy = list(
    version = "cost_proxy_v2.0.0-clean-formation-trimmed-mean",
    trim_fraction = 0.10,
    primary_method = "clean_formation_trimmed_mean_10pct"
  ),
  ranking_weights = c(half_life = 0.5, cost_adjusted_opportunity = 0.5),
  integer_sizing = list(
    maximum_gross_notional_overshoot = 0.05,
    maximum_normalised_hedge_error = 0.25
  ),
  explicit_fees = list(
    brokerage_usd_per_contract_side = 1,
    apply_exchange_fees = TRUE,
    apply_clearing_fees = TRUE,
    apply_regulatory_fees = FALSE,
    apply_transaction_taxes = FALSE,
    fx_rule = "latest causally available Bloomberg BFIX"
  ),
  threshold_mc = list(
    coarse_paths = 250L,
    intermediate_paths = 750L,
    final_paths = 10000L,
    path_batch_size = 250L,
    base_seed = 91001L,
    terminal_policy = "liquidate_at_horizon",
    no_entry_reward = 0,
    outside_option = 0,
    outside_option_tolerance = 1e-12
  ),
  gh_branches = list(
    strict_interior = list(
      gh_mode = "STRICT_INTERIOR",
      estimation_base_seed = 202608210L,
      no_boundary_router = TRUE,
      no_fallback_family = TRUE
    ),
    full_family = list(
      gh_mode = "FULL_FAMILY",
      contract = "config/alternatives/full_family_ou_gh/full_family_gh_contract.R"
    )
  ),
  performance = list(
    annualisation = 252L,
    newey_west_lag = 10L,
    newey_west_sensitivity_lags = c(5L, 9L, 20L),
    stationary_bootstrap_replications = 5000L,
    stationary_bootstrap_mean_block = 10L,
    stationary_bootstrap_seed = 20260818L
  ),
  frozen_schedule = list(
    selected_pair_windows = 167L,
    active_endpoints = 96L,
    total_endpoints = 113L,
    schedule_sha256 = "0d37da1d5ad54d87549f612f14dc32034a3db1a9b76bff78aaab0923547cbac2"
  )
)
