COMPLETE_EPISODE_MC_CONTRACT <- list(
  schema_version = "2.1.0-complete-episode-adaptive-injective-streams",
  calibration = list(
    policy_version = "complete_episode_policy_v2.0.0-adaptive-no-economic-truncation",
    objective_version = "one_complete_flat_entry_exit_ratio_of_expectations_v1",
    terminal_policy = "complete_episode_no_economic_truncation",
    initial_horizon_role = "numerical_extension_start_only",
    common_random_numbers = TRUE,
    nested_path_budgets = TRUE,
    unresolved_reason = "NUMERICAL_GUARDRAIL_UNRESOLVED",
    nonpositive_reason = "NONPOSITIVE_OBJECTIVE"
  ),
  numerical = list(
    h_max_active_minutes = 100000L,
    extension_multiplier = 2L,
    path_storage = "temporary_rds",
    path_bank_design = "lazy_nested_path_ids_with_verified_per_path_replay",
    path_stream_version = "complete_episode_path_stream_v3_injective_affine",
    guardrail_status = "guardrail_unresolved"
  ),
  provenance = list(
    empirical_results_generated = FALSE,
    testing_data_used = FALSE,
    testing_pnl_used = FALSE
  )
)
