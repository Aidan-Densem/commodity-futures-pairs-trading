# Frozen full-family OU-GH formation-only routing contract reported in the
# dissertation.  This configuration is intentionally distinct from the
# strict-interior transform bank.
FULL_FAMILY_GH_CONTRACT <- list(
  gh_mode = "FULL_FAMILY",
  version = "full_family_gh_report_contract_v2_irregular_active_horizons",
  candidate_models = c(
    "interior_GH", "VG_boundary", "skew_t_boundary",
    "symmetric_Student_t_boundary", "NIG", "hyperbolic",
    "symmetric_GH", "Gaussian_limit"
  ),
  training_fraction = 0.75,
  split_rule = "chronological_complete_segments_else_contiguous_tail",
  ccf_bank = expand.grid(
    horizon = c(1L, 5L, 20L, 60L),
    frequency = c(0.25, 0.60, 1.20),
    instrument = c(0, 0.35),
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  ),
  ccf_objective = list(
    innovation_ecf_weight = 20,
    conditional_prediction_loss_weight = 0.05,
    instrumented_moment_weight = 0.25,
    horizon_scaling_exponent = 0.5
  ),
  horizon_interpretation = "exact_cumulative_observed_active_time_no_interpolation",
  horizon_matching_tolerance = "64_machine_epsilon_times_max_1_abs_horizon",
  router = list(equivalence_absolute = 5e-5, equivalence_relative = 0.05),
  testing_data_used = FALSE,
  monetary_pnl_used = FALSE
)

stopifnot(
  identical(FULL_FAMILY_GH_CONTRACT$gh_mode, "FULL_FAMILY"),
  length(FULL_FAMILY_GH_CONTRACT$candidate_models) == 8L,
  nrow(FULL_FAMILY_GH_CONTRACT$ccf_bank) == 24L,
  identical(
    unname(unlist(FULL_FAMILY_GH_CONTRACT$ccf_objective)),
    c(20, 0.05, 0.25, 0.5)
  )
)
