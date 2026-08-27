OU_GH_CONTINUATION_VERSION <- "0.2.0-production-development"
OU_GH_FOUNDATION_BASELINE_VERSION <- "0.1.0-stage0-stage1-foundation"

OU_GH_PRODUCTION_SUPPORT <- list(
  lambda = c(-10, 10),
  zeta = c(1e-3, 1000),
  rho = c(-0.95, 0.95),
  half_life_active_minutes = c(30, 100000),
  standardised_mu = c(-8, 8),
  sigma_eta_1_relative_to_formation_scale = c(1e-4, 5),
  maximum_dimensionless_frequency_multiplier = 4,
  minimum_bessel_relative_variance_gap = 1e-10
)

OU_GH_QUADRATURE_POLICY <- list(
  coarse_nodes = 12L,
  production_nodes = 24L,
  escalated_nodes = 48L,
  reference_nodes = 64L,
  exponent_absolute_tolerance = 1e-9,
  objective_equivalence_tolerance = 1e-7,
  semigroup_tolerance = 1e-8
)

OU_GH_FULL_RUN_GATE <- list(
  pilot_checkpoint_fraction = 1,
  pilot_admissible_fraction = 0.80,
  maximum_unclassified_failure_fraction = 0,
  require_no_leakage = TRUE,
  require_replay_hash_equality = TRUE,
  preferred_projected_four_core_hours = 24
)

OU_GH_EXECUTION_CONTRACT <- list(
  authority = "local_user",
  full_selected_pair_estimation_authorised = TRUE,
  full_simulator_preflight_authorised = TRUE,
  threshold_optimisation_authorised = FALSE,
  monetary_backtest_authorised = FALSE,
  maximum_physical_workers = 4L,
  base_seed = 8042026L
)

ou_gh_validate_production_support <- function(lambda, zeta, rho, half_life = NULL) {
  values <- c(lambda, zeta, rho)
  ou_gh_assert(length(values) == 3L && all(is.finite(values)),
    "Production support coordinates must be finite scalars.")
  reasons <- character()
  if (lambda < OU_GH_PRODUCTION_SUPPORT$lambda[[1L]] ||
      lambda > OU_GH_PRODUCTION_SUPPORT$lambda[[2L]]) {
    reasons <- c(reasons, "lambda_outside_supported_numerical_region")
  }
  if (zeta < OU_GH_PRODUCTION_SUPPORT$zeta[[1L]] ||
      zeta > OU_GH_PRODUCTION_SUPPORT$zeta[[2L]]) {
    reasons <- c(reasons, "zeta_outside_supported_numerical_region")
  }
  if (rho < OU_GH_PRODUCTION_SUPPORT$rho[[1L]] ||
      rho > OU_GH_PRODUCTION_SUPPORT$rho[[2L]]) {
    reasons <- c(reasons, "rho_outside_supported_numerical_region")
  }
  if (!is.null(half_life) && (
      half_life < OU_GH_PRODUCTION_SUPPORT$half_life_active_minutes[[1L]] ||
      half_life > OU_GH_PRODUCTION_SUPPORT$half_life_active_minutes[[2L]])) {
    reasons <- c(reasons, "half_life_outside_supported_numerical_region")
  }
  list(valid = !length(reasons), reasons = reasons)
}
