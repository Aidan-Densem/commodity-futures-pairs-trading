OU_GH_PROJECT_VERSION <- "0.1.0-stage0-stage1-foundation"
OU_GH_MODE <- "STRICT_INTERIOR"
OU_GH_CONSTRUCTION <- "GH_DRIVER"
OU_GH_TIME_UNIT <- "active_minute"

OU_GH_GUARDS <- list(
  RUN_UNIT_TESTS = TRUE,
  RUN_SYNTHETIC_CORE = FALSE,
  RUN_BOUNDED_REAL = FALSE,
  RUN_FULL_SELECTED_PAIR_ESTIMATION = FALSE,
  RUN_FULL_SIMULATOR_VALIDATION = FALSE,
  RUN_THRESHOLD_PRODUCTION = FALSE,
  MAX_WORKERS = 4L,
  EXECUTION_AUTHORITY_REQUIRED_FOR_FULL_RUN = "local_user"
)

OU_GH_NUMERICAL_CONTRACT <- list(
  bessel_backend = "Bessel::BesselK_0.7-0_AMOS_TOMS644",
  bessel_scaled = TRUE,
  bessel_reference = "mpmath_1.3.0_project_local_validation_only",
  quadrature_nodes_production_candidate = 24L,
  quadrature_nodes_reference = 64L,
  small_u_threshold = 1e-5,
  phi_zero_tolerance = 1e-13,
  psi_zero_tolerance = 1e-13,
  centred_derivative_tolerance = 1e-8,
  conjugacy_tolerance = 1e-10,
  positive_real_part_tolerance = 1e-11,
  bessel_real_parity_tolerance = 1e-12,
  bessel_complex_reference_tolerance = 5e-11,
  parameter_roundtrip_tolerance = 5e-11,
  density_mass_tolerance = 2e-7,
  package_density_relative_tolerance = 2e-10,
  semigroup_tolerance = 2e-10,
  cumulant_filter_tolerance = 2e-8
)

ou_gh_assert_guards <- function() {
  stopifnot(
    identical(OU_GH_GUARDS$RUN_FULL_SELECTED_PAIR_ESTIMATION, FALSE),
    identical(OU_GH_GUARDS$RUN_FULL_SIMULATOR_VALIDATION, FALSE),
    identical(OU_GH_GUARDS$RUN_THRESHOLD_PRODUCTION, FALSE),
    OU_GH_GUARDS$MAX_WORKERS <= 4L
  )
  invisible(TRUE)
}
