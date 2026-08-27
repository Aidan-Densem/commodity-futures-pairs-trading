# Exact-transition Gaussian OU simulator for finite-horizon comparability.

v2_gaussian_ou_parameters <- function(mu, kappa, stationary_sd = NA_real_, sigma_eta_1 = NA_real_) {
  values <- c(mu = as.numeric(mu), kappa = as.numeric(kappa),
              stationary_sd = as.numeric(stationary_sd), sigma_eta_1 = as.numeric(sigma_eta_1))
  v2_assert(is.finite(values[["mu"]]) && is.finite(values[["kappa"]]) && values[["kappa"]] > 0,
            "Gaussian OU mu and kappa must be finite; kappa must be positive.")
  if (!is.finite(values[["stationary_sd"]]) && is.finite(values[["sigma_eta_1"]])) {
    values[["stationary_sd"]] <- values[["sigma_eta_1"]] / sqrt(-expm1(-2 * values[["kappa"]]))
  }
  v2_assert(is.finite(values[["stationary_sd"]]) && values[["stationary_sd"]] > 0,
            "Gaussian OU stationary scale is unavailable.")
  values
}

v2_simulate_gaussian_ou_exact <- function(active_time, x0, n_paths, seed, parameters) {
  active_time <- as.numeric(active_time)
  v2_assert(length(active_time) >= 2L && all(diff(active_time) > 0), "Invalid active-time grid.")
  p <- parameters
  paths <- matrix(NA_real_, nrow = length(active_time), ncol = as.integer(n_paths))
  paths[1L, ] <- as.numeric(x0)
  set.seed(as.integer(seed))
  for (i in seq_len(length(active_time) - 1L)) {
    dt <- active_time[[i + 1L]] - active_time[[i]]
    rho <- exp(-p[["kappa"]] * dt)
    conditional_sd <- p[["stationary_sd"]] * sqrt(-expm1(-2 * p[["kappa"]] * dt))
    paths[i + 1L, ] <- p[["mu"]] + rho * (paths[i, ] - p[["mu"]]) +
      conditional_sd * stats::rnorm(n_paths)
  }
  list(
    paths = paths, active_time = active_time, seed = as.integer(seed),
    method = "exact_Gaussian_OU_transition",
    simulator_hash = v2_hash_object(list(parameters = parameters, active_time = active_time)),
    testing_data_used = FALSE
  )
}

v2_gaussian_simulator <- function(parameters) {
  list(
    family = "Gaussian_OU_finite_horizon",
    parameters = parameters,
    method = "exact_Gaussian_OU_transition",
    simulator_hash = v2_hash_object(list(version = "gaussian_finite_horizon_v2.0.0", parameters = parameters)),
    simulate_paths = function(active_time, x0, n_paths, seed) {
      v2_simulate_gaussian_ou_exact(active_time, x0, n_paths, seed, parameters)
    }
  )
}

v2_validate_gaussian_moments <- function(seed = 9811L, n_paths = 50000L) {
  p <- v2_gaussian_ou_parameters(mu = 0.25, kappa = 0.015, stationary_sd = 0.08)
  x0 <- -0.10; dt <- 7
  sim <- v2_simulate_gaussian_ou_exact(c(0, dt), x0, n_paths, seed, p)$paths[2L, ]
  rho <- exp(-p[["kappa"]] * dt)
  exact_mean <- p[["mu"]] + rho * (x0 - p[["mu"]])
  exact_variance <- p[["stationary_sd"]]^2 * (1 - rho^2)
  data.frame(
    test = c("conditional_mean", "conditional_variance"),
    theoretical = c(exact_mean, exact_variance),
    empirical = c(mean(sim), stats::var(sim)),
    absolute_error = c(abs(mean(sim) - exact_mean), abs(stats::var(sim) - exact_variance)),
    tolerance = c(5 * sqrt(exact_variance / n_paths), 0.03 * exact_variance),
    passed = c(abs(mean(sim) - exact_mean) <= 5 * sqrt(exact_variance / n_paths),
               abs(stats::var(sim) - exact_variance) <= 0.03 * exact_variance),
    stringsAsFactors = FALSE
  )
}

