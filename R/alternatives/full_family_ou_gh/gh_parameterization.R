gh_validate_direct_parameters <- function(
    lambda, alpha, beta, delta, m = 0, stop_on_error = TRUE
) {
  reasons <- character()
  values <- c(lambda, alpha, beta, delta, m)
  if (length(values) != 5L || any(!is.finite(values))) {
    reasons <- c(reasons, "nonfinite_or_nonscalar_parameter")
  }
  if (length(alpha) != 1L || !is.finite(alpha) || alpha <= 0) {
    reasons <- c(reasons, "alpha_not_positive")
  }
  if (length(beta) != 1L || !is.finite(beta) ||
      !is.finite(alpha) || abs(beta) >= alpha) {
    reasons <- c(reasons, "abs_beta_not_strictly_below_alpha")
  }
  if (length(delta) != 1L || !is.finite(delta) || delta <= 0) {
    reasons <- c(reasons, "delta_not_positive")
  }
  valid <- !length(reasons)
  if (!valid && isTRUE(stop_on_error)) {
    stop("Invalid strict GH parameters: ",
      paste(unique(reasons), collapse = ";"), call. = FALSE)
  }
  list(valid = valid, reasons = unique(reasons))
}

gh_ou_variance_factor <- function(kappa, Delta = 1) {
  ou_gh_assert(
    length(kappa) == 1L && is.finite(kappa) && kappa > 0 &&
      length(Delta) == 1L && is.finite(Delta) && Delta > 0,
    "kappa and Delta must be positive finite scalars."
  )
  -expm1(-2 * kappa * Delta) / (2 * kappa)
}

gh_centred_location <- function(lambda, alpha, beta, delta) {
  gh_validate_direct_parameters(lambda, alpha, beta, delta, 0)
  gamma <- sqrt(alpha^2 - beta^2)
  zeta <- delta * gamma
  ratio <- exp(gh_log_bessel_ratio(lambda, 1, zeta))
  -beta * delta / gamma * ratio
}

gh_shape_scale_to_direct <- function(
    lambda, zeta, rho, sigma_eta_1, kappa, mu = 0
) {
  values <- c(lambda, zeta, rho, sigma_eta_1, kappa, mu)
  ou_gh_assert(length(values) == 6L && all(is.finite(values)),
    "Shape-scale inputs must be finite scalars.")
  ou_gh_assert(zeta > 0, "zeta must be positive.")
  ou_gh_assert(abs(rho) < 1, "rho must lie strictly inside (-1,1).")
  ou_gh_assert(sigma_eta_1 > 0 && kappa > 0,
    "sigma_eta_1 and kappa must be positive.")
  ratio_terms <- gh_bessel_ratio_terms(lambda, zeta)
  R1 <- ratio_terms[["R1"]]
  D <- ratio_terms[["D"]]
  variance_shape <- zeta * R1 +
    rho^2 * zeta^2 * D / (1 - rho^2)
  A1 <- gh_ou_variance_factor(kappa, 1)
  s_L <- sigma_eta_1 / sqrt(A1)
  gamma <- sqrt(variance_shape) / s_L
  alpha <- gamma / sqrt(1 - rho^2)
  beta <- rho * alpha
  delta <- zeta / gamma
  m0 <- gh_centred_location(lambda, alpha, beta, delta)
  c(
    mu = mu, kappa = kappa, sigma_eta_1 = sigma_eta_1,
    lambda = lambda, zeta = zeta, rho = rho,
    alpha = alpha, beta = beta, delta = delta, gamma = gamma,
    centred_m = m0, driver_sd = s_L
  )
}

gh_direct_to_shape_scale <- function(
    lambda, alpha, beta, delta, kappa, mu = 0
) {
  gh_validate_direct_parameters(lambda, alpha, beta, delta, 0)
  ou_gh_assert(length(kappa) == 1L && is.finite(kappa) && kappa > 0,
    "kappa must be a positive finite scalar.")
  gamma <- sqrt(alpha^2 - beta^2)
  zeta <- delta * gamma
  rho <- beta / alpha
  cumulants <- gh_driver_cumulants_direct(
    lambda, alpha, beta, delta, maximum_order = 2L
  )
  driver_sd <- sqrt(cumulants[["kappa2"]])
  sigma_eta_1 <- driver_sd * sqrt(gh_ou_variance_factor(kappa, 1))
  c(
    mu = mu, kappa = kappa, sigma_eta_1 = sigma_eta_1,
    lambda = lambda, zeta = zeta, rho = rho,
    alpha = alpha, beta = beta, delta = delta, gamma = gamma,
    centred_m = gh_centred_location(lambda, alpha, beta, delta),
    driver_sd = driver_sd
  )
}
