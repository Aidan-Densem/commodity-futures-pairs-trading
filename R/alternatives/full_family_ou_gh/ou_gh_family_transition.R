ou_gh_family_validate_fit <- function(fit, regime = fit[["regime"]] %||% NULL) {
  stopifnot(is.finite(fit[["mu"]]), is.finite(fit[["kappa"]]), fit[["kappa"]] > 0)
  if (!identical(regime, "Gaussian_limit")) {
    p <- ou_gh_family_to_canonical_gig(fit, regime)
    ou_gh_family_validate_canonical(
      p[["lambda"]], p[["chi"]], p[["psi"]], p[["beta"]]
    )
    if (!ou_gh_family_moment_status(fit, regime)$centred_OU_admissible) {
      stop("Fit is not admissible for a centred OU driver.", call. = FALSE)
    }
  }
  invisible(TRUE)
}

ou_gh_family_remainder_log_cf <- function(u, Delta, fit,
                                           regime = fit[["regime"]] %||% NULL,
                                           quadrature_nodes = 24L) {
  ou_gh_family_validate_fit(fit, regime)
  stopifnot(length(Delta) == 1L, is.finite(Delta), Delta > 0)
  quadrature <- ou_gh_gauss_legendre(quadrature_nodes)
  attenuation <- exp(-fit[["kappa"]] * Delta * quadrature$nodes_unit)
  u <- as.complex(u)
  total <- rep(0 + 0i, length(u))
  for (j in seq_along(attenuation)) {
    total <- total + quadrature$weights_unit[[j]] *
      ou_gh_family_driver_log_exponent(attenuation[[j]] * u, fit, regime)
  }
  out <- Delta * total
  out[Mod(u) == 0] <- 0 + 0i
  out
}

ou_gh_family_conditional_log_cf <- function(u, x0, Delta, fit,
                                             regime = fit[["regime"]] %||% NULL,
                                             quadrature_nodes = 24L) {
  location <- fit[["mu"]] + exp(-fit[["kappa"]] * Delta) *
    (x0 - fit[["mu"]])
  1i * as.complex(u) * location + ou_gh_family_remainder_log_cf(
    u, Delta, fit, regime, quadrature_nodes
  )
}

ou_gh_family_remainder_moments <- function(Delta, fit,
                                            regime = fit[["regime"]] %||% NULL,
                                            maximum_order = 4L) {
  driver <- ou_gh_family_driver_cumulants(fit, regime, maximum_order)
  order <- seq_along(driver)
  factor <- -expm1(-order * fit[["kappa"]] * Delta) /
    (order * fit[["kappa"]])
  out <- driver * factor
  names(out) <- paste0("kappa", order)
  out
}

ou_gh_family_stationary_log_cf <- function(u, fit,
                                            regime = fit[["regime"]] %||% NULL,
                                            quadrature_nodes = 64L) {
  ou_gh_family_validate_fit(fit, regime)
  quadrature <- ou_gh_gauss_legendre(quadrature_nodes)
  z <- quadrature$nodes_unit
  total <- rep(0 + 0i, length(u))
  for (j in seq_along(z)) {
    total <- total + quadrature$weights_unit[[j]] *
      ou_gh_family_driver_log_exponent(z[[j]] * u, fit, regime) / z[[j]]
  }
  out <- total / fit[["kappa"]]
  out[Mod(u) == 0] <- 0 + 0i
  out
}

ou_gh_family_semigroup_error <- function(fit, h1, h2, u,
                                          regime = fit[["regime"]] %||% NULL,
                                          quadrature_nodes = 48L) {
  left <- ou_gh_family_remainder_log_cf(u, h1 + h2, fit, regime, quadrature_nodes)
  right <- ou_gh_family_remainder_log_cf(u, h2, fit, regime, quadrature_nodes) +
    ou_gh_family_remainder_log_cf(
      exp(-fit[["kappa"]] * h2) * u, h1, fit, regime, quadrature_nodes
    )
  max(Mod(left - right))
}

ou_gh_family_stationary_scale <- function(fit,
                                           regime = fit[["regime"]] %||% NULL) {
  status <- ou_gh_family_moment_status(fit, regime)
  if (!status$variance_exists) return(NA_real_)
  variance <- ou_gh_family_driver_cumulants(fit, regime, 2L)[["kappa2"]]
  sqrt(variance / (2 * fit[["kappa"]]))
}

ou_gh_family_positive_definite_min_eigenvalue <- function(fit, points,
    regime = fit[["regime"]] %||% NULL) {
  difference <- outer(points, points, "-")
  phi <- matrix(ou_gh_family_driver_cf(as.vector(difference), fit, regime),
    nrow = length(points))
  min(eigen((phi + Conj(t(phi))) / 2, symmetric = TRUE,
    only.values = TRUE)$values)
}
