if (!exists(".ou_gh_gl_cache", inherits = FALSE)) {
  .ou_gh_gl_cache <- new.env(parent = emptyenv())
}

ou_gh_gauss_legendre <- function(n = 24L) {
  n <- as.integer(n)
  ou_gh_assert(!is.na(n) && n >= 2L, "n must be at least two.")
  key <- as.character(n)
  if (exists(key, envir = .ou_gh_gl_cache, inherits = FALSE)) {
    return(get(key, envir = .ou_gh_gl_cache, inherits = FALSE))
  }
  index <- seq_len(n - 1L)
  beta <- index / sqrt(4 * index^2 - 1)
  jacobi <- matrix(0, nrow = n, ncol = n)
  jacobi[cbind(index, index + 1L)] <- beta
  jacobi[cbind(index + 1L, index)] <- beta
  eig <- eigen(jacobi, symmetric = TRUE)
  ordering <- order(eig$values)
  nodes <- eig$values[ordering]
  vectors <- eig$vectors[, ordering, drop = FALSE]
  result <- list(
    nodes = nodes,
    weights = 2 * vectors[1L, ]^2,
    nodes_unit = (nodes + 1) / 2,
    weights_unit = vectors[1L, ]^2
  )
  assign(key, result, envir = .ou_gh_gl_cache)
  result
}

ou_gh_validate_fit <- function(fit) {
  required <- c("mu", "kappa", "lambda", "alpha", "beta", "delta")
  ou_gh_assert(all(required %in% names(fit)),
    "OU-GH fit lacks required direct coordinates.")
  gh_validate_direct_parameters(
    fit[["lambda"]], fit[["alpha"]], fit[["beta"]], fit[["delta"]], 0
  )
  ou_gh_assert(is.finite(fit[["mu"]]) && is.finite(fit[["kappa"]]) &&
      fit[["kappa"]] > 0,
    "OU centre and kappa are invalid.")
  invisible(TRUE)
}

ou_gh_remainder_log_cf <- function(
    u, Delta, fit, quadrature_nodes = 24L
) {
  ou_gh_validate_fit(fit)
  ou_gh_assert(length(Delta) == 1L && is.finite(Delta) && Delta > 0,
    "Delta must be one positive duration.")
  u <- as.complex(u)
  quadrature <- ou_gh_gauss_legendre(quadrature_nodes)
  attenuation <- exp(-fit[["kappa"]] * Delta * quadrature$nodes_unit)
  total <- rep(0 + 0i, length(u))
  for (index in seq_along(attenuation)) {
    total <- total + quadrature$weights_unit[[index]] *
      gh_driver_log_exponent(attenuation[[index]] * u, fit)
  }
  output <- Delta * total
  output[Mod(u) == 0] <- 0 + 0i
  output
}

ou_gh_remainder_cumulants <- function(
    orders = 1:4, Delta, fit
) {
  ou_gh_validate_fit(fit)
  orders <- as.integer(orders)
  ou_gh_assert(all(orders >= 1L & orders <= 4L),
    "Supported cumulant orders are 1 through 4.")
  driver <- gh_driver_cumulants_direct(
    fit[["lambda"]], fit[["alpha"]], fit[["beta"]], fit[["delta"]],
    maximum_order = max(orders)
  )[orders]
  factor <- -expm1(-orders * fit[["kappa"]] * Delta) /
    (orders * fit[["kappa"]])
  output <- driver * factor
  names(output) <- paste0("kappa", orders)
  output
}

ou_gh_stationary_log_cf <- function(u, fit, quadrature_nodes = 64L) {
  ou_gh_validate_fit(fit)
  u <- as.complex(u)
  quadrature <- ou_gh_gauss_legendre(quadrature_nodes)
  z <- quadrature$nodes_unit
  total <- rep(0 + 0i, length(u))
  for (index in seq_along(z)) {
    total <- total + quadrature$weights_unit[[index]] *
      gh_driver_log_exponent(z[[index]] * u, fit) / z[[index]]
  }
  output <- total / fit[["kappa"]]
  output[Mod(u) == 0] <- 0 + 0i
  output
}

ou_gh_transition_location <- function(x0, Delta, mu, kappa) {
  mu + exp(-kappa * Delta) * (x0 - mu)
}

ou_gh_semigroup_error <- function(
    fit, h1, h2, u, quadrature_nodes = 24L
) {
  left <- ou_gh_remainder_log_cf(u, h1 + h2, fit, quadrature_nodes)
  right <- ou_gh_remainder_log_cf(u, h2, fit, quadrature_nodes) +
    ou_gh_remainder_log_cf(
      exp(-fit[["kappa"]] * h2) * u,
      h1, fit, quadrature_nodes
    )
  max(Mod(left - right))
}
