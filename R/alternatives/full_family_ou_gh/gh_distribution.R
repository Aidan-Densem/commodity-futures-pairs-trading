gh_driver_cumulants_direct <- function(
    lambda, alpha, beta, delta, maximum_order = 4L
) {
  gh_validate_direct_parameters(lambda, alpha, beta, delta, 0)
  maximum_order <- as.integer(maximum_order)
  ou_gh_assert(maximum_order >= 1L && maximum_order <= 4L,
    "The frozen GH cumulant implementation supports orders 1 through 4.")
  gamma <- sqrt(alpha^2 - beta^2)
  cW <- gig_cumulants(lambda, delta^2, gamma^2, 4L)
  values <- c(
    0,
    cW[[1L]] + beta^2 * cW[[2L]],
    3 * beta * cW[[2L]] + beta^3 * cW[[3L]],
    3 * cW[[2L]] + 6 * beta^2 * cW[[3L]] + beta^4 * cW[[4L]]
  )
  values <- values[seq_len(maximum_order)]
  names(values) <- paste0("kappa", seq_along(values))
  values
}

gh_moments <- function(lambda, alpha, beta, delta, m = NULL) {
  if (is.null(m)) m <- gh_centred_location(lambda, alpha, beta, delta)
  gh_validate_direct_parameters(lambda, alpha, beta, delta, m)
  gamma <- sqrt(alpha^2 - beta^2)
  cW <- gig_cumulants(lambda, delta^2, gamma^2, 4L)
  cumulants <- gh_driver_cumulants_direct(
    lambda, alpha, beta, delta, maximum_order = 4L
  )
  mean <- m + beta * cW[[1L]]
  variance <- cumulants[[2L]]
  c(
    mean = mean,
    variance = variance,
    sd = sqrt(variance),
    third_cumulant = cumulants[[3L]],
    fourth_cumulant = cumulants[[4L]],
    skewness = cumulants[[3L]] / variance^(3 / 2),
    excess_kurtosis = cumulants[[4L]] / variance^2
  )
}

gh_log_density <- function(x, lambda, alpha, beta, delta, m = NULL) {
  if (is.null(m)) m <- gh_centred_location(lambda, alpha, beta, delta)
  gh_validate_direct_parameters(lambda, alpha, beta, delta, m)
  x <- as.numeric(x)
  gamma <- sqrt(alpha^2 - beta^2)
  radius2 <- delta^2 + (x - m)^2
  argument <- alpha * sqrt(radius2)
  log_normaliser <- lambda * log(gamma) - 0.5 * log(2 * pi) -
    (lambda - 0.5) * log(alpha) - lambda * log(delta) -
    Re(gh_log_bessel_k(lambda, delta * gamma, unwrap = FALSE))
  log_normaliser + ((lambda - 0.5) / 2) * log(radius2) +
    Re(gh_log_bessel_k(lambda - 0.5, argument, unwrap = FALSE)) +
    beta * (x - m)
}

gh_density <- function(x, lambda, alpha, beta, delta, m = NULL) {
  exp(gh_log_density(x, lambda, alpha, beta, delta, m))
}

gh_draw_direct <- function(
    n, lambda, alpha, beta, delta, m = NULL,
    seed = NULL, gig_backend = c("GIGrvg", "GeneralizedHyperbolic")
) {
  gig_backend <- match.arg(gig_backend)
  if (is.null(m)) m <- gh_centred_location(lambda, alpha, beta, delta)
  gh_validate_direct_parameters(lambda, alpha, beta, delta, m)
  gamma <- sqrt(alpha^2 - beta^2)
  if (!is.null(seed)) set.seed(as.integer(seed)[[1L]])
  W <- if (gig_backend == "GIGrvg") {
    gig_draw_gigrvg(n, lambda, delta^2, gamma^2)
  } else {
    gig_draw_generalized_hyperbolic(n, lambda, delta^2, gamma^2)
  }
  m + beta * W + sqrt(W) * stats::rnorm(as.integer(n))
}
