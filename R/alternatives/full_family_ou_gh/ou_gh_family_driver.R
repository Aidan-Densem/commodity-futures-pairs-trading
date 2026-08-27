ou_gh_double_factorial <- function(n) {
  n <- as.integer(n)
  if (n <= 0L) return(1)
  prod(seq.int(n, 1L, by = -2L))
}

ou_gh_mixture_raw_moment <- function(order, beta, w_moment) {
  order <- as.integer(order)
  sum(vapply(0:floor(order / 2), function(j) {
    power_w <- order - j
    choose(order, 2 * j) * ou_gh_double_factorial(2 * j - 1L) *
      beta^(order - 2 * j) * w_moment(power_w)
  }, numeric(1L)))
}

ou_gh_raw_to_cumulants4 <- function(raw) {
  m1 <- raw[[1L]]; m2 <- raw[[2L]]; m3 <- raw[[3L]]; m4 <- raw[[4L]]
  c(kappa1 = m1,
    kappa2 = m2 - m1^2,
    kappa3 = m3 - 3 * m2 * m1 + 2 * m1^3,
    kappa4 = m4 - 4 * m3 * m1 - 3 * m2^2 +
      12 * m2 * m1^2 - 6 * m1^4)
}

ou_gh_family_driver_cumulants <- function(fit, regime = fit[["regime"]] %||% NULL,
                                           maximum_order = 4L) {
  maximum_order <- as.integer(maximum_order)
  stopifnot(maximum_order >= 1L, maximum_order <= 4L)
  if (identical(regime, "Gaussian_limit")) {
    variance <- unname(fit[["driver_variance"]] %||% fit[["sigma_driver"]]^2)
    return(c(kappa1 = 0, kappa2 = variance, kappa3 = 0, kappa4 = 0)[seq_len(maximum_order)])
  }
  p <- ou_gh_family_to_canonical_gig(fit, regime)
  regime <- regime %||% ou_gh_family_regime(
    p[["lambda"]], p[["chi"]], p[["psi"]], p[["beta"]]
  )
  if (identical(regime, "interior_GH")) {
    ans <- gh_driver_cumulants_direct(
      p[["lambda"]], sqrt(p[["psi"]] + p[["beta"]]^2),
      p[["beta"]], sqrt(p[["chi"]]), maximum_order
    )
    return(ans)
  }
  w_moment <- if (identical(regime, "VG_boundary")) {
    function(r) exp(r * log(2 / p[["psi"]]) +
      lgamma(p[["lambda"]] + r) - lgamma(p[["lambda"]]))
  } else {
    a <- -p[["lambda"]]
    function(r) {
      if (r >= a) return(NA_real_)
      exp(r * log(p[["chi"]] / 2) + lgamma(a - r) - lgamma(a))
    }
  }
  raw <- vapply(1:4, function(order) {
    ou_gh_mixture_raw_moment(order, p[["beta"]], w_moment)
  }, numeric(1L))
  if (anyNA(raw)) {
    ans <- rep(NA_real_, 4L)
    names(ans) <- paste0("kappa", 1:4)
    if (isTRUE(ou_gh_family_moment_status(fit, regime)$mean_exists)) ans[[1L]] <- 0
    for (n in 2:4) {
      if (all(is.finite(raw[seq_len(n)]))) {
        ans[seq_len(n)] <- ou_gh_raw_to_cumulants4(c(raw, rep(NA_real_, 4L - length(raw))))[seq_len(n)]
        ans[[1L]] <- 0
      }
    }
    return(ans[seq_len(maximum_order)])
  }
  ans <- ou_gh_raw_to_cumulants4(raw)
  ans[[1L]] <- if (ou_gh_family_moment_status(fit, regime)$mean_exists) 0 else NA_real_
  ans[seq_len(maximum_order)]
}

ou_gh_family_skew_t_raw_log_cf <- function(u, lambda, chi, beta, m) {
  a <- -lambda
  u <- as.complex(u)
  r <- u^2 - 2i * beta * u
  output <- rep(0 + 0i, length(u))
  nz <- Mod(u) > 0
  if (any(nz)) {
    z <- gh_continuous_sqrt(chi * r[nz])
    output[nz] <- 1i * m * u[nz] + log(2) - lgamma(a) +
      (a / 2) * (log(chi / 4) + log(r[nz])) +
      gh_log_bessel_k(a, z, unwrap = FALSE)
  }
  output <- gh_unwrap_log_cf_path(u, output)
  output[!nz] <- 0 + 0i
  output
}

ou_gh_family_driver_log_exponent <- function(u, fit,
                                              regime = fit[["regime"]] %||% NULL,
                                              unwrap = TRUE) {
  if (identical(regime, "Gaussian_limit")) {
    variance <- unname(fit[["driver_variance"]] %||% fit[["sigma_driver"]]^2)
    return(-0.5 * variance * as.complex(u)^2)
  }
  p <- ou_gh_family_to_canonical_gig(fit, regime)
  regime <- regime %||% ou_gh_family_regime(
    p[["lambda"]], p[["chi"]], p[["psi"]], p[["beta"]]
  )
  m <- ou_gh_family_centring(fit, regime, stop_if_unavailable = TRUE)
  u <- as.complex(u)
  if (identical(regime, "interior_GH")) {
    out <- gh_log_cf(u, p[["lambda"]],
      sqrt(p[["psi"]] + p[["beta"]]^2), p[["beta"]],
      sqrt(p[["chi"]]), m = m, centred = TRUE, unwrap = unwrap)
  } else if (identical(regime, "VG_boundary")) {
    q <- p[["psi"]] + u^2 - 2i * p[["beta"]] * u
    out <- 1i * m * u + p[["lambda"]] * (log(p[["psi"]]) - log(q))
    if (isTRUE(unwrap)) out <- gh_unwrap_log_cf_path(u, out)
  } else {
    out <- ou_gh_family_skew_t_raw_log_cf(
      u, p[["lambda"]], p[["chi"]], p[["beta"]], m
    )
  }
  out[Mod(u) == 0] <- 0 + 0i
  out
}

ou_gh_family_driver_cf <- function(u, fit, regime = fit[["regime"]] %||% NULL) {
  exp(ou_gh_family_driver_log_exponent(u, fit, regime))
}

ou_gh_family_direct_draw <- function(n, fit, regime = fit[["regime"]] %||% NULL,
                                     seed = NULL) {
  if (!is.null(seed)) set.seed(as.integer(seed)[[1L]])
  n <- as.integer(n)
  if (identical(regime, "Gaussian_limit")) {
    sd <- sqrt(unname(fit[["driver_variance"]] %||% fit[["sigma_driver"]]^2))
    return(stats::rnorm(n, sd = sd))
  }
  p <- ou_gh_family_to_canonical_gig(fit, regime)
  regime <- regime %||% ou_gh_family_regime(
    p[["lambda"]], p[["chi"]], p[["psi"]], p[["beta"]]
  )
  m <- ou_gh_family_centring(fit, regime, stop_if_unavailable = TRUE)
  w <- if (identical(regime, "VG_boundary")) {
    stats::rgamma(n, shape = p[["lambda"]], rate = p[["psi"]] / 2)
  } else if (regime %in% c("skew_t_boundary", "symmetric_Student_t_boundary")) {
    1 / stats::rgamma(n, shape = -p[["lambda"]], rate = p[["chi"]] / 2)
  } else {
    ou_gh_use_vendor_library()
    GIGrvg::rgig(n, lambda = p[["lambda"]], chi = p[["chi"]], psi = p[["psi"]])
  }
  m + p[["beta"]] * w + sqrt(w) * stats::rnorm(n)
}

ou_gh_vg_cgmy_y0_log_exponent <- function(u, C, G, M, centred = TRUE) {
  u <- as.complex(u)
  out <- C * (log(M) - log(M - 1i * u) + log(G) - log(G + 1i * u))
  if (isTRUE(centred)) out <- out - 1i * u * C * (1 / M - 1 / G)
  out[Mod(u) == 0] <- 0 + 0i
  gh_unwrap_log_cf_path(u, out)
}
