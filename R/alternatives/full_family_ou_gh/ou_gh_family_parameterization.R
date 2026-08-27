ou_gh_family_vg_shape_scale_to_fit <- function(mu, kappa, sigma_eta_1,
                                                lambda, rho) {
  stopifnot(is.finite(mu), is.finite(kappa), kappa > 0,
    is.finite(sigma_eta_1), sigma_eta_1 > 0,
    is.finite(lambda), lambda > 0, is.finite(rho), abs(rho) < 1)
  driver_variance <- sigma_eta_1^2 / gh_ou_variance_factor(kappa, 1)
  psi <- 2 * lambda * (1 + rho^2) /
    ((1 - rho^2) * driver_variance)
  alpha <- sqrt(psi / (1 - rho^2))
  beta <- rho * alpha
  m <- -2 * lambda * beta / psi
  list(regime = "VG_boundary", mu = mu, kappa = kappa,
    sigma_eta_1 = sigma_eta_1, lambda = lambda, chi = 0,
    psi = psi, alpha = alpha, beta = beta, delta = 0,
    rho = rho, centred_m = m, driver_variance = driver_variance)
}

ou_gh_family_vg_fit_to_shape_scale <- function(fit) {
  p <- ou_gh_family_to_canonical_gig(fit, "VG_boundary")
  variance <- ou_gh_family_driver_cumulants(fit, "VG_boundary", 2L)[["kappa2"]]
  alpha <- sqrt(p[["psi"]] + p[["beta"]]^2)
  c(mu = fit[["mu"]], kappa = fit[["kappa"]],
    sigma_eta_1 = sqrt(variance * gh_ou_variance_factor(fit[["kappa"]], 1)),
    lambda = p[["lambda"]], rho = p[["beta"]] / alpha)
}

ou_gh_family_skew_t_to_fit <- function(mu, kappa, nu, delta, omega,
                                        symmetric = FALSE) {
  stopifnot(is.finite(mu), is.finite(kappa), kappa > 0,
    is.finite(nu), nu > 0, is.finite(delta), delta > 0,
    is.finite(omega))
  if (isTRUE(symmetric)) omega <- 0
  chi <- delta^2
  beta <- omega / delta
  regime <- if (isTRUE(symmetric) || beta == 0) {
    "symmetric_Student_t_boundary"
  } else "skew_t_boundary"
  fit <- list(regime = regime, mu = mu, kappa = kappa,
    lambda = -nu / 2, chi = chi, psi = 0, alpha = abs(beta),
    beta = beta, delta = delta, nu = nu, omega = omega)
  fit$centred_m <- ou_gh_family_centring(fit, regime)
  fit
}

ou_gh_family_gaussian_fit <- function(mu, kappa, sigma_eta_1) {
  driver_variance <- sigma_eta_1^2 / gh_ou_variance_factor(kappa, 1)
  list(regime = "Gaussian_limit", mu = mu, kappa = kappa,
    sigma_eta_1 = sigma_eta_1, sigma_driver = sqrt(driver_variance),
    driver_variance = driver_variance)
}

ou_gh_family_raw_names <- function(regime) {
  switch(regime,
    interior_GH = c("mu", "log_kappa", "log_sigma_eta_1",
      "lambda", "log_zeta", "atanh_rho"),
    VG_boundary = c("mu", "log_kappa", "log_sigma_eta_1",
      "log_lambda", "atanh_rho"),
    skew_t_boundary = c("mu", "log_kappa", "log_delta",
      "log_nu_minus_2", "omega"),
    symmetric_Student_t_boundary = c("mu", "log_kappa", "log_delta",
      "log_nu_minus_1"),
    Gaussian_limit = c("mu", "log_kappa", "log_sigma_eta_1"),
    stop("Unknown regime.", call. = FALSE)
  )
}

ou_gh_family_raw_to_fit <- function(raw, regime) {
  raw <- setNames(as.numeric(raw), ou_gh_family_raw_names(regime))
  if (any(!is.finite(raw))) stop("Non-finite raw parameter.", call. = FALSE)
  switch(regime,
    interior_GH = {
      fit <- gh_shape_scale_to_direct(
        lambda = raw[["lambda"]], zeta = exp(raw[["log_zeta"]]),
        rho = tanh(raw[["atanh_rho"]]),
        sigma_eta_1 = exp(raw[["log_sigma_eta_1"]]),
        kappa = exp(raw[["log_kappa"]]), mu = raw[["mu"]]
      )
      out <- as.list(fit)
      out$regime <- "interior_GH"
      out$chi <- fit[["delta"]]^2
      out$psi <- fit[["alpha"]]^2 - fit[["beta"]]^2
      out[c("regime", setdiff(names(out), "regime"))]
    },
    VG_boundary = ou_gh_family_vg_shape_scale_to_fit(
      raw[["mu"]], exp(raw[["log_kappa"]]),
      exp(raw[["log_sigma_eta_1"]]), exp(raw[["log_lambda"]]),
      tanh(raw[["atanh_rho"]])
    ),
    skew_t_boundary = ou_gh_family_skew_t_to_fit(
      raw[["mu"]], exp(raw[["log_kappa"]]),
      2 + exp(raw[["log_nu_minus_2"]]), exp(raw[["log_delta"]]),
      raw[["omega"]], symmetric = FALSE
    ),
    symmetric_Student_t_boundary = ou_gh_family_skew_t_to_fit(
      raw[["mu"]], exp(raw[["log_kappa"]]),
      1 + exp(raw[["log_nu_minus_1"]]), exp(raw[["log_delta"]]),
      0, symmetric = TRUE
    ),
    Gaussian_limit = ou_gh_family_gaussian_fit(
      raw[["mu"]], exp(raw[["log_kappa"]]),
      exp(raw[["log_sigma_eta_1"]])
    )
  )
}

ou_gh_family_fit_to_raw <- function(fit, regime = fit[["regime"]]) {
  switch(regime,
    interior_GH = setNames(c(
      fit[["mu"]], log(fit[["kappa"]]), log(fit[["sigma_eta_1"]]),
      fit[["lambda"]], log(fit[["zeta"]]), atanh(fit[["rho"]])
    ), ou_gh_family_raw_names(regime)),
    VG_boundary = {
      p <- ou_gh_family_vg_fit_to_shape_scale(fit)
      setNames(c(p[["mu"]], log(p[["kappa"]]), log(p[["sigma_eta_1"]]),
        log(p[["lambda"]]), atanh(p[["rho"]])),
        ou_gh_family_raw_names(regime))
    },
    skew_t_boundary = setNames(c(
      fit[["mu"]], log(fit[["kappa"]]), log(sqrt(fit[["chi"]])),
      log(-2 * fit[["lambda"]] - 2), fit[["beta"]] * sqrt(fit[["chi"]])
    ), ou_gh_family_raw_names(regime)),
    symmetric_Student_t_boundary = setNames(c(
      fit[["mu"]], log(fit[["kappa"]]), log(sqrt(fit[["chi"]])),
      log(-2 * fit[["lambda"]] - 1)
    ), ou_gh_family_raw_names(regime)),
    Gaussian_limit = setNames(c(
      fit[["mu"]], log(fit[["kappa"]]), log(fit[["sigma_eta_1"]])
    ), ou_gh_family_raw_names(regime)),
    stop("Unknown regime.", call. = FALSE)
  )
}

ou_gh_family_raw_bounds <- function(regime) {
  half_life <- c(30, 100000)
  log_kappa <- sort(log(log(2) / half_life))
  switch(regime,
    interior_GH = list(
      lower = setNames(c(-8, log_kappa[[1L]], log(1e-4), -10,
        log(1e-3), atanh(-0.95)), ou_gh_family_raw_names(regime)),
      upper = setNames(c(8, log_kappa[[2L]], log(5), 10,
        log(1000), atanh(0.95)), ou_gh_family_raw_names(regime))
    ),
    VG_boundary = list(
      lower = setNames(c(-8, log_kappa[[1L]], log(1e-4), log(0.03), atanh(-0.97)),
        ou_gh_family_raw_names(regime)),
      upper = setNames(c(8, log_kappa[[2L]], log(5), log(30), atanh(0.97)),
        ou_gh_family_raw_names(regime))
    ),
    skew_t_boundary = list(
      lower = setNames(c(-8, log_kappa[[1L]], log(1e-4), log(0.02), -20),
        ou_gh_family_raw_names(regime)),
      upper = setNames(c(8, log_kappa[[2L]], log(50), log(98), 20),
        ou_gh_family_raw_names(regime))
    ),
    symmetric_Student_t_boundary = list(
      lower = setNames(c(-8, log_kappa[[1L]], log(1e-4), log(0.02)),
        ou_gh_family_raw_names(regime)),
      upper = setNames(c(8, log_kappa[[2L]], log(50), log(99)),
        ou_gh_family_raw_names(regime))
    ),
    Gaussian_limit = list(
      lower = setNames(c(-8, log_kappa[[1L]], log(1e-4)),
        ou_gh_family_raw_names(regime)),
      upper = setNames(c(8, log_kappa[[2L]], log(5)),
        ou_gh_family_raw_names(regime))
    ),
    stop("Unknown regime.", call. = FALSE)
  )
}

ou_gh_family_parameter_roundtrip_error <- function(fit,
                                                    regime = fit[["regime"]]) {
  raw <- ou_gh_family_fit_to_raw(fit, regime)
  rebuilt <- ou_gh_family_raw_to_fit(raw, regime)
  p1 <- ou_gh_family_to_canonical_gig(fit, regime)
  p2 <- ou_gh_family_to_canonical_gig(rebuilt, regime)
  keys <- names(p1)[is.finite(p1) & is.finite(p2)]
  max(abs(p1[keys] - p2[keys]) / pmax(1, abs(p1[keys])))
}

ou_gh_family_rescale_fit <- function(fit, centre, scale) {
  stopifnot(is.finite(centre), is.finite(scale), scale > 0)
  regime <- fit[["regime"]]
  if (regime == "Gaussian_limit") {
    return(ou_gh_family_gaussian_fit(
      (fit[["mu"]] - centre) / scale, fit[["kappa"]],
      fit[["sigma_eta_1"]] / scale
    ))
  }
  p <- ou_gh_family_to_canonical_gig(fit, regime)
  out <- list(regime = regime, mu = (fit[["mu"]] - centre) / scale,
    kappa = fit[["kappa"]], lambda = p[["lambda"]],
    chi = p[["chi"]] / scale^2, psi = p[["psi"]] * scale^2,
    beta = p[["beta"]] * scale)
  out$delta <- sqrt(out$chi)
  out$alpha <- sqrt(out$psi + out$beta^2)
  out$centred_m <- ou_gh_family_centring(out, regime)
  if (regime == "interior_GH") {
    direct <- gh_direct_to_shape_scale(
      out$lambda, out$alpha, out$beta, out$delta, out$kappa, out$mu)
    for (name in names(direct)) out[[name]] <- direct[[name]]
    out$regime <- regime; out$chi <- out$delta^2
    out$psi <- out$alpha^2 - out$beta^2
  } else if (regime == "VG_boundary") {
    shape <- ou_gh_family_vg_fit_to_shape_scale(out)
    out$sigma_eta_1 <- shape[["sigma_eta_1"]]
    out$rho <- shape[["rho"]]
  } else {
    out$nu <- -2 * out$lambda
    out$omega <- out$beta * sqrt(out$chi)
  }
  out
}

ou_gh_family_unscale_fit <- function(fit, centre, scale) {
  stopifnot(is.finite(centre), is.finite(scale), scale > 0)
  regime <- fit[["regime"]]
  if (regime == "Gaussian_limit") {
    return(ou_gh_family_gaussian_fit(
      fit[["mu"]] * scale + centre, fit[["kappa"]],
      fit[["sigma_eta_1"]] * scale
    ))
  }
  p <- ou_gh_family_to_canonical_gig(fit, regime)
  out <- list(regime = regime, mu = fit[["mu"]] * scale + centre,
    kappa = fit[["kappa"]], lambda = p[["lambda"]],
    chi = p[["chi"]] * scale^2, psi = p[["psi"]] / scale^2,
    beta = p[["beta"]] / scale)
  out$delta <- sqrt(out$chi)
  out$alpha <- sqrt(out$psi + out$beta^2)
  out$centred_m <- ou_gh_family_centring(out, regime)
  if (regime == "interior_GH") {
    direct <- gh_direct_to_shape_scale(out$lambda, out$alpha, out$beta,
      out$delta, out$kappa, out$mu)
    for (name in names(direct)) out[[name]] <- direct[[name]]
    out$regime <- regime; out$chi <- out$delta^2
    out$psi <- out$alpha^2 - out$beta^2
  } else if (regime == "VG_boundary") {
    shape <- ou_gh_family_vg_fit_to_shape_scale(out)
    out$sigma_eta_1 <- shape[["sigma_eta_1"]]
    out$rho <- shape[["rho"]]
  } else {
    out$nu <- -2 * out$lambda
    out$omega <- out$beta * sqrt(out$chi)
  }
  out
}

ou_gh_full_family_truth_bank <- function() {
  interior <- function(id, lambda, zeta, rho, sigma, kappa) {
    fit <- gh_shape_scale_to_direct(lambda, zeta, rho, sigma, kappa, 0)
    out <- as.list(fit)
    out$truth_id <- id
    out$regime <- "interior_GH"
    out$chi <- fit[["delta"]]^2
    out$psi <- fit[["gamma"]]^2
    out[c("truth_id", "regime", setdiff(names(out), c("truth_id", "regime")))]
  }
  canonical_interior <- function(id, lambda, chi, psi, beta, kappa) {
    alpha <- sqrt(psi + beta^2); delta <- sqrt(chi)
    shape <- gh_direct_to_shape_scale(lambda, alpha, beta, delta, kappa, 0)
    out <- as.list(shape)
    out$truth_id <- id; out$regime <- "interior_GH"
    out$lambda <- lambda; out$alpha <- alpha; out$beta <- beta
    out$delta <- delta; out$chi <- chi; out$psi <- psi
    out$centred_m <- ou_gh_family_centring(out, "interior_GH")
    out[c("truth_id", "regime", setdiff(names(out), c("truth_id", "regime")))]
  }
  near_vg_source <- ou_gh_family_vg_shape_scale_to_fit(0, 0.01, 0.12, 1.2, .45)
  near_vg_p <- ou_gh_family_to_canonical_gig(near_vg_source)
  near_skew_source <- ou_gh_family_skew_t_to_fit(0, 0.01, 6, .2, .8, FALSE)
  near_skew_p <- ou_gh_family_to_canonical_gig(near_skew_source)
  bank <- list(
    interior("interior_symmetric", 0.5, 2, 0, 0.12, 0.01),
    interior("interior_positive_skew", -0.5, 1.2, 0.55, 0.10, 0.008),
    interior("interior_negative_skew", 1, 0.8, -0.65, 0.14, 0.015),
    ou_gh_family_vg_shape_scale_to_fit(0, 0.01, 0.12, 1.5, 0),
    ou_gh_family_vg_shape_scale_to_fit(0, 0.008, 0.10, 0.7, 0.55),
    ou_gh_family_vg_shape_scale_to_fit(0, 0.015, 0.14, 3, -0.6),
    ou_gh_family_skew_t_to_fit(0, 0.01, 5, 0.2, 0, TRUE),
    ou_gh_family_skew_t_to_fit(0, 0.01, 1.7, 0.15, 0, TRUE),
    ou_gh_family_skew_t_to_fit(0, 0.008, 6, 0.2, 0.8, FALSE),
    ou_gh_family_skew_t_to_fit(0, 0.015, 7, 0.25, -1, FALSE),
    ou_gh_family_skew_t_to_fit(0, 0.01, 3.2, 0.18, 0.5, FALSE),
    ou_gh_family_gaussian_fit(0, 0.01, 0.12),
    interior("direct_nig_control", -0.5, 2.8, -0.35, 0.11, 0.012),
    interior("direct_hyperbolic_control", 1, 1.6, 0.3, 0.13, 0.009),
    interior("interior_near_gaussian", 8, 80, .05, 0.10, 0.01),
    canonical_interior("interior_near_vg", near_vg_p[["lambda"]],
      1e-6, near_vg_p[["psi"]], near_vg_p[["beta"]], .01),
    canonical_interior("interior_near_skew_t", near_skew_p[["lambda"]],
      near_skew_p[["chi"]], 1e-4, near_skew_p[["beta"]], .01),
    interior("interior_long_half_life_strong_skew", -1.4, .25, .82,
      0.11, log(2) / 5000)
  )
  ids <- c("interior_symmetric", "interior_positive_skew",
    "interior_negative_skew", "vg_symmetric", "vg_positive_skew",
    "vg_negative_skew", "student_finite_variance",
    "student_infinite_variance", "skew_t_positive_finite_variance",
    "skew_t_negative_finite_variance", "skew_t_infinite_variance",
    "gaussian_control", "direct_nig_control", "direct_hyperbolic_control",
    "interior_near_gaussian", "interior_near_vg", "interior_near_skew_t",
    "interior_long_half_life_strong_skew")
  truth_candidates <- c(
    "symmetric_GH", "interior_GH", "interior_GH",
    "VG_boundary", "VG_boundary", "VG_boundary",
    "symmetric_Student_t_boundary", "symmetric_Student_t_boundary",
    "skew_t_boundary", "skew_t_boundary", "skew_t_boundary",
    "Gaussian_limit", "NIG", "hyperbolic", "interior_GH",
    "interior_GH", "interior_GH", "interior_GH"
  )
  for (j in seq_along(bank)) {
    bank[[j]]$truth_id <- ids[[j]]
    bank[[j]]$truth_candidate <- truth_candidates[[j]]
  }
  names(bank) <- ids
  bank
}
