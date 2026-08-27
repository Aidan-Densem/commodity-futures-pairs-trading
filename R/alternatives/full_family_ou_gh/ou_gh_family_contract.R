OU_GH_FAMILY_REGIMES <- c(
  "interior_GH", "VG_boundary", "skew_t_boundary",
  "symmetric_Student_t_boundary", "Gaussian_limit"
)

ou_gh_family_validate_canonical <- function(lambda, chi, psi, beta = 0,
                                             stop_on_error = TRUE) {
  values <- c(lambda = lambda, chi = chi, psi = psi, beta = beta)
  reasons <- character()
  if (length(values) != 4L || any(!is.finite(values))) {
    reasons <- c(reasons, "nonfinite_or_nonscalar_parameter")
  } else {
    if (chi < 0) reasons <- c(reasons, "chi_negative")
    if (psi < 0) reasons <- c(reasons, "psi_negative")
    if (chi == 0 && psi == 0) reasons <- c(reasons, "chi_psi_both_zero")
    if (lambda > 0 && !(chi >= 0 && psi > 0)) {
      reasons <- c(reasons, "lambda_positive_domain_violation")
    }
    if (lambda == 0 && !(chi > 0 && psi > 0)) {
      reasons <- c(reasons, "lambda_zero_requires_interior")
    }
    if (lambda < 0 && !(chi > 0 && psi >= 0)) {
      reasons <- c(reasons, "lambda_negative_domain_violation")
    }
  }
  valid <- length(reasons) == 0L
  if (!valid && isTRUE(stop_on_error)) {
    stop("Invalid canonical full-family GH/GIG parameters: ",
      paste(unique(reasons), collapse = ";"), call. = FALSE)
  }
  list(valid = valid, reasons = unique(reasons))
}

ou_gh_family_regime <- function(lambda, chi, psi, beta = 0,
                                zero_tolerance = 0) {
  ou_gh_family_validate_canonical(lambda, chi, psi, beta)
  chi_zero <- chi <= zero_tolerance
  psi_zero <- psi <= zero_tolerance
  if (!chi_zero && !psi_zero) return("interior_GH")
  if (chi_zero) return("VG_boundary")
  if (abs(beta) <= zero_tolerance) {
    "symmetric_Student_t_boundary"
  } else {
    "skew_t_boundary"
  }
}

ou_gh_family_to_canonical_gig <- function(fit, regime = fit[["regime"]] %||% NULL) {
  if (is.null(regime)) {
    if (all(c("lambda", "alpha", "beta", "delta") %in% names(fit))) {
      regime <- "interior_GH"
    } else stop("A regime is required for canonical conversion.", call. = FALSE)
  }
  if (identical(regime, "Gaussian_limit")) {
    return(c(lambda = NA_real_, chi = NA_real_, psi = NA_real_,
      beta = 0, m = 0))
  }
  lambda <- unname(fit[["lambda"]])
  beta <- unname(fit[["beta"]] %||% 0)
  chi <- if (!is.null(fit[["chi"]])) unname(fit[["chi"]]) else
    unname(fit[["delta"]])^2
  psi <- if (!is.null(fit[["psi"]])) unname(fit[["psi"]]) else
    unname(fit[["alpha"]])^2 - beta^2
  ou_gh_family_validate_canonical(lambda, chi, psi, beta)
  c(lambda = lambda, chi = chi, psi = psi, beta = beta,
    m = unname(fit[["centred_m"]] %||% fit[["m"]] %||% NA_real_))
}

ou_gh_family_moment_status <- function(fit, regime = fit[["regime"]] %||% NULL) {
  if (identical(regime, "Gaussian_limit")) {
    return(list(
      process_admissible = TRUE, first_absolute_moment = TRUE,
      mean_exists = TRUE, variance_exists = TRUE,
      third_absolute_moment = TRUE, fourth_moment = TRUE,
      centred_OU_admissible = TRUE,
      threshold_moment_contract_admissible = TRUE,
      maximum_absolute_moment_order = Inf
    ))
  }
  p <- ou_gh_family_to_canonical_gig(fit, regime)
  regime <- regime %||% ou_gh_family_regime(
    p[["lambda"]], p[["chi"]], p[["psi"]], p[["beta"]]
  )
  if (regime %in% c("interior_GH", "VG_boundary")) {
    maximum <- Inf
  } else if (identical(regime, "skew_t_boundary")) {
    maximum <- -p[["lambda"]]
  } else {
    maximum <- -2 * p[["lambda"]]
  }
  first <- maximum > 1
  variance <- maximum > 2
  third <- maximum > 3
  fourth <- maximum > 4
  list(
    process_admissible = TRUE,
    first_absolute_moment = first,
    mean_exists = first,
    variance_exists = variance,
    third_absolute_moment = third,
    fourth_moment = fourth,
    centred_OU_admissible = first,
    threshold_moment_contract_admissible = variance,
    maximum_absolute_moment_order = maximum
  )
}

ou_gh_family_centring <- function(fit, regime = fit[["regime"]] %||% NULL,
                                   stop_if_unavailable = FALSE) {
  if (identical(regime, "Gaussian_limit")) return(0)
  p <- ou_gh_family_to_canonical_gig(fit, regime)
  regime <- regime %||% ou_gh_family_regime(
    p[["lambda"]], p[["chi"]], p[["psi"]], p[["beta"]]
  )
  status <- ou_gh_family_moment_status(fit, regime)
  if (!status$centred_OU_admissible) {
    if (isTRUE(stop_if_unavailable)) {
      stop("The selected GH boundary has no finite absolute first moment; centred OU is unavailable.", call. = FALSE)
    }
    return(NA_real_)
  }
  if (identical(regime, "interior_GH")) {
    return(gh_centred_location(
      p[["lambda"]], sqrt(p[["psi"]] + p[["beta"]]^2),
      p[["beta"]], sqrt(p[["chi"]])
    ))
  }
  if (identical(regime, "VG_boundary")) {
    return(-2 * p[["lambda"]] * p[["beta"]] / p[["psi"]])
  }
  if (identical(regime, "symmetric_Student_t_boundary")) return(0)
  a <- -p[["lambda"]]
  -p[["beta"]] * p[["chi"]] / (2 * (a - 1))
}

ou_gh_family_analytic_strip <- function(fit, regime = fit[["regime"]] %||% NULL) {
  if (identical(regime, "Gaussian_limit")) {
    return(list(lower = -Inf, upper = Inf, type = "entire"))
  }
  p <- ou_gh_family_to_canonical_gig(fit, regime)
  regime <- regime %||% ou_gh_family_regime(
    p[["lambda"]], p[["chi"]], p[["psi"]], p[["beta"]]
  )
  if (regime %in% c("interior_GH", "VG_boundary")) {
    alpha <- sqrt(p[["psi"]] + p[["beta"]]^2)
    return(list(lower = -alpha - p[["beta"]],
      upper = alpha - p[["beta"]], type = "two_sided"))
  }
  if (identical(regime, "symmetric_Student_t_boundary")) {
    return(list(lower = 0, upper = 0, type = "none"))
  }
  if (p[["beta"]] > 0) {
    list(lower = -2 * p[["beta"]], upper = 0, type = "one_sided")
  } else {
    list(lower = 0, upper = -2 * p[["beta"]], type = "one_sided")
  }
}

ou_gh_family_path_class <- function(regime) {
  switch(regime,
    interior_GH = list(activity = "infinite", variation = "infinite",
      local_levy_order = "abs_x^-2", tails = "two_sided_exponential"),
    VG_boundary = list(activity = "infinite", variation = "finite",
      local_levy_order = "abs_x^-1", tails = "two_sided_exponential"),
    skew_t_boundary = list(activity = "infinite", variation = "infinite",
      local_levy_order = "abs_x^-2", tails = "one_polynomial_one_exponential"),
    symmetric_Student_t_boundary = list(activity = "infinite", variation = "infinite",
      local_levy_order = "abs_x^-2", tails = "two_sided_polynomial"),
    Gaussian_limit = list(activity = "continuous", variation = "infinite",
      local_levy_order = NA_character_, tails = "Gaussian"),
    stop("Unknown full-family regime.", call. = FALSE)
  )
}
