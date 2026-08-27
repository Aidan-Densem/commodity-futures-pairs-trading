ou_gh_huber_regression <- function(x, y, tuning = 1.345, maximum_iterations = 50L) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  valid <- is.finite(x) & is.finite(y)
  x <- x[valid]
  y <- y[valid]
  ou_gh_assert(length(x) >= 30L, "Robust regression has too few pairs.")
  design <- cbind(intercept = 1, slope = x)
  coefficients <- as.numeric(stats::lm.fit(design, y)$coefficients)
  weights <- rep(1, length(y))
  for (iteration in seq_len(maximum_iterations)) {
    residual <- y - as.numeric(design %*% coefficients)
    scale <- 1.4826 * stats::median(abs(residual - stats::median(residual)))
    if (!is.finite(scale) || scale <= .Machine$double.eps) break
    standardised <- residual / scale
    weights <- pmin(1, tuning / pmax(abs(standardised), .Machine$double.eps))
    weighted_design <- design * sqrt(weights)
    weighted_y <- y * sqrt(weights)
    next_coefficients <- as.numeric(stats::lm.fit(
      weighted_design, weighted_y
    )$coefficients)
    if (any(!is.finite(next_coefficients))) break
    if (max(abs(next_coefficients - coefficients)) <= 1e-10 *
        max(1, max(abs(coefficients)))) {
      coefficients <- next_coefficients
      break
    }
    coefficients <- next_coefficients
  }
  residual <- y - as.numeric(design %*% coefficients)
  scale <- 1.4826 * stats::median(abs(residual - stats::median(residual)))
  weighted_cross <- crossprod(design * sqrt(weights))
  covariance <- tryCatch(
    solve(weighted_cross) * scale^2,
    error = function(condition) matrix(NA_real_, 2L, 2L)
  )
  list(
    intercept = coefficients[[1L]], slope = coefficients[[2L]],
    residual_scale = scale, covariance = covariance,
    effective_n = sum(weights), n = length(y), iterations = iteration
  )
}

ou_gh_preliminary_profile <- function(
    training_transitions,
    horizons = c(1, 2, 5, 10, 15, 30, 60, 120, 240, 630)
) {
  pairs <- ou_gh_build_horizon_pairs(training_transitions, horizons)
  ou_gh_assert(nrow(pairs) > 0L, "No multi-horizon formation pairs are available.")
  lag_rows <- lapply(horizons, function(horizon) {
    one <- pairs[pairs$horizon == horizon, , drop = FALSE]
    if (nrow(one) < 100L) {
      return(data.frame(
        horizon = horizon, n_pairs = nrow(one), intercept = NA_real_,
        slope = NA_real_, huber_intercept = NA_real_, huber_slope = NA_real_,
        kappa = NA_real_, half_life = NA_real_,
        implied_mu = NA_real_, residual_scale = NA_real_,
        robust_residual_scale = NA_real_,
        slope_se = NA_real_, status = "insufficient_pairs",
        reason = "fewer_than_100_pairs", stringsAsFactors = FALSE
      ))
    }
    result <- tryCatch(
      ou_gh_huber_regression(one$x_previous, one$x_current), error = identity
    )
    if (inherits(result, "error")) {
      return(data.frame(
        horizon = horizon, n_pairs = nrow(one), intercept = NA_real_,
        slope = NA_real_, huber_intercept = NA_real_, huber_slope = NA_real_,
        kappa = NA_real_, half_life = NA_real_,
        implied_mu = NA_real_, residual_scale = NA_real_,
        robust_residual_scale = NA_real_,
        slope_se = NA_real_, status = "regression_failure",
        reason = conditionMessage(result), stringsAsFactors = FALSE
      ))
    }
    design <- cbind(1, one$x_previous)
    ols <- stats::lm.fit(design, one$x_current)
    ols_coefficients <- as.numeric(ols$coefficients)
    ols_residual_scale <- sqrt(sum(ols$residuals^2) /
      max(1, nrow(one) - 2L))
    ols_covariance <- tryCatch(
      solve(crossprod(design)) * ols_residual_scale^2,
      error = function(condition) matrix(NA_real_, 2L, 2L)
    )
    selected_slope <- ols_coefficients[[2L]]
    selected_intercept <- ols_coefficients[[1L]]
    valid_slope <- is.finite(selected_slope) &&
      selected_slope > 0 && selected_slope < 1
    kappa <- if (valid_slope) -log(selected_slope) / horizon else NA_real_
    implied_mu <- if (valid_slope && abs(1 - result$slope) > 1e-6) {
      selected_intercept / (1 - selected_slope)
    } else NA_real_
    data.frame(
      horizon = horizon, n_pairs = nrow(one), intercept = selected_intercept,
      slope = selected_slope, huber_intercept = result$intercept,
      huber_slope = result$slope, kappa = kappa,
      half_life = if (is.finite(kappa) && kappa > 0) log(2) / kappa else NA_real_,
      implied_mu = implied_mu, residual_scale = ols_residual_scale,
      robust_residual_scale = result$residual_scale,
      slope_se = sqrt(ols_covariance[[2L, 2L]]),
      status = if (valid_slope) "valid" else "invalid_mean_reversion_slope",
      reason = if (valid_slope) "" else "slope_not_strictly_inside_0_1",
      stringsAsFactors = FALSE
    )
  })
  lag_profile <- do.call(rbind, lag_rows)
  valid <- lag_profile$status == "valid" & is.finite(lag_profile$kappa) &
    lag_profile$half_life >= OU_GH_PRODUCTION_SUPPORT$half_life_active_minutes[[1L]] &
    lag_profile$half_life <= OU_GH_PRODUCTION_SUPPORT$half_life_active_minutes[[2L]]
  if (any(valid)) {
    reliability <- lag_profile$horizon[valid] * lag_profile$slope[valid]^2 /
      pmax(lag_profile$slope_se[valid], 1e-4)^2
    reliability <- pmin(reliability, stats::quantile(reliability, 0.9, names = FALSE))
    log_kappa <- sum(reliability * log(lag_profile$kappa[valid])) /
      sum(reliability)
    kappa <- exp(log_kappa)
    state_values <- c(
      training_transitions$x_previous, training_transitions$x_current
    )
    mu <- mean(state_values[is.finite(state_values)])
    status <- "preliminary_profile_valid"
  } else {
    kappa <- log(2) / 1500
    mu <- stats::median(c(training_transitions$x_previous,
      training_transitions$x_current))
    status <- "preliminary_kappa_defaulted_no_valid_lag"
  }
  rho_one <- exp(-kappa)
  innovations <- training_transitions$x_current -
    (mu + rho_one * (training_transitions$x_previous - mu))
  sigma_eta_1 <- stats::sd(innovations)
  if (!is.finite(sigma_eta_1) || sigma_eta_1 <= 1e-4) {
    sigma_eta_1 <- stats::sd(innovations)
  }
  sigma_eta_1 <- min(max(sigma_eta_1, 1e-4), 5)
  list(
    mu = min(max(mu, -8), 8), kappa = kappa,
    half_life = log(2) / kappa, sigma_eta_1 = sigma_eta_1,
    status = status, lag_profile = lag_profile,
    profile_hash = ou_gh_hash_object(list(
      mu = mu, kappa = kappa, sigma_eta_1 = sigma_eta_1,
      lag_profile = lag_profile
    ))
  )
}
