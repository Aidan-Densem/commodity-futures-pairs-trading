# Exact conditional Gaussian OU likelihood under irregular active-minute gaps.
gaussian_ou_profile_at_kappa <- function(spread, active_dt, kappa) {
  z0 <- head(spread, -1L); z1 <- tail(spread, -1L); dt <- tail(active_dt, -1L)
  phi <- exp(-kappa * dt); a <- 1 - phi
  variance_factor <- -expm1(-2 * kappa * dt) / (2 * kappa)
  response <- z1 - phi * z0
  mu <- sum(a * response / variance_factor) / sum(a^2 / variance_factor)
  error <- response - a * mu
  sigma2 <- mean(error^2 / variance_factor)
  if (!is.finite(mu) || !is.finite(sigma2) || sigma2 <= 0) {
    return(list(loglik = -Inf, mu = NA_real_, sigma2 = NA_real_))
  }
  variance <- sigma2 * variance_factor
  loglik <- sum(stats::dnorm(error, 0, sqrt(variance), log = TRUE))
  list(loglik = loglik, mu = mu, sigma2 = sigma2)
}

estimate_exact_gaussian_ou <- function(spread, active_dt,
                                       transition_valid = rep(TRUE, length(spread)),
                                       minimum_transitions = 30L,
                                       kappa_bounds = c(1e-8, 10)) {
  spread <- as.numeric(spread); active_dt <- as.numeric(active_dt)
  keep <- is.finite(spread) & is.finite(active_dt)
  spread <- spread[keep]; active_dt <- active_dt[keep]
  transition_valid <- as.logical(transition_valid[keep])
  valid <- transition_valid[-1L] & is.finite(active_dt[-1L]) & active_dt[-1L] > 0
  if (sum(valid) < minimum_transitions) stop("Too few valid exact OU transitions.", call. = FALSE)
  # Consecutive valid transitions are represented as likelihood terms; no AR(1)
  # equal-spacing approximation is used.
  z0 <- head(spread, -1L)[valid]; z1 <- tail(spread, -1L)[valid]; dt <- active_dt[-1L][valid]
  objective <- function(log_kappa) {
    kappa <- exp(log_kappa)
    phi <- exp(-kappa * dt); a <- 1 - phi
    vf <- -expm1(-2 * kappa * dt) / (2 * kappa)
    response <- z1 - phi * z0
    mu <- sum(a * response / vf) / sum(a^2 / vf)
    error <- response - a * mu
    sigma2 <- mean(error^2 / vf)
    if (!is.finite(sigma2) || sigma2 <= 0) return(.Machine$double.xmax / 100)
    -sum(stats::dnorm(error, 0, sqrt(sigma2 * vf), log = TRUE))
  }
  bounds <- log(kappa_bounds)
  opt <- stats::optimize(objective, interval = bounds, tol = 1e-10)
  kappa <- exp(opt$minimum)
  phi <- exp(-kappa * dt); a <- 1 - phi
  vf <- -expm1(-2 * kappa * dt) / (2 * kappa)
  response <- z1 - phi * z0
  mu <- sum(a * response / vf) / sum(a^2 / vf)
  error <- response - a * mu
  sigma2 <- mean(error^2 / vf); sigma <- sqrt(sigma2)
  if (!all(is.finite(c(kappa, mu, sigma, opt$objective))) || sigma <= 0 ||
      opt$minimum - bounds[[1L]] < 1e-6 || bounds[[2L]] - opt$minimum < 1e-6) {
    stop("Exact Gaussian OU optimum is inadmissible or boundary-adjacent.", call. = FALSE)
  }
  data.frame(
    kappa_per_active_minute = kappa, ou_equilibrium = mu,
    gaussian_diffusion_scale = sigma,
    gaussian_stationary_sd = sigma / sqrt(2 * kappa),
    half_life_active_minutes = log(2) / kappa,
    loglik = -opt$objective, valid_ou_transitions = sum(valid),
    exact_irregular_transition = TRUE, stringsAsFactors = FALSE
  )
}

formation_adf05 <- function(spread, minimum_observations = 30L) {
  x <- as.numeric(spread[is.finite(spread)])
  if (length(x) < minimum_observations) {
    return(data.frame(adf_statistic = NA_real_, adf_p_value = NA_real_,
                      adf_test_valid = FALSE, adf05_pass = FALSE))
  }
  if (!requireNamespace("tseries", quietly = TRUE)) stop("Package 'tseries' is required for ADF05.", call. = FALSE)
  lag <- floor((length(x) - 1)^(1 / 3))
  test <- tryCatch(tseries::adf.test(x, k = lag, alternative = "stationary"), error = function(e) NULL)
  if (is.null(test)) return(data.frame(adf_statistic = NA_real_, adf_p_value = NA_real_,
                                       adf_test_valid = FALSE, adf05_pass = FALSE))
  data.frame(adf_statistic = unname(test$statistic), adf_p_value = test$p.value,
             adf_test_valid = is.finite(test$p.value),
             adf05_pass = is.finite(test$p.value) && test$p.value <= 0.05)
}
