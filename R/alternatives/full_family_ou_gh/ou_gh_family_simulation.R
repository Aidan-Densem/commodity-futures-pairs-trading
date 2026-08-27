ou_gh_gamma_ou_remainder_exact <- function(n, activity, rate, kappa, Delta,
                                            uniforms = NULL) {
  n <- as.integer(n)
  stopifnot(n >= 1L, activity > 0, rate > 0, kappa > 0, Delta > 0)
  # Qu-Dassios-Zhao decomposition: an endpoint gamma component plus a
  # finite compound-Poisson correction whose log-rate coordinate is triangular.
  base <- stats::rgamma(n, shape = activity * Delta,
    rate = rate * exp(kappa * Delta))
  count <- stats::rpois(n, lambda = activity * kappa * Delta^2 / 2)
  correction <- numeric(n)
  for (j in which(count > 0L)) {
    u <- stats::runif(count[[j]])
    random_rate <- rate * exp(kappa * Delta * sqrt(u))
    correction[[j]] <- sum(stats::rexp(count[[j]], rate = random_rate))
  }
  base + correction
}

ou_gh_vg_remainder_exact <- function(n, fit, Delta = 1, seed = NULL) {
  ou_gh_family_validate_fit(fit, "VG_boundary")
  if (!is.null(seed)) set.seed(as.integer(seed)[[1L]])
  p <- ou_gh_family_to_canonical_gig(fit, "VG_boundary")
  alpha <- sqrt(p[["psi"]] + p[["beta"]]^2)
  positive_rate <- alpha - p[["beta"]]
  negative_rate <- alpha + p[["beta"]]
  positive <- ou_gh_gamma_ou_remainder_exact(
    n, p[["lambda"]], positive_rate, fit[["kappa"]], Delta
  )
  negative <- ou_gh_gamma_ou_remainder_exact(
    n, p[["lambda"]], negative_rate, fit[["kappa"]], Delta
  )
  drift_weight <- -expm1(-fit[["kappa"]] * Delta) / fit[["kappa"]]
  ou_gh_family_centring(fit, "VG_boundary", TRUE) * drift_weight +
    positive - negative
}

ou_gh_family_fft_frequency_grid <- function(n, dx) {
  index <- c(0:(n / 2L - 1L), (-n / 2L):(-1L))
  2 * pi * index / (n * dx)
}

ou_gh_family_scale_hint <- function(fit, Delta = 1,
                                     regime = fit[["regime"]]) {
  status <- ou_gh_family_moment_status(fit, regime)
  if (status$variance_exists) {
    value <- ou_gh_family_remainder_moments(Delta, fit, regime, 2L)[["kappa2"]]
    return(sqrt(value))
  }
  seed <- sum(utf8ToInt(regime)) + 16082026L
  direct <- ou_gh_family_direct_draw(20000L, fit, regime, seed)
  robust <- diff(stats::quantile(direct, c(.25, .75), names = FALSE, type = 8)) / 1.349
  attenuation <- sqrt(-expm1(-2 * fit[["kappa"]] * Delta) /
    (2 * fit[["kappa"]]))
  max(robust * attenuation, .Machine$double.eps^0.25)
}

ou_gh_family_fft_table_once <- function(fit, Delta, n, left_range,
                                         right_range, scale,
                                         quadrature_nodes = 48L) {
  x_min <- -left_range * scale
  x_max <- right_range * scale
  dx <- (x_max - x_min) / n
  x <- x_min + (0:(n - 1L)) * dx
  u <- ou_gh_family_fft_frequency_grid(n, dx)
  phi <- exp(ou_gh_family_remainder_log_cf(
    u, Delta, fit, quadrature_nodes = quadrature_nodes
  ))
  transformed <- phi * exp(-1i * u * x_min)
  du <- 2 * pi / (n * dx)
  density_raw <- Re(stats::fft(transformed)) * du / (2 * pi)
  density_scale <- max(density_raw)
  negative_relative_floor <- min(density_raw) /
    max(density_scale, .Machine$double.xmin)
  density <- pmax(density_raw, 0)
  cdf <- c(0, cumsum((density[-n] + density[-1L]) * dx / 2))
  mass <- tail(cdf, 1L)
  cdf <- cdf / mass
  boundary_ratio <- max(density[c(1L, n)]) /
    max(max(density), .Machine$double.xmin)
  keep <- is.finite(cdf) & cdf >= 0 & cdf <= 1
  indices <- which(keep)
  indices <- seq.int(min(indices), max(indices))
  strict <- c(TRUE, diff(cdf[indices]) > 0)
  indices <- indices[strict]
  list(x = x, density = density, density_raw = density_raw, cdf = cdf,
    supported_indices = indices, probability_support = range(cdf[indices]),
    dx = dx, du = du, mass_before_normalisation = mass,
    boundary_density_ratio = boundary_ratio,
    negative_density_relative_floor = negative_relative_floor,
    endpoint_cf_modulus = max(Mod(phi[c(n/2L, n/2L+1L)])),
    n = n, scale = scale, left_range = left_range,
    right_range = right_range, Delta = Delta, fit = fit)
}

ou_gh_family_build_fft_table <- function(fit, Delta = 1,
    n = NULL, quadrature_nodes = 48L,
    boundary_tolerance = 2e-5, negative_tolerance = 2e-4) {
  regime <- fit[["regime"]]
  scale <- ou_gh_family_scale_hint(fit, Delta, regime)
  status <- ou_gh_family_moment_status(fit, regime)
  if (is.null(n)) n <- if (status$variance_exists) 16384L else 32768L
  path <- ou_gh_family_path_class(regime)
  base <- if (status$variance_exists) 24 else 120
  left <- right <- base
  if (identical(path$tails, "one_polynomial_one_exponential")) {
    if (fit[["beta"]] > 0) right <- 2 * base else left <- 2 * base
  }
  attempts <- list()
  selected <- NULL
  for (factor in c(1, 2, 4)) {
    one <- ou_gh_family_fft_table_once(
      fit, Delta, n, left * factor, right * factor, scale, quadrature_nodes
    )
    valid <- is.finite(one$mass_before_normalisation) &&
      one$mass_before_normalisation > 0.99 && one$mass_before_normalisation < 1.01 &&
      one$boundary_density_ratio <= boundary_tolerance &&
      one$negative_density_relative_floor >= -negative_tolerance &&
      length(one$supported_indices) >= 1000L
    one$valid <- valid
    one$status <- if (valid) "controlled_exact_remainder_fft_valid" else
      "controlled_exact_remainder_fft_failed"
    attempts[[length(attempts) + 1L]] <- one
    if (valid) { selected <- one; break }
  }
  if (is.null(selected)) selected <- attempts[[length(attempts)]]
  selected$attempt_summary <- do.call(rbind, lapply(attempts, function(x) data.frame(
    n = x$n, left_range = x$left_range, right_range = x$right_range,
    mass = x$mass_before_normalisation,
    boundary_density_ratio = x$boundary_density_ratio,
    negative_density_relative_floor = x$negative_density_relative_floor,
    valid = x$valid, stringsAsFactors = FALSE)))
  selected$fingerprint <- ou_gh_hash_object(list(
    regime = regime, canonical = if (regime == "Gaussian_limit") fit else
      ou_gh_family_to_canonical_gig(fit), kappa = fit[["kappa"]],
    Delta = Delta, n = selected$n, dx = selected$dx,
    quadrature_nodes = quadrature_nodes,
    source = ou_gh_sha256(file.path(
      ou_gh_project_root(), "R", "alternatives", "full_family_ou_gh",
      "ou_gh_family_simulation.R"
    ))))
  class(selected) <- c("ou_gh_family_remainder_table", "list")
  selected
}

ou_gh_family_table_quantile <- function(probability, table) {
  stopifnot(inherits(table, "ou_gh_family_remainder_table"), isTRUE(table$valid))
  probability <- pmin(pmax(as.numeric(probability), .Machine$double.eps),
    1 - .Machine$double.eps)
  idx <- table$supported_indices
  stats::approx(table$cdf[idx], table$x[idx], xout = probability,
    method = "linear", ties = "ordered", rule = 2)$y
}

ou_gh_family_remainder_draw <- function(n, fit, Delta = 1, uniforms = NULL,
                                         table = NULL, seed = NULL) {
  if (!is.null(seed)) set.seed(as.integer(seed)[[1L]])
  if (identical(fit[["regime"]], "Gaussian_limit")) {
    variance <- ou_gh_family_remainder_moments(
      Delta, fit, "Gaussian_limit", 2L)[["kappa2"]]
    if (is.null(uniforms)) return(stats::rnorm(as.integer(n), sd = sqrt(variance)))
    return(stats::qnorm(uniforms, sd = sqrt(variance)))
  }
  if (identical(fit[["regime"]], "VG_boundary") && is.null(uniforms)) {
    return(ou_gh_vg_remainder_exact(n, fit, Delta))
  }
  if (is.null(table)) table <- ou_gh_family_build_fft_table(fit, Delta)
  if (is.null(uniforms)) uniforms <- stats::runif(as.integer(n))
  stopifnot(length(uniforms) == as.integer(n))
  ou_gh_family_table_quantile(uniforms, table)
}

ou_gh_family_simulate_path <- function(n_steps, fit, Delta = 1,
                                       uniforms = NULL, table = NULL,
                                       seed = NULL, x0 = fit[["mu"]]) {
  if (!is.null(seed)) set.seed(as.integer(seed)[[1L]])
  remainder <- ou_gh_family_remainder_draw(
    n_steps, fit, Delta, uniforms, table
  )
  path <- numeric(n_steps + 1L)
  path[[1L]] <- x0
  attenuation <- exp(-fit[["kappa"]] * Delta)
  for (j in seq_len(n_steps)) {
    path[[j + 1L]] <- fit[["mu"]] + attenuation *
      (path[[j]] - fit[["mu"]]) + remainder[[j]]
  }
  path
}

ou_gh_family_gil_pelaez_cdf <- function(x, fit, Delta = 1,
    upper = 500, subdivisions = 2000L, rel.tol = 2e-6) {
  integrand <- function(u) {
    value <- exp(-1i * u * x + ou_gh_family_remainder_log_cf(
      u, Delta, fit, quadrature_nodes = 96L))
    Im(value) / u
  }
  value <- stats::integrate(integrand, lower = 1e-8, upper = upper,
    subdivisions = subdivisions, rel.tol = rel.tol, stop.on.error = FALSE)$value
  0.5 - value / pi
}
