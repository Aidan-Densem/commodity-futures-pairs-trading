ou_gh_fft_frequency_grid <- function(n, dx) {
  n <- as.integer(n)
  ou_gh_assert(n >= 1024L && n %% 2L == 0L && bitwAnd(n, n - 1L) == 0L,
    "FFT size must be an even power of two of at least 1024.")
  ou_gh_assert(is.finite(dx) && dx > 0, "FFT spacing must be positive.")
  indices <- c(seq.int(0L, n / 2L - 1L), seq.int(-n / 2L, -1L))
  2 * pi * indices / (n * dx)
}

ou_gh_fft_remainder_table <- function(
    fit,
    Delta = 1,
    n = 8192L,
    range_sd = 18,
    quadrature_nodes = 48L,
    boundary_density_ratio_tolerance = 1e-8,
    negative_density_tolerance = 1e-9
) {
  ou_gh_validate_fit(fit)
  ou_gh_assert(is.finite(Delta) && Delta > 0, "Delta must be positive.")
  cumulants <- ou_gh_remainder_cumulants(1:4, Delta, fit)
  sd_value <- sqrt(cumulants[["kappa2"]])
  skewness <- cumulants[["kappa3"]] / sd_value^3
  excess_kurtosis <- cumulants[["kappa4"]] / sd_value^4
  asymmetry <- min(abs(skewness), 4) / 3
  left_sd <- range_sd * (1 + if (skewness < 0) asymmetry else 0)
  right_sd <- range_sd * (1 + if (skewness > 0) asymmetry else 0)
  x_min <- -left_sd * sd_value
  x_max <- right_sd * sd_value
  dx <- (x_max - x_min) / as.integer(n)
  x <- x_min + seq.int(0L, as.integer(n) - 1L) * dx
  u <- ou_gh_fft_frequency_grid(n, dx)
  log_cf <- ou_gh_remainder_log_cf(u, Delta, fit, quadrature_nodes)
  phi <- exp(log_cf)
  transformed <- phi * exp(-1i * u * x_min)
  du <- 2 * pi / (as.integer(n) * dx)
  density <- Re(stats::fft(transformed)) * du / (2 * pi)
  mass_rectangle <- sum(density) * dx
  cdf <- c(0, cumsum((density[-length(density)] + density[-1L]) * dx / 2))
  mass_trapezoid <- tail(cdf, 1L)
  density_scale <- max(density)
  boundary_ratio <- max(abs(density[c(1L, length(density))])) /
    max(density_scale, .Machine$double.xmin)
  negative_floor <- min(density) / max(density_scale, .Machine$double.xmin)
  monotone_tolerance <- negative_density_tolerance *
    max(density_scale, .Machine$double.xmin) * dx
  cdf_decrease <- min(diff(cdf))
  valid <- is.finite(mass_rectangle) && is.finite(mass_trapezoid) &&
    abs(mass_rectangle - 1) <= 1e-10 && abs(mass_trapezoid - 1) <= 1e-7 &&
    boundary_ratio <= boundary_density_ratio_tolerance &&
    negative_floor >= -negative_density_tolerance &&
    cdf_decrease >= -monotone_tolerance
  status <- if (valid) "reference_fft_cf_inversion_valid" else
    "reference_fft_cf_inversion_failed"
  increasing <- c(TRUE, diff(cdf) > 0)
  supported <- increasing & is.finite(cdf) & cdf > 0 & cdf < 1
  ou_gh_assert(sum(supported) >= 100L,
    "FFT table has no sufficiently large monotone supported segment.")
  supported_indices <- which(supported)
  supported_indices <- seq.int(min(supported_indices), max(supported_indices))
  supported_indices <- supported_indices[c(TRUE,
    diff(cdf[supported_indices]) > 0)]
  fingerprint <- ou_gh_hash_object(list(
    fit = fit[c("mu", "kappa", "lambda", "alpha", "beta", "delta")],
    Delta = Delta, n = as.integer(n), range_sd = range_sd,
    quadrature_nodes = as.integer(quadrature_nodes),
    source_hashes = c(
      direct_inversion = ou_gh_sha256(file.path(
        ou_gh_project_root(), "R", "ou_gh_direct_inversion.R"
      )),
      exponent = ou_gh_sha256(file.path(
        ou_gh_project_root(), "R", "ou_gh_transition.R"
      ))
    )
  ))
  structure(list(
    status = status,
    valid = valid,
    fit = fit,
    Delta = Delta,
    n = as.integer(n),
    range_sd = range_sd,
    quadrature_nodes = as.integer(quadrature_nodes),
    x = x,
    density = density,
    cdf = cdf,
    supported_indices = supported_indices,
    probability_support = range(cdf[supported_indices]),
    dx = dx,
    du = du,
    cumulants = cumulants,
    sd = sd_value,
    skewness = skewness,
    excess_kurtosis = excess_kurtosis,
    mass_rectangle = mass_rectangle,
    mass_trapezoid = mass_trapezoid,
    boundary_density_ratio = boundary_ratio,
    negative_density_relative_floor = negative_floor,
    cdf_min_increment = cdf_decrease,
    fingerprint = fingerprint
  ), class = "ou_gh_remainder_table")
}

ou_gh_table_cdf <- function(x, table) {
  ou_gh_assert(inherits(table, "ou_gh_remainder_table") && isTRUE(table$valid),
    "A valid OU-GH remainder table is required.")
  stats::approx(
    table$x, table$cdf, xout = as.numeric(x),
    method = "linear", ties = "ordered", rule = 1
  )$y
}

ou_gh_build_validated_fft_table <- function(
    fit,
    Delta = 1,
    n = 8192L,
    initial_range_sd = 18,
    maximum_range_sd = 34,
    range_increment = 4,
  quadrature_nodes = 48L
) {
  ranges <- seq(initial_range_sd, maximum_range_sd, by = range_increment)
  attempts <- list()
  selected_index <- NA_integer_
  for (index in seq_along(ranges)) {
    attempts[[index]] <- ou_gh_fft_remainder_table(
      fit, Delta, n, ranges[[index]], quadrature_nodes
    )
    if (isTRUE(attempts[[index]]$valid)) {
      selected_index <- index
      break
    }
  }
  ou_gh_assert(!is.na(selected_index),
    paste0("No exact-CF FFT table passed range escalation through ",
      maximum_range_sd, " remainder SDs."))
  selected <- attempts[[selected_index]]
  selected$range_attempts <- data.frame(
    range_sd = ranges[seq_len(selected_index)],
    valid = vapply(attempts[seq_len(selected_index)], function(value) {
      isTRUE(value$valid)
    }, logical(1L)),
    boundary_density_ratio = vapply(
      attempts[seq_len(selected_index)],
      function(value) value$boundary_density_ratio, numeric(1L)
    ),
    stringsAsFactors = FALSE
  )
  selected
}

ou_gh_fft_stationary_table <- function(
    fit,
    n = 8192L,
    range_sd = 18,
    quadrature_nodes = 64L,
    boundary_density_ratio_tolerance = 1e-8,
    negative_density_tolerance = 1e-9
) {
  ou_gh_validate_fit(fit)
  orders <- 1:4
  driver <- gh_driver_cumulants_direct(
    fit[["lambda"]], fit[["alpha"]], fit[["beta"]], fit[["delta"]], 4L
  )
  cumulants <- driver / (orders * fit[["kappa"]])
  cumulants[[1L]] <- 0
  names(cumulants) <- paste0("kappa", orders)
  sd_value <- sqrt(cumulants[["kappa2"]])
  skewness <- cumulants[["kappa3"]] / sd_value^3
  excess_kurtosis <- cumulants[["kappa4"]] / sd_value^4
  asymmetry <- min(abs(skewness), 4) / 3
  left_sd <- range_sd * (1 + if (skewness < 0) asymmetry else 0)
  right_sd <- range_sd * (1 + if (skewness > 0) asymmetry else 0)
  x_min <- -left_sd * sd_value
  x_max <- right_sd * sd_value
  dx <- (x_max - x_min) / as.integer(n)
  x <- x_min + seq.int(0L, as.integer(n) - 1L) * dx
  u <- ou_gh_fft_frequency_grid(n, dx)
  phi <- exp(ou_gh_stationary_log_cf(u, fit, quadrature_nodes))
  transformed <- phi * exp(-1i * u * x_min)
  du <- 2 * pi / (as.integer(n) * dx)
  density <- Re(stats::fft(transformed)) * du / (2 * pi)
  mass_rectangle <- sum(density) * dx
  cdf <- c(0, cumsum((density[-length(density)] + density[-1L]) * dx / 2))
  mass_trapezoid <- tail(cdf, 1L)
  density_scale <- max(density)
  boundary_ratio <- max(abs(density[c(1L, length(density))])) /
    max(density_scale, .Machine$double.xmin)
  negative_floor <- min(density) / max(density_scale, .Machine$double.xmin)
  monotone_tolerance <- negative_density_tolerance *
    max(density_scale, .Machine$double.xmin) * dx
  cdf_decrease <- min(diff(cdf))
  valid <- is.finite(mass_rectangle) && is.finite(mass_trapezoid) &&
    abs(mass_rectangle - 1) <= 1e-10 && abs(mass_trapezoid - 1) <= 1e-7 &&
    boundary_ratio <= boundary_density_ratio_tolerance &&
    negative_floor >= -negative_density_tolerance &&
    cdf_decrease >= -monotone_tolerance
  increasing <- c(TRUE, diff(cdf) > 0)
  supported <- increasing & is.finite(cdf) & cdf > 0 & cdf < 1
  ou_gh_assert(sum(supported) >= 100L,
    "Stationary FFT table has no monotone supported segment.")
  supported_indices <- seq.int(min(which(supported)), max(which(supported)))
  supported_indices <- supported_indices[c(TRUE,
    diff(cdf[supported_indices]) > 0)]
  fingerprint <- ou_gh_hash_object(list(
    type = "stationary", fit = fit[c(
      "mu", "kappa", "lambda", "alpha", "beta", "delta"
    )], n = as.integer(n), range_sd = range_sd,
    quadrature_nodes = as.integer(quadrature_nodes)
  ))
  structure(list(
    status = if (valid) "stationary_fft_cf_inversion_valid" else
      "stationary_fft_cf_inversion_failed",
    valid = valid, fit = fit, Delta = Inf, n = as.integer(n),
    range_sd = range_sd, quadrature_nodes = as.integer(quadrature_nodes),
    x = x, density = density, cdf = cdf,
    supported_indices = supported_indices,
    probability_support = range(cdf[supported_indices]),
    dx = dx, du = du, cumulants = cumulants, sd = sd_value,
    skewness = skewness, excess_kurtosis = excess_kurtosis,
    mass_rectangle = mass_rectangle, mass_trapezoid = mass_trapezoid,
    boundary_density_ratio = boundary_ratio,
    negative_density_relative_floor = negative_floor,
    cdf_min_increment = cdf_decrease, fingerprint = fingerprint
  ), class = "ou_gh_remainder_table")
}

ou_gh_build_validated_stationary_table <- function(
    fit,
    n = 8192L,
    initial_range_sd = 18,
    maximum_range_sd = 34,
    range_increment = 4,
    quadrature_nodes = 64L
) {
  for (range_sd in seq(initial_range_sd, maximum_range_sd, by = range_increment)) {
    table <- ou_gh_fft_stationary_table(
      fit, n, range_sd, quadrature_nodes
    )
    if (isTRUE(table$valid)) return(table)
  }
  stop("No stationary exact-CF FFT table passed range escalation.", call. = FALSE)
}

ou_gh_table_quantile <- function(probability, table) {
  ou_gh_assert(inherits(table, "ou_gh_remainder_table") && isTRUE(table$valid),
    "A valid OU-GH remainder table is required.")
  probability <- as.numeric(probability)
  ou_gh_assert(all(is.finite(probability)) && all(probability > 0 & probability < 1),
    "Probabilities must lie strictly inside (0,1).")
  support <- table$probability_support
  ou_gh_assert(all(probability >= support[[1L]] & probability <= support[[2L]]),
    paste0(
      "Probability falls outside validated inversion support [",
      format(support[[1L]], scientific = TRUE), ", ",
      format(support[[2L]], scientific = TRUE), "]."
    ))
  index <- table$supported_indices
  stats::approx(
    table$cdf[index], table$x[index], xout = probability,
    method = "linear", ties = "ordered", rule = 1
  )$y
}

ou_gh_draw_remainder_table <- function(
    n,
    table,
    seed = NULL,
    uniforms = NULL
) {
  n <- as.integer(n)
  ou_gh_assert(n >= 1L, "n must be positive.")
  if (is.null(uniforms)) {
    if (!is.null(seed)) set.seed(as.integer(seed)[[1L]])
    uniforms <- stats::runif(n)
  } else {
    uniforms <- as.numeric(uniforms)
    ou_gh_assert(length(uniforms) == n, "Uniform vector has the wrong length.")
  }
  values <- ou_gh_table_quantile(uniforms, table)
  attr(values, "uniforms") <- uniforms
  attr(values, "table_fingerprint") <- table$fingerprint
  values
}

ou_gh_gil_pelaez_cdf <- function(
    x,
    fit,
    Delta = 1,
    upper_frequency = NULL,
    subdivisions = 2000L,
    rel_tol = 1e-8,
    quadrature_nodes = 64L
) {
  variance <- ou_gh_remainder_cumulants(2, Delta, fit)[[1L]]
  sd_value <- sqrt(variance)
  if (is.null(upper_frequency)) upper_frequency <- 300 / sd_value
  vapply(as.numeric(x), function(one_x) {
    integrand <- function(u) {
      u <- as.numeric(u)
      output <- numeric(length(u))
      nonzero <- u > 0
      if (any(nonzero)) {
        phi <- exp(ou_gh_remainder_log_cf(
          u[nonzero], Delta, fit, quadrature_nodes
        ))
        output[nonzero] <- Im(exp(-1i * u[nonzero] * one_x) * phi) /
          u[nonzero]
      }
      if (any(!nonzero)) output[!nonzero] <- -one_x
      output
    }
    value <- stats::integrate(
      integrand, 0, upper_frequency, subdivisions = subdivisions,
      rel.tol = rel_tol, stop.on.error = TRUE
    )$value
    0.5 - value / pi
  }, numeric(1L))
}

ou_gh_validate_remainder_table <- function(
    fit,
    Delta = 1,
    probabilities = c(
      1e-6, 1e-5, 1e-4, 1e-3, 0.01, 0.05, 0.25, 0.5,
      0.75, 0.95, 0.99, 0.999, 1 - 1e-4, 1 - 1e-5, 1 - 1e-6
    ),
    n = 8192L,
    reference_n = 16384L,
    range_sd = 18,
    seed = 8042026L,
    monte_carlo_n = 50000L
) {
  table <- ou_gh_build_validated_fft_table(
    fit, Delta, n, range_sd, range_sd + 12, 4, 48L
  )
  reference <- ou_gh_build_validated_fft_table(
    fit, Delta, reference_n, table$range_sd + 4, table$range_sd + 16, 4, 64L
  )
  quantile <- ou_gh_table_quantile(probabilities, table)
  quantile_reference <- ou_gh_table_quantile(probabilities, reference)
  inverse_cdf <- ou_gh_table_cdf(quantile, table)
  uniforms <- (seq_len(monte_carlo_n) - 0.5) / monte_carlo_n
  set.seed(seed)
  uniforms <- sample(uniforms, length(uniforms), replace = FALSE)
  draws <- ou_gh_draw_remainder_table(
    monte_carlo_n, table, uniforms = uniforms
  )
  frequencies <- c(0.1, 0.25, 0.5, 1, 2, 4) / table$sd
  empirical_cf <- vapply(frequencies, function(u) mean(exp(1i * u * draws)),
    complex(1L))
  exact_cf <- exp(ou_gh_remainder_log_cf(
    frequencies, Delta, fit, 64L
  ))
  cf_standard_error <- sqrt(pmax(1 - Mod(exact_cf)^2, 0) / monte_carlo_n)
  density_m1 <- sum(table$x * table$density) * table$dx
  centred_x <- table$x - density_m1
  density_m2 <- sum(centred_x^2 * table$density) * table$dx
  density_m3 <- sum(centred_x^3 * table$density) * table$dx
  density_m4 <- sum(centred_x^4 * table$density) * table$dx
  density_k4 <- density_m4 - 3 * density_m2^2
  data.frame(
    status = if (max(abs(quantile - quantile_reference) / table$sd) <= 0.02 &&
      max(abs(inverse_cdf - probabilities)) <= 2e-5 &&
      max(Mod(empirical_cf - exact_cf) /
        pmax(cf_standard_error, 1 / sqrt(monte_carlo_n))) <= 5) {
      "pass"
    } else "fail",
    Delta = Delta,
    table_n = n,
    reference_n = reference_n,
    probability_min = min(probabilities),
    probability_max = max(probabilities),
    maximum_quantile_difference_sd = max(
      abs(quantile - quantile_reference) / table$sd
    ),
    maximum_inverse_cdf_error = max(abs(inverse_cdf - probabilities)),
    maximum_empirical_cf_standard_errors = max(
      Mod(empirical_cf - exact_cf) /
        pmax(cf_standard_error, 1 / sqrt(monte_carlo_n))
    ),
    mean_error_sd = abs(mean(draws)) / table$sd,
    variance_ratio = stats::var(draws) / table$cumulants[["kappa2"]],
    density_mean_error_sd = abs(density_m1) / table$sd,
    density_variance_relative_error = abs(
      density_m2 / table$cumulants[["kappa2"]] - 1
    ),
    density_third_cumulant_scaled_error = abs(
      density_m3 - table$cumulants[["kappa3"]]
    ) / table$sd^3,
    density_fourth_cumulant_scaled_error = abs(
      density_k4 - table$cumulants[["kappa4"]]
    ) / table$sd^4,
    table_mass_error = abs(table$mass_trapezoid - 1),
    table_boundary_density_ratio = table$boundary_density_ratio,
    table_negative_density_floor = table$negative_density_relative_floor,
    table_fingerprint = table$fingerprint,
    stringsAsFactors = FALSE
  )
}

ou_gh_simulate_path_table <- function(
    n,
    x0,
    fit,
    table,
    seed = NULL,
    uniforms = NULL
) {
  innovations <- ou_gh_draw_remainder_table(n, table, seed, uniforms)
  output <- numeric(n + 1L)
  output[[1L]] <- x0
  attenuation <- exp(-fit[["kappa"]] * table$Delta)
  for (index in seq_len(n)) {
    output[[index + 1L]] <- fit[["mu"]] + attenuation *
      (output[[index]] - fit[["mu"]]) + innovations[[index]]
  }
  attr(output, "table_fingerprint") <- table$fingerprint
  output
}
