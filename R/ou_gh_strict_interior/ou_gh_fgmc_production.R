if (!exists(".ou_gh_fgmc_cache", inherits = FALSE)) {
  .ou_gh_fgmc_cache <- new.env(parent = emptyenv())
}

ou_gh_fit_vector <- function(row) {
  fields <- c("mu", "kappa", "lambda", "alpha", "beta", "delta")
  value <- as.numeric(row[fields])
  names(value) <- fields
  value
}

ou_gh_standardised_remainder <- function(fit, Delta = 1,
                                          quadrature_nodes = 24L) {
  cumulants <- ou_gh_remainder_cumulants(1:4, Delta, fit)
  variance <- unname(cumulants[["kappa2"]])
  ou_gh_assert(is.finite(variance) && variance > 0,
               "OU-GH remainder variance is invalid.")
  scale <- sqrt(variance)
  log_cf <- function(z) {
    ou_gh_remainder_log_cf(
      as.complex(z) / scale, Delta, fit,
      quadrature_nodes = quadrature_nodes
    )
  }
  attenuation_integral <- -expm1(-fit[["kappa"]] * Delta) /
    fit[["kappa"]]
  list(
    log_cf = log_cf,
    mean = 0,
    scale = scale,
    variance = variance,
    cumulants = cumulants,
    quadrature_nodes = as.integer(quadrature_nodes),
    asymptotic_decay = fit[["delta"]] * attenuation_integral / scale
  )
}

ou_gh_standardised_analytic_strip <- function(fit, remainder_scale) {
  c(
    lower = -(fit[["alpha"]] + fit[["beta"]]) * remainder_scale,
    upper = (fit[["alpha"]] - fit[["beta"]]) * remainder_scale
  )
}

ou_gh_validate_shift <- function(shift, fit, remainder_scale,
                                 strict_fraction = 0.5) {
  strip <- ou_gh_standardised_analytic_strip(fit, remainder_scale)
  all(is.finite(shift)) && all(shift > strict_fraction * strip[["lower"]]) &&
    all(shift < strict_fraction * strip[["upper"]])
}

ou_gh_fgmc_fft_cdf_shift <- function(log_cf, N, h, shift) {
  index <- 0:(N - 1L)
  frequency <- (index + 0.5) * h
  x <- (index - N / 2) * 2 * pi / (N * h)
  z <- frequency - 1i * shift
  coefficient <- exp(log_cf(z)) / (shift + 1i * frequency)
  alternating <- ifelse(index %% 2L == 0L, 1, -1)
  transformed <- fft(coefficient * alternating) * exp(-1i * x * h / 2)
  probability <- as.numeric(shift > 0) -
    h * exp(-shift * x) / pi * Re(transformed)
  endpoint <- max(Mod(exp(log_cf(c(
    (N - 0.5) * h - 1i * shift,
    -(N - 0.5) * h - 1i * shift
  )))))
  list(x = x, probability = probability, endpoint_cf_modulus = endpoint)
}

ou_gh_fgmc_overlap <- function(x, lower, upper) {
  use <- is.finite(lower) & is.finite(upper) &
    lower > 1e-8 & lower < 1 - 1e-8 &
    upper > 1e-8 & upper < 1 - 1e-8 & abs(x) <= 8
  if (!any(use)) return(list(maximum = Inf, zero = Inf, median = Inf,
                             rms = Inf, points = 0L))
  error <- abs(lower[use] - upper[use])
  centre <- which.min(abs(x))
  list(
    maximum = max(error),
    zero = abs(lower[[centre]] - upper[[centre]]),
    median = stats::median(error),
    rms = sqrt(mean(error^2)),
    points = sum(use)
  )
}

ou_gh_fgmc_monotone_segment <- function(x, probability,
                                        minimum_points = 64L) {
  valid <- is.finite(probability) & probability > 0 & probability < 1
  candidate <- which(valid)
  ou_gh_assert(length(candidate) > 0L,
               "Shifted FFT produced no interior CDF values.")
  centre <- candidate[[which.min(abs(probability[candidate] - 0.5))]]
  left <- centre
  while (left > 1L && valid[[left - 1L]] &&
         probability[[left - 1L]] < probability[[left]]) left <- left - 1L
  right <- centre
  while (right < length(probability) && valid[[right + 1L]] &&
         probability[[right + 1L]] > probability[[right]]) right <- right + 1L
  ou_gh_assert(right - left + 1L >= minimum_points,
               "Shifted FFT monotone CDF segment is too short.")
  p <- probability[left:right]
  q <- x[left:right]
  strict <- c(TRUE, diff(p) > 0)
  p <- p[strict]
  q <- q[strict]
  ou_gh_assert(length(p) >= minimum_points,
               "Strict shifted-FFT CDF segment is too short.")
  list(
    probability = p, x = q,
    probability_min = p[[1L]], probability_max = tail(p, 1L),
    boundary = max(p[[1L]], 1 - tail(p, 1L)),
    monotonicity_repairs = sum(!strict)
  )
}

ou_gh_fgmc_make_lookup <- function(segment, left_rate, right_rate,
                                    lookup_size = 32769L,
                                    lookup_logit_bound = 18) {
  grid_logit <- seq(-lookup_logit_bound, lookup_logit_bound,
                    length.out = as.integer(lookup_size))
  grid_probability <- stats::plogis(grid_logit)
  p <- segment$probability
  q <- segment$x
  ou_gh_assert(p[[1L]] < grid_probability[[1L]] &&
                 tail(p, 1L) > tail(grid_probability, 1L),
               "Retained CDF does not cover the production lookup range.")
  spline <- stats::splinefun(p, q, method = "monoH.FC")
  lookup <- as.numeric(spline(grid_probability))
  ou_gh_assert(all(is.finite(lookup)) && all(diff(lookup) > 0),
               "OU-GH inverse-CDF lookup is not strictly monotone.")
  list(
    quantile_lookup = lookup,
    logit_min = grid_logit[[1L]],
    logit_step = grid_logit[[2L]] - grid_logit[[1L]],
    probability_min = grid_probability[[1L]],
    probability_max = tail(grid_probability, 1L),
    quantile_min = lookup[[1L]],
    quantile_max = tail(lookup, 1L),
    left_tail_rate = left_rate,
    right_tail_rate = right_rate,
    tail_method = "separate_GH_strip_rate_continuation"
  )
}

ou_gh_fgmc_default_h <- function(N, minimum_shift, asymptotic_decay) {
  value <- sqrt(2 * pi * minimum_shift /
                  (max(asymptotic_decay, 1e-10) * N))
  min(0.75, max(1e-6, value))
}

ou_gh_fgmc_candidate <- function(
    fit, Delta = 1, fft_M = 14L, h_multiplier = 1,
    shift_fraction = 0.49, shift_cap = 6,
    quadrature_nodes = 24L, lookup_size = 32769L,
    lookup_logit_bound = 18, compact = TRUE
) {
  started <- proc.time()[["elapsed"]]
  distribution <- ou_gh_standardised_remainder(
    fit, Delta, quadrature_nodes
  )
  strip <- ou_gh_standardised_analytic_strip(fit, distribution$scale)
  positive <- min(shift_fraction * strip[["upper"]], shift_cap)
  negative <- -min(shift_fraction * abs(strip[["lower"]]), shift_cap)
  N <- as.integer(2^as.integer(fft_M))
  h0 <- ou_gh_fgmc_default_h(
    N, min(abs(c(negative, positive))), distribution$asymptotic_decay
  )
  h <- h0 * h_multiplier
  base <- list(
    fit = fit, Delta = Delta, distribution = distribution, strip = strip,
    configuration = list(
      fft_M = as.integer(fft_M), N = N, h0 = h0,
      h_multiplier = h_multiplier, h = h,
      shift_fraction = shift_fraction,
      negative_shift = negative, positive_shift = positive,
      lookup_size = as.integer(lookup_size),
      lookup_logit_bound = lookup_logit_bound,
      quadrature_nodes = as.integer(quadrature_nodes)
    ),
    half_strip_valid = ou_gh_validate_shift(
      c(negative, positive), fit, distribution$scale, 0.5
    )
  )
  calculated <- tryCatch({
    lower <- ou_gh_fgmc_fft_cdf_shift(
      distribution$log_cf, N, h, negative
    )
    upper <- ou_gh_fgmc_fft_cdf_shift(
      distribution$log_cf, N, h, positive
    )
    overlap <- ou_gh_fgmc_overlap(
      lower$x, lower$probability, upper$probability
    )
    p <- ifelse(lower$x <= 0, lower$probability, upper$probability)
    segment <- ou_gh_fgmc_monotone_segment(lower$x, p)
    lookup <- tryCatch(
      ou_gh_fgmc_make_lookup(
        segment, abs(strip[["lower"]]), strip[["upper"]],
        lookup_size, lookup_logit_bound
      ), error = identity
    )
    list(
      calculation_finite = TRUE,
      lower = if (compact) NULL else lower,
      upper = if (compact) NULL else upper,
      segment = segment,
      overlap = overlap,
      endpoint_by_shift = c(
        negative = lower$endpoint_cf_modulus,
        positive = upper$endpoint_cf_modulus
      ),
      endpoint = max(lower$endpoint_cf_modulus, upper$endpoint_cf_modulus),
      boundary = segment$boundary,
      lookup = lookup,
      lookup_valid = !inherits(lookup, "error"),
      lookup_reason = if (inherits(lookup, "error")) conditionMessage(lookup)
        else ""
    )
  }, error = function(error) list(
    calculation_finite = FALSE, failure_reason = conditionMessage(error)
  ))
  output <- c(base, calculated)
  output$runtime_seconds <- proc.time()[["elapsed"]] - started
  output$memory_proxy_bytes <- N * 160
  class(output) <- "ou_gh_fgmc_candidate"
  output
}

ou_gh_fgmc_validate_candidate <- function(
    candidate, cdf_boundary_tolerance = 1e-6,
    endpoint_cf_tolerance = 1e-8, overlap_tolerance = 2e-5
) {
  reasons <- character()
  if (!isTRUE(candidate$calculation_finite)) {
    reasons <- c(reasons, candidate$failure_reason %||% "nonfinite")
  } else {
    if (!isTRUE(candidate$half_strip_valid)) reasons <- c(reasons, "half_strip")
    if (candidate$boundary > cdf_boundary_tolerance) reasons <- c(reasons, "boundary")
    if (candidate$endpoint > endpoint_cf_tolerance) reasons <- c(reasons, "endpoint")
    if (candidate$overlap$maximum > overlap_tolerance) reasons <- c(reasons, "overlap")
    if (!isTRUE(candidate$lookup_valid)) reasons <- c(reasons, "lookup")
  }
  list(passed = !length(reasons),
       failure_reasons = paste(unique(reasons), collapse = ";"))
}

ou_gh_fgmc_candidate_row <- function(candidate, validation, stage) {
  cfg <- candidate$configuration
  data.frame(
    stage = stage,
    fft_M = cfg$fft_M, fft_N = cfg$N,
    h_multiplier = cfg$h_multiplier, fourier_step = cfg$h,
    shift_fraction = cfg$shift_fraction,
    negative_shift = cfg$negative_shift,
    positive_shift = cfg$positive_shift,
    half_strip_valid = candidate$half_strip_valid,
    calculation_finite = candidate$calculation_finite,
    boundary = candidate$boundary %||% NA_real_,
    endpoint = candidate$endpoint %||% NA_real_,
    overlap_max = candidate$overlap$maximum %||% NA_real_,
    lookup_valid = candidate$lookup_valid %||% FALSE,
    runtime_seconds = candidate$runtime_seconds,
    accepted = validation$passed,
    failure_reasons = validation$failure_reasons,
    stringsAsFactors = FALSE
  )
}

ou_gh_fgmc_search <- function(
    fit, Delta = 1, fft_M = 14L, max_fft_M = 20L,
    shift_fraction = 0.49,
    shift_fraction_candidates = c(0.49, 0.45, 0.40, 0.32),
    h_multiplier_candidates = c(1, 0.82, 0.68, 0.50, 0.40, 0.30, 0.25,
                                1.22, 1.45),
    shift_cap = 6, quadrature_nodes = 24L,
    lookup_size = 32769L, lookup_logit_bound = 18,
    cdf_boundary_tolerance = 1e-6,
    endpoint_cf_tolerance = 1e-8, overlap_tolerance = 2e-5
) {
  attempts <- list()
  selected <- NULL
  planning_distribution <- ou_gh_standardised_remainder(
    fit, Delta, quadrature_nodes
  )
  planning_strip <- ou_gh_standardised_analytic_strip(
    fit, planning_distribution$scale
  )
  planning_shift <- min(
    shift_fraction * min(abs(planning_strip)), shift_cap
  )
  target_log <- log(1 / endpoint_cf_tolerance) * 1.10
  required_N <- target_log^2 / (
    2 * pi * planning_shift * planning_distribution$asymptotic_decay
  )
  recommended_M <- max(
    as.integer(fft_M), as.integer(ceiling(log2(max(2^fft_M, required_N))))
  )
  strip_rate <- min(abs(planning_strip))
  tail_resolution_floor <- if (strip_rate <= 0.01) 16L else
    if (strip_rate <= 0.02) 15L else as.integer(fft_M)
  recommended_M <- max(recommended_M, tail_resolution_floor)
  recommended_M <- min(as.integer(max_fft_M), recommended_M)
  evaluate <- function(one_M, multiplier, fraction, stage) {
    candidate <- ou_gh_fgmc_candidate(
      fit, Delta, one_M, multiplier, fraction, shift_cap,
      quadrature_nodes, lookup_size, lookup_logit_bound
    )
    validation <- ou_gh_fgmc_validate_candidate(
      candidate, cdf_boundary_tolerance,
      endpoint_cf_tolerance, overlap_tolerance
    )
    attempts[[length(attempts) + 1L]] <<-
      ou_gh_fgmc_candidate_row(candidate, validation, stage)
    if (validation$passed) selected <<- candidate
    invisible(candidate)
  }
  fractions <- unique(c(shift_fraction, shift_fraction_candidates))
  for (fraction in fractions) {
    for (one_M in seq.int(recommended_M, as.integer(max_fft_M))) {
      for (multiplier in h_multiplier_candidates) {
        evaluate(one_M, multiplier, fraction, "joint_M_h_shift_search")
        if (!is.null(selected)) break
      }
      if (!is.null(selected)) break
    }
    if (!is.null(selected)) break
  }
  table <- do.call(rbind, attempts)
  list(
    passed = !is.null(selected), selected = selected,
    attempts = table,
    recommended_fft_M = recommended_M,
    required_fft_N_proxy = required_N,
    failure_reasons = if (is.null(selected))
      paste(unique(table$failure_reasons), collapse = ";") else ""
  )
}

ou_gh_fgmc_build_table <- function(
    fit, Delta = 1, fft_M = 14L, max_fft_M = 20L,
    shift_fraction = 0.49,
    shift_fraction_candidates = c(0.49, 0.45, 0.40, 0.32),
    h_multiplier_candidates = c(1, 0.82, 0.68, 0.50, 0.40, 0.30, 0.25,
                                1.22, 1.45),
    shift_cap = 6, quadrature_nodes = 24L,
    lookup_size = 32769L, lookup_logit_bound = 18,
    cdf_boundary_tolerance = 1e-6,
    endpoint_cf_tolerance = 1e-8, overlap_tolerance = 2e-5,
    use_cache = TRUE
) {
  key <- ou_gh_hash_object(list(
    version = OU_GH_SIMULATOR_VERSION, fit = fit, Delta = Delta,
    fft_M = fft_M, max_fft_M = max_fft_M,
    shift_fraction = shift_fraction,
    shift_fraction_candidates = shift_fraction_candidates,
    h_multiplier_candidates = h_multiplier_candidates,
    shift_cap = shift_cap, quadrature_nodes = quadrature_nodes,
    lookup_size = lookup_size, lookup_logit_bound = lookup_logit_bound,
    cdf_boundary_tolerance = cdf_boundary_tolerance,
    endpoint_cf_tolerance = endpoint_cf_tolerance,
    overlap_tolerance = overlap_tolerance
  ))
  if (isTRUE(use_cache) && exists(key, .ou_gh_fgmc_cache, inherits = FALSE)) {
    value <- get(key, .ou_gh_fgmc_cache, inherits = FALSE)
    value$diagnostics$cache_hit <- TRUE
    return(value)
  }
  search <- ou_gh_fgmc_search(
    fit, Delta, fft_M, max_fft_M, shift_fraction,
    shift_fraction_candidates, h_multiplier_candidates,
    shift_cap, quadrature_nodes, lookup_size, lookup_logit_bound,
    cdf_boundary_tolerance, endpoint_cf_tolerance, overlap_tolerance
  )
  if (!search$passed) stop(structure(list(
    message = paste("OU-GH shifted-FGMC search failed:", search$failure_reasons),
    call = NULL, attempts = search$attempts
  ), class = c("ou_gh_fgmc_error", "error", "condition")))
  candidate <- search$selected
  cfg <- candidate$configuration
  output <- c(list(
    Delta = Delta,
    rho = exp(-fit[["kappa"]] * Delta),
    draw_location = 0,
    draw_scale = candidate$distribution$scale,
    atom_probability = 0,
    atom_value = 0
  ), candidate$lookup, list(
    fit = fit,
    distribution = candidate$distribution,
    diagnostics = list(
      method = "exact_OU_GH_remainder_dual_shift_FGMC",
      analytic_strip_lower = candidate$strip[["lower"]],
      analytic_strip_upper = candidate$strip[["upper"]],
      negative_shift = cfg$negative_shift,
      positive_shift = cfg$positive_shift,
      half_strip_valid = candidate$half_strip_valid,
      fft_M = cfg$fft_M, fft_N = cfg$N,
      fourier_step = cfg$h,
      cdf_boundary_error = candidate$boundary,
      endpoint_cf_modulus = candidate$endpoint,
      overlap_max_error = candidate$overlap$maximum,
      overlap_zero_error = candidate$overlap$zero,
      lookup_size = cfg$lookup_size,
      lookup_logit_bound = cfg$lookup_logit_bound,
      tail_method = candidate$lookup$tail_method,
      table_build_seconds = candidate$runtime_seconds,
      refinement_attempts = nrow(search$attempts),
      search_attempts = search$attempts,
      cache_hit = FALSE,
      passed = TRUE
    )
  ))
  class(output) <- "ou_gh_fgmc_table"
  if (isTRUE(use_cache)) assign(key, output, .ou_gh_fgmc_cache)
  output
}

ou_gh_fgmc_quantile <- function(probability, table) {
  probability <- as.numeric(probability)
  ou_gh_assert(all(is.finite(probability)) && all(probability > 0) &&
                 all(probability < 1),
               "Probabilities must lie strictly inside (0,1).")
  output <- numeric(length(probability))
  left <- probability < table$probability_min
  right <- probability > table$probability_max
  middle <- !(left | right)
  if (any(left)) output[left] <- table$quantile_min +
    log(probability[left] / table$probability_min) / table$left_tail_rate
  if (any(right)) output[right] <- table$quantile_max -
    log((1 - probability[right]) / (1 - table$probability_max)) /
      table$right_tail_rate
  if (any(middle)) {
    position <- (stats::qlogis(probability[middle]) - table$logit_min) /
      table$logit_step
    cell <- pmax(0, pmin(length(table$quantile_lookup) - 2L,
                         floor(position)))
    fraction <- position - cell
    index <- cell + 1L
    output[middle] <- table$quantile_lookup[index] + fraction *
      (table$quantile_lookup[index + 1L] - table$quantile_lookup[index])
  }
  output
}

ou_gh_shifted_contour_cdf <- function(
    x, fit, Delta = 1, quadrature_nodes = 64L,
    reference_tolerance = 2e-5, initial_upper = 32,
    maximum_upper = 2048, rel.tol = 2e-8, subdivisions = 2000L
) {
  distribution <- ou_gh_standardised_remainder(
    fit, Delta, quadrature_nodes
  )
  strip <- ou_gh_standardised_analytic_strip(fit, distribution$scale)
  positive <- min(0.40 * strip[["upper"]], 6)
  negative <- -min(0.40 * abs(strip[["lower"]]), 6)
  one <- function(point) {
    shift <- if (point >= 0) positive else negative
    residue <- as.numeric(shift > 0)
    lower <- 0
    upper <- initial_upper
    total <- 0
    error <- 0
    previous <- NA_real_
    blocks <- 0L
    repeat {
      integrand <- function(u) Re(
        exp(-1i * u * point + distribution$log_cf(u - 1i * shift)) /
          (shift + 1i * u)
      )
      value <- stats::integrate(
        integrand, lower, upper, rel.tol = rel.tol,
        subdivisions = as.integer(subdivisions), stop.on.error = FALSE
      )
      total <- total + value$value
      error <- error + value$abs.error
      estimate <- residue - exp(-shift * point) * total / pi
      convergence <- if (is.finite(previous)) abs(estimate - previous) else Inf
      endpoint <- Mod(exp(distribution$log_cf(upper - 1i * shift)))
      blocks <- blocks + 1L
      if ((convergence <= reference_tolerance &&
           endpoint <= reference_tolerance) || upper >= maximum_upper) break
      previous <- estimate
      lower <- upper
      upper <- min(maximum_upper, upper * 2)
    }
    data.frame(
      x = point, cdf = estimate,
      quadrature_absolute_error = error,
      truncation_proxy = endpoint,
      convergence_change = convergence,
      upper = upper, shift = shift, blocks = blocks,
      converged = convergence <= reference_tolerance &&
        endpoint <= reference_tolerance,
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, lapply(as.numeric(x), one))
}

ou_gh_fgmc_draw_innovations <- function(probability, table) {
  table$draw_location + table$draw_scale *
    ou_gh_fgmc_quantile(probability, table)
}

ou_gh_simulate_prebuilt_table <- function(
    active_time, x0, n_paths, seed, fit, table, uniforms = NULL
) {
  active_time <- as.numeric(active_time)
  step <- diff(active_time)
  ou_gh_assert(length(step) >= 1L && all(abs(step - table$Delta) <= 1e-12),
               "Prebuilt OU-GH table requires its exact active-time step.")
  n_paths <- as.integer(n_paths)
  n_steps <- length(step)
  if (is.null(uniforms)) {
    set.seed(as.integer(seed)[[1L]])
    uniforms <- matrix(stats::runif(n_steps * n_paths), nrow = n_steps)
  } else {
    uniforms <- as.matrix(uniforms)
    ou_gh_assert(identical(dim(uniforms), c(n_steps, n_paths)) &&
                   all(is.finite(uniforms)) && all(uniforms > 0) &&
                   all(uniforms < 1), "Supplied-uniform matrix is invalid.")
  }
  innovation <- matrix(
    ou_gh_fgmc_draw_innovations(as.vector(uniforms), table),
    nrow = n_steps, ncol = n_paths
  )
  paths <- matrix(NA_real_, nrow = n_steps + 1L, ncol = n_paths)
  state <- rep(as.numeric(x0), length.out = n_paths)
  paths[1L, ] <- state
  for (i in seq_len(n_steps)) {
    state <- fit[["mu"]] + table$rho * (state - fit[["mu"]]) +
      innovation[i, ]
    paths[i + 1L, ] <- state
  }
  list(
    paths = paths,
    uniforms = uniforms,
    diagnostics = list(
      route = "gh_exact_remainder_dual_shift_fgmc",
      exact_transition = TRUE,
      raw_GH_used = FALSE,
      Gaussian_fallback_used = FALSE,
      n_paths = n_paths, n_transitions = n_steps,
      simulator_version = OU_GH_SIMULATOR_VERSION
    )
  )
}
