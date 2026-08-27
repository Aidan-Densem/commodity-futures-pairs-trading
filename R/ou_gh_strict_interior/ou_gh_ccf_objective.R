ou_gh_raw_parameter_names <- function() {
  c("mu", "log_kappa", "log_sigma_eta_1", "lambda", "log_zeta", "atanh_rho")
}

ou_gh_raw_to_fit <- function(raw) {
  raw <- as.numeric(raw)
  ou_gh_assert(length(raw) == 6L && all(is.finite(raw)),
    "OU-GH raw parameter vector must contain six finite values.")
  names(raw) <- ou_gh_raw_parameter_names()
  gh_shape_scale_to_direct(
    lambda = raw[["lambda"]], zeta = exp(raw[["log_zeta"]]),
    rho = tanh(raw[["atanh_rho"]]),
    sigma_eta_1 = exp(raw[["log_sigma_eta_1"]]),
    kappa = exp(raw[["log_kappa"]]), mu = raw[["mu"]]
  )
}

ou_gh_fit_to_raw <- function(fit) {
  required <- c("mu", "kappa", "sigma_eta_1", "lambda", "zeta", "rho")
  ou_gh_assert(all(required %in% names(fit)), "Fit lacks empirical GH coordinates.")
  setNames(c(
    fit[["mu"]], log(fit[["kappa"]]), log(fit[["sigma_eta_1"]]),
    fit[["lambda"]], log(fit[["zeta"]]), atanh(fit[["rho"]])
  ), ou_gh_raw_parameter_names())
}

ou_gh_raw_bounds <- function() {
  half_life <- OU_GH_PRODUCTION_SUPPORT$half_life_active_minutes
  kappa <- sort(log(2) / rev(half_life))
  list(
    lower = setNames(c(
      OU_GH_PRODUCTION_SUPPORT$standardised_mu[[1L]], log(kappa[[1L]]),
      log(OU_GH_PRODUCTION_SUPPORT$sigma_eta_1_relative_to_formation_scale[[1L]]),
      OU_GH_PRODUCTION_SUPPORT$lambda[[1L]],
      log(OU_GH_PRODUCTION_SUPPORT$zeta[[1L]]),
      atanh(OU_GH_PRODUCTION_SUPPORT$rho[[1L]])
    ), ou_gh_raw_parameter_names()),
    upper = setNames(c(
      OU_GH_PRODUCTION_SUPPORT$standardised_mu[[2L]], log(kappa[[2L]]),
      log(OU_GH_PRODUCTION_SUPPORT$sigma_eta_1_relative_to_formation_scale[[2L]]),
      OU_GH_PRODUCTION_SUPPORT$lambda[[2L]],
      log(OU_GH_PRODUCTION_SUPPORT$zeta[[2L]]),
      atanh(OU_GH_PRODUCTION_SUPPORT$rho[[2L]])
    ), ou_gh_raw_parameter_names())
  )
}

ou_gh_clamp_raw <- function(raw, bounds = ou_gh_raw_bounds(), margin = 1e-8) {
  raw <- setNames(as.numeric(raw), ou_gh_raw_parameter_names())
  pmin(pmax(raw, bounds$lower + margin), bounds$upper - margin)
}

ou_gh_prepare_ccf_data <- function(
    transitions,
    bank,
    pair_cap = 5000L,
    frozen_profile = NULL
) {
  horizons <- sort(unique(bank$horizon))
  all_pairs <- ou_gh_build_horizon_pairs(transitions, horizons)
  ou_gh_assert(nrow(all_pairs) > 0L, "No CCF horizon pairs were constructed.")
  pair_counts <- table(factor(all_pairs$horizon, levels = horizons))
  active_horizons <- horizons[as.integer(pair_counts) >= 50L]
  unavailable_horizons <- setdiff(horizons, active_horizons)
  ou_gh_assert(1L %in% active_horizons && length(active_horizons) >= 2L,
    "Formation fragmentation leaves too few supported CCF horizons.")
  parent_bank <- bank
  bank <- bank[bank$horizon %in% active_horizons, , drop = FALSE]
  attr(bank, "parent_bank_hash") <- attr(parent_bank, "bank_hash")
  attr(bank, "bank_hash") <- ou_gh_hash_object(bank)
  horizon_data <- lapply(active_horizons, function(horizon) {
    one <- all_pairs[all_pairs$horizon == horizon, , drop = FALSE]
    if (nrow(one) > pair_cap) {
      retained <- unique(round(seq(1, nrow(one), length.out = pair_cap)))
      one <- one[retained, , drop = FALSE]
    }
    list(
      horizon = horizon,
      x_previous = one$x_previous,
      x_current = one$x_current,
      n_pairs = nrow(one),
      retained_hash = ou_gh_hash_object(one[, c(
        "segment_id", "previous_global_row", "current_global_row"
      )])
    )
  })
  names(horizon_data) <- as.character(active_horizons)
  if (is.null(frozen_profile)) {
    frozen_profile <- ou_gh_preliminary_profile(transitions, active_horizons)
  }
  innovation_scales <- vapply(horizon_data, function(data) {
    attenuation <- exp(-frozen_profile$kappa * data$horizon)
    residual <- data$x_current - (
      frozen_profile$mu + attenuation *
        (data$x_previous - frozen_profile$mu)
    )
    max(sqrt(mean(residual^2)), 1e-5)
  }, numeric(1L))
  effective_frequency <- bank$frequency /
    innovation_scales[match(as.character(bank$horizon), names(innovation_scales))]
  list(
    bank = bank,
    parent_bank_hash = attr(parent_bank, "bank_hash"),
    active_bank_hash = attr(bank, "bank_hash"),
    active_horizons = active_horizons,
    unavailable_horizons = unavailable_horizons,
    pair_counts = setNames(as.integer(pair_counts), horizons),
    horizon_data = horizon_data,
    frozen_profile = frozen_profile,
    innovation_scales = innovation_scales,
    effective_frequency = as.numeric(effective_frequency),
    pair_cap = as.integer(pair_cap),
    data_hash = ou_gh_hash_object(list(
      parent_bank_hash = attr(parent_bank, "bank_hash"),
      active_bank_hash = attr(bank, "bank_hash"),
      horizon_hashes = vapply(horizon_data, `[[`, character(1L), "retained_hash"),
      innovation_scales = innovation_scales,
      profile_hash = frozen_profile$profile_hash
    ))
  )
}

ou_gh_ccf_moments <- function(raw, prepared_ccf, quadrature_nodes = 24L) {
  fit <- ou_gh_raw_to_fit(raw)
  bank <- prepared_ccf$bank
  frequencies <- prepared_ccf$effective_frequency
  output <- numeric(2L * nrow(bank))
  remainder_cf <- complex(nrow(bank))
  for (horizon in unique(bank$horizon)) {
    index <- which(bank$horizon == horizon)
    unique_frequency <- sort(unique(frequencies[index]))
    values <- exp(ou_gh_remainder_log_cf(
      unique_frequency, horizon, fit, quadrature_nodes
    ))
    remainder_cf[index] <- values[match(frequencies[index], unique_frequency)]
  }
  term_keys <- paste(bank$horizon, format(frequencies, digits = 17), sep = "|")
  for (key in unique(term_keys)) {
    group_indices <- which(term_keys == key)
    representative <- group_indices[[1L]]
    group <- bank[representative, ]
    data <- prepared_ccf$horizon_data[[as.character(group$horizon)]]
    attenuation <- exp(-fit[["kappa"]] * group$horizon)
    location <- fit[["mu"]] + attenuation *
      (data$x_previous - fit[["mu"]])
    frequency <- frequencies[[representative]]
    residual <- exp(1i * frequency * data$x_current) -
      exp(1i * frequency * location) * remainder_cf[[representative]]
    for (index in group_indices) {
      instrument <- exp(1i * bank$instrument[[index]] * data$x_previous)
      moment <- mean(residual * instrument) * bank$weight[[index]]
      output[[2L * index - 1L]] <- Re(moment)
      output[[2L * index]] <- Im(moment)
    }
  }
  output
}

ou_gh_ccf_objective <- function(
    raw,
    prepared_ccf,
    quadrature_nodes = 24L,
    penalty = 1e12
) {
  bounds <- ou_gh_raw_bounds()
  raw <- as.numeric(raw)
  if (length(raw) != 6L || any(!is.finite(raw)) ||
      any(raw < bounds$lower) || any(raw > bounds$upper)) return(penalty)
  moments <- tryCatch(
    ou_gh_ccf_moments(raw, prepared_ccf, quadrature_nodes),
    error = function(condition) NULL
  )
  if (is.null(moments) || any(!is.finite(moments))) return(penalty)
  sum(moments^2)
}

ou_gh_population_moments <- function(raw, truth_fit, bank, quadrature_nodes = 24L) {
  candidate <- ou_gh_raw_to_fit(raw)
  h <- bank$horizon
  variance <- vapply(h, function(one_horizon) {
    ou_gh_remainder_cumulants(2, one_horizon, truth_fit)[[1L]]
  }, numeric(1L))
  u <- bank$frequency / sqrt(variance)
  a <- bank$instrument
  truth_attenuation <- exp(-truth_fit[["kappa"]] * h)
  candidate_attenuation <- exp(-candidate[["kappa"]] * h)
  truth_remainder <- candidate_remainder <- complex(nrow(bank))
  for (one_horizon in unique(h)) {
    index <- which(h == one_horizon)
    truth_remainder[index] <- ou_gh_remainder_log_cf(
      u[index], one_horizon, truth_fit, quadrature_nodes
    )
    candidate_remainder[index] <- ou_gh_remainder_log_cf(
      u[index], one_horizon, candidate, quadrature_nodes
    )
  }
  state_arguments <- c(
    a + u * truth_attenuation,
    a + u * candidate_attenuation
  )
  state_cf <- exp(
    1i * state_arguments * truth_fit[["mu"]] +
      ou_gh_stationary_log_cf(state_arguments, truth_fit, quadrature_nodes)
  )
  truth_state_cf <- state_cf[seq_len(nrow(bank))]
  candidate_state_cf <- state_cf[nrow(bank) + seq_len(nrow(bank))]
  truth_joint <- exp(1i * u * truth_fit[["mu"]] *
    (1 - truth_attenuation) + truth_remainder) * truth_state_cf
  candidate_prediction <- exp(1i * u * candidate[["mu"]] *
    (1 - candidate_attenuation) + candidate_remainder) * candidate_state_cf
  moment <- (truth_joint - candidate_prediction) * bank$weight
  output <- as.vector(rbind(Re(moment), Im(moment)))
  output
}
