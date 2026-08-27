ou_gh_empirical_skew_proxy <- function(transitions) {
  differences <- transitions$x_current - transitions$x_previous
  differences <- differences[is.finite(differences)]
  if (length(differences) < 20L) return(0)
  quantiles <- stats::quantile(
    differences, c(0.1, 0.5, 0.9), names = FALSE, type = 8
  )
  denominator <- quantiles[[3L]] - quantiles[[1L]]
  if (!is.finite(denominator) || denominator <= 0) return(0)
  min(max((quantiles[[3L]] + quantiles[[1L]] - 2 * quantiles[[2L]]) /
    denominator, -0.9), 0.9)
}

ou_gh_deterministic_starts <- function(profile, training_transitions) {
  skew <- ou_gh_empirical_skew_proxy(training_transitions)
  skew_sign <- if (abs(skew) < 0.03) 1 else sign(skew)
  shape <- data.frame(
    start_id = c(
      "symmetric_moderate", "empirical_skew", "opposite_skew", "NIG",
      "hyperbolic", "VG_near", "Gaussian_concentration", "heavy_tail"
    ),
    lambda = c(0.5, 0.5, -0.5, -0.5, 1, 1.2, 5, -2),
    zeta = c(2, 1, 1, 1.5, 2.5, 0.02, 100, 0.2),
    rho = c(0, 0.55 * skew_sign, -0.35 * skew_sign,
      0.35 * skew_sign, 0.2 * skew_sign, 0.35 * skew_sign,
      0.15 * skew_sign, 0.65 * skew_sign),
    kappa_factor = c(1, 1, 1, 1, 0.5, 2, 1, 1),
    stringsAsFactors = FALSE
  )
  bounds <- ou_gh_raw_bounds()
  starts <- lapply(seq_len(nrow(shape)), function(index) {
    one <- shape[index, ]
    raw <- c(
      mu = profile$mu,
      log_kappa = log(profile$kappa * one$kappa_factor),
      log_sigma_eta_1 = log(profile$sigma_eta_1),
      lambda = one$lambda,
      log_zeta = log(one$zeta),
      atanh_rho = atanh(one$rho)
    )
    list(start_id = one$start_id, raw = ou_gh_clamp_raw(raw, bounds))
  })
  starts
}

ou_gh_task_bounds <- function(profile, kappa_factor_range = c(0.25, 4)) {
  bounds <- ou_gh_raw_bounds()
  local_kappa <- log(profile$kappa * kappa_factor_range)
  bounds$lower[["log_kappa"]] <- max(
    bounds$lower[["log_kappa"]], min(local_kappa)
  )
  bounds$upper[["log_kappa"]] <- min(
    bounds$upper[["log_kappa"]], max(local_kappa)
  )
  bounds
}

ou_gh_projected_gradient <- function(
    raw,
    objective,
    lower,
    upper,
    step = 1e-4
) {
  gradient <- numeric(length(raw))
  for (index in seq_along(raw)) {
    scale <- step * max(1, abs(raw[[index]]))
    plus <- minus <- raw
    plus[[index]] <- min(upper[[index]], raw[[index]] + scale)
    minus[[index]] <- max(lower[[index]], raw[[index]] - scale)
    denominator <- plus[[index]] - minus[[index]]
    gradient[[index]] <- if (denominator > 0) {
      (objective(plus) - objective(minus)) / denominator
    } else 0
  }
  projected <- gradient
  at_lower <- raw <= lower + 1e-7
  at_upper <- raw >= upper - 1e-7
  projected[at_lower & gradient > 0] <- 0
  projected[at_upper & gradient < 0] <- 0
  setNames(projected, ou_gh_raw_parameter_names())
}

ou_gh_transition_law_distance <- function(
    fit_a,
    fit_b,
    horizons = c(1, 5, 30, 120, 630),
    frequencies = c(0.10, 0.25, 0.5, 1, 2),
    state_standard_deviations = c(-1, 0, 1),
    quadrature_nodes = 48L
) {
  reference_stationary_sd <- ou_gh_stationary_sd(fit_a)
  states <- fit_a[["mu"]] + state_standard_deviations * reference_stationary_sd
  errors <- vapply(horizons, function(horizon) {
    reference_sd <- sqrt(ou_gh_remainder_cumulants(
      2L, horizon, fit_a
    )[[1L]])
    u <- frequencies / reference_sd
    attenuation_a <- exp(-fit_a[["kappa"]] * horizon)
    attenuation_b <- exp(-fit_b[["kappa"]] * horizon)
    remainder_a <- ou_gh_remainder_log_cf(
      u, horizon, fit_a, quadrature_nodes
    )
    remainder_b <- ou_gh_remainder_log_cf(
      u, horizon, fit_b, quadrature_nodes
    )
    max(vapply(states, function(state) {
      location_a <- fit_a[["mu"]] + attenuation_a *
        (state - fit_a[["mu"]])
      location_b <- fit_b[["mu"]] + attenuation_b *
        (state - fit_b[["mu"]])
      cf_a <- exp(1i * u * location_a + remainder_a)
      cf_b <- exp(1i * u * location_b + remainder_b)
      max(Mod(cf_a - cf_b))
    }, numeric(1L)))
  }, numeric(1L))
  max(errors)
}

ou_gh_run_nlminb_attempt <- function(
    start_id,
    raw_start,
    prepared_ccf,
    bounds,
    quadrature_nodes,
    evaluation_budget,
    iteration_budget
) {
  objective <- function(raw) ou_gh_ccf_objective(
    raw, prepared_ccf, quadrature_nodes
  )
  started <- proc.time()[["elapsed"]]
  result <- tryCatch(
    stats::nlminb(
      start = raw_start, objective = objective,
      lower = bounds$lower, upper = bounds$upper,
      control = list(
        eval.max = evaluation_budget, iter.max = iteration_budget,
        rel.tol = 1e-8, abs.tol = 1e-10, x.tol = 1e-7,
        sing.tol = 1e-12
      )
    ),
    error = identity
  )
  runtime <- proc.time()[["elapsed"]] - started
  if (inherits(result, "error")) {
    return(list(
      start_id = start_id, status = "optimiser_error",
      reason = conditionMessage(result), objective = Inf,
      raw = raw_start, convergence = NA_integer_, message = "",
      evaluations = NA_integer_, iterations = NA_integer_,
      runtime_seconds = runtime
    ))
  }
  list(
    start_id = start_id,
    status = if (is.finite(result$objective)) "completed" else "nonfinite",
    reason = "", objective = result$objective,
    raw = setNames(result$par, ou_gh_raw_parameter_names()),
    convergence = result$convergence, message = result$message,
    evaluations = result$evaluations[[1L]], iterations = result$iterations,
    runtime_seconds = runtime
  )
}

ou_gh_attempt_summary <- function(attempt, stage) {
  raw <- setNames(as.numeric(attempt$raw), ou_gh_raw_parameter_names())
  data.frame(
    stage = stage, start_id = attempt$start_id, status = attempt$status,
    reason = attempt$reason, objective = attempt$objective,
    convergence = attempt$convergence, optimiser_message = attempt$message,
    evaluations = attempt$evaluations, iterations = attempt$iterations,
    runtime_seconds = attempt$runtime_seconds,
    mu = raw[["mu"]], log_kappa = raw[["log_kappa"]],
    log_sigma_eta_1 = raw[["log_sigma_eta_1"]], lambda = raw[["lambda"]],
    log_zeta = raw[["log_zeta"]], atanh_rho = raw[["atanh_rho"]],
    stringsAsFactors = FALSE
  )
}

ou_gh_fit_ccf <- function(
    training_transitions,
    validation_transitions,
    bank = ou_gh_load_frozen_moment_bank(),
    pair_cap = 5000L,
    coarse_quadrature_nodes = 12L,
    production_quadrature_nodes = 24L,
    start_screen_keep = 4L,
    refinement_keep = 2L,
    coarse_evaluation_budget = 180L,
    refinement_evaluation_budget = 120L
) {
  started_total <- proc.time()[["elapsed"]]
  profile <- ou_gh_preliminary_profile(training_transitions)
  bounds <- ou_gh_task_bounds(profile)
  training_ccf <- ou_gh_prepare_ccf_data(
    training_transitions, bank, pair_cap, profile
  )
  validation_ccf <- ou_gh_prepare_ccf_data(
    validation_transitions, bank, pair_cap, profile
  )
  starts <- ou_gh_deterministic_starts(profile, training_transitions)
  screen <- do.call(rbind, lapply(starts, function(start) {
    started <- proc.time()[["elapsed"]]
    value <- ou_gh_ccf_objective(
      start$raw, training_ccf, coarse_quadrature_nodes
    )
    data.frame(
      start_id = start$start_id, objective = value,
      runtime_seconds = proc.time()[["elapsed"]] - started,
      stringsAsFactors = FALSE
    )
  }))
  screen <- screen[order(screen$objective, screen$start_id), , drop = FALSE]
  retained <- head(screen$start_id[is.finite(screen$objective)], start_screen_keep)
  coarse <- lapply(retained, function(start_id) {
    start <- starts[[match(start_id, vapply(starts, `[[`, character(1L), "start_id"))]]
    ou_gh_run_nlminb_attempt(
      start_id, start$raw, training_ccf, bounds, coarse_quadrature_nodes,
      coarse_evaluation_budget, coarse_evaluation_budget
    )
  })
  finite_coarse <- which(vapply(coarse, function(value) {
    is.finite(value$objective)
  }, logical(1L)))
  if (!length(finite_coarse)) {
    return(list(
      fit_status = "all_optimisers_nonfinite", fit_failure_reason =
        "No coarse attempt returned a finite objective.", profile = profile,
      starts = starts, start_screen = screen, attempts = coarse,
      runtime_seconds = proc.time()[["elapsed"]] - started_total
    ))
  }
  order_coarse <- finite_coarse[order(vapply(coarse[finite_coarse],
    `[[`, numeric(1L), "objective"))]
  refinement_indices <- head(order_coarse, refinement_keep)
  refined <- lapply(seq_along(refinement_indices), function(index) {
    coarse_attempt <- coarse[[refinement_indices[[index]]]]
    ou_gh_run_nlminb_attempt(
      paste0(coarse_attempt$start_id, "_refined"), coarse_attempt$raw,
      training_ccf, bounds, production_quadrature_nodes,
      refinement_evaluation_budget, refinement_evaluation_budget
    )
  })
  all_attempts <- c(coarse, refined)
  finite <- which(vapply(all_attempts, function(value) {
    is.finite(value$objective)
  }, logical(1L)))
  selected_index <- finite[[which.min(vapply(all_attempts[finite],
    `[[`, numeric(1L), "objective"))]]
  selected <- all_attempts[[selected_index]]
  raw <- ou_gh_clamp_raw(selected$raw, bounds, margin = 0)
  fit_scaled <- ou_gh_raw_to_fit(raw)
  objective_final <- ou_gh_ccf_objective(
    raw, training_ccf, production_quadrature_nodes
  )
  holdout_objective <- ou_gh_ccf_objective(
    raw, validation_ccf, production_quadrature_nodes
  )
  objective_function <- function(value) ou_gh_ccf_objective(
    value, training_ccf, production_quadrature_nodes
  )
  projected_gradient <- ou_gh_projected_gradient(
    raw, objective_function, bounds$lower, bounds$upper
  )
  bound_distance <- pmin(raw - bounds$lower, bounds$upper - raw)
  near_indices <- finite[vapply(all_attempts[finite], function(value) {
    value$objective <= objective_final + max(1e-8, 0.05 * objective_final)
  }, logical(1L))]
  law_distances <- vapply(near_indices, function(index) {
    ou_gh_transition_law_distance(
      fit_scaled, ou_gh_raw_to_fit(all_attempts[[index]]$raw),
      quadrature_nodes = 24L
    )
  }, numeric(1L))
  raw_matrix <- do.call(rbind, lapply(near_indices, function(index) {
    all_attempts[[index]]$raw
  }))
  primitive_spread <- if (nrow(raw_matrix) >= 2L) {
    max(apply(raw_matrix, 2L, function(value) diff(range(value))))
  } else 0
  maximum_law_distance <- if (length(law_distances)) max(law_distances) else 0
  fit_status <- if (!is.finite(objective_final)) {
    "all_optimisers_nonfinite"
  } else if (maximum_law_distance > 0.05) {
    "law_multistart_disagreement"
  } else {
    "fit_success_interior_GH"
  }
  primitive_status <- if (primitive_spread > 1 && maximum_law_distance <= 0.02) {
    "law_identified_primitives_weak"
  } else "primitive_locally_identified"
  kappa_ratio <- fit_scaled[["kappa"]] / profile$kappa
  kappa_status <- if (raw[["log_kappa"]] <= bounds$lower[["log_kappa"]] + 1e-5 ||
      raw[["log_kappa"]] >= bounds$upper[["log_kappa"]] - 1e-5) {
    "kappa_boundary"
  } else if (kappa_ratio < 0.5 || kappa_ratio > 2) {
    "kappa_joint_material_improvement"
  } else "kappa_stable"
  attempts_table <- do.call(rbind, c(
    lapply(coarse, ou_gh_attempt_summary, stage = "coarse"),
    lapply(refined, ou_gh_attempt_summary, stage = "production_refinement")
  ))
  list(
    fit_status = fit_status,
    fit_failure_reason = if (fit_status == "fit_success_interior_GH") "" else fit_status,
    subclass_status = "full_GH_unrouted",
    primitive_identification_status = primitive_status,
    law_identification_status = if (maximum_law_distance <= 0.02) {
      "law_identified"
    } else "law_identification_weak",
    kappa_status = kappa_status,
    profile = profile, bounds = bounds, starts = starts,
    start_screen = screen, attempts = all_attempts,
    attempts_table = attempts_table,
    selected_raw = raw,
    fit_scaled = fit_scaled,
    training_objective = objective_final,
    formation_holdout_CCF_score = holdout_objective,
    projected_gradient = projected_gradient,
    projected_gradient_max = max(abs(projected_gradient)),
    bound_distance = bound_distance,
    near_optimal_law_distances = law_distances,
    maximum_near_optimal_law_distance = maximum_law_distance,
    primitive_raw_spread = primitive_spread,
    bank_hash = attr(bank, "bank_hash"),
    active_bank_hash = training_ccf$active_bank_hash,
    active_horizons = training_ccf$active_horizons,
    unavailable_horizons = training_ccf$unavailable_horizons,
    training_data_hash = training_ccf$data_hash,
    validation_data_hash = validation_ccf$data_hash,
    quadrature_policy = list(
      coarse = coarse_quadrature_nodes,
      production = production_quadrature_nodes
    ),
    runtime_seconds = proc.time()[["elapsed"]] - started_total
  )
}

ou_gh_refine_kappa_scale_ccf <- function(
    raw_template,
    training_transitions,
    validation_transitions,
    profile,
    bank = ou_gh_load_frozen_moment_bank(),
    pair_cap = 8000L,
    quadrature_nodes = 24L,
    evaluation_budget = 100L,
    refine_scale = FALSE,
    refine_mu = FALSE,
    mu_local_radius = 0.5
) {
  raw_template <- setNames(as.numeric(raw_template), ou_gh_raw_parameter_names())
  raw_template[["mu"]] <- profile$mu
  prepared_training <- ou_gh_prepare_ccf_data(
    training_transitions, bank, pair_cap, profile
  )
  prepared_validation <- ou_gh_prepare_ccf_data(
    validation_transitions, bank, pair_cap, profile
  )
  bounds <- ou_gh_task_bounds(profile)
  free_names <- c(
    if (isTRUE(refine_mu)) "mu",
    "log_kappa",
    if (isTRUE(refine_scale)) "log_sigma_eta_1"
  )
  if (isTRUE(refine_mu)) {
    bounds$lower[["mu"]] <- max(bounds$lower[["mu"]],
      profile$mu - mu_local_radius)
    bounds$upper[["mu"]] <- min(bounds$upper[["mu"]],
      profile$mu + mu_local_radius)
  }
  objective <- function(free) {
    raw <- raw_template
    raw[free_names] <- free
    ou_gh_ccf_objective(raw, prepared_training, quadrature_nodes)
  }
  started <- proc.time()[["elapsed"]]
  result <- tryCatch(stats::nlminb(
    start = raw_template[free_names], objective = objective,
    lower = bounds$lower[free_names], upper = bounds$upper[free_names],
    control = list(
      eval.max = evaluation_budget, iter.max = evaluation_budget,
      rel.tol = 1e-9, x.tol = 1e-8, abs.tol = 1e-11
    )
  ), error = identity)
  runtime <- proc.time()[["elapsed"]] - started
  if (inherits(result, "error")) return(list(
    status = "kappa_scale_ccf_error", reason = conditionMessage(result),
    selected_raw = raw_template, runtime_seconds = runtime
  ))
  selected_raw <- raw_template
  selected_raw[free_names] <- result$par
  list(
    status = "kappa_scale_ccf_refinement_completed", reason = "",
    selected_raw = selected_raw,
    selected_fit = ou_gh_raw_to_fit(selected_raw),
    training_objective = objective(result$par),
    holdout_objective = ou_gh_ccf_objective(
      selected_raw, prepared_validation, quadrature_nodes
    ),
    convergence = result$convergence, message = result$message,
    evaluations = result$evaluations[[1L]], iterations = result$iterations,
    runtime_seconds = runtime,
    bank_hash = attr(bank, "bank_hash"),
    active_bank_hash = prepared_training$active_bank_hash,
    active_horizons = prepared_training$active_horizons,
    unavailable_horizons = prepared_training$unavailable_horizons,
    training_data_hash = prepared_training$data_hash,
    validation_data_hash = prepared_validation$data_hash,
    location_estimator = "formation_state_mean_frozen",
    shape_estimator = "staged_exact_transition_likelihood",
    kappa_scale_estimator = paste0(
      "fixed_shape_multihorizon_CCF_",
      if (isTRUE(refine_mu)) "local_mu_" else "fixed_mu_",
      "kappa_", if (isTRUE(refine_scale)) "scale" else "fixed_scale"
    )
  )
}

ou_gh_unscale_fit <- function(fit_scaled, scaling) {
  gh_shape_scale_to_direct(
    lambda = fit_scaled[["lambda"]], zeta = fit_scaled[["zeta"]],
    rho = fit_scaled[["rho"]],
    sigma_eta_1 = fit_scaled[["sigma_eta_1"]] * scaling$scale,
    kappa = fit_scaled[["kappa"]],
    mu = scaling$centre + scaling$scale * fit_scaled[["mu"]]
  )
}

ou_gh_stationary_sd <- function(fit) {
  driver_variance <- gh_driver_cumulants_direct(
    fit[["lambda"]], fit[["alpha"]], fit[["beta"]], fit[["delta"]], 2L
  )[["kappa2"]]
  sqrt(driver_variance / (2 * fit[["kappa"]]))
}
