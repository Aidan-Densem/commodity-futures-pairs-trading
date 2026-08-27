ou_gh_family_normalise_segments <- function(transitions) {
  if (is.list(transitions) && !is.data.frame(transitions)) {
    transitions <- do.call(rbind, lapply(seq_along(transitions), function(i) {
      one <- transitions[[i]]
      if (is.data.frame(one)) return(one)
      values <- as.numeric(one)
      data.frame(
        segment_id = i,
        x_previous = head(values, -1L),
        x_current = tail(values, -1L),
        delta = 1,
        transition_order = seq_len(length(values) - 1L),
        stringsAsFactors = FALSE
      )
    }))
  } else if (!is.data.frame(transitions)) {
    values <- as.numeric(transitions)
    transitions <- data.frame(
      segment_id = 1L,
      x_previous = head(values, -1L),
      x_current = tail(values, -1L),
      delta = 1,
      transition_order = seq_len(length(values) - 1L),
      stringsAsFactors = FALSE
    )
  }
  needed <- c("segment_id", "x_previous", "x_current", "delta")
  stopifnot(all(needed %in% names(transitions)))
  keep <- is.finite(transitions$x_previous) & is.finite(transitions$x_current) &
    is.finite(transitions$delta) & transitions$delta > 0 &
    !is.na(transitions$segment_id)
  transitions <- transitions[keep, , drop = FALSE]
  if (!"transition_order" %in% names(transitions)) {
    transitions$transition_order <- seq_len(nrow(transitions))
  }
  parts <- split(transitions, transitions$segment_id, drop = TRUE)
  parts <- lapply(parts, function(one) {
    one[order(one$transition_order), , drop = FALSE]
  })
  parts[vapply(parts, nrow, integer(1L)) >= 1L]
}

ou_gh_family_profile_states <- function(states) {
  segments <- ou_gh_family_normalise_segments(states)
  transitions <- do.call(rbind, segments)
  stopifnot(nrow(transitions) >= 100L)
  x0 <- transitions$x_previous
  x1 <- transitions$x_current
  delta <- transitions$delta
  objective <- function(log_kappa, return_details = FALSE) {
    kappa <- exp(log_kappa)
    attenuation <- exp(-kappa * delta)
    variance_ratio <- -expm1(-2 * kappa * delta) /
      -expm1(-2 * kappa)
    weight <- 1 / pmax(variance_ratio, .Machine$double.eps)
    loading <- 1 - attenuation
    mu <- sum(weight * loading * (x1 - attenuation * x0)) /
      max(sum(weight * loading^2), .Machine$double.eps)
    residual <- x1 - mu - attenuation * (x0 - mu)
    sigma2 <- mean(residual^2 / variance_ratio)
    score <- nrow(transitions) * log(max(sigma2, .Machine$double.eps)) +
      sum(log(variance_ratio))
    if (isTRUE(return_details)) list(
      mu = mu, kappa = kappa, residual = residual,
      one_minute_equivalent_residual = residual / sqrt(variance_ratio),
      sigma_eta_1 = sqrt(max(sigma2, .Machine$double.eps)), score = score
    ) else score
  }
  interval <- log(c(log(2) / 100000, log(2) / 30))
  optimum <- stats::optimize(objective, interval = interval)
  profile <- objective(optimum$minimum, TRUE)
  residual <- profile$one_minute_equivalent_residual
  q <- stats::quantile(residual, c(.1, .5, .9), names = FALSE, type = 8)
  skew <- (q[[3L]] + q[[1L]] - 2 * q[[2L]]) /
    max(q[[3L]] - q[[1L]], .Machine$double.eps)
  list(mu = profile$mu, kappa = profile$kappa,
    sigma_eta_1 = profile$sigma_eta_1,
    residual_iqr = diff(stats::quantile(residual, c(.25,.75),
      names = FALSE, type = 8)), skew_proxy = min(max(skew, -.9), .9),
    duration_aware = TRUE,
    unique_duration_count = length(unique(round(delta, 12L))))
}

ou_gh_family_split_states <- function(states, training_fraction = .75) {
  segments <- ou_gh_family_normalise_segments(states)
  if (length(segments) >= 2L) {
    cumulative <- cumsum(vapply(segments, nrow, integer(1L)))
    target <- training_fraction * tail(cumulative, 1L)
    cut <- max(1L, min(length(segments) - 1L, which.min(abs(cumulative - target))))
    return(list(training = do.call(rbind, segments[seq_len(cut)]),
      holdout = do.call(rbind, segments[seq.int(cut + 1L, length(segments))]),
      cut = cut, split_rule = "complete_segments_chronological"))
  }
  one <- segments[[1L]]
  n <- nrow(one)
  cut <- max(100L, min(n - 100L, floor(training_fraction * n)))
  list(training = one[seq_len(cut), , drop = FALSE],
    holdout = one[seq.int(cut + 1L, n), , drop = FALSE],
    cut = cut, split_rule = "single_segment_contiguous_tail")
}

ou_gh_family_ccf_bank <- function() {
  expand.grid(horizon = c(1L, 5L, 20L, 60L),
    frequency = c(.25, .6, 1.2),
    instrument = c(0, .35), KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE)
}

ou_gh_family_validate_ccf_objective <- function(objective_weights) {
  required <- c(
    "innovation_ecf_weight", "conditional_prediction_loss_weight",
    "instrumented_moment_weight", "horizon_scaling_exponent"
  )
  stopifnot(
    is.list(objective_weights),
    identical(names(objective_weights), required),
    all(is.finite(unlist(objective_weights, use.names = FALSE))),
    all(unlist(objective_weights[required[1:3]], use.names = FALSE) >= 0),
    objective_weights$horizon_scaling_exponent > 0
  )
  objective_weights
}

ou_gh_family_ccf_score <- function(fit, states, bank = ou_gh_family_ccf_bank(),
    quadrature_nodes = 24L, pair_cap = 5000L,
    objective_weights = FULL_FAMILY_GH_CCF_OBJECTIVE) {
  objective_weights <- ou_gh_family_validate_ccf_objective(objective_weights)
  segments <- ou_gh_family_normalise_segments(states)
  transitions <- do.call(rbind, segments)
  increments <- transitions$x_current - transitions$x_previous
  scale <- max(diff(stats::quantile(increments, c(.25,.75),
    names = FALSE, type = 8)) / 1.349, 1e-5)
  total <- 0
  used <- 0L
  for (h in unique(bank$horizon)) {
    pairs <- ou_gh_build_horizon_pairs(transitions, h)
    if (!nrow(pairs)) next
    x0 <- pairs$x_previous
    xh <- pairs$x_current
    duration <- pairs$active_horizon
    if (length(x0) > pair_cap) {
      index <- unique(round(seq(1, length(x0), length.out = pair_cap)))
      x0 <- x0[index]; xh <- xh[index]; duration <- duration[index]
    }
    attenuation <- exp(-fit[["kappa"]] * duration)
    location <- fit[["mu"]] + attenuation * (x0 - fit[["mu"]])
    one <- bank[bank$horizon == h, , drop = FALSE]
    for (frequency in unique(one$frequency)) {
      u <- frequency / scale
      duration_key <- format(round(duration, 12L), scientific = FALSE, trim = TRUE)
      unique_duration <- unique(duration_key)
      remainder_by_duration <- setNames(vapply(unique_duration, function(key) {
        exp(ou_gh_family_remainder_log_cf(
          u, as.numeric(key), fit, quadrature_nodes = quadrature_nodes
        ))
      }, complex(1L)), unique_duration)
      remainder <- unname(remainder_by_duration[duration_key])
      residual <- exp(1i * u * xh) - exp(1i * u * location) * remainder
      # Squared complex prediction loss is a proper conditional-mean score for
      # E[exp(iu X[t+h]) | X[t]].  It prevents the degenerate tiny-innovation
      # solution that can make a small collection of unconditional CCF moments
      # cancel in finite samples while badly missing the transition law.
      innovation <- xh - location
      innovation_ecf_error <- mean(exp(1i * u * innovation)) - mean(remainder)
      horizon_scale <- h^objective_weights$horizon_scaling_exponent
      total <- total +
        objective_weights$innovation_ecf_weight *
          Mod(innovation_ecf_error)^2 / horizon_scale +
        objective_weights$conditional_prediction_loss_weight *
          mean(Mod(residual)^2) / horizon_scale
      used <- used + 1L
      for (instrument in one$instrument[one$frequency == frequency]) {
        moment <- mean(residual * exp(1i * instrument * x0 / scale)) /
          horizon_scale
        total <- total + objective_weights$instrumented_moment_weight *
          (Re(moment)^2 + Im(moment)^2)
        used <- used + 1L
      }
    }
  }
  if (!used) return(Inf)
  total / used
}

ou_gh_family_model_regime <- function(model_id) {
  switch(model_id,
    NIG = "interior_GH", hyperbolic = "interior_GH",
    symmetric_GH = "interior_GH", model_id)
}

ou_gh_family_start_bank <- function(model_id, profile) {
  regime <- ou_gh_family_model_regime(model_id)
  s <- profile$sigma_eta_1
  k <- profile$kappa
  mu <- profile$mu
  skew <- profile$skew_proxy
  starts <- switch(model_id,
    interior_GH = list(
      c(mu, log(k), log(s), .5, log(2), atanh(.45 * skew)),
      c(mu, log(k), log(s), -.5, log(.4), atanh(.7 * skew))),
    NIG = list(c(mu, log(k), log(s), -.5, log(1.5), atanh(.5 * skew))),
    hyperbolic = list(c(mu, log(k), log(s), 1, log(2), atanh(.5 * skew))),
    symmetric_GH = list(c(mu, log(k), log(s), .5, log(1), 0)),
    VG_boundary = list(
      c(mu, log(k), log(s), log(.7), atanh(.5 * skew)),
      c(mu, log(k), log(s), log(3), atanh(.8 * skew))),
    skew_t_boundary = list(
      c(mu, log(k), log(max(s, 1e-3)), log(4), 1.2 * skew),
      c(mu, log(k), log(max(s, 1e-3)), log(1), .6 * sign(skew + 1e-8))),
    symmetric_Student_t_boundary = list(
      c(mu, log(k), log(max(s, 1e-3)), log(4)),
      c(mu, log(k), log(max(s, 1e-3)), log(1))),
    Gaussian_limit = list(c(mu, log(k), log(s))),
    stop("Unknown model id.", call. = FALSE)
  )
  lapply(starts, function(x) setNames(x, ou_gh_family_raw_names(regime)))
}

ou_gh_family_apply_restriction <- function(raw, model_id) {
  if (model_id == "NIG") raw[["lambda"]] <- -.5
  if (model_id == "hyperbolic") raw[["lambda"]] <- 1
  if (model_id == "symmetric_GH") raw[["atanh_rho"]] <- 0
  raw
}

ou_gh_family_free_names <- function(model_id) {
  regime <- ou_gh_family_model_regime(model_id)
  names <- ou_gh_family_raw_names(regime)
  if (model_id %in% c("NIG", "hyperbolic")) names <- setdiff(names, "lambda")
  if (model_id == "symmetric_GH") names <- setdiff(names, "atanh_rho")
  names
}

ou_gh_family_fit_candidate <- function(model_id, training_states,
    holdout_states, evaluation_budget = 90L, iteration_budget = 70L,
    quadrature_nodes = 24L,
    objective_weights = FULL_FAMILY_GH_CCF_OBJECTIVE) {
  started <- proc.time()[["elapsed"]]
  profile <- ou_gh_family_profile_states(training_states)
  regime <- ou_gh_family_model_regime(model_id)
  bounds <- ou_gh_family_raw_bounds(regime)
  starts <- ou_gh_family_start_bank(model_id, profile)
  free <- ou_gh_family_free_names(model_id)
  attempts <- vector("list", 2L * length(starts))
  for (j in seq_along(starts)) {
    template <- ou_gh_family_apply_restriction(starts[[j]], model_id)
    objective <- function(value) {
      raw <- template
      raw[free] <- value
      raw <- ou_gh_family_apply_restriction(raw, model_id)
      fit <- tryCatch(ou_gh_family_raw_to_fit(raw, regime), error = function(e) NULL)
      if (is.null(fit)) return(1e6)
      status <- ou_gh_family_moment_status(fit, regime)
      if (!status$centred_OU_admissible) return(1e6)
      tryCatch(ou_gh_family_ccf_score(
        fit, training_states, quadrature_nodes = quadrature_nodes,
        objective_weights = objective_weights
      ), error = function(e) 1e6)
    }
    start_objective <- objective(template[free])
    attempts[[2L * j - 1L]] <- list(start_id = j,
      attempt_type = "profile_start", objective = start_objective,
      status = if (is.finite(start_objective)) "completed" else "nonfinite",
      message = "unoptimised deterministic profile start", raw = template)
    result <- tryCatch(stats::nlminb(
      start = template[free], objective = objective,
      lower = bounds$lower[free], upper = bounds$upper[free],
      control = list(eval.max = evaluation_budget, iter.max = iteration_budget,
        rel.tol = 1e-7, x.tol = 1e-6, abs.tol = 1e-9)
    ), error = identity)
    if (inherits(result, "error")) {
      attempts[[2L * j]] <- list(start_id = j, attempt_type = "nlminb",
        objective = Inf,
        status = "optimiser_error", message = conditionMessage(result))
    } else {
      raw <- template; raw[free] <- result$par
      raw <- ou_gh_family_apply_restriction(raw, model_id)
      attempts[[2L * j]] <- list(start_id = j, attempt_type = "nlminb",
        objective = result$objective,
        status = if (is.finite(result$objective)) "completed" else "nonfinite",
        message = result$message, convergence = result$convergence,
        evaluations = result$evaluations, raw = raw)
    }
  }
  for (j in seq_along(attempts)) {
    if (is.null(attempts[[j]]$raw)) {
      attempts[[j]]$training_score <- attempts[[j]]$holdout_score <- Inf
      next
    }
    one_fit <- tryCatch(ou_gh_family_raw_to_fit(attempts[[j]]$raw, regime),
      error = function(e) NULL)
    if (is.null(one_fit)) {
      attempts[[j]]$training_score <- attempts[[j]]$holdout_score <- Inf
    } else {
      attempts[[j]]$training_score <- ou_gh_family_ccf_score(one_fit,
        training_states, quadrature_nodes = quadrature_nodes,
        objective_weights = objective_weights)
      attempts[[j]]$holdout_score <- ou_gh_family_ccf_score(one_fit,
        holdout_states, quadrature_nodes = quadrature_nodes,
        objective_weights = objective_weights)
    }
  }
  holdout <- vapply(attempts, `[[`, numeric(1L), "holdout_score")
  selected <- if (any(is.finite(holdout))) which.min(holdout) else NA_integer_
  if (is.na(selected)) return(list(model_id = model_id, regime = regime,
    fit_status = "fit_unavailable", attempts = attempts,
    runtime_seconds = proc.time()[["elapsed"]] - started))
  fit <- ou_gh_family_raw_to_fit(attempts[[selected]]$raw, regime)
  train_score <- attempts[[selected]]$training_score
  holdout_score <- attempts[[selected]]$holdout_score
  status <- ou_gh_family_moment_status(fit, regime)
  list(model_id = model_id, regime = regime, fit_status = "success",
    fit = fit, raw = attempts[[selected]]$raw, attempts = attempts,
    training_score = train_score, formation_holdout_score = holdout_score,
    selected_start = selected, moment_status = status,
    fit_hash = ou_gh_hash_object(list(model_id=model_id, fit=fit,
      estimator="exact_transition_CCF_v2_irregular_active_horizons")),
    runtime_seconds = proc.time()[["elapsed"]] - started,
    testing_data_used = FALSE)
}

ou_gh_family_fit_candidates <- function(states,
    candidate_models = c("interior_GH", "VG_boundary", "skew_t_boundary",
      "symmetric_Student_t_boundary", "NIG", "hyperbolic",
      "symmetric_GH", "Gaussian_limit"),
    evaluation_budget = 90L, quadrature_nodes = 24L,
    objective_weights = FULL_FAMILY_GH_CCF_OBJECTIVE) {
  split <- ou_gh_family_split_states(states)
  fits <- lapply(candidate_models, function(model_id) {
    ou_gh_family_fit_candidate(model_id, split$training, split$holdout,
      evaluation_budget = evaluation_budget,
      quadrature_nodes = quadrature_nodes,
      objective_weights = objective_weights)
  })
  names(fits) <- candidate_models
  fits
}

ou_gh_family_candidate_topology <- function() {
  data.frame(
    candidate_name = c(
      "interior_GH", "VG_boundary", "skew_t_boundary",
      "symmetric_Student_t_boundary", "NIG", "hyperbolic",
      "symmetric_GH", "Gaussian_limit"
    ),
    parameter_dimension = c(6L, 5L, 5L, 4L, 5L, 5L, 5L, 3L),
    is_exact_restriction = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE),
    is_exact_boundary = c(FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE),
    parent_family = c(NA, "interior_GH", "interior_GH", "skew_t_boundary",
                      "interior_GH", "interior_GH", "interior_GH", NA),
    restriction_type = c(
      "unrestricted_interior", "chi_zero_gamma_boundary",
      "psi_zero_inverse_gamma_boundary", "symmetric_inverse_gamma_boundary",
      "lambda_minus_half", "lambda_one", "beta_zero",
      "analytic_degenerate_mixing_control"
    ),
    parsimony_eligible = c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE),
    stringsAsFactors = FALSE
  )
}

ou_gh_family_route_candidates <- function(candidate_fits,
    equivalence_absolute = 5e-5, equivalence_relative = .05,
    topology = ou_gh_family_candidate_topology()) {
  success <- candidate_fits[vapply(candidate_fits, function(x) {
    identical(x$fit_status, "success") && is.finite(x$formation_holdout_score)
  }, logical(1L))]
  if (!length(success)) return(list(router_status = "fit_unavailable"))
  missing_topology <- setdiff(names(success), topology$candidate_name)
  if (length(missing_topology)) stop(
    "Full-family router lacks topology metadata for: ",
    paste(missing_topology, collapse = ", "), call. = FALSE
  )
  score <- vapply(success, `[[`, numeric(1L), "formation_holdout_score")
  best <- min(score)
  band <- max(equivalence_absolute, equivalence_relative * best)
  equivalent <- names(score)[score <= best + band]
  equivalent_topology <- topology[match(equivalent, topology$candidate_name), , drop = FALSE]
  restrictions <- equivalent_topology$candidate_name[
    equivalent_topology$parsimony_eligible %in% TRUE
  ]
  if (length(equivalent) > 1L) {
    if (length(restrictions) == 1L) {
      selected <- restrictions[[1L]]
      status <- "unique_exact_restriction_selected"
    } else {
      selected <- names(which.min(score))
      status <- "multiple_candidates_indistinguishable_numeric_minimum_retained"
    }
  } else {
    selected <- equivalent[[1L]]
    status <- paste0(selected, "_selected")
  }
  list(selected_model = selected,
    selected_regime = success[[selected]]$regime,
    router_status = status, router_evidence = "formation_holdout_exact_transition_CCF",
    best_score = best, equivalence_band = band,
    indistinguishable_candidates = paste(equivalent, collapse = ";"),
    equivalent_exact_restrictions = paste(restrictions, collapse = ";"),
    selected_parameter_dimension = topology$parameter_dimension[
      match(selected, topology$candidate_name)
    ],
    selected_restriction_type = topology$restriction_type[
      match(selected, topology$candidate_name)
    ],
    selected_fit = success[[selected]]$fit,
    centred_OU_admissibility = success[[selected]]$moment_status$centred_OU_admissible,
    finite_variance_status = success[[selected]]$moment_status$variance_exists,
    threshold_moment_contract_status =
      success[[selected]]$moment_status$threshold_moment_contract_admissible,
    testing_data_used = FALSE)
}

ou_gh_family_law_distance <- function(fit_a, fit_b,
    horizons = c(1, 5, 20, 60), frequencies = c(.2, .5, 1, 2),
    states = c(-1, 0, 1), quadrature_nodes = 48L) {
  maximum <- 0
  for (h in horizons) {
    for (u in frequencies) {
      ka <- ou_gh_family_remainder_log_cf(u, h, fit_a,
        quadrature_nodes = quadrature_nodes)
      kb <- ou_gh_family_remainder_log_cf(u, h, fit_b,
        quadrature_nodes = quadrature_nodes)
      aa <- exp(-fit_a[["kappa"]] * h)
      ab <- exp(-fit_b[["kappa"]] * h)
      for (x in states) {
        la <- fit_a[["mu"]] + aa * (x - fit_a[["mu"]])
        lb <- fit_b[["mu"]] + ab * (x - fit_b[["mu"]])
        maximum <- max(maximum,
          Mod(exp(1i*u*la + ka) - exp(1i*u*lb + kb)))
      }
    }
  }
  maximum
}
