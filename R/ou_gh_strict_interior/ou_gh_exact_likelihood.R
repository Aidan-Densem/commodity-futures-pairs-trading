ou_gh_exact_likelihood_rows <- function(transitions, pair_cap = 12000L) {
  required <- c("x_previous", "x_current")
  ou_gh_assert(all(required %in% names(transitions)),
    "Exact-transition scoring requires previous and current states.")
  rows <- which(is.finite(transitions$x_previous) &
    is.finite(transitions$x_current))
  if (length(rows) > pair_cap) {
    rows <- rows[unique(round(seq(1, length(rows), length.out = pair_cap)))]
  }
  ou_gh_assert(length(rows) >= 500L,
    "Exact-transition scoring has too few finite one-minute pairs.")
  rows
}

ou_gh_exact_transition_log_score <- function(
    raw,
    transitions,
    retained_rows = NULL,
    fft_n = 4096L,
    range_sd = 24,
    quadrature_nodes = 24L,
    penalty = 1e6
) {
  bounds <- ou_gh_raw_bounds()
  raw <- setNames(as.numeric(raw), ou_gh_raw_parameter_names())
  if (length(raw) != 6L || any(!is.finite(raw)) ||
      any(raw < bounds$lower) || any(raw > bounds$upper)) return(penalty)
  retained_rows <- retained_rows %||% ou_gh_exact_likelihood_rows(transitions)
  fit <- tryCatch(ou_gh_raw_to_fit(raw), error = function(condition) NULL)
  if (is.null(fit)) return(penalty)
  table <- tryCatch(
    ou_gh_fft_remainder_table(
      fit, Delta = 1, n = fft_n, range_sd = range_sd,
      quadrature_nodes = quadrature_nodes,
      boundary_density_ratio_tolerance = 1e-6,
      negative_density_tolerance = 1e-7
    ),
    error = function(condition) NULL
  )
  if (is.null(table) || !isTRUE(table$valid)) return(penalty)
  attenuation <- exp(-fit[["kappa"]])
  location <- fit[["mu"]] + attenuation *
    (transitions$x_previous[retained_rows] - fit[["mu"]])
  innovations <- transitions$x_current[retained_rows] - location
  density <- stats::approx(
    table$x, pmax(table$density, .Machine$double.xmin),
    xout = innovations, method = "linear", ties = "ordered", rule = 1
  )$y
  if (any(!is.finite(density)) || any(density <= 0)) return(penalty)
  -mean(log(density))
}

ou_gh_refine_exact_likelihood <- function(
    raw_starts,
    transitions,
    bounds,
    pair_cap = 12000L,
    fft_n = 4096L,
    range_sd = 24,
    quadrature_nodes = 24L,
    evaluation_budget = 100L,
    iteration_budget = 80L
) {
  retained_rows <- ou_gh_exact_likelihood_rows(transitions, pair_cap)
  objective <- function(raw) ou_gh_exact_transition_log_score(
    raw, transitions, retained_rows, fft_n, range_sd, quadrature_nodes
  )
  attempts <- lapply(seq_along(raw_starts), function(index) {
    start <- raw_starts[[index]]
    start_names <- names(raw_starts)
    start_id <- if (!is.null(start_names) && nzchar(start_names[[index]])) {
      start_names[[index]]
    } else paste0("exact_", index)
    started <- proc.time()[["elapsed"]]
    result <- tryCatch(stats::nlminb(
      start = ou_gh_clamp_raw(start, bounds), objective = objective,
      lower = bounds$lower, upper = bounds$upper,
      control = list(
        eval.max = evaluation_budget, iter.max = iteration_budget,
        rel.tol = 1e-7, x.tol = 1e-6, abs.tol = 1e-8
      )
    ), error = identity)
    runtime <- proc.time()[["elapsed"]] - started
    if (inherits(result, "error")) return(list(
      start_id = start_id, status = "exact_likelihood_error",
      reason = conditionMessage(result), objective = Inf, raw = start,
      runtime_seconds = runtime
    ))
    list(
      start_id = start_id,
      status = if (is.finite(result$objective)) "completed" else "nonfinite",
      reason = "", objective = result$objective,
      raw = setNames(result$par, ou_gh_raw_parameter_names()),
      convergence = result$convergence, message = result$message,
      evaluations = result$evaluations[[1L]], iterations = result$iterations,
      runtime_seconds = runtime
    )
  })
  finite <- which(vapply(attempts, function(value) {
    is.finite(value$objective) && value$objective < 1e6
  }, logical(1L)))
  selected <- if (length(finite)) {
    finite[[which.min(vapply(attempts[finite], `[[`, numeric(1L), "objective"))]]
  } else NA_integer_
  list(
    status = if (is.na(selected)) "exact_likelihood_unavailable" else
      "exact_likelihood_refinement_completed",
    attempts = attempts, selected_index = selected,
    selected_raw = if (is.na(selected)) NULL else attempts[[selected]]$raw,
    selected_objective = if (is.na(selected)) NA_real_ else
      attempts[[selected]]$objective,
    retained_rows = retained_rows,
    retained_rows_hash = ou_gh_hash_object(retained_rows),
    fft_n = fft_n, range_sd = range_sd,
    quadrature_nodes = quadrature_nodes
  )
}

ou_gh_refine_exact_shape <- function(
    raw_start,
    transitions,
    profile,
    pair_cap = 8000L,
    fft_n = 2048L,
    range_sd = 24,
    quadrature_nodes = 12L,
    evaluation_budget = 60L,
    iteration_budget = 50L,
    start_id = "shape_profile_frozen"
) {
  raw_template <- setNames(as.numeric(raw_start), ou_gh_raw_parameter_names())
  raw_template[["mu"]] <- profile$mu
  raw_template[["log_kappa"]] <- log(profile$kappa)
  free_names <- c("log_sigma_eta_1", "lambda", "log_zeta", "atanh_rho")
  bounds <- ou_gh_task_bounds(profile)
  retained_rows <- ou_gh_exact_likelihood_rows(transitions, pair_cap)
  objective <- function(free) {
    raw <- raw_template
    raw[free_names] <- free
    ou_gh_exact_transition_log_score(
      raw, transitions, retained_rows, fft_n, range_sd, quadrature_nodes
    )
  }
  started <- proc.time()[["elapsed"]]
  result <- tryCatch(stats::nlminb(
    start = raw_template[free_names], objective = objective,
    lower = bounds$lower[free_names], upper = bounds$upper[free_names],
    control = list(
      eval.max = evaluation_budget, iter.max = iteration_budget,
      rel.tol = 1e-7, x.tol = 1e-6, abs.tol = 1e-8
    )
  ), error = identity)
  runtime <- proc.time()[["elapsed"]] - started
  if (inherits(result, "error")) return(list(
    status = "exact_shape_error", reason = conditionMessage(result),
    selected_raw = NULL, selected_objective = NA_real_,
    attempts = list(), runtime_seconds = runtime
  ))
  selected_raw <- raw_template
  selected_raw[free_names] <- result$par
  attempt <- list(
    start_id = start_id, status = "completed", reason = "",
    objective = result$objective, raw = selected_raw,
    convergence = result$convergence, message = result$message,
    evaluations = result$evaluations[[1L]], iterations = result$iterations,
    runtime_seconds = runtime
  )
  list(
    status = "exact_shape_refinement_completed", reason = "",
    attempts = list(attempt), selected_index = 1L,
    selected_raw = selected_raw, selected_objective = result$objective,
    retained_rows = retained_rows,
    retained_rows_hash = ou_gh_hash_object(retained_rows),
    fft_n = fft_n, range_sd = range_sd,
    quadrature_nodes = quadrature_nodes, runtime_seconds = runtime
  )
}

ou_gh_exact_screen_and_refine <- function(
    training_transitions,
    profile = ou_gh_preliminary_profile(training_transitions),
    screen_pair_cap = 3000L,
    refinement_pair_cap = 8000L,
    evaluation_budget = 60L,
    iteration_budget = 50L,
    freeze_location_profile = TRUE
) {
  bounds <- ou_gh_task_bounds(profile)
  starts <- ou_gh_deterministic_starts(profile, training_transitions)
  profile_residual <- training_transitions$x_current -
    (profile$mu + exp(-profile$kappa) *
      (training_transitions$x_previous - profile$mu))
  adaptive_range_sd <- min(120, max(24,
    ceiling(max(abs(profile_residual), na.rm = TRUE) /
      max(profile$sigma_eta_1, 1e-8) * 1.10)))
  if (isTRUE(freeze_location_profile)) {
    starts <- lapply(starts, function(start) {
      start$raw[["mu"]] <- profile$mu
      start$raw[["log_kappa"]] <- log(profile$kappa)
      start
    })
    epsilon <- 1e-10
    bounds$lower[["mu"]] <- profile$mu - epsilon
    bounds$upper[["mu"]] <- profile$mu + epsilon
    bounds$lower[["log_kappa"]] <- log(profile$kappa) - epsilon
    bounds$upper[["log_kappa"]] <- log(profile$kappa) + epsilon
  }
  retained_screen <- ou_gh_exact_likelihood_rows(
    training_transitions, screen_pair_cap
  )
  screen <- do.call(rbind, lapply(starts, function(start) {
    started <- proc.time()[["elapsed"]]
    objective <- ou_gh_exact_transition_log_score(
      start$raw, training_transitions, retained_screen,
      fft_n = 1024L, range_sd = adaptive_range_sd,
      quadrature_nodes = 12L
    )
    data.frame(
      start_id = start$start_id, objective = objective,
      runtime_seconds = proc.time()[["elapsed"]] - started,
      stringsAsFactors = FALSE
    )
  }))
  valid <- which(is.finite(screen$objective) & screen$objective < 1e6)
  if (!length(valid)) return(list(
    status = "exact_likelihood_screen_failed", profile = profile,
    bounds = bounds, screen = screen, refinement = NULL
  ))
  selected_screen <- valid[[which.min(screen$objective[valid])]]
  selected_start_id <- screen$start_id[[selected_screen]]
  selected_start <- starts[[match(
    selected_start_id, vapply(starts, `[[`, character(1L), "start_id")
  )]]$raw
  refinement <- if (isTRUE(freeze_location_profile)) {
    ou_gh_refine_exact_shape(
      selected_start, training_transitions, profile,
      pair_cap = refinement_pair_cap, fft_n = 2048L,
      range_sd = adaptive_range_sd, quadrature_nodes = 12L,
      evaluation_budget = evaluation_budget,
      iteration_budget = iteration_budget,
      start_id = selected_start_id
    )
  } else {
    ou_gh_refine_exact_likelihood(
      setNames(list(selected_start), selected_start_id),
      training_transitions, bounds,
      pair_cap = refinement_pair_cap, fft_n = 2048L,
      range_sd = adaptive_range_sd, quadrature_nodes = 12L,
      evaluation_budget = evaluation_budget,
      iteration_budget = iteration_budget
    )
  }
  list(
    status = refinement$status, profile = profile, bounds = bounds,
    starts = starts, screen = screen,
    selected_screen_start_id = selected_start_id,
    refinement = refinement,
    selected_raw = refinement$selected_raw,
    selected_fit = if (is.null(refinement$selected_raw)) NULL else
      ou_gh_raw_to_fit(refinement$selected_raw),
    location_profile_frozen = isTRUE(freeze_location_profile),
    adaptive_range_sd = adaptive_range_sd
  )
}
