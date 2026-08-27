# Formation-only affine Gaussian state-space hedge.
#
# The production route estimates q and v_e independently in every formation
# window.  The fixed-parameter route is retained only for numerical tests and
# diagnostics.  The public return object deliberately preserves the original
# six-field contract; optimisation diagnostics are exposed separately by
# fit_formation_kalman_parameters().

kalman_affine_filter <- function(y, x, active_dt, q, ve,
                                 initial_state = c(alpha = 0, beta = 1),
                                 initial_covariance = diag(2),
                                 segment_id = rep(1L, length(y)),
                                 keep_internal_paths = TRUE) {
  y <- as.numeric(y); x <- as.numeric(x); active_dt <- as.numeric(active_dt)
  segment_id <- as.character(segment_id)
  if (length(y) != length(x) || length(y) != length(active_dt) ||
      length(y) != length(segment_id) || !length(y)) {
    stop("y, x, active_dt and segment_id must have the same positive length.", call. = FALSE)
  }
  if (any(!is.finite(c(y, x, active_dt))) || any(active_dt < 0) ||
      anyNA(segment_id) || !is.finite(q) || q <= 0 ||
      !is.finite(ve) || ve <= 0) {
    stop("Kalman inputs, q and v_e violate the finite positive contract.", call. = FALSE)
  }
  # The submitted estimator has one prior per formation window.  Segment IDs
  # are retained as downstream transition diagnostics, but do not reset the
  # affine hedge state or covariance.  At an unusable incoming transition the
  # upstream clock supplies dt=0, so the filter carries its posterior forward
  # causally and performs the current observation update without inventing a
  # state-evolution interval.
  state <- setNames(as.numeric(initial_state), c("alpha", "beta"))
  covariance <- as.matrix(initial_covariance)
  if (length(state) != 2L || !all(dim(covariance) == c(2L, 2L)) ||
      any(!is.finite(c(state, covariance)))) {
    stop("The Kalman prior must be a finite two-state mean and 2x2 covariance.", call. = FALSE)
  }
  n <- length(y)
  state_path <- matrix(NA_real_, n, 2L, dimnames = list(NULL, c("alpha", "beta")))
  covariance_path <- array(NA_real_, c(2L, 2L, n))
  predicted_covariance_path <- array(NA_real_, c(2L, 2L, n))
  innovations <- innovation_variances <- rep(NA_real_, n)
  loglik <- 0
  for (i in seq_len(n)) {
    design <- c(1, x[[i]])
    predicted_covariance <- covariance + diag(q * active_dt[[i]], 2L)
    predicted_covariance <- (predicted_covariance + t(predicted_covariance)) / 2
    innovation <- y[[i]] - sum(design * state)
    innovation_variance <- drop(design %*% predicted_covariance %*% design) + ve
    if (!is.finite(innovation_variance) || innovation_variance <= 0) {
      stop("Kalman innovation variance is non-finite or non-positive.", call. = FALSE)
    }
    gain <- drop(predicted_covariance %*% design) / innovation_variance
    state <- state + gain * innovation
    covariance <- predicted_covariance - tcrossprod(gain, design) %*% predicted_covariance
    covariance <- (covariance + t(covariance)) / 2
    if (any(!is.finite(c(state, covariance)))) {
      stop("Kalman refilter produced a non-finite state or covariance.", call. = FALSE)
    }
    state_path[i, ] <- state
    covariance_path[, , i] <- covariance
    predicted_covariance_path[, , i] <- predicted_covariance
    innovations[[i]] <- innovation
    innovation_variances[[i]] <- innovation_variance
    loglik <- loglik - 0.5 * (
      log(2 * pi) + log(innovation_variance) + innovation^2 / innovation_variance
    )
  }
  out <- list(
    final_state = setNames(as.numeric(state), c("alpha", "beta")),
    final_covariance = covariance,
    state_path = state_path,
    loglik = as.numeric(loglik)
  )
  if (isTRUE(keep_internal_paths)) {
    out$covariance_path <- covariance_path
    out$predicted_covariance_path <- predicted_covariance_path
    out$innovations <- innovations
    out$innovation_variances <- innovation_variances
  }
  out
}

kalman_profile_axes <- function(y, x, n = 9L, span = 12) {
  fit <- stats::lm.fit(cbind(1, x), y)
  residual_variance <- stats::var(as.numeric(fit$residuals))
  if (!is.finite(residual_variance) || residual_variance <= 0) {
    residual_variance <- stats::var(diff(y))
  }
  if (!is.finite(residual_variance) || residual_variance <= 0) residual_variance <- 1e-8
  q_reference <- residual_variance / stats::median(1 + x^2)
  list(
    log_q = seq(log(q_reference) - span, log(q_reference) + span, length.out = n),
    log_ve = seq(log(residual_variance) - span, log(residual_variance) + span, length.out = n)
  )
}

kalman_boundary_status <- function(parameters, bounds, tolerance_fraction = 0.02) {
  width <- bounds[, 2L] - bounds[, 1L]
  near <- parameters - bounds[, 1L] <= tolerance_fraction * width |
    bounds[, 2L] - parameters <= tolerance_fraction * width
  if (any(near)) "boundary" else "interior"
}

fit_formation_kalman_parameters <- function(y, x, active_dt,
                                            segment_id = rep(1L, length(y)),
                                            profile_n = 9L,
                                            profile_span = 12,
                                            maxit = 80L,
                                            start_loglik_tolerance = 1e-3,
                                            boundary_tolerance_fraction = 0.02,
                                            covariance_psd_tolerance = -1e-10,
                                            minimum_converged_starts = 2L) {
  axes <- kalman_profile_axes(y, x, profile_n, profile_span)
  bounds <- rbind(log_q = range(axes$log_q), log_ve = range(axes$log_ve))
  objective <- function(log_parameters) {
    fit <- tryCatch(
      kalman_affine_filter(
        y, x, active_dt, q = exp(log_parameters[[1L]]),
        ve = exp(log_parameters[[2L]]), segment_id = segment_id,
        keep_internal_paths = FALSE
      ),
      error = function(e) NULL
    )
    if (is.null(fit) || !is.finite(fit$loglik)) .Machine$double.xmax / 100 else -fit$loglik
  }
  profile <- expand.grid(log_q = axes$log_q, log_ve = axes$log_ve)
  profile$negative_loglik <- apply(profile, 1L, objective)
  finite_profile <- which(is.finite(profile$negative_loglik))
  if (!length(finite_profile)) stop("The Kalman profile has no finite point.", call. = FALSE)
  best_profile <- finite_profile[[which.min(profile$negative_loglik[finite_profile])]]
  centre <- rowMeans(bounds)
  best_start <- unlist(profile[best_profile, c("log_q", "log_ve")], use.names = FALSE)
  starts <- rbind(
    profile_best = best_start,
    profile_center = centre,
    opposite_quadrant = c(
      if (best_start[[1L]] < centre[[1L]]) bounds[1L, 2L] else bounds[1L, 1L],
      if (best_start[[2L]] < centre[[2L]]) bounds[2L, 2L] else bounds[2L, 1L]
    )
  )
  optimisations <- lapply(seq_len(nrow(starts)), function(i) tryCatch(
    stats::optim(
      starts[i, ], objective, method = "L-BFGS-B",
      lower = bounds[, 1L], upper = bounds[, 2L],
      control = list(maxit = as.integer(maxit), factr = 1e7, pgtol = 1e-8)
    ),
    error = function(e) list(par = c(NA_real_, NA_real_), value = Inf,
                              convergence = 999L, message = conditionMessage(e))
  ))
  table <- do.call(rbind, lapply(seq_along(optimisations), function(i) {
    z <- optimisations[[i]]
    data.frame(
      start = rownames(starts)[[i]], log_q = z$par[[1L]], log_ve = z$par[[2L]],
      loglik = -z$value, convergence = as.integer(z$convergence),
      boundary_status = if (all(is.finite(z$par))) {
        kalman_boundary_status(z$par, bounds, boundary_tolerance_fraction)
      } else "non_finite",
      stringsAsFactors = FALSE
    )
  }))
  converged <- table$convergence == 0L & is.finite(table$loglik)
  if (!any(converged)) stop("No Kalman optimisation start converged.", call. = FALSE)
  best <- which(converged)[which.max(table$loglik[converged])]
  selected <- c(table$log_q[[best]], table$log_ve[[best]])
  refilter <- kalman_affine_filter(
    y, x, active_dt, exp(selected[[1L]]), exp(selected[[2L]]),
    segment_id = segment_id
  )
  eigen_min <- min(vapply(seq_len(dim(refilter$covariance_path)[[3L]]), function(i) {
    min(eigen(refilter$covariance_path[, , i], symmetric = TRUE, only.values = TRUE)$values)
  }, numeric(1L)))
  agreement <- diff(range(table$loglik[converged]))
  reasons <- c(
    if (sum(converged) < minimum_converged_starts) "insufficient_converged_starts",
    if (!is.finite(agreement) || agreement > start_loglik_tolerance) "deterministic_start_disagreement",
    if (table$boundary_status[[best]] != "interior") "parameter_bound_optimum",
    if (!is.finite(eigen_min) || eigen_min < covariance_psd_tolerance) "covariance_not_psd",
    if (!isTRUE(all.equal(refilter$final_state, refilter$state_path[nrow(refilter$state_path), ],
                          tolerance = 1e-12, check.attributes = FALSE))) "final_state_not_post_update"
  )
  reasons <- unique(reasons[nzchar(reasons)])
  if (length(reasons)) {
    stop("Kalman formation window is unsuitable: ", paste(reasons, collapse = ";"), call. = FALSE)
  }
  list(
    q = exp(selected[[1L]]), ve = exp(selected[[2L]]),
    log_parameters = setNames(selected, c("log_q", "log_ve")),
    loglik = refilter$loglik, refilter = refilter,
    starts = table, bounds = bounds, start_loglik_range = agreement,
    boundary_status = table$boundary_status[[best]],
    covariance_min_eigenvalue = eigen_min,
    parameterization = "log(q), log(v_e)", suitable = TRUE
  )
}

estimate_formation_kalman_hedge <- function(formation, q = NULL, ve = NULL, ...) {
  valid <- is.finite(formation$midpoint_y) & is.finite(formation$midpoint_x) &
    formation$midpoint_y > 0 & formation$midpoint_x > 0
  rows <- which(valid)
  if (length(rows) < 3L) stop("At least three accepted formation observations are required.", call. = FALSE)
  formation_valid <- formation[rows, , drop = FALSE]
  active_dt <- resolve_active_time_increments(formation, rows)
  segment_id <- if ("structural_segment_id" %in% names(formation_valid)) {
    formation_valid$structural_segment_id
  } else rep(1L, nrow(formation_valid))
  y <- log(formation_valid$midpoint_y); x <- log(formation_valid$midpoint_x)
  if (is.null(q) != is.null(ve)) stop("Supply both q and v_e, or neither.", call. = FALSE)
  fit <- if (is.null(q)) {
    fit_formation_kalman_parameters(y, x, active_dt, segment_id = segment_id, ...)$refilter
  } else {
    kalman_affine_filter(y, x, active_dt, q, ve, segment_id = segment_id)
  }
  frozen <- freeze_formation_spread(
    formation_valid, fit$final_state[["alpha"]], fit$final_state[["beta"]]
  )
  if (any(!is.finite(frozen$spread)) || !is.finite(frozen$centre)) {
    stop("Frozen formation spread is non-finite.", call. = FALSE)
  }
  # Preserve the public return schema and field order for downstream compatibility.
  list(
    final_state = fit$final_state,
    final_covariance = fit$final_covariance,
    state_path = fit$state_path,
    loglik = fit$loglik,
    frozen = frozen,
    formation_rows = rows
  )
}
