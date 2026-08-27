ou_gh_live_scale_audit <- function(manifest, maximum_tasks = 60L) {
  pair_first <- !duplicated(manifest$Pair)
  selected <- which(pair_first)
  if (length(selected) < maximum_tasks) {
    remaining <- setdiff(seq_len(nrow(manifest)), selected)
    selected <- c(selected, head(remaining, maximum_tasks - length(selected)))
  }
  selected <- head(selected, maximum_tasks)
  rows <- lapply(selected, function(index) {
    task <- manifest[index, , drop = FALSE]
    started <- proc.time()[["elapsed"]]
    result <- tryCatch({
      prepared <- ou_gh_prepare_selected_formation(task)
      innovation <- prepared$scaled_transitions$x_current -
        prepared$scaled_transitions$x_previous
      data.frame(
        task_key = task$task_key,
        Pair = task$Pair,
        formation_scale = prepared$scaling$scale,
        scaled_difference_mad = 1.4826 * stats::median(
          abs(innovation - stats::median(innovation))
        ),
        scaled_difference_sd = stats::sd(innovation),
        n_transitions = prepared$n_transitions,
        n_segments = prepared$n_segments,
        status = "success",
        reason = "",
        runtime_seconds = proc.time()[["elapsed"]] - started,
        stringsAsFactors = FALSE
      )
    }, error = function(condition) {
      data.frame(
        task_key = task$task_key, Pair = task$Pair,
        formation_scale = NA_real_, scaled_difference_mad = NA_real_,
        scaled_difference_sd = NA_real_, n_transitions = NA_integer_,
        n_segments = NA_integer_, status = "failure",
        reason = conditionMessage(condition),
        runtime_seconds = proc.time()[["elapsed"]] - started,
        stringsAsFactors = FALSE
      )
    })
    result
  })
  do.call(rbind, rows)
}

ou_gh_operational_design <- function(live_scale_audit = NULL) {
  half_lives <- c(30, 100, 300, 1500, 8000, 30000, 100000)
  horizons <- c(1, 2, 5, 10, 15, 30, 60, 120, 240, 630)
  shapes <- data.frame(
    shape_id = c(
      "negative_extreme", "symmetric_vg_near", "nig_moderate_skew",
      "near_zero_negative_skew", "hyperbolic_strong_skew",
      "positive_concentrated", "supported_edge"
    ),
    lambda = c(-10, -2, -0.5, 0.01, 1, 5, 10),
    zeta = c(0.01, 0.001, 0.1, 1, 10, 100, 1000),
    rho = c(-0.75, 0, 0.5, -0.5, 0.8, -0.8, 0.95),
    stringsAsFactors = FALSE
  )
  scale_values <- c(0.02, 0.05, 0.10, 0.20)
  if (!is.null(live_scale_audit)) {
    observed <- live_scale_audit$scaled_difference_mad[
      live_scale_audit$status == "success" &
        is.finite(live_scale_audit$scaled_difference_mad) &
        live_scale_audit$scaled_difference_mad > 0
    ]
    if (length(observed)) {
      scale_values <- unique(as.numeric(stats::quantile(
        observed, c(0.05, 0.25, 0.5, 0.95), names = FALSE, type = 8
      )))
    }
  }
  design <- expand.grid(
    half_life = half_lives,
    horizon = horizons,
    shape_row = seq_len(nrow(shapes)),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  design <- design[order(design$half_life, design$horizon, design$shape_row), ]
  design$case_id <- sprintf("operational_%04d", seq_len(nrow(design)))
  design$shape_id <- shapes$shape_id[design$shape_row]
  design$lambda <- shapes$lambda[design$shape_row]
  design$zeta <- shapes$zeta[design$shape_row]
  design$rho <- shapes$rho[design$shape_row]
  design$sigma_eta_1 <- scale_values[
    1L + ((seq_len(nrow(design)) - 1L) %% length(scale_values))
  ]
  design$shape_row <- NULL
  design
}

ou_gh_finite_difference_map_condition <- function(row, step = 1e-5) {
  base <- c(
    lambda = row$lambda,
    log_zeta = log(row$zeta),
    atanh_rho = atanh(row$rho),
    log_sigma = log(row$sigma_eta_1),
    log_kappa = log(log(2) / row$half_life)
  )
  evaluate <- function(raw) {
    fit <- gh_shape_scale_to_direct(
      raw[["lambda"]], exp(raw[["log_zeta"]]),
      tanh(raw[["atanh_rho"]]), exp(raw[["log_sigma"]]),
      exp(raw[["log_kappa"]])
    )
    c(
      log_alpha = log(fit[["alpha"]]), beta_over_alpha = fit[["rho"]],
      log_delta = log(fit[["delta"]]), centred_m = fit[["centred_m"]],
      log_driver_sd = log(fit[["driver_sd"]])
    )
  }
  jacobian <- matrix(NA_real_, nrow = 5L, ncol = 5L,
    dimnames = list(names(evaluate(base)), names(base)))
  for (column in seq_along(base)) {
    plus <- minus <- base
    plus[[column]] <- plus[[column]] + step
    minus[[column]] <- minus[[column]] - step
    jacobian[, column] <- (evaluate(plus) - evaluate(minus)) / (2 * step)
  }
  singular <- svd(jacobian)$d
  c(
    jacobian_min_singular = min(singular),
    jacobian_max_singular = max(singular),
    jacobian_condition = max(singular) / min(singular)
  )
}

ou_gh_evaluate_operational_case <- function(row) {
  started <- proc.time()[["elapsed"]]
  result <- tryCatch({
    support <- ou_gh_validate_production_support(
      row$lambda, row$zeta, row$rho, row$half_life
    )
    ou_gh_assert(support$valid, paste(support$reasons, collapse = ";"))
    fit <- gh_shape_scale_to_direct(
      row$lambda, row$zeta, row$rho, row$sigma_eta_1,
      log(2) / row$half_life
    )
    ratio <- gh_bessel_ratio_terms(row$lambda, row$zeta)
    filtered_sd <- sqrt(ou_gh_remainder_cumulants(
      2L, row$horizon, fit
    )[[1L]])
    multipliers <- c(-4, -2.5, -1.5, -1, -0.75, -0.5, -0.35,
      -0.2, -0.1, -0.05, 0, 0.05, 0.1, 0.2, 0.35, 0.5, 0.75,
      1, 1.5, 2.5, 4)
    frequencies <- multipliers / filtered_sd
    exponent <- gh_driver_log_exponent(frequencies, fit)
    permutation <- c(11, 1, 21, 2, 20, 3, 19, 4, 18, 5, 17, 6,
      16, 7, 15, 8, 14, 9, 13, 10, 12)
    permuted <- gh_driver_log_exponent(frequencies[permutation], fit)
    restored <- permuted[match(seq_along(frequencies), permutation)]
    scalar <- vapply(frequencies, function(u) {
      gh_driver_log_exponent(u, fit)
    }, complex(1L))
    quadrature <- lapply(c(12L, 24L, 48L, 64L), function(nodes) {
      ou_gh_remainder_log_cf(frequencies, row$horizon, fit, nodes)
    })
    names(quadrature) <- c("q12", "q24", "q48", "q64")
    semigroup <- ou_gh_semigroup_error(
      fit, 0.37 * row$horizon, 0.63 * row$horizon,
      frequencies, 48L
    )
    map_back <- gh_direct_to_shape_scale(
      fit[["lambda"]], fit[["alpha"]], fit[["beta"]], fit[["delta"]],
      fit[["kappa"]]
    )
    roundtrip <- max(abs(
      map_back[c("lambda", "zeta", "rho", "sigma_eta_1")] -
        fit[c("lambda", "zeta", "rho", "sigma_eta_1")]
    ) / pmax(abs(fit[c("lambda", "zeta", "rho", "sigma_eta_1")]), 1e-12))
    data.frame(
      status = "pass", reason = "",
      filtered_sd = filtered_sd,
      maximum_absolute_effective_frequency = max(abs(frequencies)),
      bessel_relative_gap = ratio[["relative_gap"]],
      roundtrip_error = roundtrip,
      order_permutation_error = max(Mod(exponent - restored)),
      scalar_vector_cf_error = max(Mod(exp(exponent) - exp(scalar))),
      branch_max_positive_real_part = max(pmax(Re(exponent), 0)),
      quadrature_12_vs_64 = max(Mod(quadrature$q12 - quadrature$q64)),
      quadrature_24_vs_64 = max(Mod(quadrature$q24 - quadrature$q64)),
      quadrature_48_vs_64 = max(Mod(quadrature$q48 - quadrature$q64)),
      semigroup_error = semigroup,
      runtime_seconds = proc.time()[["elapsed"]] - started,
      stringsAsFactors = FALSE
    )
  }, error = function(condition) {
    data.frame(
      status = "fail", reason = conditionMessage(condition),
      filtered_sd = NA_real_, maximum_absolute_effective_frequency = NA_real_,
      bessel_relative_gap = NA_real_, roundtrip_error = NA_real_,
      order_permutation_error = NA_real_, scalar_vector_cf_error = NA_real_,
      branch_max_positive_real_part = NA_real_, quadrature_12_vs_64 = NA_real_,
      quadrature_24_vs_64 = NA_real_, quadrature_48_vs_64 = NA_real_,
      semigroup_error = NA_real_,
      runtime_seconds = proc.time()[["elapsed"]] - started,
      stringsAsFactors = FALSE
    )
  })
  result
}

ou_gh_run_operational_hardening <- function(design) {
  results <- lapply(seq_len(nrow(design)), function(index) {
    cbind(design[index, , drop = FALSE],
      ou_gh_evaluate_operational_case(design[index, , drop = FALSE]))
  })
  do.call(rbind, results)
}
