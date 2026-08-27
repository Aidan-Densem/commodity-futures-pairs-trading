gaussian_analytic_threshold_from_candidate <- function(candidate) {
  sd_stat <- as.numeric(candidate$gaussian_stationary_sd[[1L]])
  cost <- as.numeric(candidate$v2_cost_primary[[1L]])
  centre <- as.numeric(candidate$ou_equilibrium[[1L]])
  solution <- gou_solve_zl_root(cost / sd_stat, criterion = "half_cycle_mean_exit")
  if (!isTRUE(solution$success)) stop("Gaussian analytic threshold solver failed.", call. = FALSE)
  distance <- solution$a * sd_stat
  objective <- gou_zl_objective_dimensionless(
    solution$a, cost / sd_stat, "half_cycle_mean_exit"
  ) * sd_stat * candidate$kappa_per_active_minute
  tradeable <- is.finite(objective) &&
    objective > production_config$threshold_mc$outside_option +
      production_config$threshold_mc$outside_option_tolerance
  data.frame(
    Pair = candidate$pair_id, Session_Date = as.Date(candidate$endpoint_session_date),
    model = "gaussian_analytic",
    upper_entry = if (tradeable) centre + distance else NA_real_,
    lower_entry = if (tradeable) centre - distance else NA_real_,
    upper_exit = if (tradeable) centre else NA_real_,
    lower_exit = if (tradeable) centre else NA_real_,
    objective_value = objective,
    route_status = if (tradeable) "TRADEABLE" else "MODEL_NO_TRADE",
    strategy_available = tradeable,
    formation_cost_proxy_roundtrip_log = cost,
    testing_data_used_for_calibration = FALSE, stringsAsFactors = FALSE
  )
}

build_gaussian_mc_threshold_tasks <- function(selected_schedule) {
  selected <- selected_schedule[selected_schedule$selected %in% TRUE, , drop = FALSE]
  lapply(seq_len(nrow(selected)), function(i) {
    row <- selected[i, , drop = FALSE]
    parameters <- v2_gaussian_ou_parameters(
      row$ou_equilibrium, row$kappa_per_active_minute,
      stationary_sd = row$gaussian_stationary_sd
    )
    list(
      model = "gaussian_mc", pair = row$pair_id,
      endpoint_date = row$endpoint_session_date,
      simulator = v2_gaussian_simulator(parameters),
      centre = row$ou_equilibrium, stationary_sd = row$gaussian_stationary_sd,
      horizon = as.integer(row$testing_active_minutes),
      roundtrip_cost = row$v2_cost_primary,
      parameter_hash = if (requireNamespace("digest", quietly = TRUE)) {
        digest::digest(parameters, algo = "sha256")
      } else paste(signif(parameters, 12), collapse = ":"),
      parameter_source_hash = "formation_candidate_metrics"
    )
  })
}

build_strict_interior_gh_threshold_tasks <- function(parameter_results,
                                                     selected_schedule) {
  selected <- selected_schedule[selected_schedule$selected %in% TRUE, , drop = FALSE]
  by_key <- setNames(parameter_results, vapply(parameter_results, function(x) {
    x$task$task_key[[1L]]
  }, character(1L)))
  tasks <- lapply(seq_len(nrow(selected)), function(i) {
    row <- selected[i, , drop = FALSE]
    key <- paste(row$endpoint_id, sprintf("rank_%02d", row$primary_rank), row$pair_id, sep = "__")
    result <- by_key[[key]]
    if (!is.null(result) && exists("validate_gh_mode", mode = "function")) {
      validate_gh_mode(result, "STRICT_INTERIOR")
    }
    if (is.null(result) || !startsWith(result$fit_status, "fit_success") ||
        is.null(result$fit_natural)) return(NULL)
    tryCatch({
      fit <- result$fit_natural
      table <- ou_gh_fgmc_build_table(fit, Delta = 1)
      simulator <- list(
        family = "strict_interior_OU_GH", fit = fit,
        simulator_hash = ou_gh_hash_object(list(fit = fit, diagnostics = table$diagnostics)),
        simulate_paths = function(active_time, x0, n_paths, seed) {
          ou_gh_simulate_prebuilt_table(active_time, x0, n_paths, seed, fit, table)
        }
      )
      list(
        model = "strict_interior_gh", pair = row$pair_id,
        endpoint_date = row$endpoint_session_date, simulator = simulator,
        centre = fit[["mu"]], stationary_sd = ou_gh_stationary_sd(fit),
        horizon = as.integer(row$testing_active_minutes),
        roundtrip_cost = row$v2_cost_primary,
        parameter_hash = result$fit_hash,
        parameter_source_hash = result$prepared$formation_hash
      )
    }, error = function(e) list(
      model = "strict_interior_gh", pair = row$pair_id,
      endpoint_date = row$endpoint_session_date,
      task_construction_failure = conditionMessage(e)
    ))
  })
  Filter(Negate(is.null), tasks)
}
