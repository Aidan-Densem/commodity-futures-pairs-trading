# Resumable, model-specific finite-horizon threshold calibration.

v2_threshold_candidate_grid <- function(stationary_sd, roundtrip_cost, n = 7L) {
  v2_assert(is.finite(stationary_sd) && stationary_sd > 0 && is.finite(roundtrip_cost) && roundtrip_cost >= 0,
            "Threshold grid needs a positive scale and non-negative cost.")
  floor <- 1.02 * roundtrip_cost
  anchors <- unique(pmax(floor, stationary_sd * c(.25, .50, .75, 1, 1.5, 2, 3)))
  grid <- expand.grid(d_plus = anchors, d_minus = anchors)
  grid$candidate_id <- sprintf("coarse_%03d", seq_len(nrow(grid)))
  grid$c_plus <- 0; grid$c_minus <- 0
  grid[, c("candidate_id", "d_plus", "d_minus", "c_plus", "c_minus")]
}

v2_refine_grid <- function(selected, level = "intermediate") {
  factors <- if (level == "intermediate") c(.80, .90, 1, 1.10, 1.20) else c(.94, .97, 1, 1.03, 1.06)
  grid <- expand.grid(d_plus = selected$d_plus[[1L]] * factors,
                      d_minus = selected$d_minus[[1L]] * factors)
  grid$candidate_id <- sprintf("%s_%03d", level, seq_len(nrow(grid)))
  grid$c_plus <- 0; grid$c_minus <- 0
  grid[, c("candidate_id", "d_plus", "d_minus", "c_plus", "c_minus")]
}

v2_combine_grid_batches <- function(batches) {
  ids <- as.character(batches[[1L]]$candidate_id)
  v2_assert(all(vapply(batches, function(x) identical(as.character(x$candidate_id), ids), logical(1L))),
            "Candidate order changed across path batches.")
  weights <- lapply(batches, function(x) x$valid_paths)
  total <- Reduce(`+`, weights)
  out <- batches[[1L]][, c("candidate_id", "d_plus", "d_minus", "c_plus", "c_minus",
                            "terminal_policy", "objective_version", "optimizer_status"), drop = FALSE]
  numeric_means <- c(
    "entry_probability", "ordinary_exit_probability", "forced_terminal_close_probability",
    "no_entry_probability", "mean_ordinary_reward", "mean_forced_close_reward",
    "forced_close_reward_q01", "forced_close_reward_q05", "forced_close_reward_q50",
    "forced_close_reward_q95", "forced_close_reward_q99", "mean_total_reward",
    "mean_duration", "mean_cycles"
  )
  for (field in numeric_means) {
    numerator <- Reduce(`+`, Map(function(x, w) {
      value <- as.numeric(x[[field]]); value[!is.finite(value)] <- 0; value * w
    }, batches, weights))
    denominator <- Reduce(`+`, Map(function(x, w) ifelse(is.finite(x[[field]]), w, 0), batches, weights))
    out[[field]] <- ifelse(denominator > 0, numerator / denominator, NA_real_)
  }
  out$objective_value <- out$mean_total_reward / out$mean_duration
  if (length(batches) == 1L) {
    out$MC_standard_error <- batches[[1L]]$MC_standard_error
  } else {
    batch_objectives <- do.call(cbind, lapply(batches, `[[`, "objective_value"))
    out$MC_standard_error <- apply(batch_objectives, 1L, stats::sd) / sqrt(length(batches))
  }
  out$valid_paths <- total
  out
}

v2_evaluate_simulator_grid <- function(simulator, horizon, centre, candidates,
                                       cost_plus, cost_minus, n_paths, path_batch_size,
                                       seed, terminal_policy = "liquidate_at_horizon") {
  starts <- seq.int(1L, as.integer(n_paths), by = as.integer(path_batch_size))
  batches <- lapply(starts, function(start) {
    count <- min(as.integer(path_batch_size), as.integer(n_paths) - start + 1L)
    simulation <- v2_simulate_paths(
      simulator, 0:as.integer(horizon), centre, count,
      as.integer((as.double(seed) + start - 1) %% .Machine$integer.max)
    )
    v2_evaluate_threshold_grid(
      simulation$paths, 0:as.integer(horizon), candidates, centre,
      cost_plus, cost_minus, terminal_policy
    )
  })
  v2_combine_grid_batches(batches)
}

v2_select_threshold <- function(table, enforce_outside_option = TRUE,
                                outside_option = 0, tolerance = 1e-12) {
  valid <- table[is.finite(table$objective_value), , drop = FALSE]
  v2_assert(nrow(valid) > 0L, "No finite threshold candidate.")
  best <- max(valid$objective_value)
  se <- valid$MC_standard_error
  se[!is.finite(se)] <- 0
  near <- valid$objective_value >= best - 2 * sqrt(se^2 + se[[which.max(valid$objective_value)]]^2)
  candidates <- valid[near, , drop = FALSE]
  centre <- stats::median(candidates$d_plus + candidates$d_minus)
  candidates$plateau_distance <- abs(candidates$d_plus + candidates$d_minus - centre)
  candidates$asymmetry <- abs(candidates$d_plus - candidates$d_minus)
  selected <- candidates[
    order(candidates$plateau_distance, candidates$asymmetry, candidates$candidate_id),
    , drop = FALSE
  ][1L, , drop = FALSE]
  selected$route_status <- if (isTRUE(enforce_outside_option) &&
                               selected$objective_value[[1L]] <= outside_option + tolerance) {
    "MODEL_NO_TRADE"
  } else "TRADEABLE"
  selected$strategy_available <- selected$route_status == "TRADEABLE"
  selected
}

calibrate_threshold_from_context <- function(
    model, pair, endpoint_date, simulator, centre, stationary_sd, horizon,
    roundtrip_cost, parameter_hash, parameter_source_hash,
    budgets = list(
      coarse_paths = 250L, intermediate_paths = 750L, final_paths = 10000L,
      path_batch_size = 250L, seed = 91001L
    ), contract = PRODUCTION_V2_CONTRACT) {
  v2_assert(is.list(simulator) && is.function(simulator$simulate_paths),
    "A validated model simulator is required.")
  v2_assert(is.finite(centre) && is.finite(stationary_sd) && stationary_sd > 0,
    "Threshold context has an invalid centre or stationary scale.")
  v2_assert(is.finite(horizon) && horizon >= 1 && is.finite(roundtrip_cost) &&
      roundtrip_cost >= 0, "Threshold horizon/cost context is invalid.")
  coarse_grid <- v2_threshold_candidate_grid(stationary_sd, roundtrip_cost)
  coarse <- v2_evaluate_simulator_grid(
    simulator, horizon, centre, coarse_grid, roundtrip_cost, roundtrip_cost,
    budgets$coarse_paths, budgets$path_batch_size, budgets$seed
  )
  intermediate_grid <- v2_refine_grid(
    v2_select_threshold(coarse, enforce_outside_option = FALSE), "intermediate"
  )
  intermediate <- v2_evaluate_simulator_grid(
    simulator, horizon, centre, intermediate_grid, roundtrip_cost, roundtrip_cost,
    budgets$intermediate_paths, budgets$path_batch_size, budgets$seed
  )
  final_grid <- v2_refine_grid(
    v2_select_threshold(intermediate, enforce_outside_option = FALSE), "final"
  )
  final <- v2_evaluate_simulator_grid(
    simulator, horizon, centre, final_grid, roundtrip_cost, roundtrip_cost,
    budgets$final_paths, budgets$path_batch_size, budgets$seed
  )
  selected <- v2_select_threshold(
    final, enforce_outside_option = TRUE,
    outside_option = production_config$threshold_mc$outside_option,
    tolerance = production_config$threshold_mc$outside_option_tolerance
  )
  selected$Pair <- as.character(pair)
  selected$Session_Date <- as.character(as.Date(endpoint_date))
  selected$model <- as.character(model)
  selected$centre <- centre
  tradeable <- selected$strategy_available %in% TRUE
  selected$upper_entry <- if (tradeable) centre + selected$d_plus else NA_real_
  selected$lower_entry <- if (tradeable) centre - selected$d_minus else NA_real_
  selected$upper_exit <- if (tradeable) centre else NA_real_
  selected$lower_exit <- if (tradeable) centre else NA_real_
  selected$formation_cost_proxy_roundtrip_log <- roundtrip_cost
  selected$terminal_policy_version <- contract$terminal$version
  selected$objective_version <- contract$terminal$objective_version
  selected$simulation_seed <- as.integer(budgets$seed)
  selected$parameter_hash <- parameter_hash
  selected$parameter_source_hash <- parameter_source_hash
  selected$simulator_hash <- simulator$simulator_hash %v2||% v2_hash_object(simulator)
  selected$configuration_hash <- v2_configuration_fingerprint(contract)
  selected$pair_sleeve_usd <- contract$capital$pair_sleeve_usd
  selected$testing_data_used_for_calibration <- FALSE
  selected$testing_pnl_used <- FALSE
  list(
    complete = TRUE, selected = selected, coarse = coarse,
    intermediate = intermediate, final = final, completed_at = v2_now()
  )
}
