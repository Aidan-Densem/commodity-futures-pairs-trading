# Isolated loader and repository adapter for the broader/full-family OU-GH
# exercise.  Symbols are sourced into a private environment and cannot replace
# strict-interior functions in the caller's namespace.

full_family_gh_environment <- function(root = repo_root()) {
  environment <- new.env(parent = baseenv())
  # Preserve namespace isolation while supplying the two utils helpers used by
  # the isolated scientific source (baseenv alone intentionally cannot see them).
  environment$head <- utils::head
  environment$tail <- utils::tail
  environment$setNames <- stats::setNames
  contract_environment <- new.env(parent = baseenv())
  sys.source(
    file.path(root, "config", "alternatives", "full_family_ou_gh",
              "full_family_gh_contract.R"),
    contract_environment
  )
  environment$FULL_FAMILY_GH_CCF_OBJECTIVE <-
    contract_environment$FULL_FAMILY_GH_CONTRACT$ccf_objective
  files <- c(
    "project_utils.R", "gh_bessel_backend.R", "gig_distribution.R",
    "gh_parameterization.R", "gh_distribution.R", "gh_driver.R",
    "ou_gh_transition.R", "ou_gh_family_contract.R", "ou_gh_family_driver.R",
    "ou_gh_family_parameterization.R", "ou_gh_family_transition.R",
    "ou_gh_family_estimation.R", "ou_gh_family_simulation.R",
    "ou_gh_family_threshold.R", "ou_gh_family_empirical.R", "ou_gh_task_adapter.R"
  )
  for (file in files) sys.source(
    file.path(root, "R", "alternatives", "full_family_ou_gh", file),
    environment
  )
  environment
}

full_family_gh_fit_task <- function(task_row, environment, contract,
                                    evaluation_budget = 90L,
                                    quadrature_nodes = 24L) {
  stopifnot(identical(contract$gh_mode, "FULL_FAMILY"))
  prepared <- environment$ou_gh_prepare_selected_formation(task_row)
  candidates <- environment$ou_gh_family_fit_candidates(
    prepared$scaled_transitions,
    candidate_models = contract$candidate_models,
    evaluation_budget = evaluation_budget,
    quadrature_nodes = quadrature_nodes,
    objective_weights = contract$ccf_objective
  )
  route <- environment$ou_gh_family_route_candidates(
    candidates,
    equivalence_absolute = contract$router$equivalence_absolute,
    equivalence_relative = contract$router$equivalence_relative
  )
  selected <- if (is.null(route$selected_fit)) NULL else route$selected_fit
  if (!is.null(selected)) {
    selected <- environment$ou_gh_family_unscale_fit(
      selected, prepared$scaling$centre, prepared$scaling$scale
    )
    selected$gh_mode <- "FULL_FAMILY"
    route$selected_fit <- selected
  }
  list(
    gh_mode = "FULL_FAMILY",
    fit_status = if (is.null(selected)) "MODEL_UNAVAILABLE" else "FIT_AVAILABLE",
    model_reason = if (is.null(selected)) route$router_status else "admissible_fit_selected",
    selected_fit = selected, route = route, candidate_fits = candidates,
    task = task_row, formation_hash = prepared$formation_hash,
    testing_data_used = FALSE, monetary_pnl_used = FALSE
  )
}

full_family_gh_expected_task_failure <- function(error) {
  message <- conditionMessage(error)
  patterns <- c(
    "too few", "Insufficient accepted", "Insufficient transitions",
    "Formation spread has too few", "Formation scale is invalid",
    "fit_unavailable", "no admissible", "moment contract",
    "optimiser", "optimization", "non-finite", "nonfinite",
    "quadrature", "Bessel", "inversion", "FFT"
  )
  any(vapply(patterns, grepl, logical(1L), x = message, ignore.case = TRUE))
}

full_family_gh_expected_threshold_failure <- function(error) {
  message <- conditionMessage(error)
  patterns <- c(
    "simulator", "threshold", "objective", "no finite", "non-finite",
    "nonfinite", "Monte Carlo", "path", "inversion", "FFT",
    "moment contract", "numerical"
  )
  any(vapply(patterns, grepl, logical(1L), x = message, ignore.case = TRUE))
}

full_family_gh_unavailable_result <- function(task_row, reason) {
  list(
    gh_mode = "FULL_FAMILY", fit_status = "MODEL_UNAVAILABLE",
    model_reason = as.character(reason), selected_fit = NULL,
    route = list(router_status = "fit_unavailable", selected_fit = NULL,
                 threshold_moment_contract_status = FALSE),
    candidate_fits = list(), task = task_row, formation_hash = NA_character_,
    testing_data_used = FALSE, monetary_pnl_used = FALSE
  )
}

# Task-level numerical/data failures are isolated, while schema and programmer
# errors remain fatal.  Keeping this control flow here makes the contract
# directly testable without launching the production estimator.
full_family_gh_run_fit_tasks <- function(manifest, fit_one) {
  stopifnot(is.data.frame(manifest), is.function(fit_one))
  lapply(seq_len(nrow(manifest)), function(i) {
    task <- manifest[i, , drop = FALSE]
    tryCatch(
      fit_one(task, i),
      error = function(error) {
        if (!full_family_gh_expected_task_failure(error)) stop(error)
        full_family_gh_unavailable_result(task, conditionMessage(error))
      }
    )
  })
}

full_family_gh_threshold_unavailable_result <- function(task, reason) {
  list(
    gh_mode = "FULL_FAMILY", threshold_task_status = "THRESHOLD_UNAVAILABLE",
    complete = FALSE,
    selected = data.frame(
      Pair = task$pair, Session_Date = as.Date(task$endpoint_date),
      model = "full_family_gh", objective_value = NA_real_,
      route_status = "THRESHOLD_UNAVAILABLE", strategy_available = FALSE,
      threshold_failure_reason = as.character(reason), gh_mode = "FULL_FAMILY",
      stringsAsFactors = FALSE
    )
  )
}

full_family_gh_run_threshold_tasks <- function(tasks, calibrate_one) {
  stopifnot(is.list(tasks), is.function(calibrate_one))
  lapply(tasks, function(task) tryCatch(
    calibrate_one(task),
    error = function(error) {
      if (!full_family_gh_expected_threshold_failure(error)) stop(error)
      full_family_gh_threshold_unavailable_result(task, conditionMessage(error))
    }
  ))
}

full_family_gh_model_availability <- function(parameter_results) {
  do.call(rbind, lapply(parameter_results, function(result) {
    reason <- result$model_reason
    if (is.null(reason)) reason <- result$route$router_status
    if (is.null(reason)) reason <- "unknown_full_family_fit_state"
    data.frame(
      pair_id = as.character(result$task$Pair[[1L]]),
      endpoint_session_date = as.Date(result$task$Session_Date[[1L]]),
      model_available = identical(result$fit_status, "FIT_AVAILABLE") &&
        !is.null(result$selected_fit),
      model_reason = as.character(reason), gh_mode = "FULL_FAMILY",
      stringsAsFactors = FALSE
    )
  }))
}

build_full_family_gh_threshold_tasks <- function(parameter_results, selected_schedule,
                                                  environment) {
  selected <- selected_schedule[selected_schedule$selected %in% TRUE, , drop = FALSE]
  by_key <- setNames(parameter_results, vapply(parameter_results, function(x) {
    as.character(x$task$task_key[[1L]])
  }, character(1L)))
  Filter(Negate(is.null), lapply(seq_len(nrow(selected)), function(i) {
    row <- selected[i, , drop = FALSE]
    key <- paste(row$endpoint_id, sprintf("rank_%02d", row$primary_rank), row$pair_id, sep = "__")
    result <- by_key[[key]]
    if (is.null(result)) return(NULL)
    validate_gh_mode(result, "FULL_FAMILY")
    fit <- result$selected_fit
    if (!identical(result$fit_status, "FIT_AVAILABLE") || is.null(fit) ||
        !isTRUE(result$route$threshold_moment_contract_status)) return(NULL)
    simulator <- list(
      family = paste0("full_family_OU_GH_", fit$regime),
      gh_mode = "FULL_FAMILY", parameters = fit,
      simulator_hash = environment$ou_gh_hash_object(list(gh_mode = "FULL_FAMILY", fit = fit)),
      simulate_paths = function(active_time, x0, n_paths, seed) list(
        paths = environment$ou_gh_family_path_matrix(
          fit, active_time, x0, n_paths, seed, table = NULL
        ), active_time = active_time, testing_data_used = FALSE
      )
    )
    list(
      model = "full_family_gh", pair = row$pair_id,
      endpoint_date = row$endpoint_session_date, simulator = simulator,
      centre = fit$mu,
      stationary_sd = environment$ou_gh_family_stationary_scale(fit),
      horizon = as.integer(row$testing_active_minutes),
      roundtrip_cost = row$v2_cost_primary,
      parameter_hash = environment$ou_gh_hash_object(fit),
      parameter_source_hash = result$formation_hash,
      gh_mode = "FULL_FAMILY"
    )
  }))
}
