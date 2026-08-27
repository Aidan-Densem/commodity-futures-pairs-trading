#!/usr/bin/env Rscript

# Current strict-interior OU--GH complete-episode threshold stage. This helper
# writes a branch-specific object that the public core wrapper merges with the
# analytic Gaussian route.
source(file.path("R", "io_helpers.R")); source(file.path("R", "data_contracts.R"))
source(file.path("config", "production_config.R")); source(file.path("config", "contracts_v2.R"))
source(file.path("config", "complete_episode_threshold_contract.R"))
source(file.path("R", "v2_common.R")); source(file.path(
  "R", "alternatives", "finite_horizon_mc", "gaussian_finite_horizon.R"
))
source(file.path("R", "threshold_objective.R")); source(file.path("R", "threshold_mc.R"))
source(file.path("R", "complete_episode_threshold_mc.R"))
source(file.path("R", "threshold_task_construction.R"))
source(file.path("R", "strategy_specification.R")); source(file.path("R", "model_route_manifest.R"))
source(file.path("R", "gh_branch_contract.R"))

complete_episode_unavailable_result <- function(task, reason) list(
  complete = FALSE,
  selected = data.frame(
    Pair = as.character(task$pair),
    Session_Date = as.Date(task$endpoint_date),
    model = as.character(task$model), objective_value = NA_real_,
    route_status = "THRESHOLD_UNAVAILABLE", strategy_available = FALSE,
    threshold_failure_reason = as.character(reason),
    reason_code = "THRESHOLD_CALIBRATION_FAILURE",
    testing_data_used_for_calibration = FALSE, testing_pnl_used = FALSE,
    stringsAsFactors = FALSE
  )
)

complete_episode_thresholds_main <- function(
    model = c("gaussian_mc", "strict_interior_gh", "full_family_gh")) {
  model <- match.arg(model)
  repo_assert(
    identical(Sys.getenv("ALLOW_EXPENSIVE_COMPLETE_EPISODE_THRESHOLDS"), "TRUE"),
    paste(
      "Set ALLOW_EXPENSIVE_COMPLETE_EPISODE_THRESHOLDS=TRUE to authorise",
      "the complete-episode threshold census."
    )
  )
  ranking <- utils::read.csv(
    repo_path("output", "ranking", "selected_schedule.csv"),
    stringsAsFactors = FALSE
  )
  budgets <- c(
    production_config$threshold_mc[c(
      "coarse_paths", "intermediate_paths", "final_paths", "path_batch_size"
    )],
    list(seed = production_config$threshold_mc$base_seed)
  )
  availability <- NULL
  gh_mode <- NULL
  if (identical(model, "gaussian_mc")) {
    tasks <- build_gaussian_mc_threshold_tasks(ranking)
    model_label <- "Gaussian complete-episode MC"
  } else if (identical(model, "strict_interior_gh")) {
    source(file.path("R", "ou_gh_strict_interior", "project_utils.R"))
    Sys.setenv(OU_GH_PROJECT_ROOT = repo_root())
    ou_gh_source_production(repo_root(), .GlobalEnv)
    sys.source(
      repo_path("R", "ou_gh_strict_interior", "ou_gh_fgmc_production.R"),
      envir = .GlobalEnv
    )
    parameters <- readRDS(repo_path(
      "output", "ou_gh_parameters", "strict_interior_ou_gh_parameter_results.rds"
    ))
    availability <- do.call(rbind, lapply(parameters, function(x) data.frame(
      pair_id = as.character(x$task$Pair[[1L]] %||% x$task$pair_id[[1L]]),
      endpoint_session_date = as.Date(x$task$Session_Date[[1L]] %||%
                                        x$task$endpoint_session_date[[1L]]),
      model_available = startsWith(x$fit_status %||% "", "fit_success") &&
        !is.null(x$fit_natural),
      model_reason = as.character(x$fit_failure_reason %||% x$failure_reason %||%
                                    x$fit_status %||% "unknown"),
      stringsAsFactors = FALSE
    )))
    tasks <- build_strict_interior_gh_threshold_tasks(parameters, ranking)
    model_label <- "Strict-interior OU-GH complete-episode MC"
    gh_mode <- "STRICT_INTERIOR"
  } else {
    source(file.path(
      "R", "alternatives", "full_family_ou_gh", "repository_adapter.R"
    ))
    parameters <- readRDS(repo_path(
      "output", "ou_gh_full_family_parameters",
      "full_family_ou_gh_parameter_results.rds"
    ))
    invisible(lapply(parameters, validate_gh_mode, expected = "FULL_FAMILY"))
    availability <- full_family_gh_model_availability(parameters)
    environment <- full_family_gh_environment()
    tasks <- build_full_family_gh_threshold_tasks(parameters, ranking, environment)
    model_label <- "Full-family OU-GH complete-episode MC"
    gh_mode <- "FULL_FAMILY"
  }
  results <- lapply(tasks, function(task) {
    if (!is.null(task$task_construction_failure)) {
      return(complete_episode_unavailable_result(
        task, task$task_construction_failure
      ))
    }
    tryCatch(
      do.call(
        calibrate_complete_episode_threshold_from_context,
        c(task, list(budgets = budgets))
      ),
      error = function(error) complete_episode_unavailable_result(
        task, conditionMessage(error)
      )
    )
  })
  if (!is.null(gh_mode)) results <- lapply(results, function(value) {
    value$gh_mode <- gh_mode
    if (is.data.frame(value$selected)) value$selected$gh_mode <- gh_mode
    value
  })
  selected <- if (length(results)) {
    v2_bind_rows(lapply(results, `[[`, "selected"))
  } else data.frame()
  strategies <- build_strategy_specifications(
    selected, ranking, model_label, production_config$pair_sleeve_usd
  )
  routes <- build_model_route_manifest(
    ranking, model_label, selected, availability,
    pair_sleeve_usd = production_config$pair_sleeve_usd
  )
  output <- repo_path("output", "thresholds_complete_episode", model)
  dir.create(output, recursive = TRUE, showWarnings = FALSE)
  repo_atomic_rds(results, file.path(output, "complete_episode_threshold_results.rds"))
  repo_atomic_rds(strategies, file.path(output, "strategy_specs.rds"))
  repo_atomic_csv(routes, file.path(output, "model_route_manifest.csv"))
  invisible(list(
    threshold_results = results, selected = selected,
    strategy_specs = strategies, model_routes = routes
  ))
}

if (sys.nframe() == 0L) complete_episode_thresholds_main("gaussian_mc")
