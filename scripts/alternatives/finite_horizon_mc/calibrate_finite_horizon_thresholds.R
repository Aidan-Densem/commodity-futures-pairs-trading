#!/usr/bin/env Rscript
source(file.path("R", "io_helpers.R")); source(file.path("R", "data_contracts.R"))
source(file.path("config", "production_config.R")); source(file.path("config", "contracts_v2.R"))
source(file.path("R", "v2_common.R")); source(file.path(
  "R", "alternatives", "finite_horizon_mc", "gaussian_finite_horizon.R"
))
source(file.path("R", "gaussian_analytic_thresholds.R")); source(file.path("R", "threshold_objective.R"))
source(file.path("R", "threshold_mc.R")); source(file.path("R", "threshold_task_construction.R"))
source(file.path("R", "strategy_specification.R")); source(file.path("R", "ou_gh_strict_interior", "project_utils.R"))
source(file.path("R", "model_route_manifest.R"))
source(file.path("R", "gh_branch_contract.R"))

calibrate_thresholds_main <- function(model = c("all", "gaussian_analytic", "gaussian_mc", "strict_interior_gh")) {
  model <- match.arg(model)
  repo_assert(
    identical(Sys.getenv("ALLOW_EXPENSIVE_THRESHOLDS"), "TRUE"),
    "Set ALLOW_EXPENSIVE_THRESHOLDS=TRUE to authorise empirical threshold calibration."
  )
  ranking <- utils::read.csv(repo_path("output", "ranking", "selected_schedule.csv"), stringsAsFactors = FALSE)
  budgets <- c(production_config$threshold_mc[c("coarse_paths", "intermediate_paths", "final_paths", "path_batch_size")],
               list(seed = production_config$threshold_mc$base_seed))
  output <- repo_path("output", "thresholds"); dir.create(output, recursive = TRUE, showWarnings = FALSE)
  threshold_tables <- list(); strategy_tables <- list(); route_tables <- list()
  if (model %in% c("all", "gaussian_analytic")) {
    selected <- ranking[ranking$selected %in% TRUE, , drop = FALSE]
    analytic <- v2_bind_rows(lapply(seq_len(nrow(selected)), function(i) tryCatch(
      gaussian_analytic_threshold_from_candidate(selected[i, , drop = FALSE]),
      error = function(e) data.frame(
        Pair = selected$pair_id[[i]],
        Session_Date = as.Date(selected$endpoint_session_date[[i]]),
        model = "gaussian_analytic", objective_value = NA_real_,
        route_status = "THRESHOLD_UNAVAILABLE", strategy_available = FALSE,
        threshold_failure_reason = conditionMessage(e), stringsAsFactors = FALSE
      )
    )))
    threshold_tables$gaussian_analytic <- analytic
    strategy_tables$gaussian_analytic <- build_strategy_specifications(
      analytic, ranking, "Gaussian analytic", production_config$pair_sleeve_usd)
    route_tables$gaussian_analytic <- build_model_route_manifest(
      ranking, "Gaussian analytic", analytic,
      pair_sleeve_usd = production_config$pair_sleeve_usd
    )
    repo_atomic_rds(analytic, file.path(output, "gaussian_analytic_thresholds.rds"))
  }
  if (model %in% c("all", "gaussian_mc")) {
    tasks <- build_gaussian_mc_threshold_tasks(ranking)
    result <- lapply(tasks, function(task) tryCatch(
      do.call(calibrate_threshold_from_context, c(task, list(budgets = budgets))),
      error = function(e) list(selected = data.frame(
        Pair = task$pair, Session_Date = as.Date(task$endpoint_date),
        model = "gaussian_mc", objective_value = NA_real_,
        route_status = "THRESHOLD_UNAVAILABLE", strategy_available = FALSE,
        threshold_failure_reason = conditionMessage(e), stringsAsFactors = FALSE
      ), complete = FALSE)
    ))
    selected <- v2_bind_rows(lapply(result, `[[`, "selected"))
    threshold_tables$gaussian_mc <- selected
    strategy_tables$gaussian_mc <- build_strategy_specifications(
      selected, ranking, "Gaussian MC", production_config$pair_sleeve_usd)
    route_tables$gaussian_mc <- build_model_route_manifest(
      ranking, "Gaussian MC", selected,
      pair_sleeve_usd = production_config$pair_sleeve_usd
    )
    repo_atomic_rds(result, file.path(output, "gaussian_mc_thresholds.rds"))
  }
  if (model %in% c("all", "strict_interior_gh")) {
    Sys.setenv(OU_GH_PROJECT_ROOT = repo_root()); ou_gh_source_production(repo_root(), .GlobalEnv)
    sys.source(repo_path("R", "ou_gh_strict_interior", "ou_gh_fgmc_production.R"), envir = .GlobalEnv)
    parameters <- readRDS(repo_path("output", "ou_gh_parameters",
                                    "strict_interior_ou_gh_parameter_results.rds"))
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
    result <- lapply(tasks, function(task) {
      if (!is.null(task$task_construction_failure)) return(list(selected = data.frame(
        Pair = task$pair, Session_Date = as.Date(task$endpoint_date),
        model = "strict_interior_gh", objective_value = NA_real_,
        route_status = "THRESHOLD_UNAVAILABLE", strategy_available = FALSE,
        threshold_failure_reason = task$task_construction_failure,
        stringsAsFactors = FALSE
      ), complete = FALSE))
      tryCatch(
        do.call(calibrate_threshold_from_context, c(task, list(budgets = budgets))),
        error = function(e) list(selected = data.frame(
        Pair = task$pair, Session_Date = as.Date(task$endpoint_date),
        model = "strict_interior_gh", objective_value = NA_real_,
        route_status = "THRESHOLD_UNAVAILABLE", strategy_available = FALSE,
        threshold_failure_reason = conditionMessage(e), stringsAsFactors = FALSE
      ), complete = FALSE)
      )
    })
    result <- lapply(result, function(x) {
      x$gh_mode <- "STRICT_INTERIOR"
      if (is.data.frame(x$selected)) x$selected$gh_mode <- "STRICT_INTERIOR"
      x
    })
    selected <- if (length(result)) v2_bind_rows(lapply(result, `[[`, "selected")) else data.frame()
    threshold_tables$strict_interior_gh <- selected
    strategy_tables$strict_interior_gh <- build_strategy_specifications(
      selected, ranking, "Strict-interior OU-GH", production_config$pair_sleeve_usd)
    route_tables$strict_interior_gh <- build_model_route_manifest(
      ranking, "Strict-interior OU-GH", selected, availability,
      production_config$pair_sleeve_usd
    )
    repo_atomic_rds(result, file.path(output, "strict_interior_gh_thresholds.rds"))
  }
  existing_path <- file.path(output, "strategy_specs.rds")
  existing <- if (model != "all" && file.exists(existing_path)) readRDS(existing_path) else data.frame()
  combined <- do.call(rbind, c(list(existing), strategy_tables)); rownames(combined) <- NULL
  if (nrow(combined)) combined <- combined[!duplicated(combined[c("pair_id", "formation_endpoint", "model_label")],
                                                       fromLast = TRUE), , drop = FALSE]
  repo_atomic_rds(combined, existing_path)
  route_path <- file.path(output, "model_route_manifest.csv")
  old_routes <- if (model != "all" && file.exists(route_path)) {
    utils::read.csv(route_path, stringsAsFactors = FALSE)
  } else data.frame()
  routes <- do.call(rbind, c(list(old_routes), route_tables)); rownames(routes) <- NULL
  if (nrow(routes)) routes <- routes[!duplicated(
    routes[c("pair_id", "endpoint_session_date", "model_label")], fromLast = TRUE
  ), , drop = FALSE]
  expected_models <- unique(routes$model_label)
  validate_model_route_manifest(routes, ranking, expected_models)
  repo_atomic_csv(routes, route_path)
  invisible(list(thresholds = threshold_tables, strategy_specs = combined,
                 model_routes = routes))
}
if (sys.nframe() == 0L) calibrate_thresholds_main("all")
