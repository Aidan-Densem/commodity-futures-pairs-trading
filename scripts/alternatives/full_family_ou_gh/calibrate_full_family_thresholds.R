#!/usr/bin/env Rscript
source(file.path("R", "io_helpers.R")); source(file.path("R", "v2_common.R"))
source(file.path("R", "threshold_objective.R")); source(file.path("R", "threshold_mc.R"))
source(file.path("R", "strategy_specification.R")); source(file.path("R", "model_route_manifest.R"))
source(file.path("R", "gh_branch_contract.R")); source(file.path(
  "R", "alternatives", "full_family_ou_gh", "repository_adapter.R"
))
source(file.path("config", "production_config.R")); source(file.path("config", "contracts_v2.R"))

calibrate_full_family_thresholds_main <- function() {
  repo_assert(identical(Sys.getenv("ALLOW_EXPENSIVE_FULL_FAMILY_GH_THRESHOLDS"), "TRUE"),
              "Set ALLOW_EXPENSIVE_FULL_FAMILY_GH_THRESHOLDS=TRUE to authorise MC calibration.")
  ranking <- utils::read.csv(repo_path("output", "ranking", "selected_schedule.csv"),
                             stringsAsFactors = FALSE)
  parameters <- readRDS(repo_path(
    "output", "ou_gh_full_family_parameters", "full_family_ou_gh_parameter_results.rds"
  ))
  invisible(lapply(parameters, validate_gh_mode, expected = "FULL_FAMILY"))
  availability <- full_family_gh_model_availability(parameters)
  environment <- full_family_gh_environment()
  tasks <- build_full_family_gh_threshold_tasks(parameters, ranking, environment)
  budgets <- c(
    production_config$threshold_mc[c("coarse_paths", "intermediate_paths", "final_paths", "path_batch_size")],
    list(seed = production_config$threshold_mc$base_seed)
  )
  results <- full_family_gh_run_threshold_tasks(tasks, function(task) {
      value <- do.call(calibrate_threshold_from_context, c(task, list(budgets = budgets)))
      value$gh_mode <- "FULL_FAMILY"
      value$threshold_task_status <- if (
        identical(value$selected$route_status[[1L]], "MODEL_NO_TRADE")
      ) "MODEL_NO_TRADE" else "THRESHOLD_AVAILABLE"
      value$selected$gh_mode <- "FULL_FAMILY"
      value
  })
  selected <- v2_bind_rows(lapply(results, `[[`, "selected"))
  strategies <- build_strategy_specifications(
    selected, ranking, "Full-family OU-GH", production_config$pair_sleeve_usd
  )
  routes <- build_model_route_manifest(
    ranking, "Full-family OU-GH", selected, availability,
    pair_sleeve_usd = production_config$pair_sleeve_usd
  )
  output <- repo_path("output", "thresholds")
  repo_atomic_rds(results, file.path(output, "full_family_gh_thresholds.rds"))
  existing <- if (file.exists(file.path(output, "strategy_specs.rds")))
    readRDS(file.path(output, "strategy_specs.rds")) else data.frame()
  combined <- v2_bind_rows(list(existing, strategies))
  combined <- combined[!duplicated(
    combined[c("pair_id", "formation_endpoint", "model_label")], fromLast = TRUE
  ), , drop = FALSE]
  repo_atomic_rds(combined, file.path(output, "strategy_specs.rds"))
  route_path <- file.path(output, "model_route_manifest.csv")
  old <- if (file.exists(route_path)) utils::read.csv(route_path, stringsAsFactors = FALSE) else data.frame()
  all_routes <- v2_bind_rows(list(old, routes))
  all_routes <- all_routes[!duplicated(
    all_routes[c("pair_id", "endpoint_session_date", "model_label")], fromLast = TRUE
  ), , drop = FALSE]
  repo_atomic_csv(all_routes, route_path)
  invisible(list(thresholds = selected, strategies = strategies, routes = routes))
}

if (sys.nframe() == 0L) calibrate_full_family_thresholds_main()
