#!/usr/bin/env Rscript

# Thesis-aligned threshold orchestration: analytic Gaussian boundary plus the
# strict-interior OU--GH complete flat-entry-exit Monte Carlo calibration.
# Scientific threshold implementations are sourced; this file only joins their
# existing route/strategy interfaces for the common backtest.

source(file.path("R", "io_helpers.R"))
source(file.path("R", "data_contracts.R"))
source(file.path("config", "production_config.R"))
source(file.path("config", "contracts_v2.R"))
source(file.path("R", "v2_common.R"))
source(file.path("R", "gaussian_analytic_thresholds.R"))
source(file.path("R", "strategy_specification.R"))
source(file.path("R", "model_route_manifest.R"))
source(file.path("scripts", "core", "internal",
                 "complete_episode_threshold_stage.R"))

calibrate_core_gaussian_analytic <- function(ranking) {
  selected <- ranking[ranking$selected %in% TRUE, , drop = FALSE]
  thresholds <- v2_bind_rows(lapply(seq_len(nrow(selected)), function(i) {
    tryCatch(
      gaussian_analytic_threshold_from_candidate(selected[i, , drop = FALSE]),
      error = function(error) data.frame(
        Pair = selected$pair_id[[i]],
        Session_Date = as.Date(selected$endpoint_session_date[[i]]),
        model = "gaussian_analytic", objective_value = NA_real_,
        route_status = "THRESHOLD_UNAVAILABLE", strategy_available = FALSE,
        threshold_failure_reason = conditionMessage(error),
        stringsAsFactors = FALSE
      )
    )
  }))
  list(
    thresholds = thresholds,
    strategies = build_strategy_specifications(
      thresholds, ranking, "Gaussian analytic",
      production_config$pair_sleeve_usd
    ),
    routes = build_model_route_manifest(
      ranking, "Gaussian analytic", thresholds,
      pair_sleeve_usd = production_config$pair_sleeve_usd
    )
  )
}

calibrate_trading_thresholds_main <- function(
    model = c("all", "gaussian_analytic", "strict_interior_gh")) {
  model <- match.arg(model)
  repo_assert(
    identical(Sys.getenv("ALLOW_EXPENSIVE_THRESHOLDS"), "TRUE"),
    paste(
      "Set ALLOW_EXPENSIVE_THRESHOLDS=TRUE to authorise the analytic",
      "Gaussian and/or complete-episode strict-interior GH calibration."
    )
  )
  ranking <- utils::read.csv(
    repo_path("output", "ranking", "selected_schedule.csv"),
    stringsAsFactors = FALSE
  )
  output <- repo_path("output", "thresholds")
  dir.create(output, recursive = TRUE, showWarnings = FALSE)
  strategies <- routes <- list()
  threshold_outputs <- list()

  if (model %in% c("all", "gaussian_analytic")) {
    gaussian <- calibrate_core_gaussian_analytic(ranking)
    threshold_outputs$gaussian_analytic <- gaussian$thresholds
    strategies$gaussian_analytic <- gaussian$strategies
    routes$gaussian_analytic <- gaussian$routes
    repo_atomic_rds(
      gaussian$thresholds,
      file.path(output, "gaussian_analytic_thresholds.rds")
    )
  }

  if (model %in% c("all", "strict_interior_gh")) {
    gh <- complete_episode_thresholds_main("strict_interior_gh")
    threshold_outputs$strict_interior_gh_complete_episode <- gh$selected
    strategies$strict_interior_gh_complete_episode <- gh$strategy_specs
    routes$strict_interior_gh_complete_episode <- gh$model_routes
  }

  existing_strategies <- if (
    model != "all" && file.exists(file.path(output, "strategy_specs.rds"))
  ) readRDS(file.path(output, "strategy_specs.rds")) else data.frame()
  combined_strategies <- v2_bind_rows(c(list(existing_strategies), strategies))
  if (nrow(combined_strategies)) combined_strategies <- combined_strategies[
    !duplicated(
      combined_strategies[c("pair_id", "formation_endpoint", "model_label")],
      fromLast = TRUE
    ), , drop = FALSE
  ]

  route_path <- file.path(output, "model_route_manifest.csv")
  existing_routes <- if (model != "all" && file.exists(route_path)) {
    utils::read.csv(route_path, stringsAsFactors = FALSE)
  } else data.frame()
  combined_routes <- v2_bind_rows(c(list(existing_routes), routes))
  if (nrow(combined_routes)) combined_routes <- combined_routes[
    !duplicated(
      combined_routes[c("pair_id", "endpoint_session_date", "model_label")],
      fromLast = TRUE
    ), , drop = FALSE
  ]
  validate_model_route_manifest(
    combined_routes, ranking, unique(combined_routes$model_label)
  )
  repo_atomic_rds(combined_strategies, file.path(output, "strategy_specs.rds"))
  repo_atomic_csv(combined_routes, route_path)
  invisible(list(
    thresholds = threshold_outputs,
    strategy_specs = combined_strategies,
    model_routes = combined_routes
  ))
}

if (sys.nframe() == 0L) calibrate_trading_thresholds_main("all")
