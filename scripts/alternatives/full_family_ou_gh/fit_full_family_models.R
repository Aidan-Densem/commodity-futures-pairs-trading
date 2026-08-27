#!/usr/bin/env Rscript
source(file.path("R", "io_helpers.R"))
source(file.path("R", "ou_gh_task_construction.R"))
source(file.path("R", "gh_branch_contract.R"))
source(file.path("R", "alternatives", "full_family_ou_gh", "repository_adapter.R"))
source(file.path("config", "alternatives", "full_family_ou_gh",
                 "full_family_gh_contract.R"))

fit_full_family_models_main <- function(evaluation_budget = 90L,
                                        quadrature_nodes = 24L) {
  repo_assert(identical(Sys.getenv("ALLOW_EXPENSIVE_FULL_FAMILY_GH_FIT"), "TRUE"),
              paste(
                "Set ALLOW_EXPENSIVE_FULL_FAMILY_GH_FIT=TRUE to authorise the",
                "eight-candidate formation-only full-family OU-GH fit."
              ))
  ranking <- utils::read.csv(
    repo_path("output", "ranking", "selected_schedule.csv"), stringsAsFactors = FALSE
  )
  paths <- readRDS(repo_path("output", "prepared", "exact_pair_series.rds"))
  output <- repo_path("output", "ou_gh_full_family_parameters")
  dir.create(output, recursive = TRUE, showWarnings = FALSE)
  manifest <- build_gh_task_manifest(
    ranking, paths, file.path(output, "formation_caches"),
    gh_mode = "FULL_FAMILY"
  )
  repo_atomic_rds(manifest, file.path(output, "full_family_gh_task_manifest.rds"))
  environment <- full_family_gh_environment()
  checkpoints <- file.path(output, "checkpoints")
  dir.create(checkpoints, recursive = TRUE, showWarnings = FALSE)
  source_hashes <- vapply(
    list.files(repo_path("R", "alternatives", "full_family_ou_gh"),
               full.names = TRUE, pattern = "[.]R$"),
    repo_sha256, character(1L)
  )
  results <- full_family_gh_run_fit_tasks(manifest, function(task, i) {
    fingerprint <- gh_checkpoint_fingerprint(task, "FULL_FAMILY", source_hashes)
    path <- file.path(checkpoints, paste0(gsub("[^A-Za-z0-9_.-]", "_", task$task_key), ".rds"))
    saved <- if (file.exists(path)) tryCatch(readRDS(path), error = function(e) NULL) else NULL
    if (!is.null(saved) && identical(saved$checkpoint_fingerprint, fingerprint)) {
      validate_gh_mode(saved, "FULL_FAMILY"); saved
    } else {
      set.seed(16082026L + i)
      result <- full_family_gh_fit_task(
        task, environment, FULL_FAMILY_GH_CONTRACT,
        evaluation_budget, quadrature_nodes
      )
      result$checkpoint_fingerprint <- fingerprint
      repo_atomic_rds(result, path)
      result
    }
  })
  # Expected task-level failures are also deterministic, resumable outcomes.
  for (i in seq_along(results)) if (is.null(results[[i]]$checkpoint_fingerprint)) {
    task <- manifest[i, , drop = FALSE]
    results[[i]]$checkpoint_fingerprint <- gh_checkpoint_fingerprint(
      task, "FULL_FAMILY", source_hashes
    )
    path <- file.path(
      checkpoints, paste0(gsub("[^A-Za-z0-9_.-]", "_", task$task_key), ".rds")
    )
    repo_atomic_rds(results[[i]], path)
  }
  summary <- do.call(rbind, lapply(results, function(x) data.frame(
    task_key = x$task$task_key, Pair = x$task$Pair,
    Session_Date = as.Date(x$task$Session_Date), gh_mode = x$gh_mode,
    fit_status = x$fit_status,
    model_reason = x$model_reason,
    selected_model = if (is.null(x$route$selected_model)) NA_character_ else x$route$selected_model,
    selected_regime = if (is.null(x$route$selected_regime)) NA_character_ else x$route$selected_regime,
    threshold_moment_contract_status = isTRUE(x$route$threshold_moment_contract_status),
    testing_data_used = FALSE, stringsAsFactors = FALSE
  )))
  validate_gh_mode(summary, "FULL_FAMILY")
  repo_atomic_rds(results, file.path(output, "full_family_ou_gh_parameter_results.rds"))
  repo_atomic_csv(summary, file.path(output, "full_family_ou_gh_parameter_snapshot.csv"))
  invisible(summary)
}

if (sys.nframe() == 0L) fit_full_family_models_main()
