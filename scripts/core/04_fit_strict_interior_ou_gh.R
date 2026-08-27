#!/usr/bin/env Rscript
source(file.path("R", "io_helpers.R")); source(file.path("R", "data_contracts.R"))
source(file.path("R", "market_data.R")); source(file.path("R", "active_market_clock.R"))
source(file.path("R", "ou_gh_task_construction.R")); source(file.path("R", "ou_gh_strict_interior", "project_utils.R"))
source(file.path("config", "production_config.R"))

fit_production_models_main <- function(workers = 4L) {
  repo_assert(identical(Sys.getenv("ALLOW_EXPENSIVE_GHI_FIT"), "TRUE"),
              "Set ALLOW_EXPENSIVE_GHI_FIT=TRUE to authorise strict-interior GHI fitting.")
  Sys.setenv(OU_GH_PROJECT_ROOT = repo_root()); ou_gh_source_production(repo_root(), .GlobalEnv)
  repo_source(file.path("R", "ou_gh_strict_interior", "strict_interior_contract.R"), .GlobalEnv)
  ranking <- utils::read.csv(repo_path("output", "ranking", "selected_schedule.csv"), stringsAsFactors = FALSE)
  pair_series <- readRDS(repo_path("output", "prepared", "exact_pair_series.rds"))
  output <- repo_path("output", "ou_gh_parameters"); dir.create(output, recursive = TRUE, showWarnings = FALSE)
  manifest <- build_strict_interior_gh_task_manifest(
    ranking, pair_series, file.path(output, "formation_caches")
  )
  manifest$gh_mode <- "STRICT_INTERIOR"
  repo_atomic_rds(manifest, file.path(output, "ou_gh_task_manifest.rds"))
  checkpoint_dir <- file.path(output, "checkpoints"); dir.create(checkpoint_dir, recursive = TRUE, showWarnings = FALSE)
  estimator_hash <- ou_gh_production_estimator_fingerprint(); results <- vector("list", nrow(manifest))
  for (i in seq_len(nrow(manifest))) {
    checkpoint <- file.path(checkpoint_dir, paste0(manifest$task_key[[i]], ".rds"))
    checkpoint_value <- if (file.exists(checkpoint)) tryCatch(readRDS(checkpoint), error = function(e) NULL) else NULL
    if (ou_gh_validate_checkpoint(checkpoint, manifest[i, , drop = FALSE], estimator_hash) &&
        !is.null(checkpoint_value) && identical(checkpoint_value$gh_mode, "STRICT_INTERIOR")) {
      results[[i]] <- checkpoint_value
    } else {
      set.seed(production_config$gh_branches$strict_interior$estimation_base_seed + i)
      results[[i]] <- strict_interior_gh_fit_task(manifest[i, , drop = FALSE], checkpoint)
    }
    results[[i]]$gh_mode <- "STRICT_INTERIOR"
  }
  snapshot <- do.call(rbind, lapply(results, ou_gh_checkpoint_summary)); rownames(snapshot) <- NULL
  snapshot$gh_mode <- "STRICT_INTERIOR"
  validate_strict_interior_snapshot(snapshot)
  repo_atomic_rds(results, file.path(output, "strict_interior_ou_gh_parameter_results.rds"))
  repo_atomic_rds(snapshot, file.path(output, "strict_interior_ou_gh_parameter_snapshot.rds"))
  invisible(snapshot)
}
if (sys.nframe() == 0L) fit_production_models_main()
