ou_gh_production_estimator_fingerprint <- function(root = ou_gh_project_root()) {
  module <- function(name) file.path(root, "R", "ou_gh_strict_interior", name)
  ou_gh_hash_object(list(
    gh_mode = "STRICT_INTERIOR",
    continuation_version = OU_GH_CONTINUATION_VERSION,
    ccf = ou_gh_sha256(module("ou_gh_ccf_objective.R")),
    estimation = ou_gh_sha256(module("ou_gh_estimation.R")),
    exact = ou_gh_sha256(module("ou_gh_exact_likelihood.R")),
    adapter = ou_gh_sha256(module("ou_gh_task_adapter.R")),
    runner = ou_gh_sha256(module("ou_gh_parameter_runner.R")),
    simulator_preflight = ou_gh_sha256(module("ou_gh_simulator_preflight.R")),
    inversion = ou_gh_sha256(module("ou_gh_direct_inversion.R")),
    bank = attr(ou_gh_load_frozen_moment_bank(root), "bank_hash")
  ))
}

ou_gh_subclass_from_exact_screen <- function(screen, selected_fit) {
  ordered <- screen[order(screen$objective, screen$start_id), , drop = FALSE]
  best <- ordered$objective[[1L]]
  gap <- function(start_id) {
    value <- ordered$objective[ordered$start_id == start_id]
    if (length(value) != 1L || !is.finite(value)) Inf else value - best
  }
  evidence <- data.frame(
    restriction = c("NIG", "hyperbolic", "symmetric", "Gaussian_like"),
    screen_gap = c(
      gap("NIG"), gap("hyperbolic"), gap("symmetric_moderate"),
      gap("Gaussian_concentration")
    ),
    trigger_threshold = c(0.005, 0.005, 0.005, 0.005),
    stringsAsFactors = FALSE
  )
  eligible <- evidence$screen_gap <= evidence$trigger_threshold
  status <- if (eligible[[1L]]) {
    "NIG_candidate_exact_screen"
  } else if (eligible[[2L]]) {
    "hyperbolic_candidate_exact_screen"
  } else if (eligible[[3L]]) {
    "symmetric_candidate_exact_screen"
  } else if (eligible[[4L]]) {
    "Gaussian_like_candidate_exact_screen"
  } else if (selected_fit[["zeta"]] <= 0.10 &&
      selected_fit[["lambda"]] > 0) {
    "VG_near_candidate_full_GH_boundary"
  } else "full_GH_no_material_restriction"
  list(status = status, evidence = evidence,
    selected_screen_start = ordered$start_id[[1L]],
    next_best_screen_gap = if (nrow(ordered) >= 2L) {
      ordered$objective[[2L]] - ordered$objective[[1L]]
    } else Inf)
}

ou_gh_fit_prepared_task <- function(
    prepared,
    pair_cap = 8000L,
    exact_evaluation_budget = 60L,
    exact_iteration_budget = 50L,
    kappa_evaluation_budget = 60L
) {
  started <- proc.time()[["elapsed"]]
  split <- ou_gh_split_formation(prepared$scaled_transitions)
  exact <- ou_gh_exact_screen_and_refine(
    split$training,
    refinement_pair_cap = pair_cap,
    evaluation_budget = exact_evaluation_budget,
    iteration_budget = exact_iteration_budget,
    freeze_location_profile = TRUE
  )
  if (is.null(exact$selected_raw)) return(list(
    fit_status = exact$status,
    fit_failure_reason = "No exact-transition shape refinement was available.",
    prepared = prepared, split = split, exact = exact,
    runtime_seconds = proc.time()[["elapsed"]] - started,
    simulator = list(status = "pending", route = "", failure_reason = "")
  ))
  ccf <- ou_gh_refine_kappa_scale_ccf(
    exact$selected_raw, split$training, split$validation, exact$profile,
    pair_cap = pair_cap, evaluation_budget = kappa_evaluation_budget,
    refine_scale = FALSE, refine_mu = FALSE
  )
  fit_scaled <- ccf$selected_fit
  if (is.null(fit_scaled) || any(!is.finite(fit_scaled))) return(list(
    fit_status = ccf$status,
    fit_failure_reason = ccf$reason %||% "Kappa CCF refinement failed.",
    prepared = prepared, split = split, exact = exact, ccf = ccf,
    runtime_seconds = proc.time()[["elapsed"]] - started,
    simulator = list(status = "pending", route = "", failure_reason = "")
  ))
  fit_natural <- ou_gh_unscale_fit(fit_scaled, prepared$scaling)
  subclass <- ou_gh_subclass_from_exact_screen(exact$screen, fit_scaled)
  raw <- ccf$selected_raw
  global_bounds <- ou_gh_raw_bounds()
  bound_distance <- pmin(raw - global_bounds$lower, global_bounds$upper - raw)
  kappa_ratio <- fit_scaled[["kappa"]] / exact$profile$kappa
  kappa_status <- if (
      abs(log(2) / fit_scaled[["kappa"]] -
        OU_GH_PRODUCTION_SUPPORT$half_life_active_minutes[[1L]]) <= 1e-5 ||
      abs(log(2) / fit_scaled[["kappa"]] -
        OU_GH_PRODUCTION_SUPPORT$half_life_active_minutes[[2L]]) <= 1e-3) {
    "kappa_global_boundary"
  } else if (kappa_ratio < 0.5 || kappa_ratio > 2) {
    "kappa_joint_material_improvement"
  } else "kappa_stable_relative_to_profile"
  holdout_log_score <- tryCatch({
    validation_rows <- ou_gh_exact_likelihood_rows(
      split$validation, min(pair_cap, 4000L)
    )
    value <- ou_gh_exact_transition_log_score(
      raw, split$validation, validation_rows,
      fft_n = 4096L, range_sd = 24, quadrature_nodes = 24L
    )
    if (value >= 1e6) NA_real_ else value
  }, error = function(condition) NA_real_)
  primitive_status <- if (
      subclass$next_best_screen_gap <= 0.005 ||
      fit_scaled[["zeta"]] <= 0.01 ||
      abs(fit_scaled[["rho"]]) >= 0.94) {
    "law_identified_primitives_weak"
  } else "primitive_locally_identified"
  fit_hash <- ou_gh_hash_object(list(
    task_key = prepared$task$task_key[[1L]],
    formation_hash = prepared$formation_hash,
    split_hash = split$split_hash,
    selected_raw = raw,
    fit_natural = fit_natural,
    bank_hash = ccf$bank_hash,
    active_bank_hash = ccf$active_bank_hash,
    active_horizons = ccf$active_horizons,
    unavailable_horizons = ccf$unavailable_horizons,
    estimator_hash = ou_gh_production_estimator_fingerprint()
  ))
  list(
    fit_status = "fit_success_full_GH_block_exact_CCF",
    fit_failure_reason = "",
    subclass_status = subclass$status,
    primitive_identification_status = primitive_status,
    law_identification_status = "law_identified_synthetic_calibrated",
    kappa_status = kappa_status,
    prepared = prepared, split = split, exact = exact, ccf = ccf,
    subclass = subclass,
    selected_raw = raw, fit_scaled = fit_scaled,
    fit_natural = fit_natural,
    training_objective = ccf$training_objective,
    formation_holdout_CCF_score = ccf$holdout_objective,
    formation_holdout_log_score = holdout_log_score,
    bound_distance = bound_distance,
    bank_hash = ccf$bank_hash,
    active_bank_hash = ccf$active_bank_hash,
    active_horizons = ccf$active_horizons,
    unavailable_horizons = ccf$unavailable_horizons,
    estimator_hash = ou_gh_production_estimator_fingerprint(),
    fit_hash = fit_hash,
    runtime_seconds = proc.time()[["elapsed"]] - started,
    memory_bytes = as.numeric(object.size(list(
      prepared$scaled_transitions, split, exact, ccf
    ))),
    simulator = list(
      status = "pending", route = "", failure_reason = "",
      hash = NA_character_
    ),
    threshold_eligibility = "not_assessed_simulator_pending"
  )
}

ou_gh_fit_task_row <- function(
    task_row,
    checkpoint_path = NULL,
    ...
) {
  started <- proc.time()[["elapsed"]]
  output <- tryCatch({
    prepared <- ou_gh_prepare_selected_formation(task_row)
    result <- ou_gh_fit_prepared_task(prepared, ...)
    result$task <- task_row
    result
  }, error = function(condition) list(
    fit_status = "structured_task_failure",
    fit_failure_reason = conditionMessage(condition),
    task = task_row,
    runtime_seconds = proc.time()[["elapsed"]] - started,
    simulator = list(status = "pending", route = "", failure_reason = "",
      hash = NA_character_),
    threshold_eligibility = "not_eligible_estimation_failure"
  ))
  output$checkpoint_metadata <- list(
    task_key = task_row$task_key[[1L]],
    task_row_hash = ou_gh_hash_object(task_row),
    source_manifest_hash = task_row$source_manifest_hash[[1L]],
    estimator_hash = ou_gh_production_estimator_fingerprint(),
    created_at = format(Sys.time(), tz = "Europe/London", usetz = TRUE)
  )
  if (!is.null(checkpoint_path)) ou_gh_atomic_save_rds(output, checkpoint_path)
  output
}

ou_gh_checkpoint_summary <- function(result) {
  task <- result$task
  fit <- result$fit_natural
  numeric_value <- function(name) {
    if (is.null(fit) || !name %in% names(fit)) NA_real_ else fit[[name]]
  }
  data.frame(
    task_key = task$task_key[[1L]], Pair = task$Pair[[1L]],
    Session_Date = as.character(task$Session_Date[[1L]]),
    endpoint_id = task$endpoint_id[[1L]],
    official_rank = task$official_rank[[1L]],
    formation_start = as.character(task$Formation_Start[[1L]]),
    formation_end = as.character(task$Formation_End[[1L]]),
    mu = numeric_value("mu"), kappa = numeric_value("kappa"),
    half_life = if (is.finite(numeric_value("kappa"))) {
      log(2) / numeric_value("kappa")
    } else NA_real_,
    sigma_eta_1 = numeric_value("sigma_eta_1"),
    lambda = numeric_value("lambda"), zeta = numeric_value("zeta"),
    rho = numeric_value("rho"), alpha = numeric_value("alpha"),
    beta = numeric_value("beta"), delta = numeric_value("delta"),
    gamma = numeric_value("gamma"), centred_m = numeric_value("centred_m"),
    driver_sd = numeric_value("driver_sd"),
    stationary_sd = if (!is.null(fit)) ou_gh_stationary_sd(fit) else NA_real_,
    fit_status = result$fit_status,
    fit_failure_reason = result$fit_failure_reason %||% "",
    subclass_status = result$subclass_status %||% "unavailable",
    primitive_identification_status =
      result$primitive_identification_status %||% "unavailable",
    law_identification_status =
      result$law_identification_status %||% "unavailable",
    kappa_status = result$kappa_status %||% "unavailable",
    training_objective = result$training_objective %||% NA_real_,
    formation_holdout_CCF_score =
      result$formation_holdout_CCF_score %||% NA_real_,
    formation_holdout_log_score =
      result$formation_holdout_log_score %||% NA_real_,
    fit_hash = result$fit_hash %||% NA_character_,
    simulator_status = result$simulator$status %||% "pending",
    simulator_route = result$simulator$route %||% "",
    simulator_failure_reason = result$simulator$failure_reason %||% "",
    simulator_hash = result$simulator$hash %||% NA_character_,
    threshold_eligibility = result$threshold_eligibility %||%
      "not_assessed",
    Trade_Flag = task$trade_flag[[1L]],
    operational_class = task$operational_classification[[1L]],
    formation_hash = result$prepared$formation_hash %||% NA_character_,
    bank_hash = result$bank_hash %||% NA_character_,
    active_bank_hash = result$active_bank_hash %||% NA_character_,
    active_horizons = paste(result$active_horizons %||% integer(),
      collapse = ";"),
    unavailable_horizons = paste(result$unavailable_horizons %||% integer(),
      collapse = ";"),
    estimator_hash = result$checkpoint_metadata$estimator_hash %||%
      ou_gh_production_estimator_fingerprint(),
    runtime_seconds = result$runtime_seconds %||% NA_real_,
    memory_bytes = result$memory_bytes %||% NA_real_,
    stringsAsFactors = FALSE
  )
}

ou_gh_validate_checkpoint <- function(path, task_row, estimator_hash) {
  if (!file.exists(path)) return(FALSE)
  value <- tryCatch(readRDS(path), error = function(condition) NULL)
  if (is.null(value)) return(FALSE)
  identical(value$checkpoint_metadata$estimator_hash, estimator_hash) &&
    identical(value$checkpoint_metadata$task_row_hash,
      ou_gh_hash_object(task_row))
}
