ou_gh_family_candidate_models <- function() c(
  "interior_GH", "VG_boundary", "skew_t_boundary",
  "symmetric_Student_t_boundary", "NIG", "hyperbolic",
  "symmetric_GH", "Gaussian_limit"
)

ou_gh_family_empirical_fingerprint <- function(manifest, evaluation_budget,
                                                quadrature_nodes) {
  root <- ou_gh_project_root()
  source_files <- file.path("R", "alternatives", "full_family_ou_gh", c(
    "ou_gh_family_contract.R", "ou_gh_family_driver.R",
    "ou_gh_family_parameterization.R", "ou_gh_family_transition.R",
    "ou_gh_family_estimation.R", "ou_gh_family_simulation.R",
    "ou_gh_family_empirical.R", "ou_gh_task_adapter.R"
  ))
  ou_gh_hash_object(list(
    contract = "full_family_empirical_exact_transition_CCF_v2_irregular_active_horizons",
    task_keys = manifest$task_key,
    candidate_models = ou_gh_family_candidate_models(),
    evaluation_budget = evaluation_budget, quadrature_nodes = quadrature_nodes,
    router = list(equivalence_absolute = 5e-5, equivalence_relative = .05),
    source_hashes = setNames(vapply(file.path(root, source_files),
      ou_gh_sha256, character(1L)), source_files), testing_data_used = FALSE))
}

ou_gh_family_parameter_string <- function(fit) {
  if (is.null(fit)) return("")
  scalar <- names(fit)[vapply(fit, function(x) length(x) == 1L &&
    (is.numeric(x) || is.character(x) || is.logical(x)), logical(1L))]
  paste(vapply(scalar, function(name) paste0(name, "=",
    format(fit[[name]], digits = 14, scientific = TRUE)), character(1L)),
    collapse = ";")
}

ou_gh_family_candidate_rows <- function(task_key, candidates) {
  do.call(rbind, lapply(names(candidates), function(model) {
    one <- candidates[[model]]
    data.frame(task_key = task_key, candidate_model = model,
      candidate_regime = one$regime,
      fit_status = one$fit_status,
      training_score = one$training_score %||% NA_real_,
      formation_holdout_score = one$formation_holdout_score %||% NA_real_,
      fit_hash = one$fit_hash %||% NA_character_,
      selected_start = one$selected_start %||% NA_integer_,
      optimiser_attempt_count = length(one$attempts),
      runtime_seconds = one$runtime_seconds %||% NA_real_,
      transformed_parameters = ou_gh_family_parameter_string(one$raw),
      canonical_parameters = ou_gh_family_parameter_string(one$fit),
      centred_OU_admissible = one$moment_status$centred_OU_admissible %||% FALSE,
      variance_exists = one$moment_status$variance_exists %||% FALSE,
      testing_data_used = one$testing_data_used %||% FALSE,
      stringsAsFactors = FALSE)
  }))
}

ou_gh_family_prior_fit <- function(row) {
  if (!nrow(row) || !is.finite(row$kappa) || !is.finite(row$alpha)) return(NULL)
  list(regime = "interior_GH", mu = row$mu, kappa = row$kappa,
    lambda = row$lambda, alpha = row$alpha, beta = row$beta,
    delta = row$delta, chi = row$delta^2,
    psi = row$alpha^2 - row$beta^2, centred_m = row$centred_m,
    sigma_eta_1 = row$sigma_eta_1, zeta = row$zeta, rho = row$rho)
}

ou_gh_family_fit_empirical_task <- function(task_row, prior_row,
    checkpoint_dir, empirical_fingerprint, evaluation_budget = 50L,
    quadrature_nodes = 18L, build_simulator = FALSE,
    run_threshold_smoke = FALSE) {
  started <- proc.time()[["elapsed"]]
  checkpoint <- file.path(checkpoint_dir,
    paste0(gsub("[^A-Za-z0-9_.-]", "_", task_row$task_key), ".rds"))
  if (file.exists(checkpoint)) {
    saved <- readRDS(checkpoint)
    if (!identical(saved$empirical_fingerprint, empirical_fingerprint)) {
      stop("Checkpoint fingerprint mismatch: ", task_row$task_key, call. = FALSE)
    }
    return(saved)
  }
  prepared <- ou_gh_prepare_selected_formation(task_row)
  candidates <- ou_gh_family_fit_candidates(prepared$scaled_transitions,
    candidate_models = ou_gh_family_candidate_models(),
    evaluation_budget = evaluation_budget, quadrature_nodes = quadrature_nodes)
  route <- ou_gh_family_route_candidates(candidates)
  selected_scaled <- route$selected_fit %||% NULL
  selected_fit <- if (is.null(selected_scaled)) NULL else
    ou_gh_family_unscale_fit(selected_scaled, prepared$scaling$centre,
      prepared$scaling$scale)
  if (!is.null(selected_fit)) route$selected_fit <- selected_fit
  old_fit <- ou_gh_family_prior_fit(prior_row)
  old_distance <- if (is.null(selected_fit) || is.null(old_fit)) NA_real_ else
    tryCatch(ou_gh_family_law_distance(selected_fit, old_fit,
      frequencies = c(50, 150, 300), states = c(-.01, 0, .01),
      quadrature_nodes = 48L), error = function(e) NA_real_)
  simulator_status <- "pending_full_simulator_gate"
  simulator_route <- ""
  simulator_hash <- NA_character_
  threshold_status <- if (isTRUE(route$threshold_moment_contract_status))
    "pending_simulator" else "threshold_ineligible_moment_contract"
  threshold_smoke_status <- "not_requested"
  simulator_table <- NULL
  if (isTRUE(build_simulator) && !is.null(selected_fit)) {
    if (selected_fit$regime == "Gaussian_limit") {
      simulator_status <- "simulator_eligible_validated_analytic_Gaussian"
      simulator_route <- "analytic_Gaussian"
      simulator_hash <- ou_gh_hash_object(list(selected_fit, simulator_route))
    } else {
      simulator_table <- tryCatch(ou_gh_family_build_fft_table(selected_fit,
        n = if (ou_gh_family_moment_status(selected_fit)$variance_exists)
          8192L else 16384L, quadrature_nodes = 32L), error = identity)
      if (!inherits(simulator_table, "error") && isTRUE(simulator_table$valid)) {
        simulator_status <- "simulator_eligible_pilot_table_valid"
        simulator_route <- if (selected_fit$regime == "VG_boundary")
          "exact_gamma_OU_with_Fourier_reference" else
            "controlled_exact_remainder_Fourier_inversion"
        simulator_hash <- simulator_table$fingerprint
      } else {
        simulator_status <- "simulator_unavailable_pilot_build_failure"
        simulator_route <- "structured_unavailable"
      }
    }
    if (!isTRUE(route$threshold_moment_contract_status)) {
      threshold_status <- "threshold_ineligible_moment_contract"
    } else if (grepl("eligible", simulator_status, fixed = TRUE)) {
      threshold_status <- "threshold_eligible_pilot_smoke_pending"
    } else threshold_status <- "threshold_unavailable_simulator"
  }
  if (isTRUE(run_threshold_smoke) && identical(threshold_status,
      "threshold_eligible_pilot_smoke_pending")) {
    threshold_smoke_status <- tryCatch({
      if (!exists("st5_evaluate_threshold_grid_cpp", mode = "function"))
        stop("compiled_event_engine_unavailable")
      scale <- ou_gh_family_stationary_scale(selected_fit)
      distances <- scale * c(.5, 1, 1.5)
      candidate_grid <- expand.grid(d_plus = distances, d_minus = distances,
        KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
      candidate_grid$c_plus <- candidate_grid$c_minus <- 0
      candidate_grid$candidate_id <- paste0("pilot_", seq_len(nrow(candidate_grid)))
      paths <- ou_gh_family_path_matrix(selected_fit, 0:120, selected_fit$mu,
        250L, 88000L + task_row$official_rank, simulator_table)
      evaluated <- st5_evaluate_threshold_grid_cpp(paths, 0:120, candidate_grid,
        selected_fit$mu, .08 * scale, .08 * scale, FALSE)
      if (!any(is.finite(evaluated$Objective))) stop("no_finite_objective")
      "threshold_smoke_success"
    }, error = function(e) paste0("threshold_numerical_failure:",
      conditionMessage(e)))
    threshold_status <- threshold_smoke_status
  }
  candidate_rows <- ou_gh_family_candidate_rows(task_row$task_key, candidates)
  summary <- data.frame(task_key = task_row$task_key, Pair = task_row$Pair,
    Session_Date = as.character(task_row$Session_Date),
    endpoint_id = task_row$endpoint_id, official_rank = task_row$official_rank,
    trade_flag = task_row$trade_flag,
    operational_classification = task_row$operational_classification,
    formation_hash = prepared$formation_hash,
    formation_transition_count = prepared$n_transitions,
    formation_segment_count = prepared$n_segments,
    testing_data_used = FALSE,
    all_candidate_fit_success_rate = mean(candidate_rows$fit_status == "success"),
    selected_model = route$selected_model %||% NA_character_,
    selected_regime = route$selected_regime %||% NA_character_,
    router_status = route$router_status,
    indistinguishable_candidates = route$indistinguishable_candidates %||% "",
    selected_fit_hash = if (is.null(selected_fit)) NA_character_ else
      ou_gh_hash_object(selected_fit),
    selected_parameters = ou_gh_family_parameter_string(selected_fit),
    centred_OU_admissible = route$centred_OU_admissibility %||% FALSE,
    variance_exists = route$finite_variance_status %||% FALSE,
    threshold_moment_contract_status = route$threshold_moment_contract_status %||% FALSE,
    simulator_status = simulator_status, simulator_route = simulator_route,
    simulator_hash = simulator_hash, threshold_status = threshold_status,
    threshold_smoke_status = threshold_smoke_status,
    old_interior_law_cf_distance = old_distance,
    runtime_seconds = proc.time()[["elapsed"]] - started,
    empirical_fingerprint = empirical_fingerprint, stringsAsFactors = FALSE)
  result <- list(summary = summary, candidate_fits = candidates,
    candidate_rows = candidate_rows, route = route,
    selected_fit = selected_fit, formation_metadata = list(
      scaling = prepared$scaling, formation_hash = prepared$formation_hash,
      n_transitions = prepared$n_transitions, n_segments = prepared$n_segments,
      testing_rows_loaded = prepared$testing_rows_loaded),
    task = task_row, empirical_fingerprint = empirical_fingerprint,
    testing_data_used = FALSE, parameters_reestimated_after_routing = FALSE)
  ou_gh_atomic_save_rds(result, checkpoint)
  result
}

ou_gh_family_pilot_indices <- function(manifest, prior, target = 36L) {
  old <- prior[match(manifest$task_key, prior$task_key), , drop = FALSE]
  take <- function(index, n = 3L) head(index, n)
  extremes <- unique(c(take(order(old$zeta)),
    take(order(old$zeta, decreasing = TRUE)),
    take(order(old$rho)), take(order(old$rho, decreasing = TRUE)),
    take(order(abs(old$rho), decreasing = TRUE)), take(order(old$half_life)),
    take(order(old$half_life, decreasing = TRUE))))
  suppressed <- which(!as.logical(manifest$trade_flag))
  diverse <- unique(round(seq(1, nrow(manifest), length.out = target)))
  pool <- unique(c(extremes,
    suppressed[seq_len(min(8L, length(suppressed)))], diverse))
  pool[seq_len(min(target, length(pool)))]
}
