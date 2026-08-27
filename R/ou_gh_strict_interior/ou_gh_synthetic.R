ou_gh_estimator_fingerprint <- function(
    route = "staged_exact_likelihood",
    root = ou_gh_project_root()
) {
  module <- function(name) file.path(root, "R", "ou_gh_strict_interior", name)
  ou_gh_hash_object(list(
    route = route,
    ccf = ou_gh_sha256(module("ou_gh_ccf_objective.R")),
    estimation = ou_gh_sha256(module("ou_gh_estimation.R")),
    exact = ou_gh_sha256(module("ou_gh_exact_likelihood.R")),
    inversion = ou_gh_sha256(module("ou_gh_direct_inversion.R")),
    synthetic = ou_gh_sha256(module("ou_gh_synthetic.R")),
    bank = attr(ou_gh_load_frozen_moment_bank(root), "bank_hash")
  ))
}

ou_gh_simulate_segmented_transitions <- function(
    fit,
    segment_lengths,
    table = NULL,
    stationary_table = NULL,
    seed = 8042026L,
    initial_state = c("stationary", "centre", "burn_in"),
    burn_in_steps = 5000L
) {
  initial_state <- match.arg(initial_state)
  segment_lengths <- as.integer(segment_lengths)
  ou_gh_assert(all(segment_lengths >= 1L) && any(segment_lengths > 630L),
    "Synthetic layout must contain positive segments and support horizon 630.")
  if (is.null(table)) {
    table <- ou_gh_build_validated_fft_table(fit, 1, 8192L, 18, 34, 4, 48L)
  }
  if (initial_state == "stationary" && is.null(stationary_table)) {
    stationary_table <- ou_gh_build_validated_stationary_table(
      fit, 8192L, 18, 34, 4, 64L
    )
  }
  set.seed(as.integer(seed)[[1L]])
  total_innovations <- sum(segment_lengths) +
    if (initial_state == "burn_in") length(segment_lengths) * burn_in_steps else 0L
  total_uniforms <- total_innovations +
    if (initial_state == "stationary") length(segment_lengths) else 0L
  uniforms <- stats::runif(total_uniforms)
  cursor <- 0L
  output <- vector("list", length(segment_lengths))
  global_offset <- 0L
  attenuation <- exp(-fit[["kappa"]])
  initial_uniforms <- if (initial_state == "stationary") {
    uniforms[seq_len(length(segment_lengths))]
  } else numeric()
  if (initial_state == "stationary") cursor <- length(segment_lengths)
  for (segment_index in seq_along(segment_lengths)) {
    length_one <- segment_lengths[[segment_index]]
    burn <- if (initial_state == "burn_in") burn_in_steps else 0L
    count <- length_one + burn
    innovations <- ou_gh_draw_remainder_table(
      count, table,
      uniforms = uniforms[cursor + seq_len(count)]
    )
    cursor <- cursor + count
    path <- numeric(count + 1L)
    path[[1L]] <- if (initial_state == "stationary") {
      fit[["mu"]] + ou_gh_table_quantile(
        initial_uniforms[[segment_index]], stationary_table
      )
    } else fit[["mu"]]
    for (index in seq_len(count)) {
      path[[index + 1L]] <- fit[["mu"]] + attenuation *
        (path[[index]] - fit[["mu"]]) + innovations[[index]]
    }
    if (burn > 0L) path <- path[(burn + 1L):length(path)]
    previous <- path[seq_len(length_one)]
    current <- path[seq_len(length_one) + 1L]
    rows <- global_offset + seq_len(length_one + 1L)
    output[[segment_index]] <- data.frame(
      pair_id = "SYNTHETIC",
      task_key = paste0("synthetic_segment_", segment_index),
      segment_id = segment_index,
      x_previous = previous,
      x_current = current,
      previous_timestamp = as.POSIXct(
        rows[seq_len(length_one)] * 60,
        origin = "2020-01-01", tz = "Europe/London"
      ),
      current_timestamp = as.POSIXct(
        rows[seq_len(length_one) + 1L] * 60,
        origin = "2020-01-01", tz = "Europe/London"
      ),
      delta = 1,
      previous_global_row = rows[seq_len(length_one)],
      current_global_row = rows[seq_len(length_one) + 1L],
      stringsAsFactors = FALSE
    )
    global_offset <- max(rows) + 10L
  }
  transitions <- do.call(rbind, output)
  attr(transitions, "simulation_contract") <- list(
    construction = "OU_GH_DRIVER",
    remainder_route = table$status,
    table_fingerprint = table$fingerprint,
    initial_state = initial_state,
    stationary_table_fingerprint = if (initial_state == "stationary") {
      stationary_table$fingerprint
    } else NA_character_,
    burn_in_steps = if (initial_state == "burn_in") burn_in_steps else 0L,
    seed = as.integer(seed)[[1L]],
    segment_lengths = segment_lengths
  )
  transitions
}

ou_gh_synthetic_truth_manifest <- function(repetitions = 5L) {
  cases <- data.frame(
    case_id = c(
      "symmetric", "nig_positive", "nig_negative", "hyperbolic",
      "vg_near", "concentrated", "strong_positive", "long_half_life"
    ),
    lambda = c(-2, -0.5, -0.5, 1, 1.2, 5, 0.5, -1),
    zeta = c(1, 1.5, 1.5, 2.5, 0.02, 100, 0.5, 0.2),
    rho = c(0, 0.55, -0.55, 0.2, 0.35, -0.5, 0.85, -0.75),
    sigma_eta_1 = c(0.02, 0.03, 0.03, 0.02, 0.05, 0.015, 0.04, 0.02),
    half_life = c(300, 1500, 1500, 8000, 100, 30000, 30, 100000),
    difficulty = c(
      "ordinary", "ordinary", "ordinary", "ordinary",
      "difficult", "difficult", "difficult", "difficult"
    ),
    stringsAsFactors = FALSE
  )
  manifest <- merge(cases, data.frame(
    repetition = seq_len(as.integer(repetitions)), stringsAsFactors = FALSE
  ), all = TRUE)
  manifest <- manifest[order(manifest$case_id, manifest$repetition), ]
  manifest$synthetic_task_key <- paste0(
    manifest$case_id, "__rep_", sprintf("%03d", manifest$repetition)
  )
  manifest$seed <- OU_GH_EXECUTION_CONTRACT$base_seed +
    match(manifest$case_id, cases$case_id) * 10000L + manifest$repetition
  manifest
}

ou_gh_synthetic_fit_one <- function(
    row,
    segment_lengths = rep(1000L, 20L),
    pair_cap = 3000L,
    coarse_evaluation_budget = 140L,
    refinement_evaluation_budget = 100L,
    table = NULL,
    estimator_route = c("staged_exact_likelihood", "ccf_only")
) {
  estimator_route <- match.arg(estimator_route)
  estimator_hash <- ou_gh_estimator_fingerprint(estimator_route)
  truth <- gh_shape_scale_to_direct(
    row$lambda, row$zeta, row$rho, row$sigma_eta_1,
    log(2) / row$half_life, mu = 0.1
  )
  if (is.null(table)) {
    table <- ou_gh_build_validated_fft_table(truth, 1, 8192L, 18, 34, 4, 48L)
  }
  stationary_table <- ou_gh_build_validated_stationary_table(
    truth, 8192L, 18, 34, 4, 64L
  )
  transitions <- ou_gh_simulate_segmented_transitions(
    truth, segment_lengths, table, stationary_table, row$seed,
    initial_state = "stationary"
  )
  split <- ou_gh_split_formation(transitions)
  if (estimator_route == "ccf_only") {
    fit <- ou_gh_fit_ccf(
      split$training, split$validation,
      pair_cap = pair_cap,
      coarse_evaluation_budget = coarse_evaluation_budget,
      refinement_evaluation_budget = refinement_evaluation_budget
    )
    estimated <- fit$fit_scaled
    fit_status <- fit$fit_status
    fit_failure_reason <- fit$fit_failure_reason
    fit_runtime <- fit$runtime_seconds
    selected_raw <- fit$selected_raw
  } else {
    started_fit <- proc.time()[["elapsed"]]
    fit <- ou_gh_exact_screen_and_refine(
      split$training,
      refinement_pair_cap = pair_cap,
      evaluation_budget = coarse_evaluation_budget,
      iteration_budget = refinement_evaluation_budget
    )
    ccf_refinement <- if (is.null(fit$selected_raw)) NULL else
      ou_gh_refine_kappa_scale_ccf(
        fit$selected_raw, split$training, split$validation, fit$profile,
        pair_cap = pair_cap,
        evaluation_budget = refinement_evaluation_budget
      )
    estimated <- if (is.null(ccf_refinement)) NULL else
      ccf_refinement$selected_fit
    fit_status <- if (is.null(ccf_refinement)) fit$status else
      ccf_refinement$status
    fit_failure_reason <- if (is.null(estimated)) fit$status else ""
    fit_runtime <- proc.time()[["elapsed"]] - started_fit
    selected_raw <- if (is.null(ccf_refinement)) fit$selected_raw else
      ccf_refinement$selected_raw
    fit$ccf_refinement <- ccf_refinement
  }
  if (is.null(estimated)) {
    return(list(summary = data.frame(
      synthetic_task_key = row$synthetic_task_key,
      case_id = row$case_id, repetition = row$repetition,
      difficulty = row$difficulty, estimator_route = estimator_route,
      estimator_hash = estimator_hash,
      status = fit_status, admissible = FALSE, reason = fit_failure_reason,
      law_distance = NA_real_, half_life_relative_error = NA_real_,
      skew_sign_correct = NA, runtime_seconds = fit_runtime,
      fit_hash = NA_character_, stringsAsFactors = FALSE
    ), fit = fit, truth = truth, split = split,
    simulation_contract = attr(transitions, "simulation_contract")))
  }
  law_distance <- ou_gh_transition_law_distance(
    truth, estimated, quadrature_nodes = 48L
  )
  diagnostic_bank <- ou_gh_load_frozen_moment_bank()
  diagnostic_profile <- if (estimator_route == "ccf_only") {
    fit$profile
  } else fit$profile
  diagnostic_training <- ou_gh_prepare_ccf_data(
    split$training, diagnostic_bank, pair_cap, diagnostic_profile
  )
  diagnostic_validation <- ou_gh_prepare_ccf_data(
    split$validation, diagnostic_bank, pair_cap, diagnostic_profile
  )
  truth_raw <- ou_gh_fit_to_raw(truth)
  truth_training_ccf <- ou_gh_ccf_objective(
    truth_raw, diagnostic_training, 24L
  )
  truth_holdout_ccf <- ou_gh_ccf_objective(
    truth_raw, diagnostic_validation, 24L
  )
  estimated_training_ccf <- ou_gh_ccf_objective(
    selected_raw, diagnostic_training, 24L
  )
  estimated_holdout_ccf <- ou_gh_ccf_objective(
    selected_raw, diagnostic_validation, 24L
  )
  truth_in_ccf_envelope <-
    truth_training_ccf <= estimated_training_ccf * 1.05 + 1e-8 &&
    abs(truth_holdout_ccf - estimated_holdout_ccf) <=
      max(estimated_holdout_ccf * 0.05, 1e-5)
  estimated_half_life <- log(2) / estimated[["kappa"]]
  fit_hash <- ou_gh_hash_object(list(
    task_key = row$synthetic_task_key,
    estimator_route = estimator_route,
    selected_raw = selected_raw,
    bank_hash = if (estimator_route == "ccf_only") fit$bank_hash else
      ou_gh_hash_object(ou_gh_load_frozen_moment_bank()),
    training_hash = ou_gh_hash_object(split$training[, c(
      "segment_id", "x_previous", "x_current"
    )]),
    source_version = OU_GH_CONTINUATION_VERSION
  ))
  summary <- data.frame(
    synthetic_task_key = row$synthetic_task_key,
    case_id = row$case_id, repetition = row$repetition,
    difficulty = row$difficulty, estimator_route = estimator_route,
    estimator_hash = estimator_hash,
    status = fit_status,
    admissible = !is.null(estimated) && all(is.finite(estimated)),
    reason = fit_failure_reason,
    law_distance = law_distance,
    truth_in_ccf_envelope = truth_in_ccf_envelope,
    law_or_truth_envelope_recovered = law_distance <= 0.10 ||
      truth_in_ccf_envelope,
    half_life_relative_error = abs(estimated_half_life / row$half_life - 1),
    skew_sign_correct = sign(estimated[["rho"]]) == sign(row$rho) || row$rho == 0,
    truth_mu = truth[["mu"]], estimated_mu = estimated[["mu"]],
    truth_kappa = truth[["kappa"]], estimated_kappa = estimated[["kappa"]],
    truth_sigma_eta_1 = truth[["sigma_eta_1"]],
    estimated_sigma_eta_1 = estimated[["sigma_eta_1"]],
    truth_lambda = truth[["lambda"]], estimated_lambda = estimated[["lambda"]],
    truth_zeta = truth[["zeta"]], estimated_zeta = estimated[["zeta"]],
    truth_rho = truth[["rho"]], estimated_rho = estimated[["rho"]],
    primitive_status = if (estimator_route == "ccf_only") {
      fit$primitive_identification_status
    } else "exact_likelihood_primitive_assessment_pending",
    law_status = if (law_distance <= 0.10) "law_recovered" else
      "law_recovery_weak",
    kappa_status = if (abs(estimated_half_life / row$half_life - 1) <= .25) {
      "kappa_recovered"
    } else "kappa_recovery_weak",
    training_objective = estimated_training_ccf,
    holdout_score = estimated_holdout_ccf,
    truth_training_ccf = truth_training_ccf,
    truth_holdout_ccf = truth_holdout_ccf,
    projected_gradient_max = NA_real_,
    runtime_seconds = fit_runtime,
    fit_hash = fit_hash,
    stringsAsFactors = FALSE
  )
  list(
    summary = summary, fit = fit, truth = truth, split = split,
    simulation_contract = attr(transitions, "simulation_contract")
  )
}
