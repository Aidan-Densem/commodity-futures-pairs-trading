# ADEMP-style scientific validation for the full-family OU-GH estimator and
# router.  This module contains no reported result and does not run on source.

ffkt_or <- function(value, fallback) if (is.null(value) || !length(value)) fallback else value

full_family_known_truth_design <- function(environment, replications = 5L) {
  bank <- environment$ou_gh_full_family_truth_bank()
  design <- expand.grid(
    truth_id = names(bank), replication = seq_len(as.integer(replications)),
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
  design$truth_candidate <- vapply(
    design$truth_id,
    function(id) bank[[id]][["truth_candidate"]],
    character(1L)
  )
  partition_bank <- c(
    "development", "frozen_validation", "frozen_validation",
    "frozen_validation", "untouched_holdout"
  )
  design$partition <- partition_bank[pmin(design$replication, length(partition_bank))]
  design$seed <- 26000000L + match(design$truth_id, names(bank)) * 100L +
    design$replication
  design$case_id <- paste(
    design$partition, design$truth_id,
    sprintf("r%02d", design$replication), sep = "__"
  )
  design
}

full_family_known_truth_duration_schedule <- function(n_transitions) {
  pattern <- c(1, 1, 2, 1, 5, 1, 2, 1, 1, 5, 2, 1)
  rep(pattern, length.out = as.integer(n_transitions))
}

full_family_known_truth_simulate_transitions <- function(
    environment, fit, durations, seed, x0 = fit[["mu"]],
    quadrature_nodes = 48L) {
  durations <- as.numeric(durations)
  stopifnot(length(durations) >= 1L, all(is.finite(durations) & durations > 0))
  set.seed(as.integer(seed)[[1L]])
  tables <- list()
  if (!fit[["regime"]] %in% c("Gaussian_limit", "VG_boundary")) {
    for (Delta in sort(unique(durations))) {
      tables[[as.character(Delta)]] <- environment$ou_gh_family_build_fft_table(
        fit, Delta = Delta, quadrature_nodes = quadrature_nodes
      )
      if (!isTRUE(tables[[as.character(Delta)]]$valid)) stop(
        "Known-truth Fourier table failed its numerical controls.", call. = FALSE
      )
    }
  }
  state <- numeric(length(durations) + 1L)
  state[[1L]] <- x0
  for (i in seq_along(durations)) {
    Delta <- durations[[i]]
    table <- tables[[as.character(Delta)]]
    remainder <- environment$ou_gh_family_remainder_draw(
      1L, fit, Delta = Delta, table = table
    )
    attenuation <- exp(-fit[["kappa"]] * Delta)
    state[[i + 1L]] <- fit[["mu"]] + attenuation *
      (state[[i]] - fit[["mu"]]) + remainder[[1L]]
  }
  data.frame(
    segment_id = 1L, transition_order = seq_along(durations),
    x_previous = head(state, -1L), x_current = tail(state, -1L),
    delta = durations, stringsAsFactors = FALSE
  )
}

full_family_known_truth_target_model <- function(fit) {
  if (!is.null(fit[["truth_candidate"]])) {
    return(as.character(fit[["truth_candidate"]]))
  }
  id <- ffkt_or(fit[["truth_id"]], "")
  if (identical(id, "direct_nig_control")) return("NIG")
  if (identical(id, "direct_hyperbolic_control")) return("hyperbolic")
  fit[["regime"]]
}

full_family_known_truth_run_case <- function(
    environment, case_row, sample_length = 20000L, burn_in = 1000L,
    candidate_models = c(
      "interior_GH", "VG_boundary", "skew_t_boundary",
      "symmetric_Student_t_boundary", "NIG", "hyperbolic",
      "symmetric_GH", "Gaussian_limit"
    ), evaluation_budget = 18L, quadrature_nodes = 12L) {
  stopifnot(is.data.frame(case_row), nrow(case_row) == 1L)
  bank <- environment$ou_gh_full_family_truth_bank()
  truth <- bank[[as.character(case_row$truth_id[[1L]])]]
  if (is.null(truth)) stop("Unknown full-family known-truth case.", call. = FALSE)
  n_total <- as.integer(sample_length + burn_in)
  full <- full_family_known_truth_simulate_transitions(
    environment, truth, full_family_known_truth_duration_schedule(n_total),
    case_row$seed[[1L]], quadrature_nodes = max(24L, quadrature_nodes)
  )
  transitions <- full[seq.int(burn_in + 1L, n_total), , drop = FALSE]
  transitions$transition_order <- seq_len(nrow(transitions))
  states <- c(transitions$x_previous[[1L]], transitions$x_current)
  centre <- stats::median(states)
  scale <- 1.4826 * stats::median(abs(states - centre))
  if (!is.finite(scale) || scale <= 1e-8) scale <- stats::sd(states)
  standardised <- transitions
  standardised$x_previous <- (standardised$x_previous - centre) / scale
  standardised$x_current <- (standardised$x_current - centre) / scale
  truth_scaled <- environment$ou_gh_family_rescale_fit(truth, centre, scale)
  truth_scaled$truth_id <- truth$truth_id
  candidates <- environment$ou_gh_family_fit_candidates(
    standardised, candidate_models = candidate_models,
    evaluation_budget = evaluation_budget, quadrature_nodes = quadrature_nodes
  )
  route <- environment$ou_gh_family_route_candidates(candidates)
  target <- full_family_known_truth_target_model(truth)
  equivalents <- strsplit(
    ffkt_or(route$indistinguishable_candidates, ""), ";", fixed = TRUE
  )[[1L]]
  selected_fit <- route$selected_fit
  law_distance <- if (is.null(selected_fit)) NA_real_ else
    environment$ou_gh_family_law_distance(
      selected_fit, truth_scaled, quadrature_nodes = max(24L, quadrature_nodes)
    )
  truth_scale <- environment$ou_gh_family_stationary_scale(truth_scaled)
  selected_scale <- if (is.null(selected_fit)) NA_real_ else
    environment$ou_gh_family_stationary_scale(selected_fit)
  boundary <- c("VG_boundary", "skew_t_boundary", "symmetric_Student_t_boundary")
  data.frame(
    case_id = case_row$case_id, partition = case_row$partition,
    truth_id = truth$truth_id, truth_regime = truth$regime,
    target_candidate = target, selected_model = ffkt_or(route$selected_model, NA_character_),
    selected_regime = ffkt_or(route$selected_regime, NA_character_),
    router_status = route$router_status,
    truth_or_equivalent = target %in% c(ffkt_or(route$selected_model, ""), equivalents),
    false_boundary = !target %in% boundary &&
      ffkt_or(route$selected_model, "") %in% boundary,
    false_interior = target %in% boundary &&
      identical(ffkt_or(route$selected_model, ""), "interior_GH"),
    kappa_truth = truth_scaled$kappa,
    kappa_estimate = if (is.null(selected_fit)) NA_real_ else selected_fit$kappa,
    kappa_relative_error = if (is.null(selected_fit)) NA_real_ else
      abs(selected_fit$kappa / truth_scaled$kappa - 1),
    scale_truth = truth_scale, scale_estimate = selected_scale,
    scale_relative_error = abs(selected_scale / truth_scale - 1),
    law_cf_distance = law_distance,
    unique_duration_count = length(unique(transitions$delta)),
    testing_data_used = FALSE, monetary_pnl_used = FALSE,
    stringsAsFactors = FALSE
  )
}

run_full_family_known_truth_validation <- function(
    environment, replications = 5L, sample_length = 20000L,
    burn_in = 1000L, evaluation_budget = 18L, quadrature_nodes = 12L) {
  if (!identical(Sys.getenv("ALLOW_EXPENSIVE_FULL_FAMILY_KNOWN_TRUTH"), "TRUE")) {
    stop(
      "Set ALLOW_EXPENSIVE_FULL_FAMILY_KNOWN_TRUTH=TRUE to run the full ADEMP design.",
      call. = FALSE
    )
  }
  design <- full_family_known_truth_design(environment, replications)
  results <- lapply(seq_len(nrow(design)), function(i) {
    full_family_known_truth_run_case(
      environment, design[i, , drop = FALSE], sample_length, burn_in,
      evaluation_budget = evaluation_budget, quadrature_nodes = quadrature_nodes
    )
  })
  table <- do.call(rbind, results)
  list(
    design = design, case_results = table,
    recovery_summary = aggregate(
      cbind(
        truth_or_equivalent = as.numeric(table$truth_or_equivalent),
        false_boundary = as.numeric(table$false_boundary),
        false_interior = as.numeric(table$false_interior),
        kappa_relative_error = table$kappa_relative_error,
        scale_relative_error = table$scale_relative_error,
        law_cf_distance = table$law_cf_distance
      ) ~ truth_regime + partition, table, mean, na.rm = TRUE
    ),
    testing_data_used = FALSE, monetary_pnl_used = FALSE
  )
}
