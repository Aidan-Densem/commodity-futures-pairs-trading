tz <- "Europe/London"
full_environment <- full_family_gh_environment()

# FF1: the full-family adapter admits every valid positive observed duration.
duration_pattern <- rep(c(1, 2, 5), length.out = 131L)
active <- c(0, cumsum(duration_pattern))
stamp <- as.POSIXct("2026-01-05 09:00", tz = tz) + active * 60
state <- sin(seq_along(active) / 9)
adapter_data <- data.frame(
  Dates = stamp, Active_Time_Minutes = active,
  Transition_Valid = c(FALSE, rep(TRUE, length(active) - 1L)),
  Structural_Segment_ID = 1L, Y_Midpoint = exp(state), X_Midpoint = 1,
  Structural_Exclusion = FALSE, Roll_Transition_From_Previous = FALSE,
  Roll_Transition_To_Next = FALSE, stringsAsFactors = FALSE
)
adapter_task <- data.frame(
  Pair = "Y_X", task_key = "ff_irregular", y_id = "Y", x_id = "X",
  y_price_column = "Y_Midpoint", x_price_column = "X_Midpoint",
  alpha = 0, beta = 0, formation_centre = 0,
  Formation_End = stamp[[131L]], Testing_Start = stamp[[132L]],
  stringsAsFactors = FALSE
)
resolved <- list(
  windows = list(data = adapter_data),
  definition = list(estimation_rows = 1:131, testing_rows = 132L)
)
prepared <- full_environment$ou_gh_prepare_selected_formation(
  adapter_task, resolved_window = resolved
)
smoke_expect(
  prepared$n_transitions == 130L && prepared$rejected_transitions == 0L,
  "FF1: full-family task adapter dropped valid 1/2/5-minute transitions"
)
smoke_expect(
  all(c("x_previous", "x_current", "delta", "segment_id") %in%
        names(prepared$transitions)) &&
    identical(sort(unique(prepared$transitions$delta)), c(1, 2, 5)) &&
    prepared$unique_duration_count == 3L,
  "FF2: full-family segment representation discarded observed durations"
)

# FF2: duration-aware profiling recovers an irregularly sampled Gaussian OU.
set.seed(26082026)
n_profile <- 5000L
profile_delta <- rep(c(1, 2, 5, 1, 1, 2), length.out = n_profile)
truth_mu <- .35; truth_kappa <- .018; truth_sigma <- .11
profile_state <- numeric(n_profile + 1L); profile_state[[1L]] <- truth_mu
for (i in seq_len(n_profile)) {
  attenuation <- exp(-truth_kappa * profile_delta[[i]])
  variance_ratio <- -expm1(-2 * truth_kappa * profile_delta[[i]]) /
    -expm1(-2 * truth_kappa)
  profile_state[[i + 1L]] <- truth_mu + attenuation *
    (profile_state[[i]] - truth_mu) + stats::rnorm(1, sd = truth_sigma * sqrt(variance_ratio))
}
profile_transitions <- data.frame(
  segment_id = 1L, transition_order = seq_len(n_profile),
  x_previous = head(profile_state, -1L), x_current = tail(profile_state, -1L),
  delta = profile_delta, stringsAsFactors = FALSE
)
profile_fit <- full_environment$ou_gh_family_profile_states(profile_transitions)
smoke_expect(
  isTRUE(profile_fit$duration_aware) && profile_fit$unique_duration_count == 3L &&
    abs(profile_fit$kappa / truth_kappa - 1) < .35,
  "FF3: irregular-duration OU profile did not recover the synthetic mean reversion"
)

# FF3: horizon pairs use exact cumulative active time and never row-lag proxies.
toy_transition <- data.frame(
  segment_id = 1L, transition_order = 1:4,
  x_previous = 0:3, x_current = 1:4, delta = c(1, 2, 2, 5),
  stringsAsFactors = FALSE
)
pairs5 <- full_environment$ou_gh_build_horizon_pairs(toy_transition, 5)
smoke_expect(nrow(pairs5) == 2L && all(pairs5$active_horizon == 5),
             "FF4: cumulative active-time horizon construction failed")
smoke_expect(identical(pairs5$row_lag, c(3L, 1L)),
             "FF5: row lag was incorrectly equated with active horizon")
smoke_expect(nrow(full_environment$ou_gh_build_horizon_pairs(toy_transition, 6)) == 0L,
             "FF6: an unmatched active horizon was interpolated or subdivided")

# Additional duration contracts: CCF scoring calls the active-time matcher; the 24-cell bank and
# branch-local metadata remain frozen.
matcher_called <- FALSE
original_matcher <- full_environment$ou_gh_build_horizon_pairs
assign("ou_gh_build_horizon_pairs", function(...) {
  matcher_called <<- TRUE
  original_matcher(...)
}, envir = full_environment)
gaussian_fit <- full_environment$ou_gh_family_gaussian_fit(0, .02, .1)
ccf_score <- full_environment$ou_gh_family_ccf_score(
  gaussian_fit, profile_transitions[1:500, , drop = FALSE],
  bank = data.frame(horizon = 5, frequency = .25, instrument = 0),
  quadrature_nodes = 8L
)
assign("ou_gh_build_horizon_pairs", original_matcher, envir = full_environment)
smoke_expect(matcher_called && is.finite(ccf_score),
             "Full-family CCF score bypassed exact active-time horizons")
configured_ccf_score <- full_environment$ou_gh_family_ccf_score(
  gaussian_fit, profile_transitions[1:500, , drop = FALSE],
  bank = data.frame(horizon = 5, frequency = .25, instrument = 0),
  quadrature_nodes = 8L,
  objective_weights = FULL_FAMILY_GH_CONTRACT$ccf_objective
)
reference_ccf_score <- full_environment$ou_gh_family_ccf_score(
  gaussian_fit, profile_transitions[1:500, , drop = FALSE],
  bank = data.frame(horizon = 5, frequency = .25, instrument = 0),
  quadrature_nodes = 8L,
  objective_weights = list(
    innovation_ecf_weight = 20,
    conditional_prediction_loss_weight = 0.05,
    instrumented_moment_weight = 0.25,
    horizon_scaling_exponent = 0.5
  )
)
smoke_expect(
  isTRUE(all.equal(
    configured_ccf_score, reference_ccf_score,
    tolerance = 1e-14, check.attributes = TRUE
  )),
  "Externalised full-family CCF weights changed the frozen objective"
)
smoke_expect(
  nrow(FULL_FAMILY_GH_CONTRACT$ccf_bank) == 24L &&
    isTRUE(all.equal(
      full_environment$ou_gh_family_ccf_bank(), FULL_FAMILY_GH_CONTRACT$ccf_bank,
      check.attributes = FALSE
    )),
  "Full-family transform bank drifted from its 24-cell branch contract"
)
smoke_expect(
  identical(FULL_FAMILY_GH_CONTRACT$gh_mode, "FULL_FAMILY") &&
    grepl("irregular_active_horizons", FULL_FAMILY_GH_CONTRACT$version, fixed = TRUE),
  "Full-family branch/duration metadata are ambiguous"
)

# FFR1-FFR7: topology-aware routing, including named exact restrictions and
# the analytic Gaussian control.
topology <- full_environment$ou_gh_family_candidate_topology()
smoke_expect(
  nrow(topology) == 8L && all(c("NIG", "hyperbolic", "symmetric_GH") %in%
    topology$candidate_name) && !topology$parsimony_eligible[topology$candidate_name == "Gaussian_limit"],
  "Candidate topology is incomplete or treats Gaussian as an ordinary boundary"
)
mock_candidates <- function(scores) {
  models <- topology$candidate_name
  values <- setNames(rep(.5, length(models)), models)
  values[names(scores)] <- scores
  setNames(lapply(models, function(model) list(
    fit_status = "success", formation_holdout_score = values[[model]],
    regime = if (model %in% c("NIG", "hyperbolic", "symmetric_GH")) "interior_GH" else model,
    fit = list(regime = model, kappa = .01),
    moment_status = list(
      centred_OU_admissible = TRUE, variance_exists = TRUE,
      threshold_moment_contract_admissible = TRUE
    )
  )), models)
}
route <- function(scores) full_environment$ou_gh_family_route_candidates(mock_candidates(scores))
smoke_expect(route(c(interior_GH = .1))$selected_model == "interior_GH",
             "FFR1: clear interior winner was not retained")
smoke_expect(route(c(interior_GH = .10002, VG_boundary = .1))$selected_model == "VG_boundary",
             "FFR2: unique VG boundary was not selected inside equivalence band")
smoke_expect(route(c(interior_GH = .10002, NIG = .1))$selected_model == "NIG",
             "FFR3: unique NIG restriction was not selected")
smoke_expect(route(c(interior_GH = .10002, hyperbolic = .1))$selected_model == "hyperbolic",
             "FFR4: unique hyperbolic restriction was not selected")
smoke_expect(route(c(interior_GH = .10002, symmetric_GH = .1))$selected_model == "symmetric_GH",
             "FFR5: unique symmetric-GH restriction was not selected")
smoke_expect(
  route(c(skew_t_boundary = .10002, symmetric_Student_t_boundary = .1))$selected_model ==
    "symmetric_Student_t_boundary",
  "Unique symmetric Student-t boundary was not selected"
)
multi_route <- route(c(interior_GH = .10002, VG_boundary = .1, NIG = .10001))
smoke_expect(
  multi_route$selected_model == "VG_boundary" &&
    multi_route$router_status == "multiple_candidates_indistinguishable_numeric_minimum_retained",
  "FFR6: multiple equivalent restrictions did not retain the numerical minimum"
)
smoke_expect(
  route(c(Gaussian_limit = .1, interior_GH = .10001))$selected_model == "Gaussian_limit",
  "FFR7: analytic Gaussian control was routed as a generic GH boundary"
)

# FR1-FR4: expected numerical failures are isolated and represented in the
# route manifest; schema/programmer errors remain fatal.
failure_manifest <- data.frame(
  Pair = c("A_B", "C_D"), Session_Date = as.Date(c("2026-01-01", "2026-01-02")),
  task_key = c("a", "b"), stringsAsFactors = FALSE
)
isolated <- full_family_gh_run_fit_tasks(failure_manifest, function(task, i) {
  if (i == 1L) stop("optimizer returned a non-finite objective", call. = FALSE)
  list(gh_mode = "FULL_FAMILY", fit_status = "FIT_AVAILABLE", model_reason = "ok",
       selected_fit = list(kappa = .01), route = list(), task = task)
})
smoke_expect(
  isolated[[2L]]$fit_status == "FIT_AVAILABLE",
  "FR1: one expected fit failure aborted or removed its successful sibling"
)
smoke_expect(isolated[[1L]]$fit_status == "MODEL_UNAVAILABLE",
             "FR2: expected failed fit did not become MODEL_UNAVAILABLE")
fatal_fit <- inherits(try(full_family_gh_run_fit_tasks(
  failure_manifest[1, , drop = FALSE], function(task, i) stop("object x not found")
), silent = TRUE), "try-error")
smoke_expect(fatal_fit, "Programmer/schema fit error was silently downgraded")
threshold_tasks <- list(
  list(pair = "A_B", endpoint_date = as.Date("2026-01-01")),
  list(pair = "C_D", endpoint_date = as.Date("2026-01-02"))
)
threshold_isolated <- full_family_gh_run_threshold_tasks(threshold_tasks, function(task) {
  if (task$pair == "A_B") stop("threshold numerical inversion failed", call. = FALSE)
  list(threshold_task_status = "THRESHOLD_AVAILABLE")
})
smoke_expect(
  threshold_isolated[[1L]]$threshold_task_status == "THRESHOLD_UNAVAILABLE" &&
    threshold_isolated[[2L]]$threshold_task_status == "THRESHOLD_AVAILABLE",
  "FR3: threshold numerical failure was not isolated"
)
route_schedule <- data.frame(
  endpoint_id = paste0("e", 1:3), endpoint_session_date = as.Date("2026-02-01") + 0:2,
  pair_id = c("A_B", "C_D", "E_F"), primary_rank = 1:3, selected = TRUE,
  testing_session_dates = as.character(as.Date("2026-02-10") + 0:2),
  testing_sessions = 1L, stringsAsFactors = FALSE
)
availability <- data.frame(
  pair_id = route_schedule$pair_id,
  endpoint_session_date = route_schedule$endpoint_session_date,
  model_available = c(FALSE, TRUE, TRUE),
  model_reason = c("fit_failure", "fit_available", "fit_available")
)
threshold_routes <- data.frame(
  Pair = c("C_D", "E_F"), Session_Date = route_schedule$endpoint_session_date[2:3],
  route_status = c("THRESHOLD_UNAVAILABLE", "MODEL_NO_TRADE"),
  strategy_available = FALSE, stringsAsFactors = FALSE
)
three_routes <- build_model_route_manifest(
  route_schedule, "Full-family OU-GH", threshold_routes, availability
)
smoke_expect(
  identical(three_routes$route_status,
    c("MODEL_UNAVAILABLE", "THRESHOLD_UNAVAILABLE", "MODEL_NO_TRADE")),
  "FR4: full-family fit/threshold/no-trade route states are not distinct"
)

# CAP1-CAP3: exact frozen testing identities drive task caches and committed
# capital dates, including non-contiguous calendar dates.
calendar_dates <- as.Date("2026-03-02") + c(0:4, 7:11, 14:18, 21:25)
pair_quotes <- data.frame(
  pair_id = "Y_X", calendar_session_date = calendar_dates,
  timestamp = as.POSIXct(paste(calendar_dates, "12:00"), tz = tz),
  statistical_quote_valid = TRUE, stringsAsFactors = FALSE
)
rolling <- build_pair_rolling_session_windows(
  pair_quotes, formation_sessions = 10L, testing_sessions = 10L, step_sessions = 10L
)
frozen_dates <- decode_session_dates(
  rolling$testing_session_dates[[1L]], rolling$testing_sessions[[1L]]
)
smoke_expect(length(frozen_dates) == 10L && identical(frozen_dates, calendar_dates[11:20]),
             "CAP1: rolling windows did not freeze the exact ten testing dates")
sync <- transform(
  pair_quotes,
  Active_Time_Minutes = seq(0, by = 10, length.out = nrow(pair_quotes)),
  midpoint_y = 100 + seq_len(nrow(pair_quotes)) / 100,
  midpoint_x = 50 + seq_len(nrow(pair_quotes)) / 100,
  structural_segment_id = 1L, transition_valid = c(FALSE, rep(TRUE, nrow(pair_quotes) - 1L)),
  roll_boundary = FALSE
)
extra_date <- setdiff(seq(min(frozen_dates), max(frozen_dates), by = "day"), frozen_dates)[[1L]]
extra_sync <- sync[1, , drop = FALSE]
extra_sync$calendar_session_date <- extra_date
extra_sync$timestamp <- as.POSIXct(paste(extra_date, "12:00"), tz = tz)
sync <- rbind(sync, extra_sync)
sync <- sync[order(sync$timestamp), , drop = FALSE]
sync$Active_Time_Minutes <- seq(0, by = 10, length.out = nrow(sync))
sync$transition_valid <- c(FALSE, rep(TRUE, nrow(sync) - 1L))
gh_selected <- transform(
  rolling[1, , drop = FALSE], selected = TRUE, primary_rank = 1L,
  alpha = 0, beta = 1, formation_centre = 0,
  y_generic = "Y", x_generic = "X"
)
manifest <- build_gh_task_manifest(
  gh_selected, list(Y_X = sync), tempfile("full_family_gh_cache_"), "FULL_FAMILY"
)
saved_cache <- readRDS(manifest$spread_object_path_or_identifier[[1L]])
testing_rows <- saved_cache$windows$windows[[1L]]$testing_rows
smoke_expect(
  length(testing_rows) == 10L &&
    identical(sort(unique(as.Date(saved_cache$windows$data$Dates[testing_rows]))), frozen_dates) &&
    !extra_date %in% as.Date(saved_cache$windows$data$Dates[testing_rows]),
  "CAP2: GH task construction expanded testing dates to a calendar range"
)
capital_route <- build_model_route_manifest(
  gh_selected, "M", data.frame(), data.frame(
    pair_id = "Y_X", endpoint_session_date = gh_selected$endpoint_session_date,
    model_available = FALSE, model_reason = "fixture"
  )
)
capital <- mab_build_selected_schedule_ledger(
  list(), data.frame(), list(), gh_selected, capital_route, list(Y_X = sync)
)
smoke_expect(
  nrow(capital$ledger) == 10L && identical(capital$ledger$session_date, frozen_dates) &&
    all(capital$ledger$committed_capital_usd == 200000),
  "CAP3: committed capital was not retained on exactly the frozen ten sessions"
)

# KT1-KT2: the complete 18-truth/five-replication design is executable, while
# the smoke run uses only a tiny irregular Gaussian path and deterministic route.
source(repo_path(
  "R", "alternatives", "full_family_ou_gh", "validation",
  "full_family_known_truth_validation.R"
))
known_design <- full_family_known_truth_design(full_environment)
smoke_expect(
  nrow(known_design) == 90L && length(unique(known_design$truth_id)) == 18L,
  "KT1: full-family known-truth ADEMP design is incomplete"
)
truth_bank <- full_environment$ou_gh_full_family_truth_bank()
smoke_expect(
  identical(truth_bank$interior_symmetric$truth_candidate, "symmetric_GH") &&
    identical(truth_bank$direct_nig_control$truth_candidate, "NIG") &&
    identical(truth_bank$direct_hyperbolic_control$truth_candidate, "hyperbolic") &&
    identical(truth_bank$gaussian_control$truth_candidate, "Gaussian_limit") &&
    identical(
      full_family_known_truth_target_model(truth_bank$interior_symmetric),
      "symmetric_GH"
    ),
  "KT2: exact known-truth restrictions are labelled as generic interior GH"
)
gaussian_truth <- full_environment$ou_gh_full_family_truth_bank()$gaussian_control
known_transitions <- full_family_known_truth_simulate_transitions(
  full_environment, gaussian_truth, rep(c(1, 2, 5), 50L), seed = 26082026L
)
smoke_expect(
  nrow(known_transitions) == 150L &&
    identical(sort(unique(known_transitions$delta)), c(1, 2, 5)) &&
    route(c(Gaussian_limit = .1))$selected_model == "Gaussian_limit",
  "KT3: tiny known-truth Gaussian path/router recovery failed"
)

cat(paste(
  "FULL_FAMILY_DURATION_ROUTER_AND_CAPITAL_CONTRACTS_PASS",
  "FF1-FF6 FFR1-FFR7 FR1-FR4 CAP1-CAP3 KT1-KT3\n"
))
