source(repo_path("config", "production_config.R"), local = TRUE)
source(repo_path("config", "contracts_v2.R"), local = TRUE)
source(repo_path("config", "complete_episode_threshold_contract.R"), local = TRUE)
source(repo_path("R", "v2_common.R"), local = TRUE)
source(repo_path("R", "data_contracts.R"), local = TRUE)
source(repo_path("R", "alternatives", "finite_horizon_mc",
                 "gaussian_finite_horizon.R"), local = TRUE)
source(repo_path("R", "threshold_objective.R"), local = TRUE)
source(repo_path("R", "threshold_mc.R"), local = TRUE)
source(repo_path("R", "strategy_specification.R"), local = TRUE)
source(repo_path("R", "model_route_manifest.R"), local = TRUE)

saw_simulator <- list(
  family = "deterministic_complete_episode_fixture",
  simulator_hash = "deterministic_complete_episode_fixture_v2",
  simulate_paths = function(active_time, x0, n_paths, seed) {
    pattern <- c(0, 4, -0.25, -4, 0.25)
    path <- rep(pattern, length.out = length(active_time)) + x0
    list(
      paths = matrix(rep(path, n_paths), nrow = length(active_time),
                     ncol = n_paths),
      active_time = active_time, seed = as.integer(seed),
      testing_data_used = FALSE
    )
  }
)
tiny_budgets <- list(
  coarse_paths = 2L, intermediate_paths = 3L, final_paths = 4L,
  path_batch_size = 2L, seed = 4101L,
  h_max_active_minutes = 12L, path_storage = "memory"
)
finite_budgets <- tiny_budgets[c(
  "coarse_paths", "intermediate_paths", "final_paths", "path_batch_size", "seed"
)]

# CE17 baseline: the settled finite-horizon function is evaluated before the
# alternative source is loaded, then again afterwards on the same tiny fixture.
finite_before <- calibrate_threshold_from_context(
  model = "finite_fixture", pair = "Y_X",
  endpoint_date = as.Date("2025-01-01"), simulator = saw_simulator,
  centre = 0, stationary_sd = 1, horizon = 8L, roundtrip_cost = 0.1,
  parameter_hash = "finite_parameter", parameter_source_hash = "finite_source",
  budgets = finite_budgets
)

source(repo_path("R", "complete_episode_threshold_mc.R"), local = TRUE)

# CE1 — identical public formals.
smoke_expect(
  identical(
    names(formals(calibrate_complete_episode_threshold_from_context)),
    names(formals(calibrate_threshold_from_context))
  ),
  "CE1: complete-episode calibrator does not match finite-horizon formals"
)

finite_after <- calibrate_threshold_from_context(
  model = "finite_fixture", pair = "Y_X",
  endpoint_date = as.Date("2025-01-01"), simulator = saw_simulator,
  centre = 0, stationary_sd = 1, horizon = 8L, roundtrip_cost = 0.1,
  parameter_hash = "finite_parameter", parameter_source_hash = "finite_source",
  budgets = finite_budgets
)
finite_before$completed_at <- finite_after$completed_at <- NULL
smoke_expect(identical(finite_before, finite_after),
             "CE17: finite-horizon estimator changed after alternative source load")

candidate <- data.frame(
  candidate_id = "episode", d_plus = 1, d_minus = 1,
  c_plus = 0, c_minus = 0, stringsAsFactors = FALSE
)
time_grid <- 0:12
episode_path <- c(0, 0, 0, 1.4, 1.3, 1.1, .8, .3, -.2, 2, -.1, 0, 0)
episode <- ce_complete_episode_one(
  episode_path, time_grid, candidate, 0, 0.1, 0.1,
  initial_horizon = 4L, h_max = 12L, extension_multiplier = 2L,
  path_id = "episode_path"
)

# CE3/CE10/CE11 — wait plus hold and completion beyond the supplied initial
# numerical horizon.
smoke_equal(episode$duration, 8, message = "CE3: duration excludes pre-entry wait")
smoke_equal(episode$wait_duration, 3, message = "CE3: waiting duration is wrong")
smoke_equal(episode$hold_duration, 5, message = "CE3: holding duration is wrong")
smoke_expect(episode$resolved && episode$extension_count >= 1L && episode$exit_time > 4,
             "CE10/CE11: adaptive same-path completion beyond horizon failed")

# CE4 — reward uses realised crossing values, including both overshoots.
smoke_equal(episode$reward, 1.4 - (-.2) - .1,
            message = "CE4: realised overshoot reward is wrong")

# CE5 — observations after the first exit cannot add a second episode.
episode_truncated <- ce_complete_episode_one(
  episode_path[1:9], time_grid[1:9], candidate, 0, 0.1, 0.1,
  initial_horizon = 4L, h_max = 8L, extension_multiplier = 2L,
  path_id = "episode_path"
)
smoke_equal(episode$reward, episode_truncated$reward,
            message = "CE5: a later re-entry changed one-episode reward")
smoke_equal(episode$duration, episode_truncated$duration,
            message = "CE5: a later re-entry changed one-episode duration")

# CE6 — ratio of expectations, not mean of pathwise ratios.
fixture_reward <- c(2, 2)
fixture_duration <- c(1, 3)
ratio_value <- ce_ratio_of_expectations(fixture_reward, fixture_duration)
smoke_equal(ratio_value, 1, message = "CE6: ratio-of-expectations identity failed")
smoke_expect(abs(ratio_value - mean(fixture_reward / fixture_duration)) > 0.1,
             "CE6: fixture does not distinguish the two ratio estimators")

# CE7 — hand-calculated delta-method marginal standard error.
smoke_equal(
  ce_ratio_delta_standard_error(fixture_reward, fixture_duration), .5,
  message = "CE7: ratio delta-method standard error is wrong"
)

# CE8/CE9 — candidates share one bank and stage budgets are literal path-ID
# prefixes, not matrix-column accidents or repeated simulations.
bank <- ce_build_common_path_bank(
  saw_simulator, centre = 0, final_paths = 7L, path_batch_size = 3L,
  seed = 900L, h_max = 12L, storage_mode = "memory",
  initial_horizon = 4L, stream_scope = "ce8"
)
ce_ensure_path_ids(bank, 2L, "coarse")
prefix_2 <- ce_path_bank_prefix(bank, 2L)
ce_ensure_path_ids(bank, 4L, "intermediate")
prefix_4 <- ce_path_bank_prefix(bank, 4L)
ce_ensure_path_ids(bank, 7L, "final")
prefix_7 <- ce_path_bank_prefix(bank, 7L)
smoke_expect(
  identical(prefix_2$paths, prefix_4$paths[, 1:2, drop = FALSE]) &&
    identical(prefix_4$paths, prefix_7$paths[, 1:4, drop = FALSE]) &&
    identical(prefix_2$path_ids, prefix_4$path_ids[1:2]) &&
    identical(prefix_4$path_ids, prefix_7$path_ids[1:4]),
  "CE9: complete-episode path budgets are not nested path-ID prefixes"
)
two_candidates <- rbind(
  candidate,
  transform(candidate, candidate_id = "episode_wide", d_plus = 1.5, d_minus = 1.5)
)
common_table <- ce_evaluate_common_path_bank(
  bank, 4L, two_candidates, 0, .1, .1, 4L,
  COMPLETE_EPISODE_MC_CONTRACT, "candidate_crn"
)
smoke_expect(length(unique(common_table$path_bank_fingerprint)) == 1L,
             "CE8: candidate comparison did not use one common path bank")

# CE12/CE13 — H_max is neither liquidation nor zero-reward censoring.
open_path <- c(0, 0, 0, 1.2, rep(1.1, 9))
open_episode <- ce_complete_episode_one(
  open_path, time_grid, candidate, 0, .1, .1, 4L, 12L, 2L, "open"
)
flat_episode <- ce_complete_episode_one(
  rep(0, length(time_grid)), time_grid, candidate, 0, .1, .1,
  4L, 12L, 2L, "flat"
)
smoke_expect(!open_episode$resolved && is.na(open_episode$reward) &&
               is.na(open_episode$duration),
             "CE12: open guardrail path received terminal liquidation")
smoke_expect(!flat_episode$resolved && is.na(flat_episode$reward) &&
               is.na(flat_episode$duration),
             "CE13: no-entry guardrail path received zero reward/censor duration")

# CE14 — one unresolved path invalidates the complete candidate bank.
mixed_paths <- cbind(episode_path, rep(0, length(time_grid)))
mixed_outcomes <- ce_candidate_outcomes(
  mixed_paths, time_grid, candidate, 0, .1, .1, 4L, 12L, 2L,
  c("resolved", "unresolved")
)
validated_candidate <- v2_validate_candidates(candidate)
mixed_summary <- ce_summarise_candidate(
  validated_candidate, mixed_outcomes, 2L, COMPLETE_EPISODE_MC_CONTRACT,
  4L, 12L, "mixed_bank"
)
smoke_expect(
  is.na(mixed_summary$objective_value) && mixed_summary$valid_paths == 0L &&
    mixed_summary$resolved_paths == 1L && mixed_summary$unresolved_paths == 1L &&
    identical(mixed_summary$optimizer_status, "guardrail_unresolved"),
  "CE14: objective was computed from a resolved subset"
)

# CE15 — no fully resolved search stage returns downstream model no-trade.
flat_simulator <- list(
  family = "flat_unresolved_fixture", simulator_hash = "flat_fixture_v2",
  simulate_paths = function(active_time, x0, n_paths, seed) list(
    paths = matrix(x0, nrow = length(active_time), ncol = n_paths),
    active_time = active_time, testing_data_used = FALSE
  )
)
guardrail_result <- calibrate_complete_episode_threshold_from_context(
  model = "guardrail_fixture", pair = "Y_X",
  endpoint_date = as.Date("2025-01-01"), simulator = flat_simulator,
  centre = 0, stationary_sd = 1, horizon = 4L, roundtrip_cost = .1,
  parameter_hash = "guardrail_parameter", parameter_source_hash = "guardrail_source",
  budgets = tiny_budgets
)
smoke_expect(
  identical(guardrail_result$selected$route_status, "MODEL_NO_TRADE") &&
    isFALSE(guardrail_result$selected$strategy_available) &&
    identical(guardrail_result$selected$reason_code,
              "NUMERICAL_GUARDRAIL_UNRESOLVED") &&
    is.na(guardrail_result$selected$upper_entry),
  "CE15: unresolved search fabricated a trading rule"
)

# CE16 — the zero outside option maps a non-positive final objective to the
# ordinary downstream no-trade state.
negative_table <- data.frame(
  candidate_id = "negative", d_plus = 1, d_minus = 1,
  c_plus = 0, c_minus = 0, objective_value = -.01,
  MC_standard_error = 0, stringsAsFactors = FALSE
)
negative_selected <- ce_select_complete_episode_threshold(
  negative_table, enforce_outside_option = TRUE, outside_option = 0,
  tolerance = production_config$threshold_mc$outside_option_tolerance
)
smoke_expect(
  identical(negative_selected$route_status, "MODEL_NO_TRADE") &&
    isFALSE(negative_selected$strategy_available),
  "CE16: non-positive complete-episode objective bypassed outside option"
)

# CE2 — an unchanged threshold task is accepted through do.call(), and the
# selected schema passes through existing strategy and route builders.
task <- list(
  model = "complete_episode_fixture", pair = "Y_X",
  endpoint_date = as.Date("2025-01-01"), simulator = saw_simulator,
  centre = 0, stationary_sd = 1, horizon = 4L, roundtrip_cost = .1,
  parameter_hash = "complete_parameter", parameter_source_hash = "complete_source"
)
alternative_result <- do.call(
  calibrate_complete_episode_threshold_from_context,
  c(task, list(budgets = tiny_budgets))
)
smoke_expect(
  !length(setdiff(names(finite_before$selected), names(alternative_result$selected))),
  "CE2: alternative selected output removed a finite-horizon field"
)
schedule <- data.frame(
  endpoint_id = "endpoint_1", endpoint_session_date = as.Date("2025-01-01"),
  pair_id = "Y_X", primary_rank = 1L, selected = TRUE,
  testing_session_dates = "2025-01-02|2025-01-03", testing_sessions = 2L,
  alpha = 0, beta = 1, formation_centre = 0,
  testing_start = "2025-01-02 00:00:00",
  testing_end = "2025-01-03 23:59:00", stringsAsFactors = FALSE
)
strategy <- build_strategy_specifications(
  alternative_result$selected, schedule, "Complete episode fixture", 200000
)
routes <- build_model_route_manifest(
  schedule, "Complete episode fixture", alternative_result$selected,
  pair_sleeve_usd = 200000
)
smoke_expect(nrow(strategy) == 1L && nrow(routes) == 1L &&
               routes$route_status == "TRADEABLE",
             "CE2: downstream strategy/route compatibility failed")

# CE18 — the actual Gaussian and full-family Gaussian-control simulators can
# be stored and replayed with stable path identities.
gaussian_parameters <- v2_gaussian_ou_parameters(
  mu = 0, kappa = .2, stationary_sd = 1
)
gaussian_bank <- ce_build_common_path_bank(
  v2_gaussian_simulator(gaussian_parameters), 0, 4L, 2L, 123L, 6L,
  "memory", initial_horizon = 3L, stream_scope = "ce18_gaussian"
)
ce_ensure_path_ids(gaussian_bank, 2L, "small")
gaussian_small <- ce_path_bank_prefix(gaussian_bank, 2L)
ce_ensure_path_ids(gaussian_bank, 4L, "large")
gaussian_large <- ce_path_bank_prefix(gaussian_bank, 4L)
smoke_expect(
  identical(gaussian_small$paths, gaussian_large$paths[, 1:2, drop = FALSE]),
  "CE18: Gaussian stored path-bank prefix is not identical"
)

source(repo_path("R", "alternatives", "full_family_ou_gh",
                 "repository_adapter.R"), local = TRUE)
full_environment <- full_family_gh_environment()
full_fit <- full_environment$ou_gh_family_gaussian_fit(
  mu = 0, kappa = .2, sigma_eta_1 = .4
)
full_simulator <- list(
  family = "full_family_OU_GH_Gaussian_limit",
  simulator_hash = full_environment$ou_gh_hash_object(full_fit),
  simulate_paths = function(active_time, x0, n_paths, seed) list(
    paths = full_environment$ou_gh_family_path_matrix(
      full_fit, active_time, x0, n_paths, seed, table = NULL
    ),
    active_time = active_time, testing_data_used = FALSE
  )
)
full_bank <- ce_build_common_path_bank(
  full_simulator, 0, 4L, 2L, 321L, 6L, "memory",
  initial_horizon = 3L, stream_scope = "ce18_full"
)
ce_ensure_path_ids(full_bank, 2L, "small")
full_small <- ce_path_bank_prefix(full_bank, 2L)
ce_ensure_path_ids(full_bank, 4L, "large")
full_large <- ce_path_bank_prefix(full_bank, 4L)
smoke_expect(
  identical(full_small$paths, full_large$paths[, 1:2, drop = FALSE]),
  "CE18: full-family OU-GH stored path-bank prefix is not identical"
)

# Fixtures for CE19–CE32 use the exact adaptive bank machinery with tiny path
# budgets and deterministic trajectories.
delayed_values <- c(0, .2, .4, .6, 1.4, 1.2, .8, -.2, 0, 0, 0, 0, 0)
delayed_simulator <- list(
  family = "delayed_episode_fixture", simulator_hash = "delayed_fixture_v1",
  simulate_paths = function(active_time, x0, n_paths, seed) {
    values <- delayed_values[as.integer(active_time) + 1L] + x0
    list(paths = matrix(rep(values, n_paths), nrow = length(values)),
         active_time = active_time, testing_data_used = FALSE)
  }
)

# CE19 — path identities are appended 2 -> 3 -> 5 only when each stage asks.
lazy_bank <- ce_build_common_path_bank(
  saw_simulator, 0, 5L, 2L, 730L, 8L, "memory",
  initial_horizon = 2L, stream_scope = "ce19"
)
smoke_equal(ce_path_bank_audit(lazy_bank)$created_path_count, 0L,
            message = "CE19: bank created paths before coarse evaluation")
ce_evaluate_common_path_bank(
  lazy_bank, 2L, candidate, 0, .1, .1, 2L,
  COMPLETE_EPISODE_MC_CONTRACT, "coarse"
)
smoke_equal(ce_path_bank_audit(lazy_bank)$created_path_count, 2L,
            message = "CE19: coarse stage did not create exactly two paths")
ce_evaluate_common_path_bank(
  lazy_bank, 3L, candidate, 0, .1, .1, 2L,
  COMPLETE_EPISODE_MC_CONTRACT, "intermediate"
)
smoke_equal(ce_path_bank_audit(lazy_bank)$created_path_count, 3L,
            message = "CE19: intermediate stage did not append path 3")
ce_evaluate_common_path_bank(
  lazy_bank, 5L, candidate, 0, .1, .1, 2L,
  COMPLETE_EPISODE_MC_CONTRACT, "final"
)
lazy_audit <- ce_path_bank_audit(lazy_bank)
smoke_expect(
  identical(lazy_audit$manifest$path_number, 1:5) &&
    all(lazy_audit$events$stage[lazy_audit$events$action == "create"] ==
          c("coarse", "coarse", "intermediate", "final", "final")),
  "CE19: lazy stage creation sequence is wrong"
)

# CE20 — earlier path bytes and seeds survive later path-budget expansion.
prefix_identity_bank <- ce_build_common_path_bank(
  saw_simulator, 0, 5L, 2L, 740L, 8L, "memory",
  initial_horizon = 2L, stream_scope = "ce20"
)
ce_ensure_path_ids(prefix_identity_bank, 2L, "coarse")
ce20_before <- lapply(
  ce_path_id(1:2), function(id) ce_read_path(prefix_identity_bank, id)
)
ce_ensure_path_ids(prefix_identity_bank, 3L, "intermediate")
ce_ensure_path_ids(prefix_identity_bank, 5L, "final")
ce20_after <- lapply(
  ce_path_id(1:2), function(id) ce_read_path(prefix_identity_bank, id)
)
smoke_expect(identical(ce20_before, ce20_after),
             "CE20: path-budget expansion changed an existing path")

# CE21 — a new path begins at the initial numerical horizon, not H_max.
initial_bank <- ce_build_common_path_bank(
  delayed_simulator, 0, 1L, 1L, 731L, 12L, "memory",
  initial_horizon = 2L, stream_scope = "ce21"
)
ce_ensure_path_ids(initial_bank, 1L, "coarse")
initial_record <- ce_read_path(initial_bank, ce_path_id(1L))
smoke_expect(initial_record$current_horizon == 2L && length(initial_record$path) == 3L,
             "CE21: new path was generated beyond the initial horizon")

# CE22 — unresolved same-path prefixes are extended until the delayed exit.
delayed_table <- ce_evaluate_common_path_bank(
  initial_bank, 1L, candidate, 0, .1, .1, 2L,
  COMPLETE_EPISODE_MC_CONTRACT, "coarse"
)
delayed_record <- ce_read_path(initial_bank, ce_path_id(1L))
smoke_expect(
  is.finite(delayed_table$objective_value) && delayed_record$current_horizon == 8L &&
    identical(delayed_record$path[1:3], delayed_values[1:3]),
  "CE22: delayed episode did not extend its preserved path to completion"
)

# CE23 — a path resolving within the starting horizon is not extended.
early_bank <- ce_build_common_path_bank(
  saw_simulator, 0, 1L, 1L, 732L, 12L, "memory",
  initial_horizon = 2L, stream_scope = "ce23"
)
ce_evaluate_common_path_bank(
  early_bank, 1L, candidate, 0, .1, .1, 2L,
  COMPLETE_EPISODE_MC_CONTRACT, "coarse"
)
smoke_expect(
  ce_read_path(early_bank, ce_path_id(1L))$current_horizon == 2L &&
    !any(ce_path_bank_audit(early_bank)$events$action == "extend"),
  "CE23: completed path was unnecessarily extended"
)

# CE24 — an early candidate remains fixed while a late candidate extends the
# single shared underlying path.
early_late_simulator <- list(
  family = "candidate_extension_fixture", simulator_hash = "candidate_extension_v1",
  simulate_paths = function(active_time, x0, n_paths, seed) {
    values <- c(0, 1.5, -.1, .2, 3.5, 3.2, 1, -.2, rep(0, 5))
    values <- values[as.integer(active_time) + 1L] + x0
    list(paths = matrix(rep(values, n_paths), nrow = length(values)),
         active_time = active_time, testing_data_used = FALSE)
  }
)
late_candidate <- transform(
  candidate, candidate_id = "late", d_plus = 3, d_minus = 3
)
early_late_bank <- ce_build_common_path_bank(
  early_late_simulator, 0, 1L, 1L, 733L, 12L, "memory",
  initial_horizon = 2L, stream_scope = "ce24"
)
early_late_outcomes <- ce_evaluate_one_adaptive_path(
  early_late_bank,
  ce_ensure_path_ids(early_late_bank, 1L, "coarse")[[1L]],
  rbind(candidate, late_candidate), 0, .1, .1,
  COMPLETE_EPISODE_MC_CONTRACT, "coarse"
)
smoke_expect(
  early_late_outcomes[[1L]]$exit_time == 2 &&
    early_late_outcomes[[1L]]$extension_count == 0L &&
    early_late_outcomes[[2L]]$exit_time == 7 &&
    early_late_outcomes[[2L]]$extension_count >= 1L,
  "CE24: candidate-specific completion was not preserved on one common path"
)

# CE25 — a candidate introduced later extends the existing path identity.
stage_bank <- ce_build_common_path_bank(
  early_late_simulator, 0, 1L, 1L, 734L, 12L, "memory",
  initial_horizon = 2L, stream_scope = "ce25"
)
ce_evaluate_common_path_bank(
  stage_bank, 1L, candidate, 0, .1, .1, 2L,
  COMPLETE_EPISODE_MC_CONTRACT, "coarse"
)
coarse_record <- ce_read_path(stage_bank, ce_path_id(1L))
ce_evaluate_common_path_bank(
  stage_bank, 1L, late_candidate, 0, .1, .1, 2L,
  COMPLETE_EPISODE_MC_CONTRACT, "intermediate"
)
stage_record <- ce_read_path(stage_bank, ce_path_id(1L))
smoke_expect(
  identical(coarse_record$path, stage_record$path[seq_along(coarse_record$path)]) &&
    coarse_record$path_seed == stage_record$path_seed &&
    stage_record$current_horizon > coarse_record$current_horizon,
  "CE25: new-stage candidate replaced rather than extended an old path"
)

# CE26 — adaptive and full pre-generated evaluation return identical episode
# identity, side, times, values, reward and duration.
reference_episode <- ce_complete_episode_one(
  delayed_values, 0:12, candidate, 0, .1, .1, 2L, 12L, 2L, "reference"
)
adaptive_episode <- ce_episode_from_prefix(
  delayed_record$path, delayed_record$active_time, candidate,
  0, .1, .1, "reference", at_guardrail = FALSE,
  extension_count = delayed_record$extension_count
)
ce26_fields <- c("side", "entry_index", "exit_index", "entry_time", "exit_time",
                 "realised_entry", "realised_exit", "reward", "duration")
smoke_expect(
  identical(reference_episode[ce26_fields], adaptive_episode[ce26_fields]),
  "CE26: pre-generated/adaptive episode outcomes differ"
)

# CE27 — Gaussian one-path replay preserves the exact prior time prefix.
gaussian_replay <- ce_build_common_path_bank(
  v2_gaussian_simulator(gaussian_parameters), 0, 1L, 1L, 735L, 8L,
  "memory", initial_horizon = 3L, stream_scope = "ce27"
)
ce_ensure_path_ids(gaussian_replay, 1L, "coarse")
gaussian_prior <- ce_read_path(gaussian_replay, ce_path_id(1L))
ce_extend_path(gaussian_replay, ce_path_id(1L), 8L, "test")
gaussian_extended <- ce_read_path(gaussian_replay, ce_path_id(1L))
smoke_expect(
  identical(gaussian_prior$path,
            gaussian_extended$path[seq_along(gaussian_prior$path)]),
  "CE27: Gaussian time-prefix replay is unsafe"
)

# CE28 — strict-interior GH prebuilt-table simulator replay preserves its time
# prefix. A tiny deterministic lookup table avoids any expensive table build.
source(repo_path("R", "ou_gh_strict_interior", "project_utils.R"), local = TRUE)
OU_GH_SIMULATOR_VERSION <- "strict_gh_prefix_smoke_v1"
sys.source(
  repo_path("R", "ou_gh_strict_interior", "ou_gh_fgmc_production.R"),
  envir = environment()
)
lookup_logit <- seq(-8, 8, length.out = 1025L)
strict_table <- list(
  Delta = 1, rho = exp(-.2), draw_location = 0, draw_scale = .2,
  probability_min = stats::plogis(lookup_logit[[1L]]),
  probability_max = stats::plogis(tail(lookup_logit, 1L)),
  quantile_min = stats::qnorm(stats::plogis(lookup_logit[[1L]])),
  quantile_max = stats::qnorm(stats::plogis(tail(lookup_logit, 1L))),
  left_tail_rate = 2, right_tail_rate = 2,
  logit_min = lookup_logit[[1L]],
  logit_step = lookup_logit[[2L]] - lookup_logit[[1L]],
  quantile_lookup = stats::qnorm(stats::plogis(lookup_logit))
)
strict_fit <- c(mu = 0, kappa = .2)
strict_simulator <- list(
  family = "strict_interior_OU_GH_prefix_smoke",
  simulator_hash = "strict_interior_OU_GH_prefix_smoke_v1",
  simulate_paths = function(active_time, x0, n_paths, seed) {
    ou_gh_simulate_prebuilt_table(
      active_time, x0, n_paths, seed, strict_fit, strict_table
    )
  }
)
strict_bank <- ce_build_common_path_bank(
  strict_simulator, 0, 1L, 1L, 736L, 8L, "memory",
  initial_horizon = 3L, stream_scope = "ce28"
)
ce_ensure_path_ids(strict_bank, 1L, "coarse")
strict_prior <- ce_read_path(strict_bank, ce_path_id(1L))
ce_extend_path(strict_bank, ce_path_id(1L), 8L, "test")
strict_extended <- ce_read_path(strict_bank, ce_path_id(1L))
smoke_expect(
  identical(strict_prior$path,
            strict_extended$path[seq_along(strict_prior$path)]),
  "CE28: strict-interior GH time-prefix replay is unsafe"
)

# CE29 — a non-Gaussian full-family VG-boundary simulator replay preserves its
# exact prior time prefix.
full_vg_fit <- full_environment$ou_gh_family_vg_shape_scale_to_fit(
  mu = 0, kappa = .2, sigma_eta_1 = .4, lambda = 1.2, rho = .15
)
full_vg_simulator <- list(
  family = "full_family_OU_GH_VG_boundary",
  simulator_hash = full_environment$ou_gh_hash_object(full_vg_fit),
  simulate_paths = function(active_time, x0, n_paths, seed) list(
    paths = full_environment$ou_gh_family_path_matrix(
      full_vg_fit, active_time, x0, n_paths, seed, table = NULL
    ),
    active_time = active_time, testing_data_used = FALSE
  )
)
full_replay <- ce_build_common_path_bank(
  full_vg_simulator, 0, 1L, 1L, 737L, 8L, "memory",
  initial_horizon = 3L, stream_scope = "ce29"
)
ce_ensure_path_ids(full_replay, 1L, "coarse")
full_prior <- ce_read_path(full_replay, ce_path_id(1L))
ce_extend_path(full_replay, ce_path_id(1L), 8L, "test")
full_extended <- ce_read_path(full_replay, ce_path_id(1L))
smoke_expect(
  identical(full_prior$path, full_extended$path[seq_along(full_prior$path)]),
  "CE29: full-family GH time-prefix replay is unsafe"
)

# CE30 — instrumentation proves no final_paths by H_max object is allocated.
allocation_audit <- ce_path_bank_audit(lazy_bank)
smoke_expect(
  allocation_audit$maximum_simulation_cells <= lazy_bank$h_max + 1L &&
    allocation_audit$maximum_simulation_cells <
      allocation_audit$theoretical_full_guardrail_cells,
  "CE30: adaptive bank allocated the full path-by-guardrail object"
)

# CE31 — adaptive guardrail preserves no-liquidation, no-zero, whole-bank
# resolution semantics.
adaptive_guardrail <- ce_build_common_path_bank(
  flat_simulator, 0, 2L, 1L, 738L, 6L, "memory",
  initial_horizon = 2L, stream_scope = "ce31"
)
guardrail_table <- ce_evaluate_common_path_bank(
  adaptive_guardrail, 2L, candidate, 0, .1, .1, 2L,
  COMPLETE_EPISODE_MC_CONTRACT, "coarse"
)
smoke_expect(
  is.na(guardrail_table$objective_value) && guardrail_table$valid_paths == 0L &&
    guardrail_table$resolved_paths == 0L && guardrail_table$unresolved_paths == 2L,
  "CE31: adaptive guardrail changed unresolved whole-bank semantics"
)

# CE32 — staged adaptive objective equals the objective from identical complete
# pre-generated paths.
parity_bank <- ce_build_common_path_bank(
  delayed_simulator, 0, 2L, 1L, 739L, 12L, "memory",
  initial_horizon = 2L, stream_scope = "ce32"
)
adaptive_summary <- ce_evaluate_common_path_bank(
  parity_bank, 2L, candidate, 0, .1, .1, 2L,
  COMPLETE_EPISODE_MC_CONTRACT, "coarse"
)
reference_paths <- matrix(rep(delayed_values, 2L), nrow = length(delayed_values))
reference_outcomes <- ce_candidate_outcomes(
  reference_paths, 0:12, candidate, 0, .1, .1, 2L, 12L, 2L,
  ce_path_id(1:2)
)
reference_summary <- ce_summarise_candidate(
  v2_validate_candidates(candidate), reference_outcomes, 2L,
  COMPLETE_EPISODE_MC_CONTRACT, 2L, 12L, "reference"
)
smoke_equal(
  adaptive_summary$objective_value, reference_summary$objective_value,
  tolerance = 1e-14, message = "CE32: staged adaptive objective lacks parity"
)
smoke_equal(
  adaptive_summary$MC_standard_error, reference_summary$MC_standard_error,
  tolerance = 1e-14, message = "CE32: staged adaptive standard error lacks parity"
)

# CE33 — construct seed identities only and prove all 10,000 are unique.
seed_scope <- list(
  model = "seed_fixture", pair = "Y_X", endpoint_date = "2025-01-01",
  parameter_hash = "seed_parameter", simulator_hash = "seed_simulator"
)
seed_allocator_10000 <- ce_allocate_path_seeds(
  master_seed = 91001L, stream_scope = seed_scope,
  path_stream_version = COMPLETE_EPISODE_MC_CONTRACT$numerical$path_stream_version,
  final_paths = 10000L
)
smoke_expect(
  length(seed_allocator_10000$seeds) == 10000L &&
    length(unique(seed_allocator_10000$seeds)) == 10000L &&
    all(seed_allocator_10000$seeds > 0L),
  "CE33: 10,000 path identities did not receive unique valid seeds"
)

# CE34 — identical scope and version reproduce the entire mapping.
seed_allocator_repeat <- ce_allocate_path_seeds(
  91001L, seed_scope,
  COMPLETE_EPISODE_MC_CONTRACT$numerical$path_stream_version, 10000L
)
smoke_expect(
  identical(seed_allocator_10000$seeds, seed_allocator_repeat$seeds) &&
    identical(seed_allocator_10000$scope_hash, seed_allocator_repeat$scope_hash),
  "CE34: complete-episode seed allocation is not deterministic"
)

# CE35 — requested bank sizes are literal prefixes of one seed mapping.
seed_allocator_250 <- ce_allocate_path_seeds(
  91001L, seed_scope,
  COMPLETE_EPISODE_MC_CONTRACT$numerical$path_stream_version, 250L
)
seed_allocator_750 <- ce_allocate_path_seeds(
  91001L, seed_scope,
  COMPLETE_EPISODE_MC_CONTRACT$numerical$path_stream_version, 750L
)
smoke_expect(
  identical(unname(seed_allocator_250$seeds),
            unname(seed_allocator_750$seeds[1:250])) &&
    identical(unname(seed_allocator_750$seeds),
              unname(seed_allocator_10000$seeds[1:750])),
  "CE35: seed mapping is not stable across 250/750/10,000 prefixes"
)

# CE36 — candidate thresholds cannot enter or perturb path seed identity.
seed_independence_bank <- ce_build_common_path_bank(
  saw_simulator, 0, 1L, 1L, 741L, 8L, "memory",
  initial_horizon = 2L, stream_scope = seed_scope
)
seed_before_candidates <- ce_path_seed(
  seed_independence_bank$seed_allocator, ce_path_id(1L)
)
ce_evaluate_common_path_bank(
  seed_independence_bank, 1L, candidate, 0, .1, .1, 2L,
  COMPLETE_EPISODE_MC_CONTRACT, "candidate_a"
)
ce_evaluate_common_path_bank(
  seed_independence_bank, 1L,
  transform(candidate, candidate_id = "candidate_b", d_plus = 1.75,
            d_minus = 1.75),
  0, .1, .1, 2L, COMPLETE_EPISODE_MC_CONTRACT, "candidate_b"
)
seed_after_candidates <- ce_path_seed(
  seed_independence_bank$seed_allocator, ce_path_id(1L)
)
smoke_expect(
  identical(seed_before_candidates, seed_after_candidates) &&
    identical(seed_before_candidates,
              ce_read_path(seed_independence_bank, ce_path_id(1L))$path_seed),
  "CE36: candidate identity changed a complete-episode path seed"
)

# CE37 — calibration scope and configured stream version separate families.
different_pair_allocator <- ce_allocate_path_seeds(
  91001L, within(seed_scope, pair <- "Z_X"),
  COMPLETE_EPISODE_MC_CONTRACT$numerical$path_stream_version, 10000L
)
different_version_allocator <- ce_allocate_path_seeds(
  91001L, seed_scope,
  paste0(COMPLETE_EPISODE_MC_CONTRACT$numerical$path_stream_version,
         "_intentional_change"),
  10000L
)
smoke_expect(
  !identical(seed_allocator_10000$seeds, different_pair_allocator$seeds) &&
    !identical(seed_allocator_10000$seeds, different_version_allocator$seeds),
  "CE37: context or path_stream_version failed to separate seed families"
)

# CE38 — retain the explicit finite-horizon parity result after seed hardening.
smoke_expect(
  identical(finite_before, finite_after),
  "CE38: optional seed allocation changed finite-horizon results"
)

cat("COMPLETE_EPISODE_THRESHOLD_MC_PASS CE1-CE38\n")
