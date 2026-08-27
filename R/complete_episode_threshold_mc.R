# Complete flat-entry-exit Monte Carlo threshold calibration used by the
# current strict-interior OU--GH thesis route. The numerical guardrail is not
# an economic terminal horizon.

ce_complete_episode_policy <- function() {
  v2_assert(exists("COMPLETE_EPISODE_MC_CONTRACT", inherits = TRUE),
            "Source config/complete_episode_threshold_contract.R first.")
  get("COMPLETE_EPISODE_MC_CONTRACT", inherits = TRUE)
}

ce_validate_budgets <- function(budgets) {
  required <- c("coarse_paths", "intermediate_paths", "final_paths",
                "path_batch_size", "seed")
  missing <- setdiff(required, names(budgets))
  v2_assert(!length(missing), paste(
    "Complete-episode budgets lack:", paste(missing, collapse = ", ")
  ))
  values <- vapply(budgets[required[1:4]], as.integer, integer(1L))
  v2_assert(all(is.finite(values)) && all(values > 0L),
            "Complete-episode path budgets must be positive integers.")
  v2_assert(values[["coarse_paths"]] <= values[["intermediate_paths"]] &&
              values[["intermediate_paths"]] <= values[["final_paths"]],
            "Complete-episode path budgets must be nested.")
  v2_assert(length(budgets$seed) == 1L && is.finite(budgets$seed),
            "Complete-episode master seed is invalid.")
  invisible(TRUE)
}

ce_extension_schedule <- function(initial_horizon, h_max, multiplier = 2L) {
  initial_horizon <- as.integer(initial_horizon)
  h_max <- as.integer(h_max)
  multiplier <- as.numeric(multiplier)
  v2_assert(initial_horizon >= 1L && h_max >= initial_horizon &&
              is.finite(multiplier) && multiplier > 1,
            "Invalid complete-episode numerical extension schedule.")
  limits <- initial_horizon
  while (tail(limits, 1L) < h_max) {
    previous <- tail(limits, 1L)
    next_limit <- min(
      h_max,
      max(previous + 1L, as.integer(ceiling(previous * multiplier)))
    )
    limits <- c(limits, next_limit)
  }
  unique(as.integer(limits))
}

ce_next_horizon <- function(current_horizon, h_max, multiplier = 2L) {
  current_horizon <- as.integer(current_horizon)
  h_max <- as.integer(h_max)
  if (current_horizon >= h_max) return(h_max)
  min(h_max, max(current_horizon + 1L,
                 as.integer(ceiling(current_horizon * multiplier))))
}

ce_episode_from_prefix <- function(path, time_grid, candidate, centre,
                                   cost_plus, cost_minus,
                                   path_id = NA_character_,
                                   at_guardrail = FALSE,
                                   extension_count = 0L) {
  path <- as.numeric(path)
  time_grid <- as.numeric(time_grid)
  v2_assert(length(path) == length(time_grid) && length(path) >= 2L &&
              all(is.finite(path)) && all(is.finite(time_grid)) &&
              all(diff(time_grid) > 0) && time_grid[[1L]] == 0,
            "Complete-episode path/time prefix is invalid.")
  x <- path - centre
  state <- "flat"
  entry_index <- exit_index <- NA_integer_
  entry_value <- exit_value <- NA_real_
  for (i in seq_along(x)) {
    if (identical(state, "flat")) {
      if (x[[i]] >= candidate$d_plus[[1L]]) {
        state <- "short"; entry_index <- i; entry_value <- x[[i]]
      } else if (x[[i]] <= -candidate$d_minus[[1L]]) {
        state <- "long"; entry_index <- i; entry_value <- x[[i]]
      }
    } else if (i > entry_index) {
      ordinary_exit <- (identical(state, "short") &&
                          x[[i]] <= candidate$c_plus[[1L]]) ||
        (identical(state, "long") &&
           x[[i]] >= -candidate$c_minus[[1L]])
      if (ordinary_exit) {
        exit_index <- i; exit_value <- x[[i]]
        break
      }
    }
  }
  if (is.na(exit_index)) {
    prefix <- if (isTRUE(at_guardrail)) "guardrail" else "prefix"
    return(data.frame(
      path_id = as.character(path_id), resolved = FALSE,
      resolution_status = paste0(
        prefix, "_unresolved_",
        if (is.na(entry_index)) "no_entry" else "open_position"
      ),
      side = if (is.na(entry_index)) NA_character_ else state,
      entry_index = entry_index, exit_index = NA_integer_,
      entry_time = if (is.na(entry_index)) NA_real_ else time_grid[[entry_index]],
      exit_time = NA_real_, wait_duration = NA_real_, hold_duration = NA_real_,
      duration = NA_real_, realised_entry = entry_value,
      realised_exit = NA_real_, reward = NA_real_,
      extension_count = as.integer(extension_count), stringsAsFactors = FALSE
    ))
  }
  gross <- if (identical(state, "short")) {
    entry_value - exit_value
  } else exit_value - entry_value
  side_cost <- if (identical(state, "short")) cost_plus else cost_minus
  entry_time <- time_grid[[entry_index]]
  exit_time <- time_grid[[exit_index]]
  data.frame(
    path_id = as.character(path_id), resolved = TRUE,
    resolution_status = "complete_episode_resolved", side = state,
    entry_index = entry_index, exit_index = exit_index,
    entry_time = entry_time, exit_time = exit_time,
    wait_duration = entry_time - time_grid[[1L]],
    hold_duration = exit_time - entry_time,
    duration = exit_time - time_grid[[1L]],
    realised_entry = entry_value, realised_exit = exit_value,
    reward = gross - side_cost,
    extension_count = as.integer(extension_count), stringsAsFactors = FALSE
  )
}

# Full-path reference evaluator retained for deterministic parity checks. The
# adaptive bank below never requires a full path to H_max up front.
ce_complete_episode_one <- function(path, time_grid, candidate, centre,
                                    cost_plus, cost_minus,
                                    initial_horizon, h_max,
                                    extension_multiplier = 2L,
                                    path_id = NA_character_) {
  path <- as.numeric(path)
  time_grid <- as.numeric(time_grid)
  v2_assert(length(path) == length(time_grid) && length(path) >= 2L &&
              all(is.finite(path)) && all(is.finite(time_grid)) &&
              all(diff(time_grid) > 0),
            "Complete-episode path/time input is invalid.")
  v2_assert(time_grid[[1L]] == 0 && tail(time_grid, 1L) >= h_max,
            "Complete-episode reference path does not reach H_max.")
  schedule <- ce_extension_schedule(
    initial_horizon, h_max, extension_multiplier
  )
  for (index in seq_along(schedule)) {
    limit <- schedule[[index]]
    use <- time_grid <= limit
    outcome <- ce_episode_from_prefix(
      path[use], time_grid[use], candidate, centre, cost_plus, cost_minus,
      path_id, at_guardrail = identical(limit, h_max),
      extension_count = index - 1L
    )
    if (outcome$resolved[[1L]]) return(outcome)
  }
  outcome
}

ce_ratio_of_expectations <- function(reward, duration) {
  reward <- as.numeric(reward); duration <- as.numeric(duration)
  v2_assert(length(reward) == length(duration) && length(reward) >= 1L &&
              all(is.finite(reward)) && all(is.finite(duration)) &&
              all(duration > 0),
            "Complete-episode reward/duration sample is invalid.")
  mean(reward) / mean(duration)
}

ce_ratio_delta_standard_error <- function(reward, duration) {
  reward <- as.numeric(reward); duration <- as.numeric(duration)
  objective <- ce_ratio_of_expectations(reward, duration)
  if (length(reward) < 2L) return(NA_real_)
  residual <- reward - objective * duration
  s_r <- sqrt(sum(residual^2) / (length(residual) - 1L))
  s_r / (mean(duration) * sqrt(length(duration)))
}

ce_candidate_outcomes <- function(paths, time_grid, candidate, centre,
                                  cost_plus, cost_minus, initial_horizon,
                                  h_max, extension_multiplier, path_ids) {
  paths <- as.matrix(paths)
  v2_assert(ncol(paths) == length(path_ids),
            "Complete-episode path identities do not match the path matrix.")
  v2_bind_rows(lapply(seq_len(ncol(paths)), function(j) {
    ce_complete_episode_one(
      paths[, j], time_grid, candidate, centre, cost_plus, cost_minus,
      initial_horizon, h_max, extension_multiplier, path_ids[[j]]
    )
  }))
}

ce_summarise_candidate <- function(candidate, outcomes, required_paths,
                                   policy, initial_horizon, h_max,
                                   path_bank_fingerprint) {
  candidate <- candidate[1L, , drop = FALSE]
  valid <- isTRUE(candidate$valid[[1L]])
  resolved_paths <- if (nrow(outcomes)) sum(outcomes$resolved %in% TRUE) else 0L
  fully_resolved <- valid && nrow(outcomes) == required_paths &&
    resolved_paths == required_paths
  if (fully_resolved) {
    reward <- outcomes$reward
    duration <- outcomes$duration
    objective <- ce_ratio_of_expectations(reward, duration)
    se <- ce_ratio_delta_standard_error(reward, duration)
    status <- "evaluated"
    guardrail_status <- "fully_resolved"
  } else {
    reward <- duration <- numeric()
    objective <- se <- NA_real_
    status <- if (valid) policy$numerical$guardrail_status else "invalid_candidate"
    guardrail_status <- status
  }
  data.frame(
    candidate_id = as.character(candidate$candidate_id),
    d_plus = as.numeric(candidate$d_plus), d_minus = as.numeric(candidate$d_minus),
    c_plus = as.numeric(candidate$c_plus), c_minus = as.numeric(candidate$c_minus),
    terminal_policy = policy$calibration$terminal_policy,
    objective_version = policy$calibration$objective_version,
    entry_probability = if (fully_resolved) 1 else NA_real_,
    ordinary_exit_probability = if (fully_resolved) 1 else NA_real_,
    forced_terminal_close_probability = if (fully_resolved) 0 else NA_real_,
    no_entry_probability = if (fully_resolved) 0 else NA_real_,
    mean_ordinary_reward = if (fully_resolved) mean(reward) else NA_real_,
    mean_forced_close_reward = NA_real_,
    forced_close_reward_q01 = NA_real_, forced_close_reward_q05 = NA_real_,
    forced_close_reward_q50 = NA_real_, forced_close_reward_q95 = NA_real_,
    forced_close_reward_q99 = NA_real_,
    mean_total_reward = if (fully_resolved) mean(reward) else NA_real_,
    mean_duration = if (fully_resolved) mean(duration) else NA_real_,
    objective_value = objective, MC_standard_error = se,
    valid_paths = if (fully_resolved) required_paths else 0L,
    mean_cycles = if (fully_resolved) 1 else NA_real_,
    optimizer_status = status, guardrail_status = guardrail_status,
    resolved_paths = resolved_paths,
    unresolved_paths = required_paths - resolved_paths,
    initial_numerical_horizon = as.integer(initial_horizon),
    numerical_guardrail_h_max = as.integer(h_max),
    calibration_policy_version = policy$calibration$policy_version,
    path_bank_fingerprint = as.character(path_bank_fingerprint),
    mean_wait_duration = if (fully_resolved) mean(outcomes$wait_duration) else NA_real_,
    mean_hold_duration = if (fully_resolved) mean(outcomes$hold_duration) else NA_real_,
    mean_realised_entry = if (fully_resolved) mean(outcomes$realised_entry) else NA_real_,
    mean_realised_exit = if (fully_resolved) mean(outcomes$realised_exit) else NA_real_,
    max_extension_count = if (nrow(outcomes)) max(outcomes$extension_count) else NA_integer_,
    stringsAsFactors = FALSE
  )
}

ce_path_id <- function(path_number) sprintf("path_%07d", as.integer(path_number))

ce_hex_mod <- function(hexadecimal, modulus) {
  hexadecimal <- tolower(as.character(hexadecimal))
  digits <- strsplit(hexadecimal, "", fixed = TRUE)[[1L]]
  alphabet <- c(as.character(0:9), letters[1:6])
  values <- match(digits, alphabet) - 1L
  v2_assert(length(values) && all(is.finite(values)),
            "Invalid hexadecimal seed-allocation hash.")
  remainder <- 0
  for (value in values) remainder <- (remainder * 16 + value) %% modulus
  remainder
}

ce_allocate_path_seeds <- function(master_seed, stream_scope,
                                   path_stream_version, final_paths,
                                   modulus = .Machine$integer.max) {
  final_paths <- as.integer(final_paths)
  modulus <- as.double(modulus)
  v2_assert(final_paths >= 1L && final_paths < modulus,
            "Complete-episode path count exceeds the injective seed domain.")
  v2_assert(length(path_stream_version) == 1L &&
              is.character(path_stream_version) && nzchar(path_stream_version),
            "Configured complete-episode path_stream_version is invalid.")
  scope_hash <- v2_hash_object(list(
    master_seed = as.integer(master_seed), stream_scope = stream_scope,
    path_stream_version = path_stream_version,
    allocator = "injective_affine_permutation_prime_seed_domain_v1"
  ))
  # 2^31-1 is prime. Any a in 1:(m-1) is therefore coprime to m, so
  # s_i=(a*i+b) mod m is injective for distinct path IDs modulo m.
  multiplier <- ce_hex_mod(substr(scope_hash, 1L, 32L), modulus - 1) + 1
  offset <- ce_hex_mod(substr(scope_hash, 33L, 64L), modulus)
  seeds <- integer(final_paths)
  value <- offset
  for (index in seq_len(final_paths)) {
    value <- (value + multiplier) %% modulus
    seeds[[index]] <- as.integer(if (value == 0) modulus else value)
  }
  v2_assert(length(unique(seeds)) == final_paths && all(seeds > 0L),
            "Complete-episode seed allocator failed its injectivity contract.")
  names(seeds) <- ce_path_id(seq_len(final_paths))
  list(
    seeds = seeds, modulus = as.integer(modulus),
    multiplier = as.integer(multiplier), offset = as.integer(offset),
    scope_hash = scope_hash,
    path_stream_version = path_stream_version,
    allocator_version = "injective_affine_permutation_prime_seed_domain_v1"
  )
}

ce_path_seed <- function(seed_allocator, path_id) {
  path_id <- as.character(path_id)
  v2_assert(length(path_id) == 1L && path_id %in% names(seed_allocator$seeds),
            "Path ID is outside the configured complete-episode seed bank.")
  unname(seed_allocator$seeds[[path_id]])
}

ce_bank_event <- function(bank, action, path_id, from_horizon, to_horizon,
                          stage = NA_character_) {
  bank$state$events[[length(bank$state$events) + 1L]] <- data.frame(
    sequence = length(bank$state$events) + 1L,
    action = as.character(action), path_id = as.character(path_id),
    from_horizon = as.integer(from_horizon), to_horizon = as.integer(to_horizon),
    stage = as.character(stage), stringsAsFactors = FALSE
  )
  invisible(TRUE)
}

ce_record_path <- function(bank, record) {
  if (identical(bank$storage_mode, "temporary_rds")) {
    path <- file.path(bank$storage_dir, paste0(record$path_id, ".rds"))
    saveRDS(record, path, version = 3, compress = FALSE)
    descriptor <- list(storage = "rds", path = path)
  } else descriptor <- list(storage = "memory", value = record)
  assign(record$path_id, descriptor, envir = bank$state$records)
  invisible(record)
}

ce_read_path <- function(bank, path_id) {
  descriptor <- get(path_id, envir = bank$state$records, inherits = FALSE)
  if (identical(descriptor$storage, "rds")) readRDS(descriptor$path) else descriptor$value
}

ce_simulate_one_path <- function(bank, path_id, horizon) {
  path_seed <- ce_path_seed(bank$seed_allocator, path_id)
  active_time <- 0:as.integer(horizon)
  simulation <- v2_simulate_paths(
    bank$simulator, active_time, bank$centre, 1L, path_seed
  )
  v2_assert(nrow(simulation$paths) == length(active_time) &&
              ncol(simulation$paths) == 1L,
            "Simulator returned an incorrectly dimensioned adaptive path.")
  bank$state$simulation_calls <- bank$state$simulation_calls + 1L
  bank$state$maximum_simulation_cells <- max(
    bank$state$maximum_simulation_cells, length(simulation$paths)
  )
  list(
    path_id = path_id, path_number = as.integer(sub("^path_", "", path_id)),
    path_seed = path_seed, current_horizon = as.integer(horizon),
    active_time = active_time, path = as.numeric(simulation$paths[, 1L]),
    extension_count = 0L
  )
}

ce_create_path <- function(bank, path_number, stage = NA_character_) {
  path_id <- ce_path_id(path_number)
  v2_assert(!exists(path_id, envir = bank$state$records, inherits = FALSE),
            "Attempted to recreate an existing complete-episode path ID.")
  record <- ce_simulate_one_path(bank, path_id, bank$initial_horizon)
  ce_record_path(bank, record)
  bank$state$created_path_numbers <- c(
    bank$state$created_path_numbers, as.integer(path_number)
  )
  ce_bank_event(bank, "create", path_id, 0L, record$current_horizon, stage)
  record
}

ce_extend_path <- function(bank, path_id, next_horizon,
                           stage = NA_character_) {
  prior <- ce_read_path(bank, path_id)
  next_horizon <- as.integer(next_horizon)
  v2_assert(next_horizon > prior$current_horizon && next_horizon <= bank$h_max,
            "Adaptive path extension horizon is invalid.")
  extended <- ce_simulate_one_path(bank, path_id, next_horizon)
  prefix_index <- seq_along(prior$path)
  prefix_identical <- identical(prior$active_time, extended$active_time[prefix_index]) &&
    identical(prior$path, extended$path[prefix_index])
  v2_assert(prefix_identical,
            paste("Simulator replay did not preserve adaptive prefix for", path_id))
  extended$extension_count <- prior$extension_count + 1L
  ce_record_path(bank, extended)
  ce_bank_event(
    bank, "extend", path_id, prior$current_horizon,
    extended$current_horizon, stage
  )
  extended
}

ce_build_common_path_bank <- function(simulator, centre, final_paths,
                                      path_batch_size, seed, h_max,
                                      storage_mode = c("temporary_rds", "memory"),
                                      initial_horizon = 1L,
                                      stream_scope = NULL,
                                      path_stream_version = NULL) {
  storage_mode <- match.arg(storage_mode)
  final_paths <- as.integer(final_paths)
  path_batch_size <- as.integer(path_batch_size)
  h_max <- as.integer(h_max)
  initial_horizon <- as.integer(initial_horizon)
  v2_assert(is.list(simulator) && is.function(simulator$simulate_paths),
            "Complete-episode path bank requires a simulator.")
  v2_assert(final_paths >= 1L && path_batch_size >= 1L && h_max >= 1L &&
              initial_horizon >= 1L && initial_horizon <= h_max,
            "Complete-episode path-bank dimensions are invalid.")
  if (is.null(path_stream_version)) {
    path_stream_version <- ce_complete_episode_policy()$numerical$path_stream_version
  }
  seed_allocator <- ce_allocate_path_seeds(
    seed, stream_scope, path_stream_version, final_paths
  )
  storage_dir <- if (identical(storage_mode, "temporary_rds")) {
    path <- tempfile("complete_episode_adaptive_bank_")
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    path
  } else NA_character_
  state <- new.env(parent = emptyenv())
  state$records <- new.env(hash = TRUE, parent = emptyenv())
  state$created_path_numbers <- integer()
  state$events <- list()
  state$simulation_calls <- 0L
  state$maximum_simulation_cells <- 0L
  simulator_hash <- simulator$simulator_hash %v2||% v2_hash_object(simulator)
  fingerprint <- v2_hash_object(list(
    version = "complete_episode_adaptive_path_bank_v2",
    simulator_hash = simulator_hash, final_paths = final_paths,
    path_batch_size = path_batch_size, seed = as.integer(seed),
    initial_horizon = initial_horizon, h_max = h_max,
    stream_scope = stream_scope,
    seed_allocator_scope_hash = seed_allocator$scope_hash,
    seed_allocator_version = seed_allocator$allocator_version,
    path_stream_version = path_stream_version
  ))
  list(
    simulator = simulator, simulator_hash = simulator_hash,
    centre = as.numeric(centre), final_paths = final_paths,
    path_batch_size = path_batch_size, seed = as.integer(seed),
    initial_horizon = initial_horizon, h_max = h_max,
    stream_scope = stream_scope, path_stream_version = path_stream_version,
    seed_allocator = seed_allocator, storage_mode = storage_mode,
    storage_dir = storage_dir, owned_storage = !is.na(storage_dir),
    state = state, fingerprint = fingerprint
  )
}

ce_release_common_path_bank <- function(bank) {
  if (isTRUE(bank$owned_storage) && is.character(bank$storage_dir) &&
      length(bank$storage_dir) == 1L && dir.exists(bank$storage_dir) &&
      startsWith(basename(bank$storage_dir), "complete_episode_adaptive_bank_")) {
    unlink(bank$storage_dir, recursive = TRUE, force = TRUE)
  }
  invisible(TRUE)
}

ce_ensure_path_ids <- function(bank, n_paths, stage = NA_character_) {
  n_paths <- as.integer(n_paths)
  v2_assert(n_paths >= 1L && n_paths <= bank$final_paths,
            "Requested complete-episode path prefix is invalid.")
  existing <- length(bank$state$created_path_numbers)
  if (existing < n_paths) {
    for (path_number in seq.int(existing + 1L, n_paths)) {
      ce_create_path(bank, path_number, stage)
    }
  }
  ce_path_id(seq_len(n_paths))
}

ce_path_bank_prefix_ids <- function(bank, n_paths) {
  n_paths <- as.integer(n_paths)
  v2_assert(n_paths >= 1L && n_paths <= bank$final_paths,
            "Requested complete-episode path prefix is invalid.")
  ids <- ce_path_id(seq_len(n_paths))
  v2_assert(all(vapply(ids, exists, logical(1L), envir = bank$state$records,
                       inherits = FALSE)),
            "Requested path IDs have not yet been lazily created.")
  ids
}

# Inspection helper returning the common stored prefix only. It never extends
# paths and therefore cannot trigger hidden simulation to H_max.
ce_path_bank_prefix <- function(bank, n_paths) {
  ids <- ce_path_bank_prefix_ids(bank, n_paths)
  records <- lapply(ids, function(id) ce_read_path(bank, id))
  common_horizon <- min(vapply(records, `[[`, integer(1L), "current_horizon"))
  rows <- seq_len(common_horizon + 1L)
  list(
    paths = do.call(cbind, lapply(records, function(x) x$path[rows])),
    active_time = 0:common_horizon, path_ids = ids,
    current_horizons = vapply(records, `[[`, integer(1L), "current_horizon")
  )
}

ce_path_bank_audit <- function(bank) {
  ids <- ce_path_id(bank$state$created_path_numbers)
  records <- lapply(ids, function(id) ce_read_path(bank, id))
  manifest <- if (length(records)) do.call(rbind, lapply(records, function(x) {
    data.frame(
      path_id = x$path_id, path_number = x$path_number,
      path_seed = x$path_seed, current_horizon = x$current_horizon,
      extension_count = x$extension_count, stringsAsFactors = FALSE
    )
  })) else data.frame()
  events <- if (length(bank$state$events)) {
    do.call(rbind, bank$state$events)
  } else data.frame()
  list(
    manifest = manifest, events = events,
    created_path_count = length(ids),
    simulation_calls = bank$state$simulation_calls,
    maximum_simulation_cells = bank$state$maximum_simulation_cells,
    theoretical_full_guardrail_cells =
      as.double(bank$final_paths) * (as.double(bank$h_max) + 1)
  )
}

ce_evaluate_one_adaptive_path <- function(bank, path_id, candidates, centre,
                                          cost_plus, cost_minus, policy,
                                          stage = NA_character_) {
  candidates <- v2_validate_candidates(candidates)
  outcomes <- vector("list", nrow(candidates))
  unresolved <- which(candidates$valid %in% TRUE)
  repeat {
    record <- ce_read_path(bank, path_id)
    if (length(unresolved)) {
      for (candidate_index in unresolved) {
        outcomes[[candidate_index]] <- ce_episode_from_prefix(
          record$path, record$active_time,
          candidates[candidate_index, , drop = FALSE], centre,
          cost_plus, cost_minus, path_id,
          at_guardrail = record$current_horizon >= bank$h_max,
          extension_count = record$extension_count
        )
      }
      unresolved <- unresolved[vapply(
        outcomes[unresolved], function(x) !isTRUE(x$resolved[[1L]]), logical(1L)
      )]
    }
    if (!length(unresolved) || record$current_horizon >= bank$h_max) break
    next_horizon <- ce_next_horizon(
      record$current_horizon, bank$h_max,
      policy$numerical$extension_multiplier
    )
    ce_extend_path(bank, path_id, next_horizon, stage)
  }
  outcomes
}

ce_evaluate_common_path_bank <- function(bank, n_paths, candidates, centre,
                                         cost_plus, cost_minus,
                                         initial_horizon, policy,
                                         stage = NA_character_) {
  candidates <- v2_validate_candidates(candidates)
  n_paths <- as.integer(n_paths)
  v2_assert(as.integer(initial_horizon) == bank$initial_horizon,
            "Adaptive bank initial horizon differs from calibration context.")
  ids <- ce_ensure_path_ids(bank, n_paths, stage)
  seeds <- vapply(ids, function(id) ce_path_seed(bank$seed_allocator, id), integer(1L))
  prefix_fingerprint <- v2_hash_object(list(
    common_bank = bank$fingerprint, path_ids = ids, path_seeds = seeds
  ))
  accumulated <- lapply(seq_len(nrow(candidates)), function(i) list())
  for (path_id in ids) {
    path_outcomes <- ce_evaluate_one_adaptive_path(
      bank, path_id, candidates, centre, cost_plus, cost_minus,
      policy, stage
    )
    for (candidate_index in seq_len(nrow(candidates))) {
      if (!is.null(path_outcomes[[candidate_index]])) {
        accumulated[[candidate_index]][[
          length(accumulated[[candidate_index]]) + 1L
        ]] <- path_outcomes[[candidate_index]]
      }
    }
  }
  rows <- lapply(seq_len(nrow(candidates)), function(candidate_index) {
    outcomes <- if (length(accumulated[[candidate_index]])) {
      v2_bind_rows(accumulated[[candidate_index]])
    } else data.frame()
    ce_summarise_candidate(
      candidates[candidate_index, , drop = FALSE], outcomes, n_paths,
      policy, initial_horizon, bank$h_max, prefix_fingerprint
    )
  })
  v2_bind_rows(rows)
}

ce_select_complete_episode_threshold <- function(table,
                                                 enforce_outside_option = TRUE,
                                                 outside_option = 0,
                                                 tolerance = 1e-12) {
  if (!nrow(table) || !any(is.finite(table$objective_value))) return(NULL)
  v2_select_threshold(
    table, enforce_outside_option = enforce_outside_option,
    outside_option = outside_option, tolerance = tolerance
  )
}

ce_no_trade_selected <- function(template, reason, policy) {
  if (is.null(template) || !is.data.frame(template) || !nrow(template)) {
    template <- data.frame(
      candidate_id = NA_character_, d_plus = NA_real_, d_minus = NA_real_,
      c_plus = NA_real_, c_minus = NA_real_,
      terminal_policy = policy$calibration$terminal_policy,
      objective_version = policy$calibration$objective_version,
      objective_value = NA_real_, MC_standard_error = NA_real_,
      optimizer_status = policy$numerical$guardrail_status,
      stringsAsFactors = FALSE
    )
  } else template <- template[1L, , drop = FALSE]
  threshold_fields <- intersect(
    c("candidate_id", "d_plus", "d_minus", "c_plus", "c_minus"),
    names(template)
  )
  for (field in threshold_fields) {
    template[[field]] <- if (field == "candidate_id") NA_character_ else NA_real_
  }
  template$objective_value <- NA_real_
  template$MC_standard_error <- NA_real_
  template$route_status <- "MODEL_NO_TRADE"
  template$strategy_available <- FALSE
  template$threshold_failure_reason <- reason
  template$reason_code <- reason
  template$optimizer_status <- if (
    identical(reason, policy$calibration$unresolved_reason)
  ) policy$numerical$guardrail_status else "outside_option_no_trade"
  template
}

ce_attach_selected_provenance <- function(selected, model, pair, endpoint_date,
                                          simulator, centre, roundtrip_cost,
                                          parameter_hash, parameter_source_hash,
                                          budgets, contract, policy, bank) {
  tradeable <- selected$strategy_available %in% TRUE
  selected$Pair <- as.character(pair)
  selected$Session_Date <- as.character(as.Date(endpoint_date))
  selected$model <- as.character(model)
  selected$centre <- centre
  selected$upper_entry <- if (tradeable) centre + selected$d_plus else NA_real_
  selected$lower_entry <- if (tradeable) centre - selected$d_minus else NA_real_
  selected$upper_exit <- if (tradeable) centre else NA_real_
  selected$lower_exit <- if (tradeable) centre else NA_real_
  selected$formation_cost_proxy_roundtrip_log <- roundtrip_cost
  selected$terminal_policy_version <- policy$calibration$policy_version
  selected$calibration_policy_version <- policy$calibration$policy_version
  selected$objective_version <- policy$calibration$objective_version
  selected$simulation_seed <- as.integer(budgets$seed)
  selected$parameter_hash <- parameter_hash
  selected$parameter_source_hash <- parameter_source_hash
  selected$simulator_hash <- simulator$simulator_hash %v2||% v2_hash_object(simulator)
  selected$configuration_hash <- v2_hash_object(list(
    production_contract = contract,
    complete_episode_contract = policy,
    budgets = budgets[c("coarse_paths", "intermediate_paths", "final_paths",
                        "path_batch_size", "seed")],
    h_max = bank$h_max, path_bank_design = "adaptive_per_path_replay_v2"
  ))
  selected$path_bank_fingerprint <- bank$fingerprint
  selected$pair_sleeve_usd <- contract$capital$pair_sleeve_usd
  selected$testing_data_used_for_calibration <- FALSE
  selected$testing_pnl_used <- FALSE
  selected$economic_terminal_horizon <- FALSE
  selected$horizon_argument_role <- policy$calibration$initial_horizon_role
  selected
}

calibrate_complete_episode_threshold_from_context <- function(
    model, pair, endpoint_date, simulator, centre, stationary_sd, horizon,
    roundtrip_cost, parameter_hash, parameter_source_hash,
    budgets = list(
      coarse_paths = 250L, intermediate_paths = 750L, final_paths = 10000L,
      path_batch_size = 250L, seed = 91001L
    ), contract = PRODUCTION_V2_CONTRACT) {
  v2_assert(is.list(simulator) && is.function(simulator$simulate_paths),
            "A validated model simulator is required.")
  v2_assert(is.finite(centre) && is.finite(stationary_sd) && stationary_sd > 0,
            "Threshold context has an invalid centre or stationary scale.")
  v2_assert(is.finite(horizon) && horizon >= 1 && is.finite(roundtrip_cost) &&
              roundtrip_cost >= 0, "Threshold horizon/cost context is invalid.")
  ce_validate_budgets(budgets)
  policy <- ce_complete_episode_policy()
  h_max <- as.integer(budgets$h_max_active_minutes %v2||%
                        policy$numerical$h_max_active_minutes)
  v2_assert(is.finite(h_max) && h_max >= as.integer(horizon),
            "Complete-episode H_max must be at least the initial numerical horizon.")
  storage_mode <- budgets$path_storage %v2||% policy$numerical$path_storage
  stream_scope <- list(
    model = as.character(model), pair = as.character(pair),
    endpoint_date = as.character(as.Date(endpoint_date)),
    simulator_hash = simulator$simulator_hash %v2||% v2_hash_object(simulator),
    parameter_hash = as.character(parameter_hash),
    calibration_policy_version = policy$calibration$policy_version
  )
  bank <- ce_build_common_path_bank(
    simulator, centre, budgets$final_paths, budgets$path_batch_size,
    budgets$seed, h_max, storage_mode,
    initial_horizon = as.integer(horizon), stream_scope = stream_scope,
    path_stream_version = policy$numerical$path_stream_version
  )
  on.exit(ce_release_common_path_bank(bank), add = TRUE)
  coarse_grid <- v2_threshold_candidate_grid(stationary_sd, roundtrip_cost)
  coarse <- ce_evaluate_common_path_bank(
    bank, budgets$coarse_paths, coarse_grid, centre,
    roundtrip_cost, roundtrip_cost, as.integer(horizon), policy, "coarse"
  )
  coarse_selected <- ce_select_complete_episode_threshold(
    coarse, enforce_outside_option = FALSE
  )
  intermediate <- final <- coarse[0, , drop = FALSE]
  if (is.null(coarse_selected)) {
    selected <- ce_no_trade_selected(
      coarse, policy$calibration$unresolved_reason, policy
    )
  } else {
    intermediate_grid <- v2_refine_grid(coarse_selected, "intermediate")
    intermediate <- ce_evaluate_common_path_bank(
      bank, budgets$intermediate_paths, intermediate_grid, centre,
      roundtrip_cost, roundtrip_cost, as.integer(horizon), policy,
      "intermediate"
    )
    intermediate_selected <- ce_select_complete_episode_threshold(
      intermediate, enforce_outside_option = FALSE
    )
    if (is.null(intermediate_selected)) {
      selected <- ce_no_trade_selected(
        intermediate, policy$calibration$unresolved_reason, policy
      )
    } else {
      final_grid <- v2_refine_grid(intermediate_selected, "final")
      final <- ce_evaluate_common_path_bank(
        bank, budgets$final_paths, final_grid, centre,
        roundtrip_cost, roundtrip_cost, as.integer(horizon), policy, "final"
      )
      selected <- ce_select_complete_episode_threshold(
        final, enforce_outside_option = TRUE,
        outside_option = production_config$threshold_mc$outside_option,
        tolerance = production_config$threshold_mc$outside_option_tolerance
      )
      if (is.null(selected)) {
        selected <- ce_no_trade_selected(
          final, policy$calibration$unresolved_reason, policy
        )
      } else if (identical(selected$route_status[[1L]], "MODEL_NO_TRADE")) {
        selected$threshold_failure_reason <- policy$calibration$nonpositive_reason
        selected$reason_code <- policy$calibration$nonpositive_reason
      } else {
        selected$threshold_failure_reason <- NA_character_
        selected$reason_code <- "POSITIVE_OBJECTIVE"
      }
    }
  }
  selected <- ce_attach_selected_provenance(
    selected, model, pair, endpoint_date, simulator, centre,
    roundtrip_cost, parameter_hash, parameter_source_hash,
    budgets, contract, policy, bank
  )
  list(
    complete = TRUE, selected = selected, coarse = coarse,
    intermediate = intermediate, final = final, completed_at = v2_now()
  )
}
