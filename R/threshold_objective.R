# Full-horizon repeated-cycle payoff evaluator v2. Every entered open state is
# marked to its simulated terminal value and charged liquidation cost once.

v2_validate_candidates <- function(candidates) {
  required <- c("candidate_id", "d_plus", "d_minus")
  v2_assert(is.data.frame(candidates) && all(required %in% names(candidates)), "Invalid threshold candidates.")
  if (!"c_plus" %in% names(candidates)) candidates$c_plus <- 0
  if (!"c_minus" %in% names(candidates)) candidates$c_minus <- 0
  candidates$valid <- with(candidates,
    is.finite(d_plus) & is.finite(d_minus) & is.finite(c_plus) & is.finite(c_minus) &
      d_plus > c_plus & d_minus > c_minus & c_plus >= 0 & c_minus >= 0)
  candidates
}

v2_compile_terminal_backend <- function() {
  if (exists("production_v2_terminal_grid_cpp", envir = .GlobalEnv, inherits = FALSE)) {
    return(invisible(TRUE))
  }
  if (!requireNamespace("Rcpp", quietly = TRUE)) return(invisible(FALSE))
  code <- '
  Rcpp::List production_v2_terminal_grid_cpp(
      Rcpp::NumericMatrix paths, double centre,
      Rcpp::NumericVector d_plus, Rcpp::NumericVector d_minus,
      Rcpp::NumericVector c_plus, Rcpp::NumericVector c_minus,
      double cost_plus, double cost_minus, bool liquidate) {
    int nt = paths.nrow(), np = paths.ncol(), nc = d_plus.size();
    Rcpp::NumericMatrix total(nc, np), ordinary_sum(nc, np), forced_reward(nc, np);
    Rcpp::IntegerMatrix entered(nc, np), ordinary_count(nc, np), forced(nc, np),
                        no_entry(nc, np), entry_count(nc, np);
    std::fill(forced_reward.begin(), forced_reward.end(), NA_REAL);
    for (int k = 0; k < nc; ++k) {
      for (int j = 0; j < np; ++j) {
        int state = 0, ordinary_n = 0, entries = 0;
        double entry_value = NA_REAL, side_cost = NA_REAL, reward_total = 0.0, ord_sum = 0.0;
        bool entered_any = false;
        for (int i = 0; i < nt; ++i) {
          double x = paths(i, j) - centre;
          if (state == 0) {
            if (x >= d_plus[k]) { state = -1; entry_value = x; side_cost = cost_plus; }
            else if (x <= -d_minus[k]) { state = 1; entry_value = x; side_cost = cost_minus; }
            else continue;
            reward_total -= side_cost / 2.0;
            entered_any = true; ++entries;
          } else {
            bool ordinary = (state == -1 && x <= c_plus[k]) ||
                            (state == 1 && x >= -c_minus[k]);
            if (!ordinary) continue;
            double gross = state == -1 ? entry_value - x : x - entry_value;
            double reward = gross - side_cost;
            reward_total += gross - side_cost / 2.0;
            ord_sum += reward; ++ordinary_n;
            state = 0; entry_value = NA_REAL; side_cost = NA_REAL;
          }
        }
        bool was_forced = false;
        if (state != 0) {
          if (liquidate) {
            double xh = paths(nt - 1, j) - centre;
            double gross = state == -1 ? entry_value - xh : xh - entry_value;
            double reward = gross - side_cost;
            reward_total += gross - side_cost / 2.0;
            forced_reward(k, j) = reward; was_forced = true;
          } else {
            reward_total += side_cost / 2.0;
          }
        }
        total(k, j) = reward_total;
        ordinary_sum(k, j) = ord_sum;
        entered(k, j) = entered_any;
        ordinary_count(k, j) = ordinary_n;
        forced(k, j) = was_forced;
        no_entry(k, j) = !entered_any;
        entry_count(k, j) = entries;
      }
    }
    return Rcpp::List::create(
      Rcpp::_["total_reward"] = total,
      Rcpp::_["entered"] = entered,
      Rcpp::_["ordinary_sum"] = ordinary_sum,
      Rcpp::_["ordinary_count"] = ordinary_count,
      Rcpp::_["forced"] = forced,
      Rcpp::_["forced_reward"] = forced_reward,
      Rcpp::_["no_entry"] = no_entry,
      Rcpp::_["entry_count"] = entry_count
    );
  }'
  ok <- tryCatch({
    Rcpp::cppFunction(code = code, env = .GlobalEnv, plugins = "cpp11", verbose = FALSE)
    TRUE
  }, error = function(error) FALSE)
  invisible(ok)
}

v2_path_payoff <- function(path, time_grid, candidate, centre,
                           cost_plus, cost_minus,
                           terminal_policy = "liquidate_at_horizon") {
  x <- as.numeric(path) - centre
  n <- length(x)
  state <- "flat"
  entry_value <- NA_real_
  side_cost <- NA_real_
  total_reward <- 0
  entered_any <- FALSE
  ordinary_rewards <- numeric()
  forced_rewards <- numeric()
  entry_count <- ordinary_count <- 0L
  last_action_index <- 0L
  for (i in seq_len(n)) {
    if (i == last_action_index) next
    if (state == "flat") {
      if (x[[i]] >= candidate$d_plus[[1L]]) {
        state <- "short"; entry_value <- x[[i]]; side_cost <- cost_plus
      } else if (x[[i]] <= -candidate$d_minus[[1L]]) {
        state <- "long"; entry_value <- x[[i]]; side_cost <- cost_minus
      } else next
      total_reward <- total_reward - side_cost / 2
      entered_any <- TRUE; entry_count <- entry_count + 1L; last_action_index <- i
    } else {
      ordinary <- (state == "short" && x[[i]] <= candidate$c_plus[[1L]]) ||
        (state == "long" && x[[i]] >= -candidate$c_minus[[1L]])
      if (!ordinary) next
      gross <- if (state == "short") entry_value - x[[i]] else x[[i]] - entry_value
      reward <- gross - side_cost
      # Half was charged at entry; add gross and the remaining half now.
      total_reward <- total_reward + gross - side_cost / 2
      ordinary_rewards <- c(ordinary_rewards, reward)
      ordinary_count <- ordinary_count + 1L
      state <- "flat"; entry_value <- side_cost <- NA_real_; last_action_index <- i
    }
  }
  forced <- FALSE
  if (state != "flat") {
    if (identical(terminal_policy, "liquidate_at_horizon")) {
      gross <- if (state == "short") entry_value - x[[n]] else x[[n]] - entry_value
      reward <- gross - side_cost
      total_reward <- total_reward + gross - side_cost / 2
      forced_rewards <- reward
      forced <- TRUE
    } else if (identical(terminal_policy, "legacy_zero_reward_censor")) {
      # Undo the entry half-cost: legacy incomplete cycles contributed zero.
      total_reward <- total_reward + side_cost / 2
    } else stop("Unsupported terminal policy.", call. = FALSE)
  }
  list(
    total_reward = total_reward,
    entered = entered_any,
    entry_count = entry_count,
    ordinary_exit_count = ordinary_count,
    forced_terminal_close = forced,
    no_entry = !entered_any,
    ordinary_rewards = ordinary_rewards,
    forced_rewards = forced_rewards,
    horizon = tail(time_grid, 1L) - time_grid[[1L]]
  )
}

v2_evaluate_threshold_grid <- function(paths, time_grid, candidates, centre,
                                       cost_plus, cost_minus,
                                       terminal_policy = "liquidate_at_horizon",
                                       objective_version = PRODUCTION_V2_CONTRACT$terminal$objective_version,
                                       use_compiled_backend = TRUE) {
  paths <- as.matrix(paths)
  time_grid <- as.numeric(time_grid)
  candidates <- v2_validate_candidates(candidates)
  v2_assert(nrow(paths) == length(time_grid) && length(time_grid) >= 2L, "Path/time dimensions differ.")
  v2_assert(all(diff(time_grid) > 0), "time_grid must strictly increase.")
  v2_assert(terminal_policy %in% c("liquidate_at_horizon", "legacy_zero_reward_censor"),
            "Unsupported terminal policy.")
  if (isTRUE(use_compiled_backend) && all(candidates$valid) && isTRUE(v2_compile_terminal_backend())) {
    raw <- production_v2_terminal_grid_cpp(
      paths, centre, candidates$d_plus, candidates$d_minus,
      candidates$c_plus, candidates$c_minus, cost_plus, cost_minus,
      identical(terminal_policy, "liquidate_at_horizon")
    )
    horizon <- tail(time_grid, 1L) - time_grid[[1L]]
    rows <- lapply(seq_len(nrow(candidates)), function(k) {
      rewards <- as.numeric(raw$total_reward[k, ])
      forced_rewards <- as.numeric(raw$forced_reward[k, ])
      forced_rewards <- forced_rewards[is.finite(forced_rewards)]
      ordinary_n <- as.numeric(raw$ordinary_count[k, ])
      ordinary_total <- sum(raw$ordinary_sum[k, ])
      fq <- if (length(forced_rewards)) {
        stats::quantile(forced_rewards, c(.01, .05, .50, .95, .99), names = FALSE, type = 8)
      } else rep(NA_real_, 5L)
      data.frame(
        candidate_id = candidates$candidate_id[[k]],
        d_plus = candidates$d_plus[[k]], d_minus = candidates$d_minus[[k]],
        c_plus = candidates$c_plus[[k]], c_minus = candidates$c_minus[[k]],
        terminal_policy = terminal_policy, objective_version = objective_version,
        entry_probability = mean(raw$entered[k, ]),
        ordinary_exit_probability = mean(ordinary_n > 0),
        forced_terminal_close_probability = mean(raw$forced[k, ]),
        no_entry_probability = mean(raw$no_entry[k, ]),
        mean_ordinary_reward = if (sum(ordinary_n) > 0) ordinary_total / sum(ordinary_n) else NA_real_,
        mean_forced_close_reward = if (length(forced_rewards)) mean(forced_rewards) else NA_real_,
        forced_close_reward_q01 = fq[[1L]], forced_close_reward_q05 = fq[[2L]],
        forced_close_reward_q50 = fq[[3L]], forced_close_reward_q95 = fq[[4L]],
        forced_close_reward_q99 = fq[[5L]], mean_total_reward = mean(rewards),
        mean_duration = horizon, objective_value = mean(rewards) / horizon,
        MC_standard_error = if (length(rewards) > 1L) stats::sd(rewards / horizon) / sqrt(length(rewards)) else NA_real_,
        valid_paths = length(rewards), mean_cycles = mean(raw$entry_count[k, ]),
        optimizer_status = "evaluated", stringsAsFactors = FALSE
      )
    })
    return(v2_bind_rows(rows))
  }
  evaluate_one <- function(candidate) {
    if (!candidate$valid[[1L]]) return(data.frame(
      candidate_id = candidate$candidate_id, objective_value = NA_real_, optimizer_status = "invalid_candidate"
    ))
    values <- lapply(seq_len(ncol(paths)), function(j) v2_path_payoff(
      paths[, j], time_grid, candidate, centre, cost_plus, cost_minus, terminal_policy
    ))
    rewards <- vapply(values, `[[`, numeric(1L), "total_reward")
    entered <- vapply(values, `[[`, logical(1L), "entered")
    forced <- vapply(values, `[[`, logical(1L), "forced_terminal_close")
    no_entry <- vapply(values, `[[`, logical(1L), "no_entry")
    ordinary_count <- vapply(values, `[[`, integer(1L), "ordinary_exit_count")
    ordinary_rewards <- unlist(lapply(values, `[[`, "ordinary_rewards"), use.names = FALSE)
    forced_rewards <- unlist(lapply(values, `[[`, "forced_rewards"), use.names = FALSE)
    horizon <- tail(time_grid, 1L) - time_grid[[1L]]
    objective <- mean(rewards) / horizon
    se <- if (length(rewards) > 1L) stats::sd(rewards / horizon) / sqrt(length(rewards)) else NA_real_
    fq <- if (length(forced_rewards)) stats::quantile(forced_rewards, c(.01, .05, .50, .95, .99), names = FALSE, type = 8) else rep(NA_real_, 5L)
    data.frame(
      candidate_id = candidate$candidate_id,
      d_plus = candidate$d_plus, d_minus = candidate$d_minus,
      c_plus = candidate$c_plus, c_minus = candidate$c_minus,
      terminal_policy = terminal_policy,
      objective_version = objective_version,
      entry_probability = mean(entered),
      ordinary_exit_probability = mean(ordinary_count > 0L),
      forced_terminal_close_probability = mean(forced),
      no_entry_probability = mean(no_entry),
      mean_ordinary_reward = if (length(ordinary_rewards)) mean(ordinary_rewards) else NA_real_,
      mean_forced_close_reward = if (length(forced_rewards)) mean(forced_rewards) else NA_real_,
      forced_close_reward_q01 = fq[[1L]], forced_close_reward_q05 = fq[[2L]],
      forced_close_reward_q50 = fq[[3L]], forced_close_reward_q95 = fq[[4L]],
      forced_close_reward_q99 = fq[[5L]],
      mean_total_reward = mean(rewards),
      mean_duration = horizon,
      objective_value = objective,
      MC_standard_error = se,
      valid_paths = length(rewards),
      mean_cycles = mean(vapply(values, `[[`, integer(1L), "entry_count")),
      optimizer_status = "evaluated",
      stringsAsFactors = FALSE
    )
  }
  v2_bind_rows(lapply(seq_len(nrow(candidates)), function(i) evaluate_one(candidates[i, , drop = FALSE])))
}

v2_evaluate_renewal_bridge <- function(paths, time_grid, candidates, centre,
                                       cost_plus, cost_minus,
                                       terminal_policy = "liquidate_at_horizon") {
  paths <- as.matrix(paths); candidates <- v2_validate_candidates(candidates)
  v2_bind_rows(lapply(seq_len(nrow(candidates)), function(k) {
    candidate <- candidates[k, , drop = FALSE]
    values <- lapply(seq_len(ncol(paths)), function(j) {
      x <- paths[, j] - centre
      upper <- which(x >= candidate$d_plus); lower <- which(x <= -candidate$d_minus)
      iu <- if (length(upper)) upper[[1L]] else Inf
      il <- if (length(lower)) lower[[1L]] else Inf
      entry <- min(iu, il)
      if (!is.finite(entry)) return(c(reward = 0, duration = tail(time_grid, 1L), entered = 0, forced = 0))
      short <- iu <= il
      later <- if (entry < nrow(paths)) seq.int(entry + 1L, nrow(paths)) else integer()
      exit <- if (short) later[x[later] <= candidate$c_plus] else later[x[later] >= -candidate$c_minus]
      forced <- !length(exit)
      exit <- if (forced) nrow(paths) else exit[[1L]]
      if (forced && terminal_policy == "legacy_zero_reward_censor") reward <- 0 else {
        gross <- if (short) x[[entry]] - x[[exit]] else x[[exit]] - x[[entry]]
        reward <- gross - if (short) cost_plus else cost_minus
      }
      c(reward = reward, duration = time_grid[[exit]], entered = 1, forced = as.integer(forced))
    })
    m <- do.call(rbind, values)
    data.frame(
      candidate_id = candidate$candidate_id,
      objective_value = mean(m[, "reward"]) / mean(m[, "duration"]),
      mean_total_reward = mean(m[, "reward"]), mean_duration = mean(m[, "duration"]),
      entry_probability = mean(m[, "entered"]),
      forced_terminal_close_probability = mean(m[, "forced"]),
      terminal_policy = terminal_policy,
      objective_version = "renewal_cycle_terminal_liquidation_v2",
      stringsAsFactors = FALSE
    )
  }))
}

v2_terminal_policy_contract <- function(contract = PRODUCTION_V2_CONTRACT) data.frame(
  field = c("starting_state", "entry", "ordinary_exit", "reentry", "no_entry", "entered_unfinished", "cost", "duration", "objective"),
  production_value = c(
    "flat", "actual simulated first crossing with overshoot",
    "first later fitted-centre crossing with overshoot", "permitted from the next observation",
    "zero reward over full horizon", "directional mark-to-terminal liquidation",
    "half formation round-trip proxy at entry plus half at any ordinary/terminal exit",
    "fixed task testing active-minute horizon",
    "mean total net reward divided by fixed active-minute horizon"
  ),
  legacy_sensitivity = c(
    "flat", "same", "same", "not applicable to single renewal cycle", "zero", "zero reward censor", "round-trip only if completed", "cycle/censor time", "mean reward / mean time"
  ),
  stringsAsFactors = FALSE
)
