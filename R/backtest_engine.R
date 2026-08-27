# Lean common execution facade. Model-specific estimation and threshold code
# ends at a standard strategy specification; no model label changes economics.
load_common_backtest_engine <- function(envir = .GlobalEnv) {
  files <- c(
    "common.R", "contract_specs.R", "bfix.R", "fee_config.R",
    "position_sizing.R", "signal_roll_adjustment.R", "monetary_accounting.R",
    "roll_policies.R", "monetary_state_machine.R", "daily_ledger.R"
  )
  for (file in files) sys.source(repo_path("R", "backtest_core", file), envir)
  invisible(files)
}

# Kept as compatibility wrappers for existing users; the implementation lives
# in R/strategy_specification.R and is sourced by production entry scripts.
standard_strategy_spec <- function(model_label, pair_id, formation_endpoint,
                                   testing_start, testing_end, thresholds,
                                   frozen, pair_sleeve_usd = 200000) {
  data.frame(
    pair_id = pair_id, formation_endpoint = as.Date(formation_endpoint),
    testing_start = as.POSIXct(testing_start, tz = "Europe/London"),
    testing_end = as.POSIXct(testing_end, tz = "Europe/London"),
    upper_entry = as.numeric(thresholds$upper_entry),
    lower_entry = as.numeric(thresholds$lower_entry),
    upper_exit = as.numeric(thresholds$upper_exit),
    lower_exit = as.numeric(thresholds$lower_exit),
    alpha = as.numeric(frozen$alpha), beta = as.numeric(frozen$beta),
    formation_centre = as.numeric(frozen$centre),
    pair_sleeve_usd = as.numeric(pair_sleeve_usd),
    model_label = as.character(model_label), stringsAsFactors = FALSE
  )
}

prepare_midpoint_monetary_data <- function(synchronised_quotes, strategy) {
  strategy <- validate_strategy_spec(strategy)
  q <- synchronised_quotes
  valid <- q$statistical_quote_valid & is.finite(q$midpoint_y) &
    is.finite(q$midpoint_x) & q$midpoint_y > 0 & q$midpoint_x > 0
  raw <- statistical_raw_spread(q$midpoint_y, q$midpoint_x,
                                strategy$alpha, strategy$beta) -
    strategy$formation_centre
  n <- nrow(q)
  contract_change <- if ("roll_boundary" %in% names(q)) q$roll_boundary else c(
    FALSE, q$contract_y[-1L] != q$contract_y[-n] |
      q$contract_x[-1L] != q$contract_x[-n]
  )
  execution_y <- if ("execution_quote_clean_y" %in% names(q) &&
                     any(!is.na(q$execution_quote_clean_y))) {
    q$execution_quote_clean_y %in% TRUE
  } else q$statistical_quote_valid_y %in% TRUE
  execution_x <- if ("execution_quote_clean_x" %in% names(q) &&
                     any(!is.na(q$execution_quote_clean_x))) {
    q$execution_quote_clean_x %in% TRUE
  } else q$statistical_quote_valid_x %in% TRUE
  if (!"Active_Time_Minutes" %in% names(q)) stop(
    "Backtest input must contain the validated Active_Time_Minutes clock; row-index fallback is prohibited.",
    call. = FALSE
  )
  active_time <- as.numeric(q$Active_Time_Minutes)
  if (length(active_time) != n || any(!is.finite(active_time)) ||
      any(diff(active_time) < 0)) stop(
    "Backtest Active_Time_Minutes must be finite, complete, and non-decreasing.", call. = FALSE
  )
  data.frame(
    timestamp = q$timestamp, global_row_index = seq_len(n), active_time = active_time,
    y_price = q$midpoint_y, x_price = q$midpoint_x,
    alpha = strategy$alpha, beta = strategy$beta,
    spread = ifelse(valid, raw, NA_real_),
    y_bid = q$bid_y, y_ask = q$ask_y, x_bid = q$bid_x, x_ask = q$ask_x,
    y_contract = q$contract_y, x_contract = q$contract_x,
    y_bid_valid = execution_y, y_ask_valid = execution_y,
    x_bid_valid = execution_x, x_ask_valid = execution_x,
    y_market_uncrossed = execution_y, x_market_uncrossed = execution_x,
    roll_boundary = contract_change, stringsAsFactors = FALSE
  )
}

strategy_threshold_table <- function(strategy) data.frame(
  rule_type = "mean_exit", success = TRUE, mean_level = 0,
  upper_entry = strategy$upper_entry, lower_entry = strategy$lower_entry,
  upper_exit = strategy$upper_exit, lower_exit = strategy$lower_exit,
  upper_band = NA_real_, lower_band = NA_real_, stringsAsFactors = FALSE
)
