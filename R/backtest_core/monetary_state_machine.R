mab_entry_side <- function(spread, rule) {
  if (!is.finite(spread)) return(NA_character_)
  if (identical(rule$rule_type, "mean_exit")) {
    if (spread <= rule$lower_entry) return("long")
    if (spread >= rule$upper_entry) return("short")
  } else if (identical(rule$rule_type, "band_to_band")) {
    if (spread <= rule$lower_band) return("long")
    if (spread >= rule$upper_band) return("short")
  }
  NA_character_
}

mab_exit_signal <- function(spread, side, rule) {
  if (!is.finite(spread)) return(FALSE)
  if (identical(rule$rule_type, "mean_exit")) {
    if (side == "long") spread >= rule$lower_exit else spread <= rule$upper_exit
  } else if (identical(rule$rule_type, "band_to_band")) {
    if (side == "long") spread >= rule$upper_band else spread <= rule$lower_band
  } else {
    FALSE
  }
}

mab_engine_event <- function(event_id, trade_id, data, i, event_type, action,
                             side = NA_character_, reason = NA_character_,
                             signal_spread = NA_real_, message = NA_character_) {
  data.frame(
    event_id = as.integer(event_id),
    strategy_trade_id = as.integer(trade_id),
    timestamp = data$timestamp[i],
    global_row_index = if ("global_row_index" %in% names(data)) data$global_row_index[i] else NA_integer_,
    event_type = as.character(event_type),
    action = as.character(action),
    strategy_side = as.character(side),
    adjusted_signal_spread = as.numeric(signal_spread),
    raw_spread = as.numeric(data$spread[i]),
    cumulative_roll_offset = as.numeric(data$cumulative_roll_offset[i]),
    y_contract = as.character(data$y_contract[i]),
    x_contract = as.character(data$x_contract[i]),
    reason = as.character(reason),
    message = as.character(message),
    stringsAsFactors = FALSE
  )
}

mab_engine_diagnostic <- function(type, severity, timestamp, message,
                                  trade_id = NA_integer_) {
  data.frame(
    diagnostic_type = as.character(type),
    severity = as.character(severity),
    timestamp = mab_time(timestamp),
    strategy_trade_id = as.integer(trade_id),
    message = as.character(message),
    stringsAsFactors = FALSE
  )
}

mab_trade_native_summary <- function(segments) {
  if (!nrow(segments)) return(NA_character_)
  values <- tapply(segments$native_gross_pnl, segments$native_currency, sum, na.rm = TRUE)
  paste(paste(names(values), format(as.numeric(values), scientific = FALSE, trim = TRUE), sep = "="),
        collapse = ";")
}

mab_build_strategy_trade <- function(trade, trade_segments, trade_fills,
                                     exit_timestamp, exit_signal_spread,
                                     exit_reason, status, accounting_complete,
                                     exit_active_time) {
  realised <- isTRUE(accounting_complete) && nrow(trade_segments) > 0L &&
    all(is.finite(trade_segments$gross_usd_pnl))
  gross <- if (realised) sum(trade_segments$gross_usd_pnl) else NA_real_
  midpoint <- if (realised) sum(trade_segments$midpoint_usd_pnl) else NA_real_
  fee_sum <- function(field) {
    if (!nrow(trade_fills) || !field %in% names(trade_fills)) return(0)
    sum(as.numeric(trade_fills[[field]]), na.rm = TRUE)
  }
  total_fees <- fee_sum("total_fee_usd")
  equivalent <- mab_equivalent_log_fee(
    total_fees, trade$K_hat_entry, trade$K_ideal_entry
  )
  data.frame(
    strategy_trade_id = trade$trade_id,
    strategy_side = trade$side,
    trade_status = status,
    accounting_complete = accounting_complete,
    entry_signal_time = trade$entry_signal_time,
    entry_fill_time = trade$entry_fill_time,
    exit_signal_time = mab_time(exit_timestamp),
    exit_fill_time = mab_time(exit_timestamp),
    entry_adjusted_signal_spread = trade$entry_signal_spread,
    exit_adjusted_signal_spread = as.numeric(exit_signal_spread),
    beta = trade$beta,
    y_contract_entry = trade$y_contract_entry,
    x_contract_entry = trade$x_contract_entry,
    segment_count = if (nrow(trade_segments)) length(unique(trade_segments$segment_id)) else 0L,
    roll_count = trade$roll_count,
    native_gross_pnl_by_currency = if (realised) mab_trade_native_summary(trade_segments) else NA_character_,
    gross_usd_pnl = gross,
    midpoint_usd_pnl = midpoint,
    embedded_bidask_cost_usd = if (realised) midpoint - gross else NA_real_,
    brokerage_usd = fee_sum("brokerage_usd"),
    exchange_fee_usd = fee_sum("exchange_fee_usd"),
    clearing_fee_usd = fee_sum("clearing_fee_usd"),
    regulatory_fee_usd = fee_sum("regulatory_fee_usd"),
    total_explicit_fees_usd = total_fees,
    net_usd_pnl = if (realised) gross - total_fees else NA_real_,
    K_ideal_entry = trade$K_ideal_entry,
    K_hat_entry = trade$K_hat_entry,
    equivalent_total_fee_log_spread = equivalent$value,
    equivalent_fee_scale_basis = equivalent$basis,
    holding_time_minutes = as.numeric(exit_active_time) - trade$entry_active_time,
    holding_time_basis = "joint_pair_active_minutes",
    exit_reason = exit_reason,
    forced_exit = identical(exit_reason, "forced_end_of_window"),
    stringsAsFactors = FALSE
  )
}

mab_state_machine_summary <- function(data, trades, events, sizing, rolls,
                                      pair_committed_capital_usd,
                                      accounting_complete) {
  realised <- if (nrow(trades)) trades[trades$accounting_complete &
                                         is.finite(trades$net_usd_pnl), , drop = FALSE] else data.frame()
  net <- if (isTRUE(accounting_complete)) {
    if (nrow(realised)) sum(realised$net_usd_pnl) else 0
  } else NA_real_
  event_types <- if (nrow(events)) events$event_type else character()
  data.frame(
    number_of_observations = nrow(data),
    number_of_entry_signals = sum(event_types == "entry_signal"),
    number_of_entries = sum(event_types == "entry_fill"),
    number_of_completed_trades = nrow(realised),
    number_of_forced_window_exits = if (nrow(realised)) sum(realised$exit_reason == "forced_end_of_window") else 0L,
    number_of_rejected_missing_quote_signals = sum(grepl("rejected_missing_quotes", event_types)),
    number_of_rejected_sizing_signals = sum(event_types == "entry_rejected_sizing"),
    number_of_rolls = if (nrow(rolls)) sum(rolls$roll_success) else 0L,
    number_of_failed_rolls = if (nrow(rolls)) sum(!rolls$roll_success) else 0L,
    active_pair_flag = nrow(realised) > 0L,
    gross_usd_pnl = if (isTRUE(accounting_complete)) {
      if (nrow(realised)) sum(realised$gross_usd_pnl) else 0
    } else NA_real_,
    midpoint_usd_pnl = if (isTRUE(accounting_complete)) {
      if (nrow(realised)) sum(realised$midpoint_usd_pnl) else 0
    } else NA_real_,
    embedded_bidask_cost_usd = if (isTRUE(accounting_complete)) {
      if (nrow(realised)) sum(realised$embedded_bidask_cost_usd) else 0
    } else NA_real_,
    total_explicit_fees_usd = if (isTRUE(accounting_complete)) {
      if (nrow(realised)) sum(realised$total_explicit_fees_usd) else 0
    } else NA_real_,
    brokerage_usd = if (isTRUE(accounting_complete)) {
      if (nrow(realised)) sum(realised$brokerage_usd) else 0
    } else NA_real_,
    other_explicit_fees_usd = if (isTRUE(accounting_complete)) {
      if (nrow(realised)) sum(realised$exchange_fee_usd + realised$clearing_fee_usd +
                                realised$regulatory_fee_usd) else 0
    } else NA_real_,
    equivalent_trade_fee_log_spread = if (isTRUE(accounting_complete)) {
      if (nrow(realised)) sum(realised$equivalent_total_fee_log_spread, na.rm = TRUE) else 0
    } else NA_real_,
    roll_bidask_cost_usd = if (isTRUE(accounting_complete)) {
      if (nrow(rolls)) sum(rolls$roll_bidask_cost_usd, na.rm = TRUE) else 0
    } else NA_real_,
    roll_explicit_fees_usd = if (isTRUE(accounting_complete)) {
      if (nrow(rolls)) sum(rolls$total_roll_fees_usd, na.rm = TRUE) else 0
    } else NA_real_,
    equivalent_roll_fee_log_spread = if (isTRUE(accounting_complete)) {
      if (nrow(rolls)) sum(rolls$equivalent_roll_fee_log_spread, na.rm = TRUE) else 0
    } else NA_real_,
    net_usd_pnl = net,
    committed_pair_capital_usd = pair_committed_capital_usd,
    return_on_committed_pair_allocation = if (is.finite(net)) net / pair_committed_capital_usd else NA_real_,
    feasible_entry_attempts = if (nrow(sizing)) sum(sizing$feasibility) else 0L,
    infeasible_entry_attempts = if (nrow(sizing)) sum(!sizing$feasibility) else 0L,
    accounting_complete = accounting_complete,
    pnl_units = "USD",
    stringsAsFactors = FALSE
  )
}

backtest_monetary_spread_threshold_strategy <- function(
    spread_data,
    threshold_table,
    contract_specs,
    bfix,
    fee_config,
    y_generic,
    x_generic,
    pair_committed_capital_usd,
    scenario_id = "monetary_baseline_seamless",
    gross_notional_multiplier = 1,
    max_fx_age_days = 7,
    notional_overshoot_tolerance = 0.05,
    max_normalised_hedge_error = 0.25,
    include_path = FALSE,
    verbose = FALSE) {
  scenario <- mab_scenario_definitions(scenario_id)
  mab_assert(nrow(scenario) == 1L, "The monetary state machine executes one scenario at a time.")
  mab_assert(is.data.frame(threshold_table) && nrow(threshold_table) == 1L,
             "The monetary state machine requires one canonical threshold rule.")
  rule <- as.list(threshold_table[1L, , drop = FALSE])
  mab_assert(rule$rule_type %in% c("mean_exit", "band_to_band"),
             "Unsupported canonical rule type.")
  required <- c(
    "timestamp", "global_row_index", "active_time", "y_price", "x_price", "alpha", "beta",
    "spread", "y_bid", "y_ask", "x_bid", "x_ask", "y_contract", "x_contract"
  )
  mab_assert(all(required %in% names(spread_data)), paste0(
    "Monetary state machine lacks canonical columns: ",
    paste(setdiff(required, names(spread_data)), collapse = ", ")
  ))
  mab_assert(length(unique(spread_data$alpha[is.finite(spread_data$alpha)])) == 1L &&
               length(unique(spread_data$beta[is.finite(spread_data$beta)])) == 1L,
             "Frozen alpha and beta must be constant within a monetary task.")
  adjusted <- mab_adjust_signal_spread(spread_data)
  data <- adjusted$data
  n <- nrow(data)
  beta <- as.numeric(data$beta[[1L]])
  y_generic <- toupper(as.character(y_generic))
  x_generic <- toupper(as.character(x_generic))
  roll_policy <- mab_validate_roll_policy(scenario$roll_policy[[1L]])
  apply_explicit_fees <- isTRUE(scenario$apply_explicit_fees[[1L]])

  events <- list(); fills <- list(); segments <- list(); trades <- list()
  sizings <- list(); rolls <- list(); diagnostics <- list(); contract_audits <- list()
  event_n <- fill_n <- segment_n <- trade_n <- sizing_n <- roll_n <- diagnostic_n <- audit_n <- 0L
  trade_counter <- 0L
  current <- NULL
  accounting_complete <- TRUE
  block_entries <- FALSE
  path_side <- rep(NA_character_, n)
  path_trade <- rep(NA_integer_, n)

  add_event <- function(i, event_type, action, trade_id = NA_integer_, side = NA_character_,
                        reason = NA_character_, signal_spread = NA_real_, message = NA_character_) {
    event_n <<- event_n + 1L
    events[[event_n]] <<- mab_engine_event(
      event_n, trade_id, data, i, event_type, action, side, reason, signal_spread, message
    )
  }
  add_diagnostic <- function(type, severity, i, message, trade_id = NA_integer_) {
    diagnostic_n <<- diagnostic_n + 1L
    diagnostics[[diagnostic_n]] <<- mab_engine_diagnostic(
      type, severity, data$timestamp[i], message, trade_id
    )
  }
  add_contract_audit <- function(audit, fill_id = NA_integer_, leg = NA_character_) {
    if (!nrow(audit)) return(invisible(NULL))
    audit_n <<- audit_n + 1L
    audit$fill_id <- as.integer(fill_id)
    audit$leg <- as.character(leg)
    contract_audits[[audit_n]] <<- audit
  }
  make_fill <- function(i, leg, generic, delta, action, trade_id, segment_id, k_hat, k_ideal) {
    next_fill_id <- fill_n + 1L
    result <- mab_make_monetary_fill(
      fill_id = next_fill_id, strategy_trade_id = trade_id, segment_id = segment_id,
      action_type = action, data = data, i = i, leg = leg, generic = generic,
      quantity_change = delta, specs = contract_specs, bfix = bfix,
      fee_config = fee_config, max_fx_age_days = max_fx_age_days,
      apply_explicit_fees = apply_explicit_fees, k_hat = k_hat, k_ideal = k_ideal
    )
    if (is.null(result$fill)) return(result)
    fill_n <<- next_fill_id
    fills[[fill_n]] <<- result$fill
    add_contract_audit(result$contract_audit, fill_n, leg)
    result
  }
  current_trade_tables <- function(trade_id) {
    all_segments <- mab_bind_rows(segments)
    all_fills <- mab_bind_rows(fills)
    list(
      segments = if (nrow(all_segments)) all_segments[all_segments$strategy_trade_id == trade_id, , drop = FALSE] else data.frame(),
      fills = if (nrow(all_fills)) all_fills[all_fills$strategy_trade_id == trade_id, , drop = FALSE] else data.frame()
    )
  }
  append_trade <- function(exit_i, exit_signal_spread, exit_reason, status, complete) {
    tables <- current_trade_tables(current$trade_id)
    trade_n <<- trade_n + 1L
    trades[[trade_n]] <<- mab_build_strategy_trade(
      current, tables$segments, tables$fills, data$timestamp[exit_i], exit_signal_spread,
      exit_reason, status, complete, data$active_time[exit_i]
    )
  }
  close_active_segments <- function(i, y_exit, x_exit, y_mid, x_mid,
                                    reason, y_exit_fill = NA_integer_, x_exit_fill = NA_integer_,
                                    y_exit_fee = 0, x_exit_fee = 0) {
    y_row <- mab_close_segment(
      current$active_segments$y, data$timestamp[i], y_exit, y_mid, reason, bfix,
      max_fx_age_days, current$active_segments$y$entry_fee_usd, y_exit_fee, y_exit_fill
    )
    x_row <- mab_close_segment(
      current$active_segments$x, data$timestamp[i], x_exit, x_mid, reason, bfix,
      max_fx_age_days, current$active_segments$x$entry_fee_usd, x_exit_fee, x_exit_fill
    )
    segment_n <<- segment_n + 1L; segments[[segment_n]] <<- y_row
    segment_n <<- segment_n + 1L; segments[[segment_n]] <<- x_row
    invisible(list(y = y_row, x = x_row))
  }
  start_segments <- function(i, segment_number, y_spec, x_spec, y_entry, x_entry,
                             y_mid, x_mid, y_fill_id = NA_integer_, x_fill_id = NA_integer_,
                             y_fee = 0, x_fee = 0, reason = "strategy_entry") {
    y <- mab_new_segment(
      current$trade_id, segment_number, "y", current$signed_y_quantity, y_spec,
      data$y_contract[i], data$timestamp[i], y_entry, y_mid, y_fill_id, reason
    )
    x <- mab_new_segment(
      current$trade_id, segment_number, "x", current$signed_x_quantity, x_spec,
      data$x_contract[i], data$timestamp[i], x_entry, x_mid, x_fill_id, reason
    )
    y$entry_fee_usd <- as.numeric(y_fee); x$entry_fee_usd <- as.numeric(x_fee)
    current$active_segments <<- list(y = y, x = x)
    current$segment_number <<- segment_number
  }
  future_close_exists <- function(i, signed_y, signed_x) {
    if (i >= n) return(FALSE)
    for (j in seq.int(i + 1L, n)) {
      yq <- mab_quote_for_quantity_change(data, j, "y", -signed_y)
      xq <- mab_quote_for_quantity_change(data, j, "x", -signed_x)
      if (isTRUE(yq$eligible[[1L]]) && isTRUE(xq$eligible[[1L]])) return(TRUE)
    }
    FALSE
  }

  open_trade <- function(i, side, signal_spread) {
    side_sign <- if (side == "long") 1 else -1
    provisional_y <- side_sign
    provisional_x <- sign(-side_sign * beta)
    y_quote <- mab_quote_for_quantity_change(data, i, "y", provisional_y)
    x_quote <- mab_quote_for_quantity_change(data, i, "x", provisional_x)
    proposed <- trade_counter + 1L
    add_event(i, "entry_signal", "entry", proposed, side, "threshold_condition_met", signal_spread)
    if (!isTRUE(y_quote$eligible[[1L]]) || !isTRUE(x_quote$eligible[[1L]])) {
      add_event(i, "entry_rejected_missing_quotes", "entry", proposed, side,
                paste(na.omit(c(y_quote$failure_reason, x_quote$failure_reason)), collapse = ";"), signal_spread)
      return(FALSE)
    }
    y_match <- mab_fill_contract_spec(data, i, "y", y_generic, contract_specs)
    x_match <- mab_fill_contract_spec(data, i, "x", x_generic, contract_specs)
    y_fx <- mab_align_fx(data$timestamp[i], y_match$spec$PnLCurrency[[1L]], bfix, max_fx_age_days)
    x_fx <- mab_align_fx(data$timestamp[i], x_match$spec$PnLCurrency[[1L]], bfix, max_fx_age_days)
    sizing <- mab_size_position(
      beta = beta, side = side, pair_committed_capital_usd = pair_committed_capital_usd,
      gross_notional_multiplier = gross_notional_multiplier,
      y_price = data$y_price[i], x_price = data$x_price[i],
      y_spec = y_match$spec, x_spec = x_match$spec,
      y_fx = y_fx$fx_rate_usd_per_native[[1L]], x_fx = x_fx$fx_rate_usd_per_native[[1L]],
      notional_overshoot_tolerance = notional_overshoot_tolerance,
      max_normalised_hedge_error = max_normalised_hedge_error
    )
    sizing_n <<- sizing_n + 1L
    sizing$entry_attempt_id <- sizing_n
    sizing$proposed_strategy_trade_id <- proposed
    sizing$entry_timestamp <- data$timestamp[i]
    sizing$y_raw_contract <- data$y_contract[i]
    sizing$x_raw_contract <- data$x_contract[i]
    sizing$y_exact_contract <- y_match$spec$BloombergSecurityResolved[[1L]]
    sizing$x_exact_contract <- x_match$spec$BloombergSecurityResolved[[1L]]
    sizing$y_bfix_date <- y_fx$bfix_date
    sizing$x_bfix_date <- x_fx$bfix_date
    sizings[[sizing_n]] <<- sizing
    if (!isTRUE(sizing$feasibility[[1L]])) {
      add_event(i, "entry_rejected_sizing", "entry", proposed, side,
                "integer_position_infeasible", signal_spread, sizing$failure_reason[[1L]])
      return(FALSE)
    }
    if (!future_close_exists(i, sizing$signed_y_quantity, sizing$signed_x_quantity)) {
      add_event(i, "entry_rejected_no_exit_horizon", "entry", proposed, side,
                "no_later_eligible_side_specific_close", signal_spread)
      return(FALSE)
    }
    trade_counter <<- proposed
    segment_id <- paste0("trade", proposed, "_segment01")
    y_result <- make_fill(
      i, "y", y_generic, sizing$signed_y_quantity, "strategy_entry", proposed,
      segment_id, sizing$K_hat_usd_per_log_spread, sizing$K_ideal_usd_per_log_spread
    )
    x_result <- make_fill(
      i, "x", x_generic, sizing$signed_x_quantity, "strategy_entry", proposed,
      segment_id, sizing$K_hat_usd_per_log_spread, sizing$K_ideal_usd_per_log_spread
    )
    mab_assert(!is.null(y_result$fill) && !is.null(x_result$fill),
               "Entry quote eligibility changed unexpectedly during fill construction.")
    current <<- list(
      trade_id = proposed, side = side, beta = beta,
      signed_y_quantity = sizing$signed_y_quantity[[1L]],
      signed_x_quantity = sizing$signed_x_quantity[[1L]],
      entry_signal_time = data$timestamp[i], entry_fill_time = data$timestamp[i],
      entry_active_time = as.numeric(data$active_time[i]),
      entry_signal_spread = signal_spread,
      y_contract_entry = data$y_contract[i], x_contract_entry = data$x_contract[i],
      K_ideal_entry = sizing$K_ideal_usd_per_log_spread[[1L]],
      K_hat_entry = sizing$K_hat_usd_per_log_spread[[1L]],
      target_gross_notional_usd = sizing$target_gross_notional_usd[[1L]],
      segment_number = 1L, roll_count = 0L, active_segments = NULL
    )
    start_segments(
      i, 1L, y_match$spec, x_match$spec,
      y_result$fill$fill_price_displayed, x_result$fill$fill_price_displayed,
      data$y_price[i], data$x_price[i], y_result$fill$fill_id, x_result$fill$fill_id,
      y_result$fill$total_fee_usd, x_result$fill$total_fee_usd, "strategy_entry"
    )
    add_event(i, "entry_fill", "entry", proposed, side, "eligible", signal_spread)
    TRUE
  }

  close_trade <- function(i, signal_spread, reason, forced = FALSE) {
    y_quote <- mab_quote_for_quantity_change(data, i, "y", -current$signed_y_quantity)
    x_quote <- mab_quote_for_quantity_change(data, i, "x", -current$signed_x_quantity)
    if (!isTRUE(y_quote$eligible[[1L]]) || !isTRUE(x_quote$eligible[[1L]])) return(FALSE)
    segment_id <- current$active_segments$y$segment_id
    y_result <- make_fill(
      i, "y", y_generic, -current$signed_y_quantity,
      if (forced) "forced_window_exit" else "strategy_exit", current$trade_id,
      segment_id, current$K_hat_entry, current$K_ideal_entry
    )
    x_result <- make_fill(
      i, "x", x_generic, -current$signed_x_quantity,
      if (forced) "forced_window_exit" else "strategy_exit", current$trade_id,
      segment_id, current$K_hat_entry, current$K_ideal_entry
    )
    close_active_segments(
      i, y_result$fill$fill_price_displayed, x_result$fill$fill_price_displayed,
      data$y_price[i], data$x_price[i], reason,
      y_result$fill$fill_id, x_result$fill$fill_id,
      y_result$fill$total_fee_usd, x_result$fill$total_fee_usd
    )
    append_trade(
      i, signal_spread, reason,
      if (forced) "force_closed_realised" else "completed_realised", TRUE
    )
    add_event(i, if (forced) "forced_exit_fill" else "exit_fill", "exit",
              current$trade_id, current$side, reason, signal_spread)
    current <<- NULL
    TRUE
  }

  handle_roll <- function(i) {
    outgoing_i <- i - 1L
    old_trade_id <- current$trade_id
    old_side <- current$side
    old_y <- current$active_segments$y$raw_contract
    old_x <- current$active_segments$x$raw_contract
    new_y_match <- mab_fill_contract_spec(data, i, "y", y_generic, contract_specs)
    new_x_match <- mab_fill_contract_spec(data, i, "x", x_generic, contract_specs)
    y_fx <- mab_align_fx(data$timestamp[i], new_y_match$spec$PnLCurrency[[1L]], bfix, max_fx_age_days)
    x_fx <- mab_align_fx(data$timestamp[i], new_x_match$spec$PnLCurrency[[1L]], bfix, max_fx_age_days)
    drift <- mab_roll_notional_drift(
      data, i, current$side, beta, current$signed_y_quantity, current$signed_x_quantity,
      new_y_match$spec, new_x_match$spec,
      y_fx$fx_rate_usd_per_native[[1L]], x_fx$fx_rate_usd_per_native[[1L]],
      current$target_gross_notional_usd
    )
    if (roll_policy == "seamless_continuation") {
      close_active_segments(
        outgoing_i, data$y_price[outgoing_i], data$x_price[outgoing_i],
        data$y_price[outgoing_i], data$x_price[outgoing_i], "seamless_roll_segment_end"
      )
      new_segment <- current$segment_number + 1L
      start_segments(
        i, new_segment, new_y_match$spec, new_x_match$spec,
        data$y_price[i], data$x_price[i], data$y_price[i], data$x_price[i],
        reason = "seamless_roll_segment_start"
      )
      current$roll_count <<- current$roll_count + 1L
      roll_n <<- roll_n + 1L
      rolls[[roll_n]] <<- cbind(data.frame(
        strategy_trade_id = old_trade_id, roll_number = current$roll_count,
        roll_policy = roll_policy, roll_timestamp = data$timestamp[i],
        outgoing_timestamp = data$timestamp[outgoing_i],
        outgoing_y_contract = old_y, incoming_y_contract = data$y_contract[i],
        outgoing_x_contract = old_x, incoming_x_contract = data$x_contract[i],
        y_quantity_preserved = current$signed_y_quantity,
        x_quantity_preserved = current$signed_x_quantity,
        roll_bidask_cost_usd = 0, brokerage_usd = 0, other_fees_usd = 0,
        roll_bidask_cost_basis = "no_execution_seamless",
        total_roll_fees_usd = 0, equivalent_roll_fee_log_spread = 0,
        signal_roll_offset_delta = data$roll_offset_delta[i],
        cumulative_signal_offset = data$cumulative_roll_offset[i],
        roll_success = TRUE, failure_reason = NA_character_,
        stringsAsFactors = FALSE
      ), drift)
      add_event(i, "roll_complete", "roll", old_trade_id, old_side,
                "seamless_accounting_continuation", data$adjusted_signal_spread[i])
      return(TRUE)
    }
    quote_plan <- mab_roll_quote_plan(
      data, outgoing_i, i, current$signed_y_quantity, current$signed_x_quantity
    )
    if (!isTRUE(quote_plan$eligible)) {
      roll_n <<- roll_n + 1L
      rolls[[roll_n]] <<- cbind(data.frame(
        strategy_trade_id = old_trade_id, roll_number = current$roll_count + 1L,
        roll_policy = roll_policy, roll_timestamp = data$timestamp[i],
        outgoing_timestamp = data$timestamp[outgoing_i],
        outgoing_y_contract = old_y, incoming_y_contract = data$y_contract[i],
        outgoing_x_contract = old_x, incoming_x_contract = data$x_contract[i],
        y_quantity_preserved = current$signed_y_quantity,
        x_quantity_preserved = current$signed_x_quantity,
        roll_bidask_cost_usd = NA_real_, brokerage_usd = NA_real_, other_fees_usd = NA_real_,
        roll_bidask_cost_basis = "unavailable_failed_roll",
        total_roll_fees_usd = NA_real_, equivalent_roll_fee_log_spread = NA_real_,
        signal_roll_offset_delta = data$roll_offset_delta[i],
        cumulative_signal_offset = data$cumulative_roll_offset[i],
        roll_success = FALSE, failure_reason = quote_plan$missing_sides,
        stringsAsFactors = FALSE
      ), drift)
      append_trade(i, data$adjusted_signal_spread[i], "roll_failed",
                   "roll_failed_unresolved", FALSE)
      add_event(i, "roll_failed", "roll", old_trade_id, old_side,
                "required_roll_quote_not_valid", data$adjusted_signal_spread[i], quote_plan$missing_sides)
      add_diagnostic("roll_execution_failure", "error", i, quote_plan$missing_sides, old_trade_id)
      current <<- NULL
      accounting_complete <<- FALSE
      block_entries <<- TRUE
      return(FALSE)
    }
    old_k <- current$K_hat_entry
    segment_id <- current$active_segments$y$segment_id
    y_close <- make_fill(outgoing_i, "y", y_generic, -current$signed_y_quantity,
                         "roll_close", old_trade_id, segment_id, old_k, current$K_ideal_entry)
    x_close <- make_fill(outgoing_i, "x", x_generic, -current$signed_x_quantity,
                         "roll_close", old_trade_id, segment_id, old_k, current$K_ideal_entry)
    close_active_segments(
      outgoing_i, y_close$fill$fill_price_displayed, x_close$fill$fill_price_displayed,
      data$y_price[outgoing_i], data$x_price[outgoing_i], "explicit_roll_close",
      y_close$fill$fill_id, x_close$fill$fill_id,
      y_close$fill$total_fee_usd, x_close$fill$total_fee_usd
    )
    new_segment <- current$segment_number + 1L
    new_segment_id <- paste0("trade", old_trade_id, "_segment", sprintf("%02d", new_segment))
    y_open <- make_fill(i, "y", y_generic, current$signed_y_quantity,
                        "roll_open", old_trade_id, new_segment_id, drift$K_hat_at_roll, current$K_ideal_entry)
    x_open <- make_fill(i, "x", x_generic, current$signed_x_quantity,
                        "roll_open", old_trade_id, new_segment_id, drift$K_hat_at_roll, current$K_ideal_entry)
    start_segments(
      i, new_segment, new_y_match$spec, new_x_match$spec,
      y_open$fill$fill_price_displayed, x_open$fill$fill_price_displayed,
      data$y_price[i], data$x_price[i], y_open$fill$fill_id, x_open$fill$fill_id,
      y_open$fill$total_fee_usd, x_open$fill$total_fee_usd, "explicit_roll_open"
    )
    current$roll_count <<- current$roll_count + 1L
    roll_fill_rows <- mab_bind_rows(list(y_close$fill, x_close$fill, y_open$fill, x_open$fill))
    roll_fees <- sum(roll_fill_rows$total_fee_usd)
    roll_brokerage <- sum(roll_fill_rows$brokerage_usd)
    close_cost <- sum(c(
      current$signed_y_quantity * (data$y_price[outgoing_i] - y_close$fill$fill_price_displayed) *
        y_close$fill$point_value_native_per_displayed_point * y_close$fill$pnl_fx_rate_usd_per_native,
      current$signed_x_quantity * (data$x_price[outgoing_i] - x_close$fill$fill_price_displayed) *
        x_close$fill$point_value_native_per_displayed_point * x_close$fill$pnl_fx_rate_usd_per_native
    ))
    open_cost <- sum(c(
      current$signed_y_quantity * (y_open$fill$fill_price_displayed - data$y_price[i]) *
        y_open$fill$point_value_native_per_displayed_point * y_open$fill$pnl_fx_rate_usd_per_native,
      current$signed_x_quantity * (x_open$fill$fill_price_displayed - data$x_price[i]) *
        x_open$fill$point_value_native_per_displayed_point * x_open$fill$pnl_fx_rate_usd_per_native
    ))
    eq <- mab_equivalent_log_fee(roll_fees, drift$K_hat_at_roll, current$K_ideal_entry)
    roll_n <<- roll_n + 1L
    rolls[[roll_n]] <<- cbind(data.frame(
      strategy_trade_id = old_trade_id, roll_number = current$roll_count,
      roll_policy = roll_policy, roll_timestamp = data$timestamp[i],
      outgoing_timestamp = data$timestamp[outgoing_i],
      outgoing_y_contract = old_y, incoming_y_contract = data$y_contract[i],
      outgoing_x_contract = old_x, incoming_x_contract = data$x_contract[i],
      y_quantity_preserved = current$signed_y_quantity,
      x_quantity_preserved = current$signed_x_quantity,
      roll_bidask_cost_usd = close_cost + open_cost,
      roll_bidask_cost_basis = "immediate_fill_slippage_at_causal_roll_fx",
      brokerage_usd = roll_brokerage,
      other_fees_usd = roll_fees - roll_brokerage,
      total_roll_fees_usd = roll_fees,
      equivalent_roll_fee_log_spread = eq$value,
      signal_roll_offset_delta = data$roll_offset_delta[i],
      cumulative_signal_offset = data$cumulative_roll_offset[i],
      roll_success = TRUE, failure_reason = NA_character_,
      stringsAsFactors = FALSE
    ), drift)
    add_event(outgoing_i, "roll_close", "roll", old_trade_id, old_side,
              "explicit_outgoing_close", data$adjusted_signal_spread[outgoing_i])
    add_event(i, "roll_open", "roll", old_trade_id, old_side,
              "explicit_incoming_open", data$adjusted_signal_spread[i])
    add_event(i, "roll_complete", "roll", old_trade_id, old_side,
              "explicit_close_reopen", data$adjusted_signal_spread[i])
    TRUE
  }

  for (i in seq_len(n)) {
    if (!is.null(current) && isTRUE(data$roll_boundary[i])) {
      handle_roll(i)
      path_side[i] <- if (is.null(current)) NA_character_ else current$side
      path_trade[i] <- if (is.null(current)) NA_integer_ else current$trade_id
      next
    }
    signal_spread <- data$adjusted_signal_spread[i]
    if (!is.null(current)) {
      if (mab_exit_signal(signal_spread, current$side, rule)) {
        add_event(i, "exit_signal", "exit", current$trade_id, current$side,
                  "threshold_condition_met", signal_spread)
        if (!close_trade(i, signal_spread, "threshold_exit", FALSE)) {
          add_event(i, "exit_rejected_missing_quotes", "exit", current$trade_id, current$side,
                    "required_quote_not_valid", signal_spread)
        }
      }
    } else if (!block_entries) {
      side <- mab_entry_side(signal_spread, rule)
      if (!is.na(side)) open_trade(i, side, signal_spread)
    }
    path_side[i] <- if (is.null(current)) NA_character_ else current$side
    path_trade[i] <- if (is.null(current)) NA_integer_ else current$trade_id
  }

  if (!is.null(current)) {
    eligible_i <- NA_integer_
    for (i in rev(seq_len(n))) {
      if (data$timestamp[i] < current$entry_fill_time) next
      if (!identical(as.character(data$y_contract[i]), current$active_segments$y$raw_contract) ||
          !identical(as.character(data$x_contract[i]), current$active_segments$x$raw_contract)) next
      yq <- mab_quote_for_quantity_change(data, i, "y", -current$signed_y_quantity)
      xq <- mab_quote_for_quantity_change(data, i, "x", -current$signed_x_quantity)
      if (isTRUE(yq$eligible[[1L]]) && isTRUE(xq$eligible[[1L]])) {
        eligible_i <- i
        break
      }
    }
    if (is.na(eligible_i)) {
      append_trade(n, NA_real_, "forced_end_unavailable", "unresolved_dropped", FALSE)
      add_event(n, "trade_dropped", "drop", current$trade_id, current$side,
                "no_eligible_force_close", data$adjusted_signal_spread[n])
      add_diagnostic("unclosed_trade", "error", n,
                     "No eligible side-specific closing observation exists at or after entry.", current$trade_id)
      accounting_complete <- FALSE
      current <- NULL
    } else {
      close_trade(eligible_i, data$adjusted_signal_spread[eligible_i],
                  "forced_end_of_window", TRUE)
    }
  }

  fills_df <- mab_bind_rows(fills)
  segments_df <- mab_bind_rows(segments)
  trades_df <- mab_bind_rows(trades)
  events_df <- mab_bind_rows(events)
  sizing_df <- mab_bind_rows(sizings)
  rolls_df <- mab_bind_rows(rolls)
  diagnostics_df <- mab_bind_rows(diagnostics)
  contract_audit_df <- mab_bind_rows(contract_audits)
  if (nrow(trades_df) && any(!trades_df$accounting_complete)) accounting_complete <- FALSE
  summary <- mab_state_machine_summary(
    data, trades_df, events_df, sizing_df, rolls_df,
    pair_committed_capital_usd, accounting_complete
  )
  path <- if (isTRUE(include_path)) data.frame(
    timestamp = data$timestamp,
    raw_spread = data$spread,
    adjusted_signal_spread = data$adjusted_signal_spread,
    cumulative_roll_offset = data$cumulative_roll_offset,
    strategy_side = path_side,
    strategy_trade_id = path_trade,
    stringsAsFactors = FALSE
  ) else NULL
  output <- list(
    fills = fills_df,
    segments = segments_df,
    roll_events = rolls_df,
    trades = trades_df,
    events = events_df,
    sizing_audit = sizing_df,
    contract_join_audit = contract_audit_df,
    signal_roll_audit = adjusted$roll_audit,
    summary = summary,
    diagnostics = diagnostics_df,
    path = path,
    settings = list(
      scenario_id = scenario_id,
      roll_policy = roll_policy,
      price_mode = "realised_bidask",
      tradeable_time_rule = "side_specific",
      execution_timing = "same_observation",
      end_of_window_policy = "force_close",
      pair_committed_capital_usd = pair_committed_capital_usd,
      gross_notional_multiplier = gross_notional_multiplier,
      max_fx_age_days = max_fx_age_days,
      formation_cost_deducted = FALSE,
      embedded_bidask_cost_deducted_again = FALSE,
      equivalent_log_fee_deducted = FALSE,
      apply_explicit_fees = apply_explicit_fees
    )
  )
  class(output) <- "monetary_threshold_backtest"
  if (isTRUE(verbose)) message("Monetary task complete: ", nrow(trades_df), " strategy trades.")
  output
}
