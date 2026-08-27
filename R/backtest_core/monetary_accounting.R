mab_quote_for_quantity_change <- function(data, i, leg, quantity_change) {
  mab_assert(leg %in% c("y", "x"), "Leg must be y or x.")
  delta <- as.numeric(quantity_change)
  mab_assert(length(delta) == 1L && is.finite(delta) && delta != 0,
             "A contract-side fill requires one non-zero quantity change.")
  side <- if (delta > 0) "ask" else "bid"
  price_col <- paste0(leg, "_", side)
  valid_col <- paste0(leg, "_", side, "_valid")
  market_col <- paste0(leg, "_market_uncrossed")
  midpoint_col <- paste0(leg, "_price")
  required <- c(price_col, midpoint_col)
  mab_assert(all(required %in% names(data)), paste0("Canonical data lacks ", paste(required, collapse = ", "), "."))
  price <- as.numeric(data[[price_col]][i])
  midpoint <- as.numeric(data[[midpoint_col]][i])
  valid <- is.finite(price) && price > 0 && is.finite(midpoint) && midpoint > 0
  if (valid_col %in% names(data)) valid <- valid && isTRUE(data[[valid_col]][i])
  if (market_col %in% names(data)) valid <- valid && isTRUE(data[[market_col]][i])
  data.frame(
    leg = leg,
    quantity_change = delta,
    quote_side = side,
    fill_price = if (valid) price else NA_real_,
    midpoint = midpoint,
    eligible = valid,
    failure_reason = if (valid) NA_character_ else paste0("required_", leg, "_", side, "_not_valid"),
    stringsAsFactors = FALSE
  )
}

mab_fill_contract_spec <- function(data, i, leg, generic, specs) {
  contract_col <- paste0(leg, "_contract")
  mab_assert(contract_col %in% names(data), paste0("Canonical data lacks ", contract_col, "."))
  match <- mab_match_contract_spec_one(data[[contract_col]][i], generic, specs)
  if (is.null(match$spec)) {
    stop(
      "Exact-contract specification join failed for ", generic, ":",
      as.character(data[[contract_col]][i]), ".", call. = FALSE
    )
  }
  match
}

mab_make_monetary_fill <- function(
    fill_id, strategy_trade_id, segment_id, action_type,
    data, i, leg, generic, quantity_change,
    specs, bfix, fee_config, max_fx_age_days,
    apply_explicit_fees, k_hat = NA_real_, k_ideal = NA_real_) {
  quote <- mab_quote_for_quantity_change(data, i, leg, quantity_change)
  if (!isTRUE(quote$eligible[[1L]])) {
    return(list(fill = NULL, quote = quote, contract_audit = data.frame()))
  }
  matched <- mab_fill_contract_spec(data, i, leg, generic, specs)
  spec <- matched$spec
  pnl_fx <- mab_align_fx(data$timestamp[i], spec$PnLCurrency[[1L]], bfix, max_fx_age_days)
  fees <- mab_calculate_fill_fees(
    quantity_change = quantity_change,
    spec = spec,
    timestamp = data$timestamp[i],
    fee_config = fee_config,
    bfix = bfix,
    max_fx_age_days = max_fx_age_days,
    apply_explicit_fees = apply_explicit_fees,
    fill_price_displayed = quote$fill_price[[1L]]
  )
  equivalent <- mab_equivalent_log_fee(fees$total_fee_usd[[1L]], k_hat, k_ideal)
  fill <- data.frame(
    fill_id = as.integer(fill_id),
    strategy_trade_id = as.integer(strategy_trade_id),
    segment_id = as.character(segment_id),
    action_type = as.character(action_type),
    timestamp = data$timestamp[i],
    global_row_index = if ("global_row_index" %in% names(data)) data$global_row_index[i] else NA_integer_,
    leg = leg,
    generic = generic,
    raw_execution_contract = as.character(data[[paste0(leg, "_contract")]][i]),
    exact_contract = as.character(spec$BloombergSecurityResolved[[1L]]),
    contract_month = as.character(spec$ContractMonth[[1L]]),
    root = as.character(spec$Root[[1L]]),
    exchange = as.character(spec$ExchangeCode[[1L]]),
    quantity_change = as.numeric(quantity_change),
    contracts_charged = abs(as.numeric(quantity_change)),
    quote_side = quote$quote_side,
    fill_price_displayed = quote$fill_price,
    midpoint_displayed = quote$midpoint,
    point_value_native_per_displayed_point = as.numeric(spec$PointValueNativePerDisplayedPoint[[1L]]),
    tick_size_displayed = as.numeric(spec$MinimumPriceIncrementDisplayed[[1L]]),
    tick_value_native = as.numeric(spec$TickValueNative[[1L]]),
    native_currency = as.character(spec$PnLCurrency[[1L]]),
    pnl_fx_rate_usd_per_native = pnl_fx$fx_rate_usd_per_native,
    pnl_bfix_date = pnl_fx$bfix_date,
    pnl_bfix_publication_timestamp = pnl_fx$publication_timestamp,
    pnl_fx_age_calendar_days = pnl_fx$fx_age_calendar_days,
    pnl_fx_carry_forward = pnl_fx$carry_forward_flag,
    brokerage_native = fees$brokerage_native,
    exchange_fee_native = fees$exchange_fee_native,
    clearing_fee_native = fees$clearing_fee_native,
    regulatory_fee_native = fees$regulatory_fee_native,
    total_fee_native = fees$total_fee_native,
    fee_currency = fees$fee_currency,
    fee_fx_rate = fees$fee_fx_rate,
    brokerage_usd = fees$brokerage_usd,
    exchange_fee_usd = fees$exchange_fee_usd,
    clearing_fee_usd = fees$clearing_fee_usd,
    regulatory_fee_usd = fees$regulatory_fee_usd,
    total_fee_usd = fees$total_fee_usd,
    equivalent_log_spread_fee = equivalent$value,
    equivalent_fee_scale_usd = equivalent$scale,
    equivalent_fee_scale_basis = equivalent$basis,
    fee_matching_method = fees$fee_matching_method,
    stringsAsFactors = FALSE
  )
  list(fill = fill, quote = quote, contract_audit = matched$audit)
}

mab_new_segment <- function(strategy_trade_id, segment_number, leg, signed_quantity,
                            contract_spec, raw_contract, entry_timestamp,
                            entry_price, entry_midpoint, entry_fill_id = NA_integer_,
                            entry_reason = "strategy_entry") {
  list(
    strategy_trade_id = as.integer(strategy_trade_id),
    segment_number = as.integer(segment_number),
    segment_id = paste0("trade", strategy_trade_id, "_segment", sprintf("%02d", segment_number)),
    leg = as.character(leg),
    signed_quantity = as.numeric(signed_quantity),
    spec = contract_spec,
    raw_contract = as.character(raw_contract),
    entry_timestamp = mab_time(entry_timestamp),
    entry_price = as.numeric(entry_price),
    entry_midpoint = as.numeric(entry_midpoint),
    entry_fill_id = as.integer(entry_fill_id),
    entry_reason = as.character(entry_reason)
  )
}

mab_close_segment <- function(segment, exit_timestamp, exit_price, exit_midpoint,
                              exit_reason, bfix, max_fx_age_days,
                              entry_fee_usd = 0, exit_fee_usd = 0,
                              exit_fill_id = NA_integer_, tolerance = 1e-8) {
  spec <- segment$spec
  point <- as.numeric(spec$PointValueNativePerDisplayedPoint[[1L]])
  tick <- as.numeric(spec$MinimumPriceIncrementDisplayed[[1L]])
  tick_value <- as.numeric(spec$TickValueNative[[1L]])
  quantity <- as.numeric(segment$signed_quantity)
  native_gross <- quantity * (as.numeric(exit_price) - segment$entry_price) * point
  tick_parity <- quantity * ((as.numeric(exit_price) - segment$entry_price) / tick) * tick_value
  parity_error <- native_gross - tick_parity
  mab_assert(abs(parity_error) <= tolerance * max(1, abs(native_gross), abs(tick_parity)),
             "Segment point-value and tick-value P&L do not reconcile.")
  native_midpoint <- quantity * (as.numeric(exit_midpoint) - segment$entry_midpoint) * point
  fx <- mab_align_fx(exit_timestamp, spec$PnLCurrency[[1L]], bfix, max_fx_age_days)
  gross_usd <- native_gross * fx$fx_rate_usd_per_native[[1L]]
  midpoint_usd <- native_midpoint * fx$fx_rate_usd_per_native[[1L]]
  fees <- as.numeric(entry_fee_usd) + as.numeric(exit_fee_usd)
  data.frame(
    strategy_trade_id = segment$strategy_trade_id,
    segment_id = segment$segment_id,
    segment_number = segment$segment_number,
    leg = segment$leg,
    signed_quantity = quantity,
    raw_contract = segment$raw_contract,
    exact_contract = as.character(spec$BloombergSecurityResolved[[1L]]),
    contract_month = as.character(spec$ContractMonth[[1L]]),
    native_currency = as.character(spec$PnLCurrency[[1L]]),
    entry_timestamp = segment$entry_timestamp,
    exit_timestamp = mab_time(exit_timestamp),
    entry_price_displayed = segment$entry_price,
    exit_price_displayed = as.numeric(exit_price),
    entry_midpoint_displayed = segment$entry_midpoint,
    exit_midpoint_displayed = as.numeric(exit_midpoint),
    point_value_native_per_displayed_point = point,
    tick_size_displayed = tick,
    tick_value_native = tick_value,
    native_gross_pnl = native_gross,
    native_midpoint_pnl = native_midpoint,
    tick_parity_native_pnl = tick_parity,
    tick_parity_error = parity_error,
    pnl_fx_rate_usd_per_native = fx$fx_rate_usd_per_native,
    pnl_bfix_date = fx$bfix_date,
    pnl_bfix_publication_timestamp = fx$publication_timestamp,
    pnl_fx_age_calendar_days = fx$fx_age_calendar_days,
    pnl_fx_carry_forward = fx$carry_forward_flag,
    gross_usd_pnl = gross_usd,
    midpoint_usd_pnl = midpoint_usd,
    embedded_bidask_cost_usd = midpoint_usd - gross_usd,
    entry_fee_usd = as.numeric(entry_fee_usd),
    exit_fee_usd = as.numeric(exit_fee_usd),
    total_explicit_fees_usd = fees,
    net_usd_pnl = gross_usd - fees,
    entry_fill_id = segment$entry_fill_id,
    exit_fill_id = as.integer(exit_fill_id),
    entry_reason = segment$entry_reason,
    segment_reason = as.character(exit_reason),
    stringsAsFactors = FALSE
  )
}
