# Formation prospective costs use the same exact-contract fee/FX primitives as
# realised execution. Each clean event values entry and reversal of the actual
# integer position at contemporaneous bid/ask quotes.

roundtrip_leg_concession_usd <- function(quantity, bid, ask, point_value, fx) {
  values <- c(quantity, bid, ask, point_value, fx)
  if (any(!is.finite(values)) || quantity == 0 || bid <= 0 || ask < bid || point_value <= 0 || fx <= 0) {
    return(NA_real_)
  }
  abs(quantity) * (ask - bid) * point_value * fx
}

formation_roundtrip_event_cost <- function(timestamp, side, beta, sizing,
                                           y_bid, y_ask, x_bid, x_ask,
                                           y_spec, x_spec, y_fx, x_fx,
                                           fee_config, bfix, max_fx_age_days = 7L,
                                           fee_timestamp = timestamp,
                                           fixed_k_hat = NULL,
                                           fee_fx_override = NULL) {
  qy <- as.numeric(sizing$signed_y_quantity[[1L]])
  qx <- as.numeric(sizing$signed_x_quantity[[1L]])
  y_mid <- (y_bid + y_ask) / 2; x_mid <- (x_bid + x_ask) / 2
  scale <- if (is.null(fixed_k_hat)) mab_effective_spread_scale(
      side, beta, qy, qx, y_mid, x_mid,
      y_spec$PointValueNativePerDisplayedPoint[[1L]],
      x_spec$PointValueNativePerDisplayedPoint[[1L]], y_fx, x_fx
    ) else list(k_hat = as.numeric(fixed_k_hat), stable = is.finite(fixed_k_hat) && fixed_k_hat > 0)
  mab_assert(isTRUE(scale$stable), "Frozen integer spread sensitivity is unavailable.")
  concession <- roundtrip_leg_concession_usd(
    qy, y_bid, y_ask, y_spec$PointValueNativePerDisplayedPoint[[1L]], y_fx
  ) + roundtrip_leg_concession_usd(
    qx, x_bid, x_ask, x_spec$PointValueNativePerDisplayedPoint[[1L]], x_fx
  )
  leg_fees <- function(quantity, spec, entry_price, exit_price) {
    mab_calculate_fill_fees(quantity, spec, timestamp, fee_config, bfix, max_fx_age_days,
                            TRUE, entry_price, fee_timestamp, fee_fx_override)$total_fee_usd[[1L]] +
      mab_calculate_fill_fees(-quantity, spec, timestamp, fee_config, bfix, max_fx_age_days,
                              TRUE, exit_price, fee_timestamp, fee_fx_override)$total_fee_usd[[1L]]
  }
  y_entry <- if (qy > 0) y_ask else y_bid; y_exit <- if (qy > 0) y_bid else y_ask
  x_entry <- if (qx > 0) x_ask else x_bid; x_exit <- if (qx > 0) x_bid else x_ask
  fees <- leg_fees(qy, y_spec, y_entry, y_exit) + leg_fees(qx, x_spec, x_entry, x_exit)
  data.frame(
    side = side, bidask_roundtrip_usd = concession,
    explicit_fee_roundtrip_usd = fees, total_roundtrip_usd = concession + fees,
    effective_dollar_spread_sensitivity = scale$k_hat,
    total_roundtrip_log = (concession + fees) / scale$k_hat,
    signed_y_quantity = qy, signed_x_quantity = qx,
    frozen_y_fx_usd_per_native = y_fx, frozen_x_fx_usd_per_native = x_fx,
    fee_reference_timestamp = as.character(as.POSIXct(fee_timestamp, tz = "Europe/London")),
    stringsAsFactors = FALSE
  )
}

trimmed_prospective_cost <- function(event_costs, trim = production_config$cost_proxy$trim_fraction) {
  x <- as.numeric(event_costs[is.finite(event_costs) & event_costs >= 0])
  if (!length(x)) stop("No valid clean formation event costs.", call. = FALSE)
  mean(x, trim = trim)
}
