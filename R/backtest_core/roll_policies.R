mab_validate_roll_policy <- function(roll_policy) {
  match.arg(roll_policy, c("seamless_continuation", "explicit_close_reopen"))
}

mab_roll_quote_plan <- function(data, outgoing_i, incoming_i,
                                signed_y_quantity, signed_x_quantity) {
  requirements <- list(
    outgoing_y = mab_quote_for_quantity_change(data, outgoing_i, "y", -signed_y_quantity),
    outgoing_x = mab_quote_for_quantity_change(data, outgoing_i, "x", -signed_x_quantity),
    incoming_y = mab_quote_for_quantity_change(data, incoming_i, "y", signed_y_quantity),
    incoming_x = mab_quote_for_quantity_change(data, incoming_i, "x", signed_x_quantity)
  )
  audit <- mab_bind_rows(lapply(names(requirements), function(name) {
    row <- requirements[[name]]
    row$roll_fill_stage <- name
    row$observation_index <- if (grepl("outgoing", name)) outgoing_i else incoming_i
    row
  }))
  list(
    eligible = nrow(audit) == 4L && all(audit$eligible),
    missing_sides = paste(audit$failure_reason[!audit$eligible], collapse = ";"),
    audit = audit,
    requirements = requirements
  )
}

mab_roll_notional_drift <- function(
    data, incoming_i, side, beta, signed_y_quantity, signed_x_quantity,
    y_spec, x_spec, y_fx, x_fx, target_gross_notional_usd) {
  y_notional <- abs(signed_y_quantity) * data$y_price[incoming_i] *
    y_spec$PointValueNativePerDisplayedPoint[[1L]] * y_fx
  x_notional <- abs(signed_x_quantity) * data$x_price[incoming_i] *
    x_spec$PointValueNativePerDisplayedPoint[[1L]] * x_fx
  actual_ratio <- x_notional / y_notional
  scale <- mab_effective_spread_scale(
    side = side,
    beta = beta,
    signed_y_quantity = signed_y_quantity,
    signed_x_quantity = signed_x_quantity,
    y_price = data$y_price[incoming_i],
    x_price = data$x_price[incoming_i],
    y_point_value = y_spec$PointValueNativePerDisplayedPoint[[1L]],
    x_point_value = x_spec$PointValueNativePerDisplayedPoint[[1L]],
    y_fx = y_fx,
    x_fx = x_fx
  )
  data.frame(
    incoming_y_notional_usd = y_notional,
    incoming_x_notional_usd = x_notional,
    incoming_gross_notional_usd = y_notional + x_notional,
    gross_notional_drift_usd = y_notional + x_notional - target_gross_notional_usd,
    target_notional_ratio = abs(beta),
    incoming_actual_notional_ratio = actual_ratio,
    beta_approximation_error_after_roll = actual_ratio - abs(beta),
    K_hat_at_roll = scale$k_hat,
    K_hat_at_roll_stable = scale$stable,
    stringsAsFactors = FALSE
  )
}
