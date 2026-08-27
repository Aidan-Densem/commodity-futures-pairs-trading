mab_quantity_candidates <- function(ideal, minimum_lot, increment, radius = 4L) {
  mab_assert(is.finite(ideal) && ideal >= 0 && is.finite(minimum_lot) && minimum_lot > 0 &&
               is.finite(increment) && increment > 0,
             "Quantity candidate inputs must be finite and positive.")
  base_steps <- round((ideal - minimum_lot) / increment)
  base <- minimum_lot + max(0, base_steps) * increment
  candidates <- base + seq.int(-as.integer(radius), as.integer(radius)) * increment
  candidates <- unique(c(minimum_lot, candidates[candidates >= minimum_lot]))
  sort(candidates)
}

mab_effective_spread_scale <- function(
    side, beta, signed_y_quantity, signed_x_quantity,
    y_price, x_price, y_point_value, x_point_value, y_fx, x_fx) {
  side_sign <- if (identical(as.character(side), "long")) 1 else if (
    identical(as.character(side), "short")) -1 else NA_real_
  mab_assert(is.finite(side_sign), "Strategy side must be long or short.")
  A <- signed_y_quantity * y_price * y_point_value * y_fx
  B <- signed_x_quantity * x_price * x_point_value * x_fx
  k_hat <- side_sign * (A - beta * B) / (1 + beta^2)
  stable <- is.finite(k_hat) && k_hat > sqrt(.Machine$double.eps)
  list(
    A = A,
    B = B,
    k_hat = if (stable) k_hat else NA_real_,
    stable = stable,
    target_y_sign = side_sign,
    target_x_sign = sign(-side_sign * beta)
  )
}

mab_size_position <- function(
    beta,
    side,
    pair_committed_capital_usd,
    gross_notional_multiplier,
    y_price,
    x_price,
    y_spec,
    x_spec,
    y_fx,
    x_fx,
    notional_overshoot_tolerance = 0.05,
    max_normalised_hedge_error = 0.25,
    local_search_radius = 4L) {
  beta <- as.numeric(beta)
  capital <- as.numeric(pair_committed_capital_usd)
  multiplier <- as.numeric(gross_notional_multiplier)
  values <- c(beta, capital, multiplier, y_price, x_price, y_fx, x_fx)
  mab_assert(all(is.finite(values)) && capital > 0 && multiplier > 0 &&
               y_price > 0 && x_price > 0 && y_fx > 0 && x_fx > 0,
             "Sizing inputs must be finite, with positive capital, prices, multiplier, and FX.")
  mab_assert(abs(beta) > 1e-12,
             "A two-leg executable monetary position requires a non-zero frozen beta.")
  mab_assert(side %in% c("long", "short"), "Sizing side must be long or short.")
  target_gross <- capital * multiplier
  y_point <- as.numeric(y_spec$PointValueNativePerDisplayedPoint[[1L]])
  x_point <- as.numeric(x_spec$PointValueNativePerDisplayedPoint[[1L]])
  y_value <- abs(y_price) * y_point * y_fx
  x_value <- abs(x_price) * x_point * x_fx
  mab_assert(all(is.finite(c(y_value, x_value))) && y_value > 0 && x_value > 0,
             "USD notional per contract must be finite and positive.")
  ideal_y <- target_gross / ((1 + abs(beta)) * y_value)
  ideal_x <- abs(beta) * target_gross / ((1 + abs(beta)) * x_value)
  y_min <- as.numeric(y_spec$MinimumLotContracts[[1L]])
  x_min <- as.numeric(x_spec$MinimumLotContracts[[1L]])
  y_inc <- as.numeric(y_spec$LotIncrementContracts[[1L]])
  x_inc <- as.numeric(x_spec$LotIncrementContracts[[1L]])
  y_candidates <- mab_quantity_candidates(ideal_y, y_min, y_inc, local_search_radius)
  x_candidates <- mab_quantity_candidates(ideal_x, x_min, x_inc, local_search_radius)
  grid <- expand.grid(y_quantity = y_candidates, x_quantity = x_candidates)
  grid$y_notional_usd <- grid$y_quantity * y_value
  grid$x_notional_usd <- grid$x_quantity * x_value
  grid$actual_gross_notional_usd <- grid$y_notional_usd + grid$x_notional_usd
  grid$target_notional_ratio <- abs(beta)
  grid$actual_notional_ratio <- grid$x_notional_usd / grid$y_notional_usd
  grid$beta_approximation_error <- grid$actual_notional_ratio - abs(beta)
  ratio_denominator <- max(abs(beta), 1e-8)
  grid$normalised_hedge_error <- abs(grid$beta_approximation_error) / ratio_denominator
  grid$gross_target_error <- abs(grid$actual_gross_notional_usd - target_gross) / target_gross
  grid$unused_target_fraction <- pmax(target_gross - grid$actual_gross_notional_usd, 0) / target_gross
  grid$overshoot_fraction <- pmax(grid$actual_gross_notional_usd - target_gross, 0) / target_gross
  grid$objective <- 4 * grid$normalised_hedge_error + grid$gross_target_error +
    0.5 * grid$unused_target_fraction + 3 * grid$overshoot_fraction
  grid$within_overshoot <- grid$overshoot_fraction <= notional_overshoot_tolerance + 1e-12
  grid$within_hedge_tolerance <- grid$normalised_hedge_error <= max_normalised_hedge_error + 1e-12
  feasible_grid <- grid[grid$within_overshoot & grid$within_hedge_tolerance, , drop = FALSE]
  chosen_pool <- if (nrow(feasible_grid)) feasible_grid else grid
  chosen_pool <- chosen_pool[order(
    chosen_pool$objective, chosen_pool$normalised_hedge_error,
    chosen_pool$gross_target_error, chosen_pool$actual_gross_notional_usd,
    chosen_pool$y_quantity, chosen_pool$x_quantity
  ), , drop = FALSE]
  chosen <- chosen_pool[1L, , drop = FALSE]
  feasible <- nrow(feasible_grid) > 0L
  side_sign <- if (side == "long") 1 else -1
  signed_y <- side_sign * chosen$y_quantity
  signed_x <- sign(-side_sign * beta) * chosen$x_quantity
  scale <- mab_effective_spread_scale(
    side, beta, signed_y, signed_x, y_price, x_price, y_point, x_point, y_fx, x_fx
  )
  k_ideal <- target_gross / (1 + abs(beta))
  data.frame(
    sizing_method = "nearest_increment_plus_deterministic_local_search",
    strategy_side = side,
    pair_committed_capital_usd = capital,
    gross_notional_multiplier = multiplier,
    target_gross_notional_usd = target_gross,
    beta = beta,
    y_price_for_sizing = y_price,
    x_price_for_sizing = x_price,
    y_point_value_native = y_point,
    x_point_value_native = x_point,
    y_fx_usd_per_native = y_fx,
    x_fx_usd_per_native = x_fx,
    y_usd_notional_per_contract = y_value,
    x_usd_notional_per_contract = x_value,
    ideal_y_quantity = ideal_y,
    ideal_x_quantity = ideal_x,
    executable_y_quantity = chosen$y_quantity,
    executable_x_quantity = chosen$x_quantity,
    signed_y_quantity = signed_y,
    signed_x_quantity = signed_x,
    y_leg_notional_usd = chosen$y_notional_usd,
    x_leg_notional_usd = chosen$x_notional_usd,
    actual_gross_notional_usd = chosen$actual_gross_notional_usd,
    unused_allocation_usd = pmax(capital - chosen$actual_gross_notional_usd, 0),
    gross_notional_overshoot_usd = pmax(chosen$actual_gross_notional_usd - target_gross, 0),
    target_notional_ratio = chosen$target_notional_ratio,
    actual_notional_ratio = chosen$actual_notional_ratio,
    beta_approximation_error = chosen$beta_approximation_error,
    normalised_hedge_error = chosen$normalised_hedge_error,
    objective_value = chosen$objective,
    notional_overshoot_tolerance = notional_overshoot_tolerance,
    max_normalised_hedge_error = max_normalised_hedge_error,
    K_ideal_usd_per_log_spread = k_ideal,
    K_hat_usd_per_log_spread = scale$k_hat,
    K_hat_stable = scale$stable,
    feasibility = feasible && scale$stable,
    failure_reason = if (!feasible) {
      paste0(
        "No local-search integer position satisfied both the gross-notional overshoot and ",
        "beta-approximation tolerances."
      )
    } else if (!scale$stable) {
      "Actual integer exposure produced a non-positive or unstable monetary spread scale."
    } else {
      NA_character_
    },
    stringsAsFactors = FALSE
  )
}

mab_equivalent_log_fee <- function(fee_usd, k_hat = NA_real_, k_ideal = NA_real_) {
  fee <- as.numeric(fee_usd)
  mab_assert(length(fee) == 1L && is.finite(fee) && fee >= 0,
             "Monetary fee must be finite and non-negative.")
  if (is.finite(k_hat) && k_hat > sqrt(.Machine$double.eps)) {
    return(list(value = fee / k_hat, scale = k_hat, basis = "actual_integer_K_hat"))
  }
  if (is.finite(k_ideal) && k_ideal > sqrt(.Machine$double.eps)) {
    return(list(value = fee / k_ideal, scale = k_ideal, basis = "ideal_K"))
  }
  list(value = NA_real_, scale = NA_real_, basis = "unavailable_invalid_scale")
}

mab_minimum_practical_gross_notional <- function(
    beta, y_price, x_price, y_spec, x_spec, y_fx, x_fx,
    max_normalised_hedge_error = 0.25,
    notional_overshoot_tolerance = 0.05,
    search_steps = 24L) {
  beta <- abs(as.numeric(beta))
  mab_assert(is.finite(beta) && beta > 1e-12,
             "Minimum practical notional requires a non-zero finite beta.")
  y_value <- abs(y_price) * y_spec$PointValueNativePerDisplayedPoint[[1L]] * y_fx
  x_value <- abs(x_price) * x_spec$PointValueNativePerDisplayedPoint[[1L]] * x_fx
  y_min <- y_spec$MinimumLotContracts[[1L]]; x_min <- x_spec$MinimumLotContracts[[1L]]
  y_inc <- y_spec$LotIncrementContracts[[1L]]; x_inc <- x_spec$LotIncrementContracts[[1L]]
  round_lot <- function(value, minimum, increment) {
    minimum + pmax(0, round((value - minimum) / increment)) * increment
  }
  x_seeds <- x_min + seq.int(0L, as.integer(search_steps)) * x_inc
  y_for_x <- round_lot(x_seeds * x_value / (beta * y_value), y_min, y_inc)
  y_seeds <- y_min + seq.int(0L, as.integer(search_steps)) * y_inc
  x_for_y <- round_lot(beta * y_seeds * y_value / x_value, x_min, x_inc)
  grid <- unique(rbind(
    data.frame(y_quantity = y_for_x, x_quantity = x_seeds),
    data.frame(y_quantity = y_seeds, x_quantity = x_for_y)
  ))
  grid$y_notional_usd <- grid$y_quantity * y_value
  grid$x_notional_usd <- grid$x_quantity * x_value
  grid$actual_ratio <- grid$x_notional_usd / grid$y_notional_usd
  grid$normalised_hedge_error <- abs(grid$actual_ratio - beta) / beta
  grid$gross_notional_usd <- grid$y_notional_usd + grid$x_notional_usd
  feasible <- grid[grid$normalised_hedge_error <= max_normalised_hedge_error + 1e-12, , drop = FALSE]
  mab_assert(nrow(feasible) > 0L,
             "No practical minimum-lot hedge was found within the deterministic search bound.")
  feasible <- feasible[order(feasible$gross_notional_usd, feasible$normalised_hedge_error,
                             feasible$y_quantity, feasible$x_quantity), , drop = FALSE]
  chosen <- feasible[1L, , drop = FALSE]
  data.frame(
    beta_absolute = beta,
    y_usd_notional_per_contract = y_value,
    x_usd_notional_per_contract = x_value,
    minimum_practical_y_quantity = chosen$y_quantity,
    minimum_practical_x_quantity = chosen$x_quantity,
    minimum_practical_gross_notional_usd = chosen$gross_notional_usd,
    minimum_target_gross_notional_usd =
      chosen$gross_notional_usd / (1 + notional_overshoot_tolerance),
    actual_notional_ratio = chosen$actual_ratio,
    normalised_hedge_error = chosen$normalised_hedge_error,
    max_normalised_hedge_error = max_normalised_hedge_error,
    notional_overshoot_tolerance = notional_overshoot_tolerance,
    method = "minimum-lot ratio search; no performance input",
    stringsAsFactors = FALSE
  )
}
