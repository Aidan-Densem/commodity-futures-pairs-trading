# Formation-stage candidate construction on prepared exact-contract pair paths.
# Structural transition annotations are created upstream on the unfiltered raw
# synchronised topology. Statistical estimation uses statistical midpoints;
# implementability and costs use the separately frozen V2 execution cleaner.

formation_candidate_failure <- function(window, pair, reason) {
  data.frame(
    endpoint_id = window$endpoint_id,
    endpoint_session_date = as.Date(window$endpoint_session_date),
    pair_id = pair$pair_id, y_generic = pair$y_generic,
    x_generic = pair$x_generic,
    formation_start = as.POSIXct(window$formation_start, tz = "Europe/London"),
    formation_end = as.POSIXct(window$formation_end, tz = "Europe/London"),
    testing_start = as.POSIXct(window$testing_start, tz = "Europe/London"),
    testing_end = as.POSIXct(window$testing_end, tz = "Europe/London"),
    testing_session_dates = as.character(window$testing_session_dates),
    testing_sessions = as.integer(window$testing_sessions),
    half_life_sessions = NA_real_, cost_adjusted_opportunity = NA_real_,
    robust_spread_scale = NA_real_, v2_cost_primary = NA_real_,
    N_opp = NA_integer_, N_clean = NA_integer_,
    clean_share_of_raw_two_leg_opportunities = NA_real_,
    technical_object_valid = FALSE, whole_contract_implementable = FALSE,
    v2_cost_status_valid = FALSE, final_quote_feasibility_pass = FALSE,
    adf05_pass = FALSE, candidate_failure_reason = as.character(reason),
    stringsAsFactors = FALSE
  )
}

formation_adf05_final_levels <- function(spread, segment_id = NULL,
                                         minimum_observations = 30L) {
  # Production convention: apply tseries::adf.test to every
  # finite, chronologically ordered formation-only frozen-spread level.  This
  # level-sequence gate does not define an OU/Levy transition and therefore
  # does not replace the segment-safe transition masks used downstream.
  usable <- is.finite(spread)
  out <- formation_adf05(spread[usable], minimum_observations)
  out$adf_sample_convention <- "all_finite_formation_frozen_spread_levels"
  out$adf_observations <- sum(usable)
  out$adf_segment_count_diagnostic <- if (is.null(segment_id)) NA_integer_ else
    length(unique(segment_id[usable & !is.na(segment_id)]))
  out
}

match_pair_specs <- function(contract_y, contract_x, pair, contract_specs) {
  gy <- if (grepl("1$", pair$y_generic)) pair$y_generic else paste0(pair$y_generic, "1")
  gx <- if (grepl("1$", pair$x_generic)) pair$x_generic else paste0(pair$x_generic, "1")
  ym <- mab_match_contract_spec_one(contract_y, gy, contract_specs)
  xm <- mab_match_contract_spec_one(contract_x, gx, contract_specs)
  if (is.null(ym$spec) || is.null(xm$spec)) stop("exact_contract_metadata_unavailable", call. = FALSE)
  list(y = ym$spec, x = xm$spec)
}

calibrate_pair_execution_cleaner <- function(formation, pair, contract_specs,
                                             production_config) {
  matches <- lapply(seq_len(nrow(formation)), function(i) match_pair_specs(
    formation$contract_y[[i]], formation$contract_x[[i]], pair, contract_specs
  ))
  y_ticks <- vapply(matches, function(z) z$y$MinimumPriceIncrementDisplayed[[1L]], numeric(1L))
  x_ticks <- vapply(matches, function(z) z$x$MinimumPriceIncrementDisplayed[[1L]], numeric(1L))
  leg <- function(name) data.frame(
    timestamp = formation$timestamp,
    bid = formation[[paste0("bid_", name)]],
    ask = formation[[paste0("ask_", name)]],
    close = formation[[paste0("close_", name)]], stringsAsFactors = FALSE
  )
  y_contract <- calibrate_execution_quote_leg(
    leg("y"), stats::median(y_ticks), production_config$quote_quality
  )
  x_contract <- calibrate_execution_quote_leg(
    leg("x"), stats::median(x_ticks), production_config$quote_quality
  )
  list(
    data = apply_frozen_pair_quote_cleaner(formation, y_contract, x_contract, "formation"),
    y_contract = y_contract, x_contract = x_contract, matches = matches
  )
}

formation_prospective_costs <- function(cleaned, matches, pair, beta,
                                        contract_specs, bfix, fee_config,
                                        production_config) {
  endpoint_candidates <- which(cleaned$execution_quote_clean %in% TRUE)
  if (!length(endpoint_candidates)) stop("whole_contract_position_infeasible", call. = FALSE)
  endpoint_row <- tail(endpoint_candidates, 1L)
  endpoint_timestamp <- cleaned$timestamp[[endpoint_row]]
  endpoint_specs <- matches[[endpoint_row]]
  endpoint_y_contract <- cleaned$contract_y[[endpoint_row]]
  endpoint_x_contract <- cleaned$contract_x[[endpoint_row]]
  rows <- endpoint_candidates[
    cleaned$contract_y[endpoint_candidates] == endpoint_y_contract &
      cleaned$contract_x[endpoint_candidates] == endpoint_x_contract
  ]
  y_fx <- mab_align_fx(
    endpoint_timestamp, endpoint_specs$y$PnLCurrency[[1L]], bfix
  )$fx_rate_usd_per_native[[1L]]
  x_fx <- mab_align_fx(
    endpoint_timestamp, endpoint_specs$x$PnLCurrency[[1L]], bfix
  )$fx_rate_usd_per_native[[1L]]
  applicable_fee_rows <- lapply(
    list(endpoint_specs$y, endpoint_specs$x),
    function(spec) lapply(c("buy", "sell"), function(side) {
      mab_resolve_fee_rows(spec, endpoint_timestamp, fee_config, side)
    })
  )
  applicable_fee_rows <- unlist(applicable_fee_rows, recursive = FALSE)
  applicable_fee_rows <- applicable_fee_rows[
    vapply(applicable_fee_rows, function(x) is.data.frame(x) && nrow(x), logical(1L))
  ]
  fee_currencies <- if (length(applicable_fee_rows)) unique(unlist(lapply(
    applicable_fee_rows, function(x) as.character(x$fee_currency)
  ))) else character()
  all_currencies <- unique(c(
    endpoint_specs$y$PnLCurrency[[1L]], endpoint_specs$x$PnLCurrency[[1L]], fee_currencies
  ))
  endpoint_fx <- setNames(vapply(all_currencies, function(currency) {
    mab_align_fx(endpoint_timestamp, currency, bfix)$fx_rate_usd_per_native[[1L]]
  }, numeric(1L)), all_currencies)
  sizing_by_side <- setNames(lapply(c("long", "short"), function(side) {
    mab_size_position(
      beta = beta, side = side,
      pair_committed_capital_usd = production_config$pair_sleeve_usd,
      gross_notional_multiplier = 1,
      y_price = cleaned$midpoint_y[[endpoint_row]],
      x_price = cleaned$midpoint_x[[endpoint_row]],
      y_spec = endpoint_specs$y, x_spec = endpoint_specs$x,
      y_fx = y_fx, x_fx = x_fx,
      notional_overshoot_tolerance = production_config$integer_sizing$maximum_gross_notional_overshoot,
      max_normalised_hedge_error = production_config$integer_sizing$maximum_normalised_hedge_error
    )
  }), c("long", "short"))
  if (!all(vapply(sizing_by_side, function(z) isTRUE(z$feasibility[[1L]]), logical(1L)))) {
    stop("whole_contract_position_infeasible", call. = FALSE)
  }
  costs <- vector("list", length(rows) * 2L); k <- 0L
  feasible <- logical(length(rows))
  for (j in seq_along(rows)) {
    i <- rows[[j]]
    for (side in c("long", "short")) {
      sizing <- sizing_by_side[[side]]
      k <- k + 1L
      costs[[k]] <- formation_roundtrip_event_cost(
        cleaned$timestamp[[i]], side, beta, sizing,
        cleaned$bid_y[[i]], cleaned$ask_y[[i]],
        cleaned$bid_x[[i]], cleaned$ask_x[[i]],
        endpoint_specs$y, endpoint_specs$x, y_fx, x_fx, fee_config, bfix,
        fee_timestamp = endpoint_timestamp,
        fixed_k_hat = sizing$K_hat_usd_per_log_spread[[1L]],
        fee_fx_override = endpoint_fx
      )
      feasible[[j]] <- TRUE
    }
  }
  if (!k) stop("whole_contract_position_infeasible", call. = FALSE)
  table <- do.call(rbind, costs[seq_len(k)])
  list(
    event_costs = table,
    primary = trimmed_prospective_cost(
      table$total_roundtrip_log, production_config$cost_proxy$trim_fraction
    ),
    endpoint_timestamp = endpoint_timestamp,
    endpoint_contract_y = endpoint_y_contract,
    endpoint_contract_x = endpoint_x_contract,
    endpoint_y_fx = y_fx, endpoint_x_fx = x_fx,
    endpoint_fx = endpoint_fx,
    sizing_by_side = sizing_by_side,
    event_feasible_share = mean(feasible),
    whole_contract_implementable = all(feasible)
  )
}

formation_candidate_metrics_one <- function(window, pair, pair_series,
                                             contract_specs, bfix, fee_config,
                                             production_config) {
  pair <- as.list(pair[1L, , drop = FALSE])
  fail <- function(reason) formation_candidate_failure(window, pair, reason)
  tryCatch({
    start <- as.POSIXct(window$formation_start, tz = production_config$timezone)
    end <- as.POSIXct(window$formation_end, tz = production_config$timezone)
    formation <- pair_series[pair_series$timestamp >= start & pair_series$timestamp <= end, , drop = FALSE]
    if (nrow(formation) < 31L) stop("too_few_raw_synchronous_formation_opportunities")
    if (!all(c("transition_valid", "structural_segment_id", "active_dt_minutes",
               "calendar_session_date") %in% names(formation))) {
      stop("prepared_pair_path_lacks_segment_safe_active_clock")
    }
    statistical_rows <- which(formation$statistical_quote_valid %in% TRUE &
                                is.finite(formation$midpoint_y) & is.finite(formation$midpoint_x))
    if (length(statistical_rows) < 31L) stop("too_few_statistical_formation_midpoints")
    statistical <- formation[statistical_rows, , drop = FALSE]
    active_dt <- resolve_active_time_increments(formation, statistical_rows)
    segments <- statistical$structural_segment_id
    parameters <- fit_formation_kalman_parameters(
      log(statistical$midpoint_y), log(statistical$midpoint_x), active_dt,
      segment_id = segments
    )
    fit <- estimate_formation_kalman_hedge(
      formation, q = parameters$q, ve = parameters$ve
    )
    transition_valid <- statistical$transition_valid %in% TRUE
    ou <- estimate_exact_gaussian_ou(
      fit$frozen$spread, active_dt, transition_valid
    )
    sessions <- unique(statistical$calendar_session_date[!is.na(statistical$calendar_session_date)])
    active_per_session <- sum(active_dt[transition_valid], na.rm = TRUE) / length(sessions)
    if (!is.finite(active_per_session) || active_per_session <= 0) {
      stop("formation_active_minutes_per_session_unavailable")
    }
    half_life_sessions <- ou$half_life_active_minutes / active_per_session
    adf <- formation_adf05_final_levels(fit$frozen$spread, segments)
    robust_scale <- stats::mad(fit$frozen$spread, constant = 1.4826, na.rm = TRUE)

    cleaner <- calibrate_pair_execution_cleaner(
      formation, pair, contract_specs, production_config
    )
    feasibility <- quote_feasibility_summary(cleaner$data, production_config$quote_rule)
    cost <- formation_prospective_costs(
      cleaner$data, cleaner$matches, pair, fit$frozen$beta,
      contract_specs, bfix, fee_config, production_config
    )
    data.frame(
      endpoint_id = window$endpoint_id,
      endpoint_session_date = as.Date(window$endpoint_session_date),
      pair_id = pair$pair_id, y_generic = pair$y_generic, x_generic = pair$x_generic,
      formation_start = start, formation_end = end,
      testing_start = as.POSIXct(window$testing_start, tz = production_config$timezone),
      testing_end = as.POSIXct(window$testing_end, tz = production_config$timezone),
      testing_session_dates = as.character(window$testing_session_dates),
      testing_sessions = as.integer(window$testing_sessions),
      alpha = fit$frozen$alpha, beta = fit$frozen$beta,
      formation_centre = fit$frozen$centre,
      q = parameters$q, v_e = parameters$ve,
      kappa_per_active_minute = ou$kappa_per_active_minute,
      ou_equilibrium = ou$ou_equilibrium,
      gaussian_diffusion_scale = ou$gaussian_diffusion_scale,
      gaussian_stationary_sd = ou$gaussian_stationary_sd,
      testing_active_minutes = as.integer(round(active_per_session * window$testing_sessions)),
      half_life_sessions = half_life_sessions,
      robust_spread_scale = robust_scale,
      v2_cost_primary = cost$primary,
      cost_adjusted_opportunity = robust_scale / cost$primary,
      cost_endpoint_timestamp = as.POSIXct(cost$endpoint_timestamp, tz = production_config$timezone),
      cost_endpoint_contract_y = cost$endpoint_contract_y,
      cost_endpoint_contract_x = cost$endpoint_contract_x,
      cost_endpoint_y_fx_usd_per_native = cost$endpoint_y_fx,
      cost_endpoint_x_fx_usd_per_native = cost$endpoint_x_fx,
      cost_long_signed_y_quantity = cost$sizing_by_side$long$signed_y_quantity[[1L]],
      cost_long_signed_x_quantity = cost$sizing_by_side$long$signed_x_quantity[[1L]],
      cost_short_signed_y_quantity = cost$sizing_by_side$short$signed_y_quantity[[1L]],
      cost_short_signed_x_quantity = cost$sizing_by_side$short$signed_x_quantity[[1L]],
      cost_long_frozen_K_hat = cost$sizing_by_side$long$K_hat_usd_per_log_spread[[1L]],
      cost_short_frozen_K_hat = cost$sizing_by_side$short$K_hat_usd_per_log_spread[[1L]],
      prospective_cost_fx_convention = "endpoint_BFIX_frozen_throughout_formation_proxy",
      adf_statistic = adf$adf_statistic, adf_p_value = adf$adf_p_value,
      adf_sample_convention = adf$adf_sample_convention,
      adf_observations = adf$adf_observations,
      adf_segment_count_diagnostic = adf$adf_segment_count_diagnostic,
      N_opp = feasibility$N_opp, N_clean = feasibility$N_clean,
      clean_share_of_raw_two_leg_opportunities =
        feasibility$clean_share_of_raw_two_leg_opportunities,
      sessions_with_at_least_60_clean = feasibility$sessions_with_at_least_60_clean,
      whole_contract_event_feasible_share = cost$event_feasible_share,
      quote_quality_contract_y = cleaner$y_contract$quote_quality_version,
      quote_quality_contract_x = cleaner$x_contract$quote_quality_version,
      cost_proxy_version = production_config$cost_proxy$version,
      technical_object_valid = TRUE,
      whole_contract_implementable = cost$whole_contract_implementable,
      v2_cost_status_valid = is.finite(cost$primary) && cost$primary > 0,
      final_quote_feasibility_pass = feasibility$final_quote_feasibility_pass,
      adf05_pass = adf$adf05_pass,
      candidate_failure_reason = NA_character_, stringsAsFactors = FALSE
    )
  }, error = function(e) fail(conditionMessage(e)))
}

build_formation_candidate_metrics <- function(windows, candidate_pairs,
                                              prepared_pair_series, contract_specs,
                                              bfix, fee_config,
                                              production_config) {
  required <- c("pair_id", "y_generic", "x_generic")
  if (length(setdiff(required, names(candidate_pairs)))) stop(
    "candidate_pairs.csv must contain pair_id, y_generic and x_generic.", call. = FALSE
  )
  if (!is.list(prepared_pair_series) || is.null(names(prepared_pair_series))) stop(
    "prepared_pair_series must be a named list keyed by pair_id.", call. = FALSE
  )
  rows <- vector("list", nrow(windows))
  for (i in seq_len(nrow(windows))) {
    pair_hit <- match(windows$pair_id[[i]], candidate_pairs$pair_id)
    if (is.na(pair_hit)) stop("Window references an unknown pair_id.", call. = FALSE)
    path <- prepared_pair_series[[as.character(windows$pair_id[[i]])]]
    if (is.null(path)) stop("Prepared pair path is unavailable: ", windows$pair_id[[i]], call. = FALSE)
    rows[[i]] <- formation_candidate_metrics_one(
      windows[i, , drop = FALSE], candidate_pairs[pair_hit, , drop = FALSE],
      path, contract_specs, bfix, fee_config, production_config
    )
  }
  fields <- unique(unlist(lapply(rows, names), use.names = FALSE))
  rows <- lapply(rows, function(x) {
    for (field in setdiff(fields, names(x))) x[[field]] <- NA
    x[fields]
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}
