tz <- "Europe/London"
interval <- function(contract, session, open, close) data.frame(
  contract = contract, session_id = session,
  open_timestamp = as.POSIXct(open, tz = tz),
  close_timestamp = as.POSIXct(close, tz = tz),
  admissible = TRUE, stringsAsFactors = FALSE
)
ycal <- rbind(
  interval("YF6", "2026-01-09", "2026-01-09 09:00", "2026-01-09 17:00"),
  interval("YF6", "2026-01-12", "2026-01-12 09:00", "2026-01-12 17:00")
)
xcal <- rbind(
  interval("XF6", "2026-01-09", "2026-01-09 10:00", "2026-01-09 16:00"),
  interval("XF6", "2026-01-12", "2026-01-12 10:00", "2026-01-12 16:00")
)
smoke_equal(active_minutes_between(
  as.POSIXct("2026-01-09 10:00", tz = tz), as.POSIXct("2026-01-09 10:01", tz = tz),
  ycal, xcal, "YF6", "XF6"
), 1, message = "Active clock: one-minute case failed")
smoke_equal(active_minutes_between(
  as.POSIXct("2026-01-09 10:00", tz = tz), as.POSIXct("2026-01-09 10:05", tz = tz),
  ycal, xcal, "YF6", "XF6"
), 5, message = "Active clock: open five-minute gap was truncated")
smoke_equal(active_minutes_between(
  as.POSIXct("2026-01-09 15:59", tz = tz), as.POSIXct("2026-01-12 10:01", tz = tz),
  ycal, xcal, "YF6", "XF6"
), 2, message = "Active clock: weekend closure was counted")
smoke_equal(active_minutes_between(
  as.POSIXct("2026-01-09 09:30", tz = tz), as.POSIXct("2026-01-09 10:30", tz = tz),
  ycal, xcal, "YF6", "XF6"
), 30, message = "Active clock: calendar intersection failed")

topology <- data.frame(
  timestamp = as.POSIXct(c("2026-01-09 10:00", "2026-01-09 10:01",
                           "2026-01-09 10:02", "2026-01-09 10:03"), tz = tz),
  contract_y = c("YF6", "YF6", "YF6", "YG6"),
  contract_x = c("XF6", "XF6", "XF6", "XG6"),
  opportunity_index = 1:4,
  statistical_quote_valid = c(TRUE, FALSE, TRUE, TRUE), stringsAsFactors = FALSE
)
# Provide calendars for the incoming exact contracts as well.
ycal2 <- rbind(ycal, transform(ycal, contract = "YG6"))
xcal2 <- rbind(xcal, transform(xcal, contract = "XG6"))
clocked <- add_pair_active_clock(topology, ycal2, xcal2, tz)
smoke_expect(!clocked$transition_valid[[3L]],
             "Rejected observation was bridged by accepted neighbours")
smoke_expect(!clocked$transition_valid[[4L]],
             "Exact-contract lifecycle boundary was bridged")

kalman <- kalman_affine_filter(
  y = 1, x = 0, active_dt = 5, q = 0.2, ve = 0.1,
  initial_covariance = diag(2)
)
smoke_equal(diag(kalman$predicted_covariance_path[, , 1L]), c(2, 2),
            message = "Kalman q*Delta covariance increment changed")

lifecycle <- data.frame(
  generic = rep(c("Y", "X"), each = 2),
  contract = c("YF6", "YG6", "XF6", "XG6"),
  delivery_month = c("2026-01", "2026-02", "2026-01", "2026-02"),
  first_trade_date = as.Date(c("2025-01-01", "2026-01-01", "2025-01-01", "2026-01-01")),
  last_trade_date = as.Date(c("2026-01-20", "2026-02-20", "2026-01-22", "2026-02-22")),
  stringsAsFactors = FALSE
)
roll <- build_synchronous_pair_roll_schedule(
  lifecycle, "Y_X", "Y", "X", as.Date("2026-01-05"), as.Date("2026-01-31")
)
smoke_expect(roll$y_contract[[1L]] == "YF6" && roll$x_contract[[1L]] == "XF6",
             "Roll construction chose incorrect outgoing contracts")
smoke_expect(roll$roll_date[[1L]] == as.Date("2026-01-13"),
             "Roll date is not five weekdays before earlier last trade")
smoke_expect(roll$y_contract[[2L]] == "YG6" && roll$x_contract[[2L]] == "XG6" &&
               isTRUE(roll$causal_reference_only[[1L]]),
             "Synchronous incoming roll or causal flag failed")
exact_quotes <- data.frame(
  timestamp = rep(as.POSIXct(c("2026-01-12 15:59", "2026-01-13 10:00"), tz = tz), each = 2),
  generic = rep(c("Y", "X"), 2), contract = c("YF6", "XF6", "YG6", "XG6"),
  bid = c(99, 49, 199, 79), ask = c(101, 51, 201, 81),
  close = c(100, 50, 200, 80), stringsAsFactors = FALSE
)
active_exact <- construct_active_exact_pair_series(exact_quotes, roll, "Y", "X", tz)
smoke_expect(nrow(active_exact) == 2L && active_exact$roll_boundary[[2L]] &&
               active_exact$contract_y[[2L]] == "YG6" &&
               active_exact$contract_x[[2L]] == "XG6",
             "Exact-contract construction did not apply the synchronous roll schedule")
roll_ycal <- rbind(
  interval("YF6", "old", "2026-01-12 15:00", "2026-01-12 16:00"),
  interval("YG6", "new", "2026-01-13 10:00", "2026-01-13 11:00")
)
roll_xcal <- rbind(
  interval("XF6", "old", "2026-01-12 15:00", "2026-01-12 16:00"),
  interval("XG6", "new", "2026-01-13 10:00", "2026-01-13 11:00")
)
active_exact <- add_pair_active_clock(active_exact, roll_ycal, roll_xcal, tz)
smoke_expect(!active_exact$transition_valid[[2L]],
             "Cross-maturity roll jump became an ordinary model/P&L transition")

pair_rows <- do.call(rbind, lapply(seq_len(31L), function(i) data.frame(
  pair_id = "Y_X", calendar_session_date = as.Date("2026-01-01") + i - 1L,
  timestamp = as.POSIXct("2026-01-01 10:00", tz = tz) + (i - 1L) * 86400,
  statistical_quote_valid = TRUE, stringsAsFactors = FALSE
)))
windows <- build_pair_rolling_session_windows(pair_rows, 20L, 10L, 1L)
smoke_expect(nrow(windows) == 2L && all(windows$formation_sessions == 20L) &&
               all(windows$testing_sessions == 10L),
             "Pair-specific 20:10:1 rolling window contract failed")
pair_b <- pair_rows
pair_b$pair_id <- "B_C"; pair_b$calendar_session_date <- as.Date(pair_b$timestamp +
                                                                    c(0:3, 5:31) * 86400 -
                                                                    (0:30) * 86400)
pair_b$timestamp <- pair_rows$timestamp + c(0:3, 5:31) * 86400 - (0:30) * 86400
both_windows <- build_rolling_session_windows(rbind(pair_rows, pair_b), 20L, 10L, 1L)
a_first <- min(as.Date(both_windows$formation_end[both_windows$pair_id == "Y_X"]))
b_first <- min(as.Date(both_windows$formation_end[both_windows$pair_id == "B_C"]))
smoke_expect(b_first == a_first + 1,
             "A global date from another pair entered Pair B's session index")

mini_cfg <- production_config$quote_quality
mini_cfg$minimum_clean_formation_quotes <- 10L
mini_cfg$local_reference_minimum <- 5L
stamp <- as.POSIXct("2026-01-05 10:00", tz = tz) + 0:19 * 60
leg <- data.frame(timestamp = stamp, bid = rep(99.99, 20), ask = rep(100.01, 20),
                  close = rep(100, 20))
frozen_cleaner <- calibrate_execution_quote_leg(leg, .01, mini_cfg)
leg$ask[[11L]] <- 200
assessment <- assess_execution_quote_leg(leg, frozen_cleaner)
smoke_expect(assessment$execution_quote_clean[[1L]], "Normal V2 quote did not pass")
smoke_expect(!assessment$execution_quote_clean[[11L]],
             "Frozen V2 cleaner accepted a formation-calibrated spread outlier")
invalid_leg <- leg; invalid_leg$bid[[1L]] <- NA_real_
invalid_assessment <- assess_execution_quote_leg(invalid_leg, frozen_cleaner)
smoke_expect(!invalid_assessment$execution_quote_clean[[1L]],
             "Invalid bid passed the V2 execution cleaner")
local_leg <- data.frame(timestamp = stamp, bid = rep(99.99, 20), ask = rep(100.01, 20),
                        close = rep(100, 20))
local_leg$bid[[20L]] <- 129.99; local_leg$ask[[20L]] <- 130.01; local_leg$close[[20L]] <- 130
local_assessment <- assess_execution_quote_leg(local_leg, frozen_cleaner)
smoke_expect(!local_assessment$execution_quote_clean[[20L]],
             "Local midpoint-level outlier passed the frozen cleaner")
contract_before <- serialize(frozen_cleaner, NULL)
invisible(assess_execution_quote_leg(local_leg, frozen_cleaner))
smoke_expect(identical(contract_before, serialize(frozen_cleaner, NULL)),
             "Trading observations recalibrated the formation-frozen cleaner")
smoke_expect(is.finite(accepted_quote_midpoint(99, 101)) &&
               !assess_execution_quote_leg(
                 data.frame(timestamp = stamp[[1L]], bid = 99, ask = 150, close = 124.5),
                 frozen_cleaner
               )$execution_quote_clean,
             "Statistical midpoint and executable-clean status are still conflated")
formation_quality <- data.frame(
  raw_simultaneous_opportunity = rep(TRUE, 20),
  execution_quote_clean = assessment$execution_quote_clean,
  calendar_session_date = rep(as.Date(c("2026-01-05", "2026-01-06")), each = 10)
)
rule <- list(minimum_clean_events = 10L, minimum_sessions_with_60_clean = 0L,
             minimum_conditional_clean_share = .90)
quality <- quote_feasibility_summary(formation_quality, rule)
smoke_expect(quality$N_opp == 20L && quality$N_clean == 19L &&
               quality$clean_share_of_raw_two_leg_opportunities == 19 / 20,
             "Quote feasibility denominator is not raw simultaneous opportunities")
y_more <- data.frame(
  timestamp = stamp[1:3], generic = "Y", contract = "YF6", bid = 99, ask = 101,
  close = 100
)
x_less <- data.frame(
  timestamp = stamp[1:2], generic = "X", contract = "XF6", bid = 49, ask = 51,
  close = 50
)
unequal_sync <- synchronise_quote_legs(y_more, x_less)
smoke_expect(nrow(unequal_sync) == 2L && sum(unequal_sync$raw_simultaneous_opportunity) == 2L,
             "Raw two-leg opportunity count used the longer one-leg row count")
smoke_equal(roundtrip_leg_concession_usd(2, 99, 101, 10, 1.5), 60,
            message = "Prospective monetary bid/ask concession formula failed")
smoke_equal(trimmed_prospective_cost(c(rep(1, 18), 100, 100), .10), 1,
            message = "Prospective cost is not the 10% trimmed mean")
event_sizing <- data.frame(signed_y_quantity = 2, signed_x_quantity = -3)
y_event_spec <- data.frame(
  PointValueNativePerDisplayedPoint = 10, PnLCurrency = "USD",
  resolved_key = "YF6", original_key = "YF6", Root = "Y", ExchangeCode = "TEST"
)
x_event_spec <- data.frame(
  PointValueNativePerDisplayedPoint = 20, PnLCurrency = "USD",
  resolved_key = "XF6", original_key = "XF6", Root = "X", ExchangeCode = "TEST"
)
event_cost <- formation_roundtrip_event_cost(
  stamp[[1L]], "long", 1, event_sizing,
  99, 101, 49, 51, y_event_spec, x_event_spec, 1, 1,
  mab_fee_configuration(1), data.frame()
)
smoke_equal(event_cost$bidask_roundtrip_usd, 160,
            message = "Two-leg prospective bid/ask event cost failed")
smoke_equal(event_cost$explicit_fee_roundtrip_usd, 10,
            message = "Two-leg prospective brokerage event cost failed")
smoke_equal(event_cost$total_roundtrip_log, 170 / 2500,
            message = "Prospective event K_hat normalisation failed")

selected <- data.frame(
  endpoint_id = "endpoint_20260109", endpoint_session_date = as.Date("2026-01-09"),
  pair_id = "Y_X", primary_rank = 1L, selected = TRUE,
  testing_start = as.POSIXct("2026-01-12 10:00", tz = tz),
  testing_end = as.POSIXct("2026-01-12 10:01", tz = tz),
  testing_session_dates = "2026-01-12", testing_sessions = 1L,
  stringsAsFactors = FALSE
)
threshold_table <- data.frame(
  candidate_id = c("a", "b"), d_plus = c(1, 2), d_minus = c(1, 2),
  objective_value = c(-.2, -.1), MC_standard_error = 0
)
outside <- v2_select_threshold(threshold_table, TRUE, 0, 1e-12)
smoke_expect(outside$route_status == "MODEL_NO_TRADE" &&
               !outside$strategy_available,
             "Non-positive threshold objective did not choose the outside option")
positive <- threshold_table; positive$objective_value[[1L]] <- .1
smoke_expect(v2_select_threshold(positive, TRUE, 0, 1e-12)$route_status == "TRADEABLE",
             "Positive finite-horizon objective did not select a threshold")
zero <- threshold_table; zero$objective_value <- c(0, -.1)
smoke_expect(v2_select_threshold(zero, TRUE, 0, 1e-12)$route_status == "MODEL_NO_TRADE",
             "Exactly zero objective did not choose the outside option")
selected_for_spec <- transform(
  selected,
  alpha = 0, beta = 1, formation_centre = 0
)
no_trade_threshold <- data.frame(
  Pair = "Y_X", Session_Date = as.Date("2026-01-09"),
  strategy_available = FALSE, route_status = "MODEL_NO_TRADE"
)
smoke_expect(nrow(build_strategy_specifications(
  no_trade_threshold, selected_for_spec, "G", 200000
)) == 0L, "Model no-trade route entered the monetary strategy interface")

levy_dt <- c(0, 1, 2, 5, 5)
levy_spread <- c(0.2, 0.1, -0.05, 0.3, 0.4)
levy_mu <- 0.03
levy_kappa <- 0.08
levy_remainder <- levy_exact_ou_remainder(
  levy_spread, levy_mu, levy_kappa, levy_dt
)
levy_expected <- levy_spread[-1L] - levy_mu -
  exp(-levy_kappa * levy_dt[-1L]) * (head(levy_spread, -1L) - levy_mu)
smoke_equal(levy_remainder[-1L], levy_expected,
            message = "Exact OU remainder did not use each realised duration")
levy_mask <- levy_empirical_transition_mask(
  c(FALSE, TRUE, TRUE, TRUE, FALSE), levy_dt, levy_remainder
)
smoke_expect(identical(which(levy_mask), 2:4) && sum(levy_mask) == 3L,
             paste(
               "Final Levy adapter did not admit exactly the observed 1/2/5-minute",
               "transitions or admitted a structural-boundary transition"
             ))

unavailable <- data.frame(
  pair_id = "Y_X", endpoint_session_date = as.Date("2026-01-09"),
  model_available = FALSE, model_reason = "structured_fit_failure"
)
routes <- build_model_route_manifest(selected, "GH", data.frame(), unavailable, 200000)
validate_model_route_manifest(routes, selected, "GH")
smoke_expect(routes$route_status == "MODEL_UNAVAILABLE" &&
               routes$selected_pair_sleeve_usd == 200000,
             "Unavailable model removed selected capital")
pair_path <- data.frame(timestamp = as.POSIXct(c(
  "2026-01-12 10:00", "2026-01-12 10:01"
), tz = tz))
ledger <- mab_build_selected_schedule_ledger(
  list(), data.frame(), list(), selected, routes, list(Y_X = pair_path)
)
smoke_expect(all(ledger$ledger$committed_capital_usd == 200000) &&
               all(ledger$ledger$net_usd_pnl == 0),
             "Idle unavailable route did not retain its committed sleeve")

selected_three <- do.call(rbind, lapply(c("A_B", "C_D", "E_F"), function(id) {
  transform(selected, pair_id = id)
}))
routes_three <- data.frame(
  endpoint_id = selected_three$endpoint_id,
  endpoint_session_date = selected_three$endpoint_session_date,
  pair_id = selected_three$pair_id, primary_rank = 1:3, model_label = "M",
  route_status = c("TRADEABLE", "MODEL_NO_TRADE", "MODEL_UNAVAILABLE"),
  route_reason = c("positive_optimised_objective", "outside_option_dominates", "fit_failure"),
  selected_pair_sleeve_usd = 200000, selection_preserved = TRUE,
  strategy_available = c(TRUE, FALSE, FALSE), stringsAsFactors = FALSE
)
tradeable_strategy <- data.frame(
  pair_id = "A_B", formation_endpoint = as.Date("2026-01-09"),
  testing_start = selected$testing_start, testing_end = selected$testing_end,
  upper_entry = 1, lower_entry = -1, upper_exit = 0, lower_exit = 0,
  alpha = 0, beta = 1, formation_centre = 0, pair_sleeve_usd = 200000,
  model_label = "M", stringsAsFactors = FALSE
)
empty_result <- list(segments = data.frame())
paths_three <- setNames(rep(list(pair_path), 3), selected_three$pair_id)
capital_three <- mab_build_selected_schedule_ledger(
  list(empty_result), tradeable_strategy, list(data.frame()),
  selected_three, routes_three, paths_three
)
smoke_expect(all(capital_three$ledger$committed_capital_usd == 600000) &&
               all(capital_three$ledger$net_usd_pnl == 0),
             "No-trade/unavailable selected sleeves disappeared from committed capital")

trade_stub <- list(
  trade_id = 1L, side = "long", entry_signal_time = as.POSIXct("2026-01-09 15:59", tz = tz),
  entry_fill_time = as.POSIXct("2026-01-09 15:59", tz = tz), entry_active_time = 100,
  entry_signal_spread = -1, beta = 1, y_contract_entry = "YF6", x_contract_entry = "XF6",
  roll_count = 0L, K_hat_entry = 1000, K_ideal_entry = 1000
)
holding <- mab_build_strategy_trade(
  trade_stub, data.frame(), data.frame(), as.POSIXct("2026-01-12 10:01", tz = tz),
  0, "unresolved", "unresolved", FALSE, 102
)
smoke_expect(holding$holding_time_minutes == 2 &&
               holding$holding_time_basis == "joint_pair_active_minutes",
             "Holding period used wall-clock rather than pair-active minutes")

daily <- data.frame(
  session_date = rep(as.Date("2026-01-01") + 0:5, 2),
  model_label = rep(c("A", "B"), each = 6),
  return_committed = c(-.02, -.01, 0, .01, .02, .01,
                       -.01, -.005, 0, .005, .01, .005)
)
hac <- pairwise_hac_return_differences(daily, 2L)
smoke_expect(nrow(hac) == 1L && is.finite(hac$hac_p_two_sided) &&
               is.finite(hac$holm_p_all_pairwise_mean_return_family),
             "Aligned pairwise HAC/Holm inference is unavailable")
risk <- daily_performance_metrics(daily$return_committed[daily$model_label == "A"], 252, 2)
smoke_expect(risk$historical_var_05 < 0 &&
               risk$historical_es_05 <= risk$historical_var_05,
             "Historical VaR/ES loss signs are incorrect")

cat("DATA_SPINE_ROUTE_AND_INFERENCE_CONTRACTS_PASS\n")
