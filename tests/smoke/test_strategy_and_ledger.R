schedule <- data.frame(
  selected = TRUE, pair_id = "Y_X", endpoint_session_date = as.Date("2025-12-01"),
  alpha = 0, beta = 1, formation_centre = 0,
  testing_start = as.POSIXct("2025-12-02", tz = "Europe/London"),
  testing_end = as.POSIXct("2025-12-03 23:59:00", tz = "Europe/London"),
  testing_session_dates = "2025-12-02;2025-12-03", testing_sessions = 2L,
  stringsAsFactors = FALSE)
threshold <- data.frame(Pair = "Y_X", Session_Date = as.Date("2025-12-01"),
                        upper_entry = 1, lower_entry = -1, upper_exit = 0, lower_exit = 0)
specification <- build_strategy_specifications(threshold, schedule, "fixture")
smoke_expect(nrow(specification) == 1L && specification$model_label == "fixture",
             "Strategy-spec builder did not connect threshold to execution interface")

segment <- data.frame(
  leg = "y", entry_timestamp = as.POSIXct("2025-12-02 09:00", tz = "Europe/London"),
  exit_timestamp = as.POSIXct("2025-12-03 16:00", tz = "Europe/London"),
  raw_contract = "YF6", entry_price_displayed = 10, exit_price_displayed = 12,
  signed_quantity = 1, point_value_native_per_displayed_point = 10,
  pnl_fx_rate_usd_per_native = 1, entry_fee_usd = 1, exit_fee_usd = 1,
  segment_id = "s1", stringsAsFactors = FALSE)
monetary <- data.frame(
  timestamp = as.POSIXct(c("2025-12-02 16:00", "2025-12-03 16:00"), tz = "Europe/London"),
  y_contract = "YF6", x_contract = "XF6", y_price = c(11, 12), x_price = 1,
  stringsAsFactors = FALSE)
result <- list(segments = segment)
ledger <- mab_build_committed_capital_ledger(list(result), specification, list(monetary))
smoke_expect(nrow(ledger$ledger) == 2L, "Ledger did not retain both committed-capital sessions")
smoke_equal(sum(ledger$ledger$net_usd_pnl), 18, message = "Ledger P&L does not reconcile")
smoke_equal(ledger$ledger$committed_capital_usd, c(200000, 200000),
            message = "Ledger committed capital is wrong")
smoke_equal(ledger$ledger$return_committed,
            ledger$ledger$net_usd_pnl / ledger$ledger$committed_capital_usd,
            message = "Ledger return denominator is wrong")
cat("STRATEGY_SPEC_AND_DAILY_LEDGER_PASS\n")
