# Synthetic, redistribution-safe proof of the production roll-accounting route.
# The fixture deliberately inserts a large cross-maturity price-level jump.  A
# correct explicit close/reopen policy must never book that jump as strategy P&L.

wrapper_text <- paste(
  readLines(repo_path("scripts", "core", "06_run_exact_contract_backtest.R"), warn = FALSE),
  collapse = "\n"
)
smoke_expect(
  grepl('scenario_id = "monetary_realistic_explicit_roll"', wrapper_text, fixed = TRUE),
  "The core wrapper does not select the explicit close/reopen scenario."
)
smoke_expect(
  !grepl('scenario_id = "monetary_realistic_seamless_roll"', wrapper_text, fixed = TRUE),
  "The core wrapper still selects the seamless-roll scenario."
)

scenario <- mab_scenario_definitions("monetary_realistic_explicit_roll")
smoke_expect(
  identical(scenario$roll_policy[[1L]], "explicit_close_reopen") &&
    isTRUE(scenario$apply_explicit_fees[[1L]]),
  "The selected scenario is not registered as fee-bearing explicit close/reopen."
)

make_spec <- function(generic, root, month, ticker) data.frame(
  Generic = generic,
  Root = root,
  ContractMonth = month,
  BloombergTickerOriginal = ticker,
  BloombergSecurityResolved = paste(ticker, "Comdty"),
  ExchangeCode = "SYNTHETIC",
  PnLCurrency = "USD",
  ContractQuantity = 1,
  PointValueNativePerDisplayedPoint = 1,
  MinimumPriceIncrementDisplayed = 0.01,
  TickValueNative = 0.01,
  MinimumLotContracts = 1,
  LotIncrementContracts = 1,
  stringsAsFactors = FALSE
)

specs <- mab_bind_rows(list(
  make_spec("Y1", "Y", "2026-01", "YF6"),
  make_spec("Y1", "Y", "2026-02", "YG6"),
  make_spec("X1", "X", "2026-01", "XF6"),
  make_spec("X1", "X", "2026-02", "XG6")
))
specs$original_key <- mab_normalize_security_id(specs$BloombergTickerOriginal)
specs$resolved_key <- mab_normalize_security_id(specs$BloombergSecurityResolved)

timestamp <- as.POSIXct(
  sprintf("2026-01-15 10:%02d:00", 0:5), tz = "Europe/London"
)
y_mid <- c(100, 99, 99.5, 200, 200.5, 200.5)
x_mid <- c(1, 1, 1, 2, 2, 2)
spread <- c(0, -0.02, -0.015, -0.015, 0.001, 0.001)
fixture <- data.frame(
  timestamp = timestamp,
  global_row_index = seq_along(timestamp),
  active_time = seq_along(timestamp),
  y_price = y_mid,
  x_price = x_mid,
  alpha = log(100),
  beta = 1,
  spread = spread,
  y_bid = y_mid - 0.01,
  y_ask = y_mid + 0.01,
  x_bid = x_mid - 0.001,
  x_ask = x_mid + 0.001,
  y_bid_valid = TRUE,
  y_ask_valid = TRUE,
  x_bid_valid = TRUE,
  x_ask_valid = TRUE,
  y_market_uncrossed = TRUE,
  x_market_uncrossed = TRUE,
  y_contract = c(rep("YF6", 3L), rep("YG6", 3L)),
  x_contract = c(rep("XF6", 3L), rep("XG6", 3L)),
  stringsAsFactors = FALSE
)
rule <- data.frame(
  rule_type = "mean_exit",
  lower_entry = -0.01,
  upper_entry = 0.05,
  lower_exit = 0,
  upper_exit = 0,
  lower_band = NA_real_,
  upper_band = NA_real_,
  stringsAsFactors = FALSE
)

result <- backtest_monetary_spread_threshold_strategy(
  spread_data = fixture,
  threshold_table = rule,
  contract_specs = specs,
  bfix = data.frame(),
  fee_config = mab_fee_configuration(1),
  y_generic = "Y1",
  x_generic = "X1",
  pair_committed_capital_usd = 1000000,
  scenario_id = "monetary_realistic_explicit_roll"
)

roll_fills <- result$fills[result$fills$action_type %in% c("roll_close", "roll_open"), ]
smoke_equal(nrow(roll_fills), 4L, message = "A two-leg roll must create four fills.")
smoke_expect(
  setequal(unique(roll_fills$action_type), c("roll_close", "roll_open")),
  "The roll fill ledger lacks a close or reopen action."
)
smoke_equal(nrow(result$roll_events), 1L, message = "Expected one synthetic roll event.")
smoke_expect(
  isTRUE(result$roll_events$roll_success[[1L]]) &&
    result$roll_events$total_roll_fees_usd[[1L]] > 0 &&
    result$roll_events$roll_bidask_cost_usd[[1L]] > 0,
  "The explicit roll did not charge positive fees and bid/ask execution cost."
)
expected_roll_fees <- 2 * (
  abs(result$sizing_audit$signed_y_quantity[[1L]]) +
    abs(result$sizing_audit$signed_x_quantity[[1L]])
)
smoke_equal(
  result$roll_events$total_roll_fees_usd[[1L]], expected_roll_fees,
  message = "Roll fees do not reconcile to close plus reopen contract-sides."
)

smoke_equal(nrow(result$segments), 4L, message = "Each leg must have outgoing and incoming segments.")
smoke_expect(
  all(result$segments$segment_number %in% c(1L, 2L)) &&
    all(table(result$segments$leg) == 2L),
  "The explicit roll did not split both legs into two contract-homogeneous segments."
)
smoke_expect(
  all(abs(result$segments$exit_midpoint_displayed -
            result$segments$entry_midpoint_displayed) < 2),
  "The cross-maturity price-level jump leaked into segment P&L."
)
smoke_expect(
  all(result$segments$raw_contract[result$segments$segment_number == 1L] %in% c("YF6", "XF6")) &&
    all(result$segments$raw_contract[result$segments$segment_number == 2L] %in% c("YG6", "XG6")),
  "Outgoing and incoming exact contracts were not separated correctly."
)
smoke_expect(
  isTRUE(result$summary$accounting_complete) && nrow(result$trades) == 1L,
  "The synthetic explicit-roll trade did not reconcile completely."
)
cat("EXPLICIT_ROLL_ACCOUNTING_PASS\n")
