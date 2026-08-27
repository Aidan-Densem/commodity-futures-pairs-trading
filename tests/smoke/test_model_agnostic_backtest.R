load_common_backtest_engine(.GlobalEnv)
quotes <- smoke_quotes()
sync <- synchronise_quote_legs(quotes$y, quotes$x)
sync$Active_Time_Minutes <- seq_len(nrow(sync)) - 1L

thresholds <- list(
  upper_entry = 0.05, lower_entry = -0.01,
  upper_exit = 0, lower_exit = 0
)
frozen <- list(alpha = log(100), beta = 1, centre = 0)
make_spec <- function(label) standard_strategy_spec(
  label, "Y_X", as.Date("2025-12-14"),
  min(sync$timestamp), max(sync$timestamp), thresholds, frozen, 200000
)

contract_specs <- data.frame(
  Generic = c("Y1", "X1"), Root = c("Y", "X"),
  ContractMonth = "2026-01",
  BloombergTickerOriginal = c("YF6", "XF6"),
  BloombergSecurityResolved = c("YF6 Comdty", "XF6 Comdty"),
  ExchangeCode = "TEST", PnLCurrency = "USD",
  PointValueNativePerDisplayedPoint = c(10, 1000),
  MinimumPriceIncrementDisplayed = c(0.01, 0.001),
  TickValueNative = c(0.1, 1),
  MinimumLotContracts = 1, LotIncrementContracts = 1,
  stringsAsFactors = FALSE
)
contract_specs$original_key <- mab_normalize_security_id(contract_specs$BloombergTickerOriginal)
contract_specs$resolved_key <- mab_normalize_security_id(contract_specs$BloombergSecurityResolved)

run_one <- function(label) {
  strategy <- make_spec(label)
  data <- prepare_midpoint_monetary_data(sync, strategy)
  backtest_monetary_spread_threshold_strategy(
    spread_data = data,
    threshold_table = strategy_threshold_table(strategy),
    contract_specs = contract_specs,
    bfix = data.frame(),
    fee_config = mab_fee_configuration(1),
    y_generic = "Y1", x_generic = "X1",
    pair_committed_capital_usd = 200000,
    scenario_id = "monetary_baseline_seamless",
    include_path = TRUE,
    verbose = FALSE
  )
}

gaussian <- run_one("Gaussian MC")
gh <- run_one("Strict-interior OU-GH")
fields <- c(
  "entry_fill_time", "exit_fill_time", "strategy_side", "gross_usd_pnl",
  "midpoint_usd_pnl", "total_explicit_fees_usd", "net_usd_pnl",
  "exit_reason", "forced_exit"
)
smoke_expect(identical(gaussian$trades[fields], gh$trades[fields]),
             "Different model labels changed trade economics")
smoke_expect(identical(gaussian$fills, gh$fills),
             "Different model labels changed fills")
smoke_expect(identical(gaussian$segments, gh$segments),
             "Different model labels changed segment accounting")

cat("MODEL_AGNOSTIC_BACKTEST_PASS\n")
