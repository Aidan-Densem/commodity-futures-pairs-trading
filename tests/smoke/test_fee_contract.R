fee_table <- data.frame(
  fee_key_type = c("exchange", "exchange"), exact_contract = "", root = "",
  exchange = "TEST", fee_component = c("exchange", "clearing"),
  fee_basis = "per_contract", fee_rate = c(.50, .25), fee_currency = "USD",
  fee_unit = "per_contract_side", charge_side = "both",
  effective_from = "2020-01-01", effective_until = "2030-01-01",
  source = "synthetic verified fixture", verified = TRUE, stringsAsFactors = FALSE)
path <- tempfile(fileext = ".csv"); utils::write.csv(fee_table, path, row.names = FALSE)
config <- mab_fee_configuration(1, mab_read_fee_table(path), TRUE, TRUE, FALSE)
spec <- data.frame(resolved_key = "YF6", original_key = "YF6", Root = "Y",
                   ExchangeCode = "TEST", stringsAsFactors = FALSE)
entry_y <- mab_calculate_fill_fees(2, spec, as.POSIXct("2025-12-01", tz = "Europe/London"),
                                  config, data.frame())
entry_x <- mab_calculate_fill_fees(-3, spec, as.POSIXct("2025-12-01", tz = "Europe/London"),
                                  config, data.frame())
roundtrip <- 2 * (entry_y$total_fee_usd + entry_x$total_fee_usd)
smoke_equal(2 * (entry_y$brokerage_usd + entry_x$brokerage_usd), 10,
            message = "Fee: USD 1 brokerage arithmetic failed")
smoke_equal(2 * (entry_y$exchange_fee_usd + entry_x$exchange_fee_usd), 5,
            message = "Fee: exchange charge failed")
smoke_equal(2 * (entry_y$clearing_fee_usd + entry_x$clearing_fee_usd), 2.5,
            message = "Fee: clearing charge failed")
smoke_equal(roundtrip, 17.5, message = "Fee: total round trip failed")
smoke_equal(entry_y$regulatory_fee_usd + entry_x$regulatory_fee_usd, 0,
            message = "Fee: regulatory charge entered primary convention")
smoke_expect(isFALSE(config$apply_transaction_taxes), "Fee: taxes are enabled")

basis_table <- fee_table
basis_table$fee_basis <- c("per_physical_unit", "transaction_notional")
basis_table$fee_rate <- c(.01, .001)
basis_path <- tempfile(fileext = ".csv")
utils::write.csv(basis_table, basis_path, row.names = FALSE)
basis_config <- mab_fee_configuration(1, mab_read_fee_table(basis_path), TRUE, TRUE, FALSE)
basis_spec <- transform(spec, ContractQuantity = 50,
                        PointValueNativePerDisplayedPoint = 10)
basis_fee <- mab_calculate_fill_fees(
  2, basis_spec, as.POSIXct("2025-12-01", tz = "Europe/London"),
  basis_config, data.frame(), fill_price_displayed = 100
)
smoke_equal(basis_fee$exchange_fee_usd, 1,
            message = "Fee: per-physical-unit basis failed")
smoke_equal(basis_fee$clearing_fee_usd, 2,
            message = "Fee: transaction-notional basis failed")
smoke_equal(basis_fee$total_fee_usd, 5,
            message = "Fee: generalised basis total failed")
unlink(path)
unlink(basis_path)
cat("FEE_CONTRACT_PASS\n")
