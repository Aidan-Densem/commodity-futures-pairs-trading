tz <- "Europe/London"
interval <- function(contract, id, open, close) data.frame(
  contract = contract, session_id = id,
  open_timestamp = as.POSIXct(open, tz = tz),
  close_timestamp = as.POSIXct(close, tz = tz),
  admissible = TRUE, stringsAsFactors = FALSE
)

# Session identity and cross-closure active time are separate contracts.
ycal <- rbind(
  interval("Y", "Y_FRI", "2026-01-09 09:00", "2026-01-09 16:00"),
  interval("Y", "Y_MON", "2026-01-12 09:00", "2026-01-12 16:00")
)
xcal <- rbind(
  interval("X", "X_FRI", "2026-01-09 10:00", "2026-01-09 16:00"),
  interval("X", "X_MON", "2026-01-12 10:00", "2026-01-12 16:00")
)
cross <- data.frame(
  timestamp = as.POSIXct(c("2026-01-09 15:59", "2026-01-12 10:01"), tz = tz),
  contract_y = "Y", contract_x = "X", opportunity_index = 1:2,
  statistical_quote_valid = TRUE, roll_boundary = c(FALSE, FALSE)
)
cross <- add_pair_active_clock(cross, ycal, xcal, tz)
smoke_expect(cross$transition_valid[[2L]] && cross$active_dt_minutes[[2L]] == 2,
             "A valid scheduled-closure transition was rejected or mis-timed")
smoke_expect(cross$admissible_interval_id[[1L]] != cross$admissible_interval_id[[2L]] &&
               cross$structural_segment_id[[1L]] == cross$structural_segment_id[[2L]],
             "Exchange interval identity incorrectly created a structural break")
smoke_expect(identical(as.character(cross$calendar_session_date),
                       c("2026-01-09", "2026-01-12")),
             "Statistical session is not the London calendar date")

split_y <- rbind(
  interval("Y", "Y_AM", "2026-01-13 09:00", "2026-01-13 12:00"),
  interval("Y", "Y_PM", "2026-01-13 13:00", "2026-01-13 16:00")
)
split_x <- rbind(
  interval("X", "X_AM", "2026-01-13 09:30", "2026-01-13 12:00"),
  interval("X", "X_PM", "2026-01-13 13:00", "2026-01-13 15:30")
)
same_date <- data.frame(
  timestamp = as.POSIXct(c("2026-01-13 11:59", "2026-01-13 13:01"), tz = tz),
  contract_y = "Y", contract_x = "X", opportunity_index = 1:2,
  statistical_quote_valid = TRUE, roll_boundary = FALSE
)
same_date <- add_pair_active_clock(same_date, split_y, split_x, tz)
smoke_expect(same_date$transition_valid[[2L]] &&
               same_date$admissible_interval_id[[1L]] != same_date$admissible_interval_id[[2L]] &&
               length(unique(same_date$calendar_session_date)) == 1L,
             "Same-date exchange intervals were conflated with statistical sessions")

# An intervening weekday holiday still counts toward the report's roll offset.
smoke_expect(subtract_weekdays(as.Date("2026-01-26"), 5L) == as.Date("2026-01-19"),
             "Five-weekday roll incorrectly skipped a weekday holiday")

# The Kalman prior is used once per formation window, not once per segment.
y <- c(0.2, 0.1, -0.1, 0.05); x <- c(1, 1.1, 0.9, 1.05); dt <- c(0, 1, 0, 1)
one_prior <- kalman_affine_filter(y, x, dt, q = 0.02, ve = 0.1,
                                  segment_id = c(1, 1, 2, 2))
diagnostic_only <- kalman_affine_filter(y, x, dt, q = 0.02, ve = 0.1,
                                        segment_id = rep(1, 4))
smoke_equal(one_prior$state_path, diagnostic_only$state_path,
            message = "Kalman state/covariance was reset at a segment label")

# Fixed quantities, endpoint FX and endpoint K-hat remain fixed while observed
# historical bid/ask concessions can vary.
sizing <- data.frame(signed_y_quantity = 2, signed_x_quantity = -3)
spec_y <- data.frame(PointValueNativePerDisplayedPoint = 10, PnLCurrency = "USD",
                     resolved_key = "Y", original_key = "Y", Root = "Y", ExchangeCode = "T")
spec_x <- data.frame(PointValueNativePerDisplayedPoint = 20, PnLCurrency = "USD",
                     resolved_key = "X", original_key = "X", Root = "X", ExchangeCode = "T")
frozen1 <- formation_roundtrip_event_cost(
  as.POSIXct("2026-01-01 10:00", tz = tz), "long", 1, sizing,
  99, 101, 49, 51, spec_y, spec_x, 1.2, 0.8,
  mab_fee_configuration(1), data.frame(), fixed_k_hat = 2500,
  fee_fx_override = c(USD = 1)
)
frozen2 <- formation_roundtrip_event_cost(
  as.POSIXct("2025-12-01 10:00", tz = tz), "long", 1, sizing,
  98, 102, 48.5, 51.5, spec_y, spec_x, 1.2, 0.8,
  mab_fee_configuration(1), data.frame(), fixed_k_hat = 2500,
  fee_fx_override = c(USD = 1)
)
smoke_expect(
  identical(frozen1[c("signed_y_quantity", "signed_x_quantity",
                      "frozen_y_fx_usd_per_native", "frozen_x_fx_usd_per_native",
                      "effective_dollar_spread_sensitivity")],
            frozen2[c("signed_y_quantity", "signed_x_quantity",
                      "frozen_y_fx_usd_per_native", "frozen_x_fx_usd_per_native",
                      "effective_dollar_spread_sensitivity")]) &&
    frozen1$bidask_roundtrip_usd != frozen2$bidask_roundtrip_usd,
  "Prospective endpoint quantities/FX/K were not frozen independently of historical quotes"
)

# The production ADF convention uses every finite level, not a longest
# segment surrogate.
levels <- sin(seq(0, 8, length.out = 80)) + seq_len(80) / 1000
segments <- rep(c("a", "b"), each = 40)
adf_production <- formation_adf05_final_levels(levels, segments)
adf_direct <- formation_adf05(levels)
smoke_equal(adf_production$adf_p_value, adf_direct$adf_p_value,
            message = "ADF gate did not use all finite formation levels")
smoke_expect(adf_production$adf_observations == 80L &&
               adf_production$adf_segment_count_diagnostic == 2L,
             "ADF sample provenance is incomplete")

# Production backtests must never synthesize time from row order.
strategy <- data.frame(
  pair_id = "Y_X", formation_endpoint = as.Date("2026-01-01"),
  testing_start = as.POSIXct("2026-01-02 10:00", tz = tz),
  testing_end = as.POSIXct("2026-01-02 10:01", tz = tz),
  upper_entry = 1, lower_entry = -1, upper_exit = 0, lower_exit = 0,
  alpha = 0, beta = 1, formation_centre = 0, pair_sleeve_usd = 200000,
  model_label = "G", stringsAsFactors = FALSE
)
quotes <- data.frame(
  timestamp = strategy$testing_start + 0:1 * 60,
  statistical_quote_valid = TRUE, midpoint_y = c(100, 101), midpoint_x = c(50, 50),
  bid_y = c(99, 100), ask_y = c(101, 102), bid_x = c(49, 49), ask_x = c(51, 51),
  contract_y = "Y", contract_x = "X",
  statistical_quote_valid_y = TRUE, statistical_quote_valid_x = TRUE
)
missing_clock_error <- tryCatch({
  prepare_midpoint_monetary_data(quotes, strategy); FALSE
}, error = function(e) grepl("row-index fallback is prohibited", conditionMessage(e), fixed = TRUE))
smoke_expect(missing_clock_error, "Backtest accepted input with no active-market clock")

# GH branch objects are mode-bound in both directions.
strict <- list(gh_mode = "STRICT_INTERIOR")
full <- list(gh_mode = "FULL_FAMILY")
smoke_expect(inherits(try(validate_gh_mode(strict, "FULL_FAMILY"), silent = TRUE), "try-error") &&
               inherits(try(validate_gh_mode(full, "STRICT_INTERIOR"), silent = TRUE), "try-error"),
             "Cross-branch GH object reuse was silently accepted")

full_environment <- full_family_gh_environment()
smoke_expect(isTRUE(all.equal(
  full_environment$ou_gh_family_ccf_bank(),
  FULL_FAMILY_GH_CONTRACT$ccf_bank,
  check.attributes = FALSE
)), "Full-family GH source does not use its isolated frozen CCF bank")

smoke_expect(!file.exists(repo_path("R", "reported_analyses", "driver_process_diagnostics.R")),
             "Abandoned driver-first source remains in the default repository path")

python <- Sys.getenv("PYTHON", unset = Sys.which("python3"))
if (!nzchar(python)) stop("Python is required for exact-transition smoke tests.", call. = FALSE)
status <- system2(
  python, repo_path("python", "smoke_exact_transition_durations.py"),
  env = "PYTHONDONTWRITEBYTECODE=1"
)
smoke_expect(identical(status, 0L), "Duration-aware Python exact-likelihood smoke failed")

cat("ACTIVE_TIME_ROLL_AND_GH_BRANCH_CONTRACTS_PASS\n")
