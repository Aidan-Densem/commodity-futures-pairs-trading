# Fixed synthetic values computed through the authoritative R V2 helpers.
q <- c(.001, .0011, .0009, .0012, .00105, .00115, .001, .00095, .0013, .05)
sigma <- v2_robust_sigma(log(q))
robust <- exp(stats::median(log(q)) + 8 * sigma)
q_max <- max(
  4 * .01 / 100,
  v2_safe_quantile(q, .995),
  min(robust, 50 * stats::median(q)),
  na.rm = TRUE
)
moves <- c(.001, .002, .0015, .003, .0025, .004, .006, .005)
h_mid <- max(
  log(1.25), 15 * v2_robust_sigma(moves),
  2 * v2_safe_quantile(moves, .999), na.rm = TRUE
)
smoke_equal(sigma, 0.1351549700513581, tolerance = 1e-15,
            message = "Quote-cleaner parity robust scale changed")
smoke_equal(robust, 0.003168600261216029, tolerance = 1e-15,
            message = "Quote-cleaner parity robust envelope changed")
smoke_equal(q_max, .05, tolerance = 1e-15,
            message = "Quote-cleaner parity q_max changed")
smoke_equal(h_mid, log(1.25), tolerance = 1e-15,
            message = "Quote-cleaner parity h_mid changed")

# CM2 edge parity: when usable Close evidence is absent but width calibration
# succeeds, the authoritative R contract is exactly relative_cutoff / 2.
edge_config <- list(
  version = "quote_quality_v2.0.0-causal-formation-robust",
  minimum_clean_formation_quotes = 3L,
  log_spread_mad_multiplier = 8,
  spread_median_multiple = 50,
  spread_empirical_quantile = .995,
  tick_floor_multiple = 4,
  price_return_mad_multiplier = 15,
  price_return_quantile = .999,
  price_quantile_multiplier = 2,
  price_relative_floor = log(1.25),
  local_reference_observations = 60L,
  local_reference_minimum = 20L,
  close_mid_mad_multiplier = 8,
  close_mid_empirical_quantile = .995
)
edge_formation <- data.frame(
  timestamp = as.POSIXct("2025-01-01 09:00:00", tz = "Europe/London") + 60 * 0:2,
  bid = c(99.99, 100.00, 100.01),
  ask = c(100.01, 100.02, 100.03),
  close = rep(NA_real_, 3L),
  stringsAsFactors = FALSE
)
edge_contract <- suppressWarnings(calibrate_execution_quote_leg(
  edge_formation, tick_size = .01, config = edge_config
))
smoke_expect(is.finite(edge_contract$close_mid_consistency_cutoff),
             "Close-mid no-evidence edge did not retain a finite width floor")
smoke_equal(
  edge_contract$close_mid_consistency_cutoff,
  edge_contract$relative_spread_cutoff / 2,
  tolerance = 1e-15,
  message = "Close-mid no-evidence edge differs from authoritative q_max / 2"
)
cat("QUOTE_CLEANING_V2_FORMULA_PARITY_PASS\n")
