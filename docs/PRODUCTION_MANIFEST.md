# Production manifest

## Ordered core route

| Stage | Entry point | Principal role/output |
|---|---|---|
| 00 | `scripts/core/00_validate_inputs.R` | environment, licensed-input and public-ancillary preflight |
| 01 | `scripts/core/01_prepare_data_contracts_and_chronology.R` | exact-contract schedules, synchronous pair paths, active clocks and 20:10:1 windows |
| 02 | `scripts/core/02_construct_spreads_gaussian_economics_and_select_pairs.R` | Kalman/frozen spreads, Gaussian fits, whole-contract economics and ADF05 top-two schedule |
| 03 | `scripts/core/03_compare_levy_families.R` | segment-safe exact transition inputs, nine-family conditional evidence, cAIC/cBIC |
| 04 | `scripts/core/04_fit_strict_interior_ou_gh.R` | strict-interior formation tasks, checkpoints and fitted snapshot |
| 05 | `scripts/core/05_calibrate_trading_thresholds.R` | analytic Gaussian boundary plus complete-episode strict-interior GH thresholds and common routes |
| 06 | `scripts/core/06_run_exact_contract_backtest.R` | ten-session whole-contract bid/ask backtest, explicit close/reopen rolls and reconciled daily capital ledger |
| 07 | `scripts/core/07_performance_inference.R` | risk/performance summaries and dependence-robust inference |

`scripts/run_pipeline.R` invokes only this core route. Stages 01 and 02 are deliberately combined implementations: their names expose the multiple adjacent dissertation steps they perform without duplicating or rewriting validated science.

## Frozen scientific contracts

- 13-product universe in `data/ancillary/product_universe_13.csv`; deterministic 78-pair configuration in `data/ancillary/candidate_pairs_78.csv`.
- Exact listed contracts; both legs roll five weekdays before the earlier last-trade date.
- Synchronous finite uncrossed bid/ask midpoint; no interpolation and no `Close` fallback.
- Joint exchange-admissible active time; Europe/London calendar-date sessions; no transition across rejected observations, roll boundaries or explicit structural segments.
- 20 formation sessions, 10 testing sessions, one-session step.
- One formation-specific affine Kalman prior; terminal post-update hedge state frozen through testing.
- Exact conditional Gaussian OU on accepted positive active durations.
- USD 200,000 sleeve, strictly positive whole-contract counts, hedge error at most 0.25 and gross-notional overshoot at most 5%.
- Formation-only V2 cleaning; 10% trimmed mean of position-aware bid/ask plus brokerage/exchange/clearing concession.
- ADF at 5%; equal half-life/opportunity percentile weights; `N_w=min(2,E_w)`; no downstream substitution.
- Nine-family exact-transition evidence conditional on the common Gaussian OU skeleton.
- `STRICT_INTERIOR` is the sole core non-Gaussian route.
- Gaussian analytic renewal-cycle boundary; GH complete-episode Monte Carlo with 250/750/10,000 nested paths, seed 91001 and no economic calibration horizon.
- Every held roll explicitly closes the outgoing two-leg position and reopens the same contract quantities in the incoming contracts, using causal side-specific quotes and charging bid/ask cost plus fees on all four fills. P&L is computed only within contract-homogeneous segments, so maturity-basis jumps are excluded.
- Ten-session empirical execution separately liquidates any open position at the trading horizon.
- Selected sleeves remain committed on exact frozen testing dates, including idle/no-trade sleeves.
- USD 1 brokerage per contract-side plus the included fee calibration; no taxes/regulatory levies.

## Public and licensed inputs

The authoritative public root universe is `data/ancillary/product_universe_13.csv`, with pair orientation in `data/ancillary/candidate_pairs_78.csv` and the final 208 listed-security identifiers in `data/ancillary/exact_contract_manifest.csv`. The included fee schedule is `data/ancillary/fees.csv`; market-session metadata is `data/ancillary/market_sessions_13_products.json`. Licensed exact-contract quotes/lifecycle/monetary metadata and BFIX remain external as specified in `data/README.md`.

The included session metadata does not replace the required date-expanded exact-contract `session_intervals.csv`; reconstruction requirements are documented in the README and data guide.

## Non-core retained branches

- `scripts/alternatives/finite_horizon_mc/`: earlier terminal-liquidation calibration.
- `scripts/alternatives/full_family_ou_gh/`: earlier eight-candidate GH router and its own validation.
- `scripts/analyses/`: secondary scientific analyses excluded from production selection.
- `tools/quote_cleaning/`: standalone generic cleaner excluded from the dissertation production route.

All generated empirical objects belong under ignored `output/`. No generated output is accepted as a raw-source substitute.
