# Model-Based Energy-Futures Pairs Trading

This repository implements the computational methodology of Aidan Densem's MSc Statistics dissertation. It is organised to follow the methodological sequence of the dissertation, from exact-contract data construction through model estimation, threshold calibration, monetary backtesting and dependence-robust inference.

## Core computational lineage

1. **Data, exact contracts and chronology.** One-minute exact-contract bid/ask quotes are validated, two legs are rolled synchronously five weekdays before the earlier last-trade date, and accepted observations are measured on the intersection of exchange-admissible intervals. Dissertation sessions are Europe/London calendar dates.
2. **Dynamic hedge and frozen spread.** A formation-only affine Kalman filter supplies the terminal hedge state; that state is frozen to construct the centred spread.
3. **Gaussian benchmark and formation economics.** The exact conditional Gaussian OU model is fitted on segment-safe active-time transitions. Whole-contract feasibility and the formation-frozen, all-cost proxy are computed for a USD 200,000 sleeve.
4. **Dynamic pair selection.** The 5% ADF gate is followed by equal-weight percentile ranks for active-session half-life and robust cost-adjusted opportunity. Each endpoint selects `N_w = min(2, E_w)` without ex-post substitution.
5. **Lévy-family transition evidence.** Nine background-driver candidates are compared conditionally on the common fitted Gaussian OU skeleton using exact finite-interval transition likelihood, cAIC and cBIC.
6. **Strict-interior OU–GH.** The core non-Gaussian model is the centred strict-interior GH/GIG driver, estimated on a chronological 75/25 formation split and simulated through exact OU-remainder Fourier inversion.
7. **Trading thresholds.** The Gaussian comparator uses the analytic renewal-cycle boundary. OU–GH uses the untruncated complete flat–entry–exit Monte Carlo objective `E[reward] / E[episode duration]`; its remote `H_max` is a numerical guardrail, not an economic horizon.
8. **Exact-contract execution and inference.** The common ten-session backtest executes buys at ask and sells at bid, explicitly closes outgoing contracts and reopens incoming contracts at each roll (charging both bid/ask cost and fees), liquidates open positions at the trading horizon, reconciles daily and route ledgers, and reports committed-capital performance with dependence-robust inference. Cross-maturity price-level jumps are never treated as strategy P&L.

The ordered public entry point is [`scripts/run_pipeline.R`](scripts/run_pipeline.R). The compact visual lineage is in [`docs/PIPELINE.md`](docs/PIPELINE.md), stage details are in [`docs/PRODUCTION_MANIFEST.md`](docs/PRODUCTION_MANIFEST.md), and the thesis-to-code map is in [`docs/METHODS_TO_CODE.md`](docs/METHODS_TO_CODE.md).

## Repository structure

```text
R/                         scientific implementations and stage map
python/                    exact-transition likelihood and model selection
scripts/core/              thesis-ordered public production stages
scripts/alternatives/      finite-horizon MC and full-family GH branches
scripts/analyses/          secondary scientific result-support calculations
config/                    current contracts; alternatives are nested explicitly
data/                      input contracts, 13-product universe and public ancillary inputs
docs/                      acquisition, lineage, methods, validation and execution documentation
tests/                     synthetic and deterministic smoke tests
tools/quote_cleaning/      standalone generic ticker cleaner
```

## Data availability

The Bloomberg Professional one-minute quotes, exact-contract lifecycle and monetary metadata, and empirical BFIX fixings are licence-restricted and are not included. An authorised Bloomberg user can reconstruct the canonical inputs using [`data/README.md`](data/README.md) and [`docs/DATA_ACQUISITION.md`](docs/DATA_ACQUISITION.md). The 13-product universe and deterministic 78-pair configuration are machine-readable at [`data/ancillary/product_universe_13.csv`](data/ancillary/product_universe_13.csv) and [`data/ancillary/candidate_pairs_78.csv`](data/ancillary/candidate_pairs_78.csv). The identifier-only list of all 208 exact delivery contracts is provided at [`data/ancillary/exact_contract_manifest.csv`](data/ancillary/exact_contract_manifest.csv), with extraction provenance in [`data/ancillary/exact_contract_manifest_provenance.csv`](data/ancillary/exact_contract_manifest_provenance.csv); it contains no prices, lifecycle dates or monetary specifications.

The exchange/clearing calibration and source ledger are included at [`data/ancillary/fees.csv`](data/ancillary/fees.csv) and [`data/ancillary/fee_schedule_provenance.csv`](data/ancillary/fee_schedule_provenance.csv). Source-linked regular-session, break and holiday-calendar metadata for all 13 products are included at [`data/ancillary/market_sessions_13_products.json`](data/ancillary/market_sessions_13_products.json).

### Historical session-calendar reconstruction

The repository does not include the date-expanded exact-contract session interval table. The included product-level metadata documents regular sessions, breaks, named time zones and public calendar sources, but it does not by itself encode every historical holiday or early close. An empirical reconstruction must build and independently check `session_intervals.csv` from the cited sources.

The U6/ZS rate is verified against MCX Circular `MCX/F&A/631/2024`, preserved in an official MCX corporate filing with BSE. See [`docs/DATA_LINEAGE.md`](docs/DATA_LINEAGE.md) and [`data/INPUT_MANIFEST.csv`](data/INPUT_MANIFEST.csv).

## Full core pipeline

After constructing the required date-expanded session table and placing authorised inputs under one directory:

```sh
export DISSERTATION_DATA_ROOT=/path/to/authorised/inputs
export PYTHON=/path/to/python3.9
export ALLOW_EXPENSIVE_PIPELINE=TRUE
Rscript scripts/run_pipeline.R
```

This is computationally expensive and requires explicit authority switches. The smoke-test command does not invoke it. See [`docs/REPRODUCIBILITY.md`](docs/REPRODUCIBILITY.md) for individual guards and execution boundaries.

## Alternatives, validation and utilities

- `scripts/alternatives/finite_horizon_mc/`: terminal-liquidation threshold calibration retained as a non-core alternative.
- `scripts/alternatives/full_family_ou_gh/`: eight-candidate full-family GH topology retained as a non-core alternative.
- `scripts/validation/` and method-specific tests: validation code, never part of production selection.
- `tools/quote_cleaning/`: reusable standalone quote cleaner. It does not replace the dissertation's pair-window-specific V2 production cleaner.

## Environment and reproducibility status

The recorded reference environment is R 4.6.0 on arm64 macOS and CPython 3.9.13. Direct dependencies are declared in [`config/package_requirements.R`](config/package_requirements.R) and [`config/python_requirements.txt`](config/python_requirements.txt). No complete valid transitive lockfile is available, so byte-identical package restoration is not claimed.

The included validation procedures are structural and synthetic; they do not reproduce the empirical estimates. Citation metadata are in [`CITATION.cff`](CITATION.cff). No software licence is asserted; absent a licence, copyright remains with the author.
