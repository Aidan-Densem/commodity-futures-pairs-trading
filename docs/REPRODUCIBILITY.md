# Reproducibility and execution

Run commands from the repository root. The repository itself is location-independent; empirical inputs are discovered through `DISSERTATION_DATA_ROOT`.

## Environment

- Recorded reference environment: R 4.6.0 on arm64 macOS; CPython 3.9.13.
- R dependencies: `config/package_requirements.R`.
- Python dependencies: `config/python_requirements.txt`.
- A compatible C/C++ toolchain is required for Rcpp modules.
- No complete valid transitive lockfile is available; byte-identical package restoration is not claimed.

## Data preflight

Read `data/README.md` and `docs/DATA_ACQUISITION.md`. Licensed Bloomberg/BFIX inputs remain external. Public fee/session metadata, the 13-product universe and the exact 208-security identifier manifest are included. The U6/ZS MCX rate is source-verified. A complete empirical reconstruction additionally requires a date-expanded session-calendar table built and checked against the cited public sources.

## Smoke tests

```sh
export PYTHON=/path/to/environment/satisfying/config/python_requirements.txt
Rscript tests/smoke/run_smoke_tests.R
```

The smoke runner clears every expensive/empirical authority variable and uses only deterministic or synthetic fixtures. It includes a large artificial maturity-basis jump to prove that the core explicit close/reopen route creates four roll fills, charges roll costs and keeps P&L within exact-contract segments. Smoke success verifies interfaces and invariants, not regeneration of dissertation estimates.

## Full core pipeline

```sh
export DISSERTATION_DATA_ROOT=/authorised/input/root
export PYTHON=/path/to/python3.9
export ALLOW_EXPENSIVE_PIPELINE=TRUE
Rscript scripts/run_pipeline.R
```

The public runner executes the stage order in `docs/PRODUCTION_MANIFEST.md`: data/chronology; Kalman, Gaussian formation economics and pair selection; Lévy evidence; strict-interior GH; analytic Gaussian and complete-episode GH thresholds; explicit close/reopen exact-contract backtest; performance inference.

Individual stages are guarded:

| Operation | Authority variable |
|---|---|
| exact-contract/session preparation | `ALLOW_FULL_EXACT_CONTRACT_PREPARATION=TRUE` |
| candidate census and ranking | `ALLOW_EXPENSIVE_RANKING=TRUE` |
| transition materialisation | `ALLOW_LEVY_INPUT_CONSTRUCTION=TRUE` |
| exact-transition likelihood census | `ALLOW_EXPENSIVE_LEVY_SCREEN=TRUE` |
| strict-interior GH fit | `ALLOW_EXPENSIVE_GHI_FIT=TRUE` |
| analytic/core threshold stage | `ALLOW_EXPENSIVE_THRESHOLDS=TRUE` |
| complete-episode GH simulation | `ALLOW_EXPENSIVE_COMPLETE_EPISODE_THRESHOLDS=TRUE` |
| monetary backtest | `ALLOW_FULL_EMPIRICAL_BACKTEST=TRUE` |
| performance inference | `ALLOW_FULL_PERFORMANCE_INFERENCE=TRUE` |

Exact-transition commands are `prepare`, `audit`, `fit`, `validate`, `aggregate`. Checkpoints bind scientific inputs/configuration. Strict/full-family GH objects bind incompatible `gh_mode` values and fail closed across branches.

## Alternatives and secondary analyses

The finite-horizon threshold and full-family GH launchers under `scripts/alternatives/` are not invoked by the public runner. The conditional-dynamics calculation under `scripts/analyses/` is descriptive and never enters model selection. The generic cleaner under `tools/quote_cleaning/` is a separate future-research utility.

## Validation boundary

The supplied validation workflow covers parsing, import/compile checks and inexpensive synthetic smoke tests. It does not run exact-contract reconstruction, the 78-pair rolling census, the Lévy census, GH estimation, 10,000-path threshold calibration, the monetary backtest, full inference or proprietary-data processing.
