# Core pipeline

This diagram follows the dissertation methodology. It is
an expository lineage; the runner defers costly threshold work until after pair
selection so it does not evaluate unselected pair-windows.

```text
licensed Bloomberg exact-contract quotes, lifecycle/specification data and BFIX
                                  |
public session metadata ----------+---------- public fee/provenance inputs
                                  v
             exact contracts, synchronous rolls and V2 cleaning
                                  |
                                  v
        joint active-market clock and pair-specific 20:10:1 windows
                                  |
                                  v
       dynamic Kalman formation hedge and frozen centred midpoint spread
                                  |
                                  v
          exact Gaussian OU, whole-contract sizing and prospective costs
                                  |
                                  v
                ADF05 deterministic dynamic top-two selection
                                  |
                                  v
        exact-transition conditional evidence under the Gaussian skeleton
                                  |
                                  v
                   strict-interior OU-GH formation fit
                                  |
                         +--------+--------+
                         |                 |
                         v                 v
                 analytic Gaussian   complete-episode
                     boundary         OU-GH MC boundary
                         |                 |
                         +--------+--------+
                                  |
                                  v
       common exact-contract bid/ask monetary backtest with explicit
        outgoing close / incoming reopen and contract-segment P&L
                                  |
                                  v
           chronological daily P&L and committed-capital returns
                                  |
                                  v
                    dependence-robust inference
```

The public entry order is `scripts/core/00_...` through `07_...`; safely
combined stages are explained in `docs/PRODUCTION_MANIFEST.md`. The core
orchestrator is `scripts/run_pipeline.R`. Retained finite-horizon calibration
and full-family OU-GH code are explicitly isolated under `scripts/alternatives/`
and `R/alternatives/`; neither is called by the core runner.

The generic ticker cleaner under `tools/quote_cleaning/` is also outside this
pipeline. The dissertation-specific V2 quote-quality implementation remains
`R/quote_quality_v2.R`.
