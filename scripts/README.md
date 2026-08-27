# Entry-script order

`run_pipeline.R` invokes only `core/` in numeric order:

```text
00 validate
01 data/contracts/chronology
02 windows, Kalman, Gaussian formation economics and selection
03 Lévy-family evidence
04 strict-interior OU–GH
05 analytic Gaussian + complete-episode GH thresholds
06 exact-contract backtest and daily ledger
07 performance inference
```

Stage 02 intentionally combines adjacent formation, estimation, economics and selection operations behind one guarded entry point. `alternatives/`, `validation/`, `analyses/` and `tools/` are excluded from the core runner.
