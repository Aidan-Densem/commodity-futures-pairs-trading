# Scientific source map

Core source is mapped by dissertation stage:

```text
01 data/chronology       data_contracts, market_data, exact_contract_roll,
                         active_market_clock, rolling_windows, quote_quality_v2
02 spread/Gaussian       kalman_hedge, spread_construction,
                         gaussian_ou_estimation, formation_candidates,
                         prospective_cost, dynamic_ranking
03 Lévy evidence         levy_exact_input + ../python exact-transition engine
04 strict OU–GH          ou_gh_strict_interior/
05 thresholds            gaussian_analytic_thresholds,
                         complete_episode_threshold_mc, threshold/task adapters
06–07 execution/inference backtest_engine, backtest_core/, performance_statistics
```

`alternatives/finite_horizon_mc/` and `alternatives/full_family_ou_gh/` are explicitly non-core. `analyses/` contains secondary scientific diagnostics. Modules at the R root are shared across adjacent stages; their exact mapping is in `docs/METHODS_TO_CODE.md`.
