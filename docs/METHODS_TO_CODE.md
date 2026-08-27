# Dissertation methods-to-code crosswalk

The dissertation methodology is authoritative. Script numbers reflect dependency order; combined stages perform adjacent thesis steps without duplicating scientific implementations.

| Thesis method | Role | Public entry / implementation | Classification |
|---|---|---|---|
| Data, exact contracts and chronology | licensed schema, synchronous five-weekday roll, quote midpoints, admissible intervals, active time, 20:10:1 windows | `scripts/core/01_prepare_data_contracts_and_chronology.R`; `R/data_contracts.R`, `R/exact_contract_roll.R`, `R/market_data.R`, `R/active_market_clock.R`, `R/rolling_windows.R` | CORE |
| Dynamic hedge and frozen spread | formation Kalman state and centred fixed spread | combined stage 02; `R/kalman_hedge.R`, `R/spread_construction.R` | CORE |
| Gaussian benchmark | segment-safe exact conditional OU | combined stage 02; `R/gaussian_ou_estimation.R` | CORE |
| Formation economics | integer sizing, quote feasibility, 10% trimmed prospective cost | combined stage 02; `R/formation_candidates.R`, `R/quote_quality_v2.R`, `R/prospective_cost.R`, `R/backtest_core/position_sizing.R` | CORE |
| Dynamic pair selection | ADF05, half-life/opportunity score, `min(2,E)`, no substitution | combined stage 02; `R/dynamic_ranking.R` | CORE |
| Lévy-family transition evidence | exact OU remainders, nine conditional likelihoods, cAIC/cBIC | `scripts/core/03_compare_levy_families.R`; `R/levy_exact_input.R`, `python/exact_transition_engine.py`, `python/model_selection.py` | CORE |
| Strict-interior OU–GH | 75/25 formation fit, centred GH/GIG, exact Fourier remainder simulation | `scripts/core/04_fit_strict_interior_ou_gh.R`; `R/ou_gh_strict_interior/` | CORE |
| Gaussian analytic threshold | complete renewal-cycle analytic comparator | stage 05; `R/gaussian_analytic_thresholds.R` | CORE |
| OU–GH complete-episode MC | one flat-entry-exit episode, overshoot reward, ratio of expectations, numerical guardrail only | `scripts/core/05_calibrate_trading_thresholds.R`; `scripts/core/internal/complete_episode_threshold_stage.R`; `R/complete_episode_threshold_mc.R` | CORE |
| Exact-contract OOS execution | bid/ask fills, explicit outgoing close/incoming reopen with roll costs and fees, contract-homogeneous P&L segments, ten-session terminal liquidation | `scripts/core/06_run_exact_contract_backtest.R`; `R/backtest_engine.R`, `R/backtest_core/` | CORE |
| Daily capital and inference | ledger reconciliation, committed capital, HAC and bootstrap | stages 06–07; `R/backtest_core/daily_ledger.R`, `R/performance_statistics.R` | CORE |
| Finite-horizon MC threshold calibration | terminal-liquidation calibration retained from an earlier contract | `scripts/alternatives/finite_horizon_mc/`, `R/alternatives/finite_horizon_mc/` | ALTERNATIVE / NON-CORE |
| Full-family OU–GH | eight-candidate interior/boundary/restriction router | `scripts/alternatives/full_family_ou_gh/`, `R/alternatives/full_family_ou_gh/`, `config/alternatives/full_family_ou_gh/` | ALTERNATIVE / NON-CORE |
| Conditional-dynamics diagnostics | residual dependence/scale warnings, not family selection | `scripts/analyses/run_conditional_dynamics_diagnostics.R`, `R/analyses/conditional_dynamics_diagnostics.R` | SECONDARY ANALYSIS |
| Generic ticker cleaner | future-research standalone fit/apply utility | `tools/quote_cleaning/` | TOOL / NOT DISSERTATION DATA ROUTE |

The threshold helper sources the retained finite-horizon compatibility module for shared simulator/task interfaces. This is an implementation dependency, not a claim that the finite-horizon economic objective is core.
