# Complete-episode OU–GH Monte Carlo calibration

The dissertation calibrates the strict-interior OU–GH entry boundary on a complete renewal episode, not on the ten-session empirical trading horizon.

For every candidate pair of upper/lower distances, a simulated path starts flat, waits for first entry, records the realised overshoot, and continues until the prescribed centre exit. The objective is the ratio of Monte Carlo mean net reward to Monte Carlo mean complete episode duration. Waiting and holding time are both included.

The path bank uses common random numbers and nested 250/750/10,000 budgets. Candidate comparisons use the same path identities. A delta-method marginal standard error and near-optimal plateau rule stabilise selection on a noisy/flat objective.

`H_max` is a remote numerical guardrail. An unresolved path is neither liquidated nor assigned zero reward; the candidate is unavailable unless the required path bank resolves. A non-positive selected objective maps to the zero-reward outside option/model no-trade.

Core implementation:

- `config/complete_episode_threshold_contract.R`
- `R/complete_episode_threshold_mc.R`
- `scripts/core/internal/complete_episode_threshold_stage.R`
- `scripts/core/05_calibrate_trading_thresholds.R`

The internal stage retains Gaussian/full-family task adapters for compatibility and testing, but the public core wrapper selects only the analytic Gaussian comparator and strict-interior GH route.

The empirical backtest is a separate finite ten-session exercise and does liquidate open positions at its trading horizon. Calibration and testing terminal conventions therefore have intentionally different roles.
