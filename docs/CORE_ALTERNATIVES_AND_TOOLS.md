# Core, alternatives, validation and tools

| Component | Classification | Role in the thesis methodology |
|---|---|---|
| Strict-interior OU–GH | CORE | current production non-Gaussian model |
| Complete-episode OU–GH Monte Carlo | CORE | current GH threshold objective; no economic calibration horizon |
| Analytic Gaussian boundary | CORE | dissertation comparator on the common renewal-cycle objective |
| Exact-transition nine-family evidence | CORE | current driver-family evidence conditional on common OU skeleton |
| Explicit close/reopen roll execution | CORE | selected common backtest policy; charges outgoing close and incoming reopen and keeps P&L contract-segmented |
| Seamless-continuation roll execution | ALTERNATIVE / COMPATIBILITY | retained validated scenario, but not selected by the core production wrapper |
| Finite-horizon Monte Carlo calibration | ALTERNATIVE | terminal-liquidation threshold contract retained for inspection and compatibility |
| Full-family GH router | ALTERNATIVE | broader boundary/restriction topology is not the current production GH model |
| Gaussian Monte Carlo calibration | COMPATIBILITY / VALIDATION | retained task/simulator interface and benchmark, not the primary analytic Gaussian route |
| Generic ticker cleaner | STANDALONE TOOL | future-research utility; does not replace pair-window V2 production cleaning |
| Known-truth and simulator studies | VALIDATION | test methods; not production selection stages |
| Conditional dynamics and retained sensitivities | SECONDARY ANALYSIS | support interpretation; do not change selected models/thresholds |

An implementation dependency can cross classifications. In particular, the complete-episode stage reuses shared objects originally housed with finite-horizon code. Documentation follows the economic method, while source lineage records that lower-level dependency explicitly.
