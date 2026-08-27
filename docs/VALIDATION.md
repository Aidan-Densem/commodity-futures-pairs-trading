# Validation status

Repository validation is structural and synthetic. The smoke suite does not require licensed data and does not invoke an empirical stage.

## What the smoke suite covers

- parsing/source loading and authority-switch safety;
- synchronous midpoint construction without `Close` fallback;
- segment-safe active time, exact-contract roll and calendar-session contracts;
- Kalman public output, exact Gaussian OU, ADF05 and top-two ranking invariants;
- whole-contract sizing, fee schemas, strategy routes, execution and daily-ledger reconciliation;
- exact-transition positive/irregular-duration inputs;
- strict/full GH mode incompatibility and retained full-family duration/router contracts;
- complete-episode wait-plus-hold duration, overshoot reward, ratio-of-expectations, delta-method uncertainty, nested path identities, adaptive extension and guardrail semantics;
- generic quote-cleaner causal fit/apply behaviour and R/Python formula parity;
- performance/HAC/bootstrap interfaces on synthetic returns.

Alternative finite-horizon tests remain in the suite because the retained compatibility module is still sourced by shared task/simulator adapters. Their presence does not make that economic objective part of the current production route.

## Additional validation checks

The validation procedure also:

- parses every R file;
- compiles/imports every Python file without writing bytecode into the repository;
- sources every public entry script in definition-only mode;
- checks public documentation paths and case-sensitive references;
- validates `CITATION.cff`, `data/INPUT_MANIFEST.csv`, the 13-product universe, 208-security identifier manifest, fee schedule and session JSON;
- checks for absolute personal paths, enabled production authorities and accidental proprietary empirical files;
- supports source-hash comparison for protected scientific implementations.

## Known limitation

1. The exact historical date-expanded session interval table is absent; public product/session metadata and official calendar sources are present but require reconstruction/review.

The U6/ZS fee rows are supported by MCX Circular `MCX/F&A/631/2024`; all enabled fee rows pass the fail-closed verified/source contract.

The session-table requirement is a data-reconstruction limitation, not a smoke-test formula failure.

## Validation boundary

The required smoke command is:

```sh
PYTHON=/path/to/environment/satisfying/config/python_requirements.txt \
  Rscript tests/smoke/run_smoke_tests.R
```

The required terminal marker is `ALL_SMOKE_TESTS_PASS`.

The smoke suite does not perform a full empirical run. Synthetic success establishes code contracts, not numerical reproduction of dissertation tables.
