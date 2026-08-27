# Data lineage

```text
Bloomberg Professional (licensed; not redistributed)
    |-- exact-contract one-minute bid/ask + diagnostic close
    |-- exact-contract lifecycle / last-trade metadata
    |-- exact-contract monetary specifications
    `-- London 16:00 BFIX fixings
                     |
Public ancillary configuration
    |-- 13-product universe
    |-- deterministic 78-pair configuration
    |-- final 208-security exact-contract identifier manifest
    |-- exchange/clearing fee calibration + provenance
    `-- market-session metadata + holiday-calendar sources
                     |
                     v
canonical input schemas and validation
                     |
                     v
synchronous exact-contract roll construction
                     |
                     v
quote validation, midpoint alignment, explicit structural segments
                     |
                     v
joint exchange-admissible active clock + London calendar sessions
                     |
                     v
20:10:1 formation/testing windows
                     |
                     v
Kalman hedge -> frozen spread -> Gaussian OU -> sizing/costs
                     |
                     v
ADF05 dynamic top-two schedule
                     |
                     v
exact-transition Lévy evidence -> strict-interior OU-GH
                     |
                     v
analytic Gaussian + complete-episode GH thresholds
                     |
                     v
ten-session exact-contract execution -> daily capital -> inference
```

## Machine-readable contracts

- `data/INPUT_MANIFEST.csv` identifies each input's licensing class, location, schema and first pipeline consumer.
- `data/ancillary/product_universe_13.csv` is the canonical root-level universe.
- `data/ancillary/exact_contract_manifest.csv` fixes the final 208 listed-security identifiers without redistributing prices, lifecycle dates or monetary specifications; its extraction ledger records the authoritative source SHA-256.
- `data/ancillary/fees.csv` is the actual retained fee calibration; `fee_schedule_provenance.csv` records sources and transformations, including the MCX circular supporting U6/ZS.
- `data/ancillary/market_sessions_13_products.json` records public regular-session/break/holiday-source metadata.
- `R/data_contracts.R`, `R/backtest_core/contract_specs.R`, `R/backtest_core/bfix.R` and `R/backtest_core/fee_config.R` enforce the principal empirical schemas.
- `config/production_config.R`, `config/contracts_v2.R` and `config/complete_episode_threshold_contract.R` preserve scientific settings.

## Traceability boundary

The public repository retains definitions, schemas, public calibrations and scientific code. It intentionally excludes quote histories, lifecycle observations, exact-month Bloomberg monetary records, BFIX histories, empirical spreads, transition samples, parameter censuses, Monte Carlo paths and trade ledgers.

The included session JSON is source-rich product metadata, but it is not a date-expanded historical interval table. Historical intervals must be reconstructed from the cited public calendars rather than replaced with a simplified calendar. The exact-contract identifier universe and fee provenance are complete; neither supplies the required calendar expansion.
