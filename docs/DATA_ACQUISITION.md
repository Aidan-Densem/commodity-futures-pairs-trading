# Reconstructing the licensed empirical inputs

This guide is for a researcher with authorised Bloomberg Professional access. It specifies the canonical repository inputs without redistributing the licensed observations.

## 1. Fix the study universe and dates

Use the 13 generics in `data/ancillary/product_universe_13.csv`, the exact 208-security request/join list in `data/ancillary/exact_contract_manifest.csv`, and the empirical interval 12 November 2025 to 28 May 2026. The manifest covers 16 delivery months per generic from 2025-07 through 2026-10. Acquire their licensed lifecycle and observation fields with sufficient coverage to implement the synchronous roll five Monday–Friday weekdays before the earlier leg's last-trade date; do not guess or substitute delivery securities.

The candidate configuration is the 78 unordered pairs of the 13 generics. The included `data/ancillary/candidate_pairs_78.csv` fixes the deterministic leg orientation; a local `candidate_pairs.csv` may override it without changing schema.

## 2. Export exact-contract one-minute quotes

For every relevant delivery contract, export one-minute timestamped bid, ask and diagnostic close/last observations. The repository uses the canonical schema:

```text
timestamp,generic,contract,bid,ask,close
```

The exact vendor field-request mnemonics are unavailable, so the repository specifies the required economic fields rather than inventing vendor labels. The listed-security identifiers are provided in `data/ancillary/exact_contract_manifest.csv`. Request executable bid, executable ask and a retained close/last diagnostic for exactly that universe. Do not export only a generic continuous series, interpolate missing minutes or fill missing bid/ask from close.

Normalise timestamps to unambiguous `Europe/London` datetimes after retaining the source timezone needed to audit conversion.

## 3. Export lifecycle metadata

For every exact contract, retain:

```text
generic,contract,delivery_month,first_trade_date,last_trade_date
```

The project evidence identifies Bloomberg `LAST_TRADEABLE_DT` as the expiry field used in roll ordering. `delivery_month` is `YYYY-MM`; `contract` must match the quote security identifier exactly. Do not supply a precomputed roll assignment—the repository constructs it.

## 4. Export monetary metadata

For each exact contract, construct `contract_specs.csv` with the full schema in `data/README.md`. At minimum the monetary engine needs:

- exact security and root/product identifiers;
- quotation and P&L currencies;
- delivery-month contract quantity and physical unit;
- displayed-price scale into P&L currency per physical unit;
- point value, minimum displayed tick and tick value;
- minimum lot and lot increment;
- retrieval/source/verification fields.

Month-specific FN and TZT quantities must be exported per delivery contract. Do not replace exact specifications with root medians or infer them from observed prices.

## 5. Reconstruct exchange-admissible intervals

Start from `data/ancillary/market_sessions_13_products.json`. For each exact contract and sample date:

1. convert regular open/close times with the named exchange timezone;
2. apply documented scheduled breaks and trading-date aliases;
3. consult the linked official holiday calendar for closures and early closes;
4. retain schedule changes using explicit effective dates;
5. write each admissible interval to `session_intervals.csv` with a stable `session_id`.

Do not apply static London offsets across DST transitions. The precise historical date-expanded interval artifact was not retained publicly, so this reconstruction must be independently reviewed against exchange calendars before an empirical run.

## 6. Export BFIX

Acquire London 16:00 BFIX observations for EUR, GBP, CNY and INR against USD, covering the causal lookback needed for the study. Preserve the workbook layout specified in `data/README.md`, including dates and LAST/BID/ASK columns. Currency direction/inversion is handled by the reader; never use a fixing published after the valuation time.

## 7. Use the public fee calibration

The repository automatically uses `data/ancillary/fees.csv` unless a local `DISSERTATION_DATA_ROOT/fees.csv` override exists. Brokerage remains USD 1 per contract-side in `config/production_config.R`. The unchanged U6/ZS transaction-notional rate is verified against MCX Circular `MCX/F&A/631/2024`; its authoritative filing URL and exact unit conversion are recorded in `data/ancillary/fee_schedule_provenance.csv`.

## 8. Place and validate inputs

The authorised input directory should contain:

```text
market_quotes.csv
contract_lifecycle.csv
session_intervals.csv
contract_specs.csv
bfix.xlsx
```

An identically schemed `candidate_pairs.csv` may also be supplied to override
the included `data/ancillary/candidate_pairs_78.csv`; otherwise the public
configuration is used automatically.

Then run, from the repository root:

```sh
export DISSERTATION_DATA_ROOT=/authorised/input/root
Rscript scripts/core/00_validate_inputs.R
```

Validation checks schema and presence; it cannot establish that a vendor export or holiday reconstruction is economically correct. Keep a private acquisition log with Bloomberg security identifiers, requested fields, retrieval dates and row/file hashes.
