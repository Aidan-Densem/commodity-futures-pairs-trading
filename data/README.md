# Data boundary, schemas and public ancillary inputs

## What is not redistributed

The empirical one-minute Bloomberg Professional quotes, Bloomberg-derived exact-contract lifecycle and monetary records, and Bloomberg BFIX histories are licence-restricted and are not present in this repository. Do not commit them or any raw/cleaned derivative that reproduces their observations.

Set `DISSERTATION_DATA_ROOT` to an authorised local directory containing the five licensed/reconstructed files below. The included public fee schedule and 78-pair configuration are used automatically unless identically schemed overrides are placed in that directory.

## Final 13-product universe

The authoritative root-level universe is [`ancillary/product_universe_13.csv`](ancillary/product_universe_13.csv):

```text
CO1 CL1 HO1 XB1 NG1 QS1 EN1 OQA1 FN1 TZT1 BIT1 U61 ZS1
```

The CSV records product names, venues, currencies, public root-level contract multipliers/ticks and the corresponding session-metadata key. The exact delivery-contract identifiers used by the final study are separately listed in [`ancillary/exact_contract_manifest.csv`](ancillary/exact_contract_manifest.csv). Exact-month quantities, lifecycle values and observations remain authoritative in the licensed external inputs. The study sample runs from 12 November 2025 through 28 May 2026; acquire the listed contracts and verify that their lifecycle coverage supports every active interval and the five-weekday pre-expiry roll.

## Required external files

### `market_quotes.csv`

Required columns:

```text
timestamp,generic,contract,bid,ask,close
```

- one row per exact listed contract and one-minute timestamp;
- timestamp is interpreted in `Europe/London` and must be unambiguous;
- `generic` is one of the 13 canonical generics;
- `contract` is the stable exact-security identifier shared by every input table;
- bid/ask are the statistical and executable quote fields;
- close is retained only as a diagnostic reference and is never substituted for missing bid/ask.

### `candidate_pairs.csv`

Required columns:

```text
pair_id,y_generic,x_generic
```

The dissertation starts from all 78 unordered pairs of the 13 products. The canonical deterministic orientation is included as [`ancillary/candidate_pairs_78.csv`](ancillary/candidate_pairs_78.csv). A local `candidate_pairs.csv` with the same schema may override it. `pair_id` must be unique (for example `CO_CL`), and leg orientation must remain fixed across stages. This configuration contains no fitted parameters, testing outcomes or P&L.

### `contract_lifecycle.csv`

Required columns:

```text
generic,contract,delivery_month,first_trade_date,last_trade_date
```

`delivery_month` is `YYYY-MM`. The Bloomberg lifecycle field known to have been consumed for expiry ordering is `LAST_TRADEABLE_DT`; the exact original export mnemonic for every remaining canonical column was not independently recoverable and is not invented here. The repository derives its own synchronous schedule and rolls both legs five Monday–Friday weekdays before the earlier last-trade date.

### `session_intervals.csv`

Required columns:

```text
contract,session_id,open_timestamp,close_timestamp,admissible
```

This is a date-expanded exact-contract table. Intervals must reflect regular sessions, named time zones, holidays, early closes and schedule changes over the sample. Product-level regular hours, scheduled breaks, session aliases, named zones, holiday-calendar URLs and source links are publicly included in [`ancillary/market_sessions_13_products.json`](ancillary/market_sessions_13_products.json).

**Session reconstruction requirement:** the date-expanded interval table is not included. The public JSON provides substantive product-level metadata, but it cannot reproduce every historical holiday or early close without consulting the linked exchange calendars. Construct and independently check the table before an empirical run.

### `contract_specs.csv`

Required fields:

```text
Generic,Root,ContractMonth,BloombergTickerOriginal,
BloombergSecurityResolved,BloombergProductName,Product,ExchangeCode,
ProductCode,ProductCodeSource,QuotationCurrency,PnLCurrency,
ContractQuantity,ContractUnit,PriceQuotation,
PriceScaleToPnLCurrencyPerUnit,PointValueNativePerDisplayedPoint,
MinimumPriceIncrementDisplayed,TickValueNative,MinimumLotContracts,
LotIncrementContracts,SpecificationClass,CoreSpecificationSource,
RetrievalDate,CoreMonetarySpecificationVerified,VerificationScope,
VerificationNotes,BloombergGlobalID
```

Exact identifiers must join unambiguously to quotes and lifecycle data. Quantity, unit, currency, price scale, point value, tick size/value and lot rules drive whole-contract sizing and monetary P&L and must be verified for each delivery month rather than inferred from prices.

### `ancillary/exact_contract_manifest.csv`

Identifier-only public manifest of the final 208-security universe: 13 generics by 16 delivery months from 2025-07 through 2026-10. It gives the recovered resolved security, source alias, delivery month, product and venue. [`ancillary/exact_contract_manifest_provenance.csv`](ancillary/exact_contract_manifest_provenance.csv) records the authoritative source-snapshot SHA-256 and excluded fields. This manifest fixes what to request and how to join it; it does **not** replace licensed lifecycle dates, exact-month monetary specifications or observations.

### `bfix.xlsx`

The first worksheet uses the retained production layout. Row 4 has `EUR L160 CURNCY`, `GBP L160 CURNCY`, `CNY L160 CURNCY` and `INR L160 CURNCY` in columns B, E, H and K. Data begin after row 6: column A is date; each currency has `LAST`, `BID`, `ASK` in three adjacent columns. The code treats USD as identity and uses the most recent causally available London 16:00 fixing, never a later observation.

The strict importer expects 232 unique dates from 2025-09-01 through 2026-07-21, with 229 populated observations per currency and common non-fixing dates 2025-12-25, 2026-01-01 and 2026-04-03. The empirical workbook is not redistributed.

## Included public ancillary files

### `ancillary/fees.csv`

The actual long-form research calibration consumed by the monetary fee engine. Required fields are:

```text
fee_key_type,exact_contract,root,exchange,fee_component,fee_basis,
fee_rate,fee_currency,fee_unit,charge_side,effective_from,
effective_until,source,verified
```

Extra provenance columns are retained. Brokerage is separately configured as USD 1 per contract-side. Exchange/clearing sources, units and transformations are recorded in [`ancillary/fee_schedule_provenance.csv`](ancillary/fee_schedule_provenance.csv).

All enabled rows are verified. In particular, U6 and ZS retain the calibrated rate `0.000021` of transaction notional per side, now traced to MCX Circular `MCX/F&A/631/2024` (INR 2.10 per INR 100,000 turnover, effective 2024-10-01), preserved in [MCX's official corporate filing with BSE](https://www.bseindia.com/xml-data/corpfiling/AttachHis/c4a7c1d1-37c0-4c00-82c3-66ebf482d383.pdf). No taxes, GST, CTT, stamp duty or invented clearing split are added.

### `ancillary/market_sessions_13_products.json`

Project-authored reference metadata for the 13 products. It records venue, exchange timezone, regular weekly hours, breaks, settlement/boundary metadata, session-date aliases, holiday-calendar sources and public source URLs. Confidence and verification caveats identify remaining source uncertainty. London-time values labelled as hints must not replace named-zone conversion, and this JSON is not a date-expanded production interval table.

### Other traceability files

- [`ancillary/product_universe_13.csv`](ancillary/product_universe_13.csv): canonical public root-level universe.
- [`ancillary/candidate_pairs_78.csv`](ancillary/candidate_pairs_78.csv): deterministic public 78-pair configuration.
- [`ancillary/exact_contract_manifest.csv`](ancillary/exact_contract_manifest.csv): final 208-security identifier-only universe.
- [`INPUT_MANIFEST.csv`](INPUT_MANIFEST.csv): machine-readable licensed/public boundary and pipeline entry points.
- [`../docs/DATA_ACQUISITION.md`](../docs/DATA_ACQUISITION.md): reconstructive acquisition procedure.
- [`../docs/DATA_LINEAGE.md`](../docs/DATA_LINEAGE.md): source-to-model lineage.

## Derived objects are not external inputs

The repository produces roll assignments, clean midpoint series, active clocks, segments, windows, Kalman/OU fits, candidates, costs, rankings, transition samples, GH fits, thresholds, routes, fills, ledgers and inference beneath ignored `output/`. Supplying historical fitted objects as raw substitutes is not supported.

## Synthetic fixture

`sample/tiny_quotes.csv` is artificial and supports smoke tests only. It cannot reproduce an empirical estimate or validate licensed-data coverage.
