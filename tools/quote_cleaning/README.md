# Reusable ticker-level quote cleaner

`quote_cleaning.py` and `clean_ticker_quotes.py` provide a standalone, auditable cleaner for future ticker-level one-minute futures data. They are not imported by `scripts/run_pipeline.R` and do not replace the dissertation's frozen production route.

## Price concepts

- `bid` and `ask` are executable quote sides.
- `quote_mid = (bid + ask) / 2` is the literal midpoint derived internally whenever both sides exist.
- a vendor or statistical `Close` is a distinct reference price. It is never treated as the quote midpoint.
- a supplied midpoint is checked against bid/ask only when configuration explicitly declares it to be a literal midpoint.

The cleaner detects and documents problems; it never interpolates, fills, swaps bid/ask, rounds timestamps into validity, winsorises prices or silently overwrites input.

## Hard rejection and warning rules

Hard rejection covers unparseable, missing or timezone-indeterminate timestamps; configured sample-bound failures; non-finite, zero or negative price fields; crossed quotes; conflicting duplicates by default; collapsed copies of exact duplicates; excessive relative quote width; and implausible causal local-midpoint displacement. Locked quotes, input disorder, off-grid timestamps, large positive gaps and stale-looking runs are warnings by default. Large gaps are computed chronologically and separately within ticker: `timestamp_gap` is the current valid timestamp minus the previous valid timestamp, and `large_positive_gap` means it exceeds `expected_frequency`. The first valid observation has no gap. Grid alignment and gap length are independent concepts. Gap rejection is available through `timing_gaps.reject_large_positive_gap`, but remains off by default because this generic utility has no exchange calendar.

Every row retains `original_row_id`, every applicable Boolean flag, and semicolon-delimited `reject_reasons` and `warning_reasons`; multiple reasons are retained. Rejected rows never lack a reason code.

## Authoritative V2 formula translation

The numerical definitions mirror `R/quote_quality_v2.R` and `config/production_config.R`:

```text
q = (ask - bid) / quote_mid

q_max = max(
  4 * tick_size / median_mid,
  Q_type8,0.995(q),
  min(exp(median(log(q)) + 8 * scaled_MAD(log(q))), 50 * median(q))
)

h_mid = max(
  log(1.25),
  15 * scaled_MAD(abs(diff(log(quote_mid)))),
  2 * Q_type8,0.999(abs(diff(log(quote_mid))))
)
```

`scaled_MAD(x) = 1.4826 * median(abs(x - median(x)))`; quantiles reproduce R type 8 directly. Width fitting uses strictly positive widths, so locked quotes never enter `log(0)`. If tick size is absent, it is not inferred: the remaining empirical components are fitted and `tick_size_floor_unavailable` is visible in the audit. Threshold JSON records all components and its SHA-256 content fingerprint.

The reference-close facility also translates the existing dissertation formula, but remains disabled by default because `Close` and literal midpoint are different objects. When enabled it uses the frozen fitted cutoff and never recalibrates on application data. Its edge behaviour is identical to the authoritative R definition: if empirical close-mid evidence is absent but a width cutoff is available, the close-mid cutoff is `q_max / 2`; if neither source is available, the rule is reported unavailable rather than inventing a cutoff.

## Calibration availability and fail-closed semantics

Full cleaning requires a fitted local-midpoint rule, a fitted width rule in bid/ask mode, and a fitted close-mid rule only when that optional rule is enabled. Rule availability is recorded separately as `AVAILABLE`, `UNAVAILABLE_MISSING_INPUT`, `UNAVAILABLE_INSUFFICIENT_CALIBRATION`, or `DISABLED_BY_CONFIG`. Input availability is not used as a substitute for calibration status.

The default `allow_partial_cleaning: false` fails closed when a mandatory fitted rule is unavailable. Available checks still run, but affected rows receive `cleaning_incomplete = true`, `calibration_unavailable = true`, the non-corruption reason `calibration_unavailable`, and can never have `clean_quote = true`. With the explicit opt-in `allow_partial_cleaning: true`, available rules still run and passing rows may have `partial_clean_quote = true`; they remain `clean_quote = false` because the full contract was not evaluated. Under-calibration is ticker-specific and does not contaminate a sufficiently calibrated ticker.

## Causal local reference

Application data are processed chronologically within ticker. The reference is the median of at most the previous 60 accepted midpoints and activates only after 20 exist. The current row and all future rows are excluded, and rejected rows do not enter subsequent history. A separately fitted cleaner stores only the accepted tail of its calibration period as starting history for a future application period. `fit-apply` deliberately omits that starting tail but is still labelled unsuitable for causal research because its thresholds use the same sample.

## Midpoint-only mode

With timestamp, ticker and midpoint but no bid/ask, the cleaner still performs timestamp, duplicate, positivity, ordering, grid, local-displacement and stale-price checks. Crossed, locked, quote-width and bid/ask-midpoint checks are nullable in row output and are explicitly reported as `NOT AVAILABLE — bid/ask fields absent` in summaries; zero failures are never fabricated.

## Fit, apply and outputs

Fit on a calibration/formation period, then apply its frozen JSON to a later period:

```bash
python tools/quote_cleaning/clean_ticker_quotes.py \
  --input data/formation_quotes.parquet \
  --output-dir outputs/formation_cleaning \
  --config tools/quote_cleaning/config/quote_cleaning_example.yaml \
  --mode fit

python tools/quote_cleaning/clean_ticker_quotes.py \
  --input data/future_quotes.parquet \
  --output-dir outputs/future_cleaning \
  --config tools/quote_cleaning/config/quote_cleaning_example.yaml \
  --mode apply \
  --thresholds outputs/formation_cleaning/quote_cleaning_thresholds.json
```

Exploratory same-sample cleaning is available but emits a causal-use warning:

```bash
python tools/quote_cleaning/clean_ticker_quotes.py \
  --input data/example_quotes.parquet \
  --output-dir outputs/quote_cleaning \
  --config tools/quote_cleaning/config/quote_cleaning_example.yaml \
  --mode fit-apply
```

CSV and Parquet input are supported; Parquet requires `pyarrow` or `fastparquet`. Outputs are `clean_quotes`, `quote_audit`, `rejected_quotes`, `quote_cleaning_summary.csv`, `quote_cleaning_thresholds.json`, and `quote_cleaning_by_session.csv`. Original inputs are never modified.

## Deliberate separation from pair feasibility

This utility makes no pair-window admission decision. `jointly_clean_opportunity_counts()` can align two already-audited streams and report raw and jointly clean counts, but it does not apply the dissertation's 200-event, 18-of-20-session or 90% pair criteria.

## Assumptions and unavailable metadata

Tick sizes and timezones must be supplied in data, metadata or configuration; neither is inferred. Numeric timestamp units must be explicit. Large positive time gaps are diagnostic rather than automatic corruption because exchange closures and scheduled breaks require exchange calendars not bundled with this generic tool.
