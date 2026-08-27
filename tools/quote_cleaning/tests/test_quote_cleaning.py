#!/usr/bin/env python3
"""Synthetic-only unit tests for the standalone ticker quote cleaner."""

from __future__ import annotations

import copy
import json
import math
import sys
import tempfile
import unittest
from pathlib import Path

import numpy as np
import pandas as pd

TOOL_ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(TOOL_ROOT))

from quote_cleaning import (  # noqa: E402
    NOT_AVAILABLE_BID_ASK,
    apply_quote_cleaner,
    fit_apply_quote_cleaner,
    fit_quote_cleaner,
    load_fitted_cleaner,
    r_type8_quantile,
    save_fitted_cleaner,
    scaled_mad,
    write_cleaning_outputs,
)


def base_config(**overrides):
    config = {
        "timezone": "UTC",
        "expected_frequency": "1min",
        "v2": {
            "minimum_calibration_quotes": 3,
            "local_reference_observations": 60,
            "local_reference_minimum": 3,
        },
        "output": {"format": "csv"},
    }
    for key, value in overrides.items():
        if isinstance(value, dict) and isinstance(config.get(key), dict):
            config[key].update(value)
        else:
            config[key] = value
    return config


def quotes(n=30, ticker="A", start="2025-01-01 09:00:00", drift=0.01):
    timestamp = pd.date_range(start, periods=n, freq="min").astype(str)
    mid = 100.0 + np.arange(n) * drift
    return pd.DataFrame({
        "timestamp": timestamp,
        "ticker": ticker,
        "bid": mid - 0.01,
        "ask": mid + 0.01,
        "close": mid,
        "tick_size": 0.01,
    })


class QuoteCleanerTests(unittest.TestCase):
    def fitted(self, calibration=None, config=None):
        calibration = quotes() if calibration is None else calibration
        return fit_quote_cleaner(calibration, config=config or base_config())

    def test_ordinary_valid_quote(self):
        data = quotes(5)
        result = fit_apply_quote_cleaner(data, config=base_config())
        self.assertTrue(result.audit["clean_quote"].all())

    def test_crossed_quote(self):
        calibration = quotes()
        fitted = self.fitted(calibration)
        data = quotes(1, start="2025-02-01 09:00:00")
        data.loc[0, ["bid", "ask"]] = [101.0, 100.0]
        result = apply_quote_cleaner(data, fitted, use_calibration_history=False)
        self.assertTrue(result.audit.loc[0, "crossed_quote"])
        self.assertIn("crossed_quote", result.audit.loc[0, "reject_reasons"])

    def test_locked_quote_warns_but_passes(self):
        fitted = self.fitted()
        data = quotes(1, start="2025-02-01 09:00:00")
        data.loc[0, ["bid", "ask"]] = [100.0, 100.0]
        result = apply_quote_cleaner(data, fitted, use_calibration_history=False)
        self.assertTrue(result.audit.loc[0, "locked_quote"])
        self.assertTrue(result.audit.loc[0, "clean_quote"])
        self.assertIn("locked_quote", result.audit.loc[0, "warning_reasons"])

    def test_declared_literal_midpoint_inconsistency_rejected(self):
        config = base_config(quotes={"supplied_midpoint_is_literal": True})
        calibration = quotes()
        calibration["mid"] = (calibration["bid"] + calibration["ask"]) / 2
        fitted = self.fitted(calibration, config)
        data = calibration.iloc[:1].copy()
        data.loc[0, "timestamp"] = "2025-02-01 09:00:00"
        data.loc[0, "mid"] = 999.0
        result = apply_quote_cleaner(data, fitted, use_calibration_history=False)
        self.assertIn("literal_midpoint_inconsistent", result.audit.loc[0, "reject_reasons"])

    def test_optional_reference_close_rule(self):
        config = base_config(close_mid_consistency={"enabled": True, "reject": True})
        fitted = self.fitted(config=config)
        data = quotes(1, start="2025-02-01 09:00:00")
        data.loc[0, "close"] = 150.0
        result = apply_quote_cleaner(data, fitted, use_calibration_history=False)
        self.assertTrue(result.audit.loc[0, "close_mid_inconsistent"])
        self.assertIn("close_mid_inconsistent", result.audit.loc[0, "reject_reasons"])

    def test_stale_quote_is_diagnostic_by_default(self):
        config = base_config(stale_quote={"enabled": True, "run_length": 3, "reject": False})
        fitted = self.fitted(config=config)
        data = quotes(3, start="2025-02-01 09:00:00", drift=0)
        result = apply_quote_cleaner(data, fitted, use_calibration_history=False)
        self.assertTrue(result.audit.loc[2, "possible_stale_quote"])
        self.assertTrue(result.audit.loc[2, "clean_quote"])
        self.assertIn("possible_stale_quote", result.audit.loc[2, "warning_reasons"])

    def test_missing_infinite_zero_and_negative_prices(self):
        fitted = self.fitted()
        cases = [(np.nan, 1.0), (1.0, np.inf), (0.0, 1.0), (-1.0, 1.0)]
        data = quotes(len(cases), start="2025-02-01 09:00:00")
        for index, values in enumerate(cases):
            data.loc[index, ["bid", "ask"]] = values
        result = apply_quote_cleaner(data, fitted, use_calibration_history=False)
        self.assertTrue(result.audit["hard_reject"].all())
        self.assertTrue((result.audit["reject_reasons"].str.len() > 0).all())

    def test_timestamp_parse_corruption(self):
        fitted = self.fitted()
        data = quotes(1)
        data.loc[0, "timestamp"] = "not-a-timestamp"
        result = apply_quote_cleaner(data, fitted, use_calibration_history=False)
        self.assertIn("timestamp_parse_failure", result.audit.loc[0, "reject_reasons"])

    def test_numeric_timestamp_unit_is_not_guessed(self):
        fitted = self.fitted()
        data = quotes(1)
        data.loc[0, "timestamp"] = 1700000000
        result = apply_quote_cleaner(data, fitted, use_calibration_history=False)
        self.assertIn("ambiguous_numeric_timestamp", result.audit.loc[0, "reject_reasons"])

    def test_exact_duplicate_collapses_deterministically(self):
        fitted = self.fitted()
        one = quotes(1, start="2025-02-01 09:00:00")
        data = pd.concat([one, one], ignore_index=True)
        result = apply_quote_cleaner(data, fitted, use_calibration_history=False)
        self.assertEqual(int(result.audit["duplicate_exact"].sum()), 1)
        self.assertEqual(len(result.clean), 1)

    def test_conflicting_duplicates_are_rejected(self):
        fitted = self.fitted()
        data = pd.concat([
            quotes(1, start="2025-02-01 09:00:00"),
            quotes(1, start="2025-02-01 09:00:00"),
        ], ignore_index=True)
        data.loc[1, ["bid", "ask"]] = [101.0, 101.02]
        result = apply_quote_cleaner(data, fitted, use_calibration_history=False)
        self.assertTrue(result.audit["duplicate_conflict"].all())
        self.assertTrue(result.audit["hard_reject"].all())

    def test_out_of_order_is_audited(self):
        fitted = self.fitted()
        data = quotes(3, start="2025-02-01 09:00:00")
        data.loc[:, "timestamp"] = [
            "2025-02-01 09:01:00", "2025-02-01 09:00:00", "2025-02-01 09:02:00"
        ]
        result = apply_quote_cleaner(data, fitted, use_calibration_history=False)
        self.assertEqual(int(result.audit["out_of_order_originally"].sum()), 1)
        self.assertEqual(len(result.clean), 3)

    def test_off_grid_is_warning_by_default(self):
        fitted = self.fitted()
        data = quotes(1)
        data.loc[0, "timestamp"] = "2025-02-01 09:00:30"
        result = apply_quote_cleaner(data, fitted, use_calibration_history=False)
        self.assertTrue(result.audit.loc[0, "timestamp_off_grid"])
        self.assertTrue(result.audit.loc[0, "clean_quote"])

    def test_excessive_width_rejected(self):
        fitted = self.fitted()
        data = quotes(2, start="2025-02-01 09:00:00")
        data.loc[1, ["bid", "ask"]] = [90.0, 110.0]
        result = apply_quote_cleaner(data, fitted, use_calibration_history=False)
        self.assertFalse(result.audit.loc[0, "excessive_relative_quote_width"])
        self.assertTrue(result.audit.loc[1, "excessive_relative_quote_width"])

    def test_v2_formula_matches_authoritative_fixed_values(self):
        q = np.array([.001, .0011, .0009, .0012, .00105, .00115, .001, .00095, .0013, .05])
        sigma = scaled_mad(np.log(q))
        robust = math.exp(np.median(np.log(q)) + 8 * sigma)
        q_max = max(.0004, r_type8_quantile(q, .995), min(robust, 50 * np.median(q)))
        self.assertAlmostEqual(sigma, 0.1351549700513581, places=15)
        self.assertAlmostEqual(robust, 0.003168600261216029, places=15)
        self.assertAlmostEqual(q_max, 0.05, places=15)
        moves = np.array([.001, .002, .0015, .003, .0025, .004, .006, .005])
        h_mid = max(math.log(1.25), 15 * scaled_mad(moves), 2 * r_type8_quantile(moves, .999))
        self.assertAlmostEqual(h_mid, 0.22314355131420976, places=15)

    def test_local_midpoint_corruption(self):
        calibration = quotes(30)
        fitted = self.fitted(calibration)
        data = quotes(5, start="2025-02-01 09:00:00")
        data.loc[4, ["bid", "ask", "close"]] = [199.99, 200.01, 200.0]
        result = apply_quote_cleaner(data, fitted, use_calibration_history=False)
        self.assertTrue(result.audit.loc[4, "implausible_local_midpoint_displacement"])

    def test_coherent_ten_percent_move_passes_floor(self):
        fitted = self.fitted()
        data = quotes(5, start="2025-02-01 09:00:00", drift=0)
        data.loc[4, ["bid", "ask", "close"]] = [109.99, 110.01, 110.0]
        result = apply_quote_cleaner(data, fitted, use_calibration_history=False)
        self.assertFalse(result.audit.loc[4, "implausible_local_midpoint_displacement"])
        self.assertTrue(result.audit.loc[4, "clean_quote"])

    def test_causality_future_change_does_not_change_past(self):
        fitted = self.fitted()
        first = quotes(10, start="2025-02-01 09:00:00")
        second = first.copy()
        second.loc[9, ["bid", "ask", "close"]] = [999.99, 1000.01, 1000.0]
        result_a = apply_quote_cleaner(first, fitted, use_calibration_history=False).audit
        result_b = apply_quote_cleaner(second, fitted, use_calibration_history=False).audit
        columns = ["clean_quote", "reject_reasons", "local_reference_mid"]
        pd.testing.assert_frame_equal(result_a.loc[:8, columns], result_b.loc[:8, columns])

    def test_local_history_minimum_and_sixty_cap(self):
        config = base_config(v2={
            "minimum_calibration_quotes": 3,
            "local_reference_minimum": 20,
            "local_reference_observations": 60,
        })
        fitted = self.fitted(config=config)
        fitted = copy.deepcopy(fitted)
        fitted["tickers"]["A"]["local_history_mid"] = []
        data = quotes(65, start="2025-02-01 09:00:00", drift=1)
        result = apply_quote_cleaner(data, fitted, use_calibration_history=False).audit
        self.assertTrue(pd.isna(result.loc[19, "local_reference_mid"]))
        self.assertTrue(pd.notna(result.loc[20, "local_reference_mid"]))
        expected = float(np.median((100 + np.arange(60))))
        self.assertAlmostEqual(result.loc[60, "local_reference_mid"], expected)

    def test_per_ticker_independence(self):
        a = quotes(20, ticker="A")
        b = quotes(20, ticker="B")
        combined = pd.concat([a, b], ignore_index=True)
        base_fit = fit_quote_cleaner(combined, config=base_config())
        changed = combined.copy()
        changed.loc[0, ["bid", "ask"]] = [50.0, 150.0]
        changed_fit = fit_quote_cleaner(changed, config=base_config())
        self.assertEqual(base_fit["tickers"]["B"]["q_max"], changed_fit["tickers"]["B"]["q_max"])
        self.assertEqual(base_fit["tickers"]["B"]["h_mid"], changed_fit["tickers"]["B"]["h_mid"])

    def test_frozen_fit_apply_does_not_change_thresholds(self):
        fitted = self.fitted()
        before = json.dumps(fitted, sort_keys=True)
        extreme = quotes(5, start="2025-02-01 09:00:00")
        extreme.loc[4, ["bid", "ask"]] = [1.0, 1000.0]
        apply_quote_cleaner(extreme, fitted, use_calibration_history=False)
        self.assertEqual(before, json.dumps(fitted, sort_keys=True))

    def test_midpoint_only_marks_bid_ask_rules_unavailable(self):
        data = quotes(10)[["timestamp", "ticker", "close"]].rename(columns={"close": "mid"})
        fitted = fit_quote_cleaner(data, config=base_config())
        result = apply_quote_cleaner(data, fitted, use_calibration_history=False)
        self.assertTrue(result.audit["crossed_quote"].isna().all())
        self.assertTrue(result.audit["excessive_relative_quote_width"].isna().all())
        self.assertEqual(result.summary.loc[0, "bid_ask_rules_status"], NOT_AVAILABLE_BID_ASK)

    def test_idempotence_under_frozen_thresholds(self):
        fitted = self.fitted()
        data = quotes(10)
        data.loc[4, ["bid", "ask"]] = [101.0, 100.0]
        first = apply_quote_cleaner(data, fitted, use_calibration_history=False)
        second = apply_quote_cleaner(first.clean, fitted, use_calibration_history=False)
        self.assertEqual(
            first.clean["original_row_id"].tolist(),
            second.clean["original_row_id"].tolist(),
        )

    def test_multiple_reasons_are_retained(self):
        fitted = self.fitted()
        data = quotes(1)
        data.loc[0, "timestamp"] = "bad"
        data.loc[0, ["bid", "ask"]] = [0.0, -1.0]
        result = apply_quote_cleaner(data, fitted, use_calibration_history=False)
        reasons = result.audit.loc[0, "reject_reasons"].split(";")
        self.assertGreaterEqual(len(reasons), 3)

    def test_threshold_serialisation_and_csv_outputs(self):
        result = fit_apply_quote_cleaner(quotes(10), config=base_config())
        with tempfile.TemporaryDirectory() as directory:
            threshold_path = Path(directory) / "thresholds.json"
            save_fitted_cleaner(result.fitted_cleaner, threshold_path)
            loaded = load_fitted_cleaner(threshold_path)
            self.assertEqual(loaded["fingerprint"], result.fitted_cleaner["fingerprint"])
            paths = write_cleaning_outputs(result, directory, output_format="csv")
            self.assertTrue(all(path.exists() for path in paths.values()))
            rejected = pd.read_csv(paths["rejected"])
            self.assertTrue(rejected.empty or rejected["reject_reasons"].str.len().gt(0).all())

    # QG1-QG5: large positive gaps are chronological, per-ticker diagnostics
    # and are deliberately distinct from timestamp-grid alignment.
    def test_QG1_large_positive_gap_is_warning_by_default(self):
        fitted = self.fitted()
        data = quotes(2, start="2025-02-01 09:00:00")
        data.loc[:, "timestamp"] = [
            "2025-02-01 09:00:00", "2025-02-01 09:10:00"
        ]
        result = apply_quote_cleaner(data, fitted, use_calibration_history=False)
        self.assertFalse(result.audit.loc[0, "large_positive_gap"])
        self.assertTrue(result.audit.loc[1, "large_positive_gap"])
        self.assertEqual(result.audit.loc[1, "timestamp_gap"], pd.Timedelta("10min"))
        self.assertTrue(result.audit["clean_quote"].all())
        self.assertIn("large_positive_gap", result.audit.loc[1, "warning_reasons"])

    def test_QG2_expected_gap_is_not_large(self):
        fitted = self.fitted()
        result = apply_quote_cleaner(
            quotes(2, start="2025-02-01 09:00:00"), fitted,
            use_calibration_history=False,
        )
        self.assertFalse(result.audit["large_positive_gap"].any())
        self.assertEqual(result.audit.loc[1, "timestamp_gap"], pd.Timedelta("1min"))

    def test_QG3_off_grid_and_large_gap_are_distinct(self):
        fitted = self.fitted()
        data = quotes(2, start="2025-02-01 09:00:00")
        data.loc[:, "timestamp"] = [
            "2025-02-01 09:00:30", "2025-02-01 09:01:00"
        ]
        result = apply_quote_cleaner(data, fitted, use_calibration_history=False)
        self.assertTrue(result.audit.loc[0, "timestamp_off_grid"])
        self.assertFalse(result.audit["large_positive_gap"].any())

    def test_QG4_gap_state_is_independent_by_ticker(self):
        data = pd.concat([
            quotes(2, ticker="A", start="2025-02-01 09:00:00"),
            quotes(2, ticker="B", start="2025-02-01 09:09:00"),
        ], ignore_index=True).iloc[[0, 2, 1, 3]].reset_index(drop=True)
        calibration = pd.concat([quotes(10, ticker="A"), quotes(10, ticker="B")])
        fitted = fit_quote_cleaner(calibration, config=base_config())
        result = apply_quote_cleaner(data, fitted, use_calibration_history=False)
        self.assertFalse(result.audit["large_positive_gap"].any())
        self.assertEqual(result.audit["timestamp_gap"].notna().sum(), 2)

    def test_QG5_first_valid_observation_has_no_gap(self):
        fitted = self.fitted()
        data = quotes(2, start="2025-02-01 09:00:00")
        result = apply_quote_cleaner(data, fitted, use_calibration_history=False)
        self.assertTrue(pd.isna(result.audit.loc[0, "timestamp_gap"]))
        self.assertFalse(result.audit.loc[0, "large_positive_gap"])

    # QC1-QC5: full cleaning fails closed when a mandatory frozen rule is
    # unavailable. Partial mode is explicit and never claims full cleanliness.
    def test_QC1_inadequate_calibration_fails_closed(self):
        fitted = self.fitted(quotes(2), base_config())
        result = apply_quote_cleaner(
            quotes(1, start="2025-02-01 09:00:00"), fitted,
            use_calibration_history=False,
        )
        self.assertTrue(result.audit.loc[0, "cleaning_incomplete"])
        self.assertTrue(result.audit.loc[0, "calibration_unavailable"])
        self.assertFalse(result.audit.loc[0, "clean_quote"])

    def test_QC2_calibration_unavailable_is_non_corruption_reason(self):
        fitted = self.fitted(quotes(2), base_config())
        result = apply_quote_cleaner(
            quotes(1, start="2025-02-01 09:00:00"), fitted,
            use_calibration_history=False,
        )
        reasons = result.audit.loc[0, "reject_reasons"]
        self.assertEqual(reasons, "calibration_unavailable")
        self.assertNotIn("crossed_quote", reasons)
        self.assertNotIn("excessive_relative_quote_width", reasons)

    def test_QC3_partial_mode_is_explicit_and_not_full_clean(self):
        config = base_config(allow_partial_cleaning=True)
        fitted = self.fitted(quotes(2), config)
        result = apply_quote_cleaner(
            quotes(1, start="2025-02-01 09:00:00"), fitted,
            use_calibration_history=False,
        )
        self.assertTrue(result.audit.loc[0, "cleaning_incomplete"])
        self.assertTrue(result.audit.loc[0, "partial_clean_quote"])
        self.assertFalse(result.audit.loc[0, "clean_quote"])
        self.assertFalse(result.audit.loc[0, "hard_reject"])
        self.assertIn(
            "calibration_unavailable_partial_cleaning",
            result.audit.loc[0, "warning_reasons"],
        )

    def test_QC4_adequate_calibration_remains_fully_clean(self):
        fitted = self.fitted()
        result = apply_quote_cleaner(
            quotes(1, start="2025-02-01 09:00:00"), fitted,
            use_calibration_history=False,
        )
        self.assertFalse(result.audit.loc[0, "cleaning_incomplete"])
        self.assertTrue(result.audit.loc[0, "clean_quote"])

    def test_QC5_calibration_availability_is_ticker_specific(self):
        calibration = pd.concat([
            quotes(2, ticker="A"), quotes(4, ticker="B")
        ], ignore_index=True)
        fitted = fit_quote_cleaner(calibration, config=base_config())
        application = pd.concat([
            quotes(1, ticker="A", start="2025-02-01 09:00:00"),
            quotes(1, ticker="B", start="2025-02-01 09:00:00"),
        ], ignore_index=True)
        result = apply_quote_cleaner(
            application, fitted, use_calibration_history=False
        ).audit.set_index("ticker_clean")
        self.assertTrue(result.loc["A", "cleaning_incomplete"])
        self.assertFalse(result.loc["A", "clean_quote"])
        self.assertFalse(result.loc["B", "cleaning_incomplete"])
        self.assertTrue(result.loc["B", "clean_quote"])

    # CM1-CM4: optional close-mid rule, including exact q_max / 2 parity.
    def test_CM1_populated_close_evidence_produces_cutoff(self):
        config = base_config(close_mid_consistency={"enabled": True, "reject": True})
        fitted = self.fitted(config=config)
        contract = fitted["tickers"]["A"]
        self.assertEqual(contract["close_mid_rule_status"], "AVAILABLE")
        self.assertTrue(math.isfinite(contract["close_mid_consistency_cutoff"]))
        self.assertEqual(contract["close_mid_calibration_basis"], "EMPIRICAL_PLUS_QMAX_FLOOR")

    def test_CM2_close_absence_uses_half_width_floor(self):
        config = base_config(close_mid_consistency={"enabled": True, "reject": True})
        calibration = quotes(10)
        calibration["close"] = np.nan
        fitted = self.fitted(calibration, config)
        contract = fitted["tickers"]["A"]
        self.assertEqual(contract["close_mid_rule_status"], "AVAILABLE")
        self.assertEqual(contract["close_mid_calibration_basis"], "QMAX_FLOOR_ONLY")
        self.assertAlmostEqual(
            contract["close_mid_consistency_cutoff"], contract["q_max"] / 2.0
        )

    def test_CM3_close_rule_disabled_is_machine_readable(self):
        contract = self.fitted()["tickers"]["A"]
        self.assertEqual(contract["close_mid_rule_status"], "DISABLED_BY_CONFIG")
        self.assertIsNone(contract["close_mid_consistency_cutoff"])

    def test_CM4_no_close_evidence_and_no_width_floor_is_unavailable(self):
        config = base_config(close_mid_consistency={"enabled": True, "reject": True})
        calibration = quotes(3)
        calibration["bid"] = calibration["ask"] = 100.0
        calibration["close"] = np.nan
        fitted = self.fitted(calibration, config)
        contract = fitted["tickers"]["A"]
        self.assertIsNone(contract["q_max"])
        self.assertIsNone(contract["close_mid_consistency_cutoff"])
        self.assertEqual(
            contract["close_mid_rule_status"],
            "UNAVAILABLE_INSUFFICIENT_CALIBRATION",
        )


if __name__ == "__main__":
    unittest.main(verbosity=2)
