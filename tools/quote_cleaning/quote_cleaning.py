"""Auditable ticker-level futures quote cleaning.

This standalone future-research utility mirrors the dissertation V2 robust
quote-width and local-midpoint formulae.  It is intentionally not imported by
the dissertation production pipeline.
"""

from __future__ import annotations

import copy
import hashlib
import json
import math
import re
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Dict, Iterable, List, Mapping, MutableMapping, Optional, Sequence, Tuple

import numpy as np
import pandas as pd


UTILITY_VERSION = "ticker_quote_cleaner_v1.1.0"
V2_FORMULA_VERSION = "quote_quality_v2.0.0-causal-formation-robust"
NOT_AVAILABLE_BID_ASK = "NOT AVAILABLE — bid/ask fields absent"
RULE_AVAILABLE = "AVAILABLE"
RULE_MISSING_INPUT = "UNAVAILABLE_MISSING_INPUT"
RULE_INSUFFICIENT_CALIBRATION = "UNAVAILABLE_INSUFFICIENT_CALIBRATION"
RULE_DISABLED = "DISABLED_BY_CONFIG"


DEFAULT_CONFIG: Dict[str, Any] = {
    "columns": {
        "timestamp": "timestamp",
        "ticker": "ticker",
        "bid": "bid",
        "ask": "ask",
        "midpoint": "mid",
        "close": "close",
        "tick_size": "tick_size",
    },
    "timezone": None,
    "timezone_by_ticker": {},
    "numeric_timestamp_unit": None,
    "expected_frequency": "1min",
    "strict_grid": False,
    "timing_gaps": {"reject_large_positive_gap": False},
    "allow_partial_cleaning": False,
    "sample_bounds": {"earliest": None, "latest": None},
    "duplicates": {
        "collapse_exact": True,
        "reject_conflicting": True,
    },
    "quotes": {
        "reject_locked": False,
        "supplied_midpoint_is_literal": False,
        "reject_midpoint_inconsistency": True,
        "midpoint_absolute_tolerance": 1e-12,
        "midpoint_relative_tolerance": 1e-10,
    },
    "v2": {
        "minimum_calibration_quotes": 200,
        "log_spread_mad_multiplier": 8.0,
        "spread_median_multiple": 50.0,
        "spread_empirical_quantile": 0.995,
        "tick_floor_multiple": 4.0,
        "price_return_mad_multiplier": 15.0,
        "price_return_quantile": 0.999,
        "price_quantile_multiplier": 2.0,
        "price_relative_floor": math.log(1.25),
        "local_reference_observations": 60,
        "local_reference_minimum": 20,
        "close_mid_mad_multiplier": 8.0,
        "close_mid_empirical_quantile": 0.995,
    },
    "close_mid_consistency": {
        "enabled": False,
        "reject": True,
    },
    "stale_quote": {
        "enabled": True,
        "run_length": 30,
        "reject": False,
    },
    "output": {"format": "parquet"},
}


def deep_merge(base: Mapping[str, Any], override: Optional[Mapping[str, Any]]) -> Dict[str, Any]:
    """Return a recursive copy of ``base`` updated by ``override``."""
    result = copy.deepcopy(dict(base))
    if not override:
        return result
    for key, value in override.items():
        if isinstance(value, Mapping) and isinstance(result.get(key), Mapping):
            result[key] = deep_merge(result[key], value)
        else:
            result[key] = copy.deepcopy(value)
    return result


def resolved_config(config: Optional[Mapping[str, Any]] = None) -> Dict[str, Any]:
    result = deep_merge(DEFAULT_CONFIG, config)
    unit = result.get("numeric_timestamp_unit")
    if unit not in (None, "s", "ms", "us", "ns"):
        raise ValueError("numeric_timestamp_unit must be one of s, ms, us, ns, or null")
    if int(result["v2"]["local_reference_minimum"]) < 1:
        raise ValueError("local_reference_minimum must be positive")
    if int(result["v2"]["local_reference_observations"]) < int(
        result["v2"]["local_reference_minimum"]
    ):
        raise ValueError("local reference window cannot be smaller than its minimum")
    return result


def scaled_mad(values: Iterable[float]) -> float:
    """Scaled MAD used by the authoritative R V2 implementation."""
    x = np.asarray(list(values), dtype=float)
    x = x[np.isfinite(x)]
    if x.size < 2:
        return math.nan
    centre = float(np.median(x))
    return 1.4826 * float(np.median(np.abs(x - centre)))


def r_type8_quantile(values: Iterable[float], probability: float) -> float:
    """R ``quantile(..., type=8)`` without depending on NumPy version."""
    x = np.asarray(list(values), dtype=float)
    x = np.sort(x[np.isfinite(x)])
    if x.size == 0:
        return math.nan
    p = float(probability)
    if not 0 <= p <= 1:
        raise ValueError("probability must lie in [0, 1]")
    if p == 0:
        return float(x[0])
    if p == 1:
        return float(x[-1])
    h = (x.size + 1.0 / 3.0) * p + 1.0 / 3.0
    j = math.floor(h)
    gamma = h - j
    if j <= 0:
        return float(x[0])
    if j >= x.size:
        return float(x[-1])
    return float((1.0 - gamma) * x[j - 1] + gamma * x[j])


def _jsonable(value: Any) -> Any:
    if isinstance(value, Mapping):
        return {str(k): _jsonable(v) for k, v in value.items()}
    if isinstance(value, (list, tuple)):
        return [_jsonable(v) for v in value]
    if isinstance(value, (np.integer,)):
        return int(value)
    if isinstance(value, (np.floating,)):
        return None if not np.isfinite(value) else float(value)
    if isinstance(value, (pd.Timestamp,)):
        return value.isoformat()
    if isinstance(value, float) and not math.isfinite(value):
        return None
    return value


def cleaner_fingerprint(payload: Mapping[str, Any]) -> str:
    encoded = json.dumps(_jsonable(payload), sort_keys=True, separators=(",", ":"))
    return hashlib.sha256(encoded.encode("utf-8")).hexdigest()


def load_config(path: str | Path) -> Dict[str, Any]:
    """Load JSON or YAML configuration without executing arbitrary code."""
    config_path = Path(path)
    text = config_path.read_text(encoding="utf-8")
    try:
        value = json.loads(text)
    except json.JSONDecodeError:
        try:
            import yaml  # type: ignore
        except ImportError as error:
            raise RuntimeError(
                "YAML configuration requires PyYAML; JSON configuration remains available"
            ) from error
        value = yaml.safe_load(text)
    if not isinstance(value, Mapping):
        raise ValueError("quote-cleaner configuration must be a mapping")
    return resolved_config(value)


def _column(config: Mapping[str, Any], logical_name: str) -> Optional[str]:
    value = config["columns"].get(logical_name)
    return str(value) if value not in (None, "") else None


def _available_column(frame: pd.DataFrame, config: Mapping[str, Any], logical_name: str) -> bool:
    name = _column(config, logical_name)
    return name is not None and name in frame.columns


def _ticker_timezone(ticker: str, config: Mapping[str, Any]) -> Optional[str]:
    mapping = config.get("timezone_by_ticker", {}) or {}
    value = mapping.get(str(ticker), config.get("timezone"))
    return None if value in (None, "") else str(value)


def _parse_one_timestamp(value: Any, timezone: Optional[str], numeric_unit: Optional[str]) -> Tuple[pd.Timestamp, str]:
    if value is None or (not isinstance(value, str) and pd.isna(value)):
        return pd.NaT, "missing_timestamp"
    if isinstance(value, (int, float, np.integer, np.floating)) and not isinstance(value, bool):
        if numeric_unit is None:
            return pd.NaT, "ambiguous_numeric_timestamp"
        try:
            return pd.to_datetime(value, unit=numeric_unit, utc=True), ""
        except Exception:
            return pd.NaT, "timestamp_parse_failure"
    try:
        parsed = pd.Timestamp(value)
    except Exception:
        return pd.NaT, "timestamp_parse_failure"
    if pd.isna(parsed):
        return pd.NaT, "timestamp_parse_failure"
    try:
        if parsed.tzinfo is None:
            if timezone is None:
                return pd.NaT, "timestamp_timezone_unavailable"
            parsed = parsed.tz_localize(timezone, ambiguous="NaT", nonexistent="NaT")
            if pd.isna(parsed):
                return pd.NaT, "timestamp_parse_failure"
        return parsed.tz_convert("UTC"), ""
    except Exception:
        return pd.NaT, "timestamp_parse_failure"


def _parse_bound(value: Any, timezone: Optional[str]) -> Optional[pd.Timestamp]:
    if value in (None, ""):
        return None
    parsed, reason = _parse_one_timestamp(value, timezone, None)
    if reason:
        raise ValueError(f"invalid configured sample bound: {value!r} ({reason})")
    return parsed


def _append_reason(target: List[List[str]], index: int, reason: str) -> None:
    if reason not in target[index]:
        target[index].append(reason)


def _normalise_ticker(frame: pd.DataFrame, config: Mapping[str, Any], ticker: Optional[str]) -> pd.Series:
    ticker_column = _column(config, "ticker")
    if ticker_column and ticker_column in frame.columns:
        result = frame[ticker_column].astype("string")
    elif ticker:
        result = pd.Series([ticker] * len(frame), index=frame.index, dtype="string")
    else:
        raise ValueError("ticker column is absent; supply a ticker explicitly")
    if result.isna().any() or (result.str.len().fillna(0) == 0).any():
        raise ValueError("ticker values must be non-missing")
    return result.astype(str)


def _coerce_numeric(series: pd.Series) -> np.ndarray:
    return pd.to_numeric(series, errors="coerce").to_numpy(dtype=float)


def _base_audit(data: pd.DataFrame, config: Mapping[str, Any], ticker: Optional[str] = None) -> Tuple[pd.DataFrame, Dict[str, Any]]:
    """Construct immutable row IDs and non-sequential validation flags."""
    frame = data.copy(deep=True).reset_index(drop=True)
    if "original_row_id" not in frame.columns:
        frame["original_row_id"] = np.arange(len(frame), dtype=np.int64)
    if "original_input_order" not in frame.columns:
        frame["original_input_order"] = np.arange(len(frame), dtype=np.int64)
    frame["_processing_input_order"] = np.arange(len(frame), dtype=np.int64)
    frame["ticker_clean"] = _normalise_ticker(frame, config, ticker)
    n = len(frame)
    reject_reasons: List[List[str]] = [[] for _ in range(n)]
    warning_reasons: List[List[str]] = [[] for _ in range(n)]

    timestamp_column = _column(config, "timestamp")
    if not timestamp_column or timestamp_column not in frame.columns:
        raise ValueError("configured timestamp column is absent")
    parsed: List[pd.Timestamp] = []
    parse_reason: List[str] = []
    for value, ticker_value in zip(frame[timestamp_column], frame["ticker_clean"]):
        timestamp, reason = _parse_one_timestamp(
            value, _ticker_timezone(str(ticker_value), config),
            config.get("numeric_timestamp_unit")
        )
        parsed.append(timestamp)
        parse_reason.append(reason)
    frame["timestamp_clean"] = pd.to_datetime(pd.Series(parsed), utc=True, errors="coerce")
    frame["timestamp_valid"] = frame["timestamp_clean"].notna()
    for index, reason in enumerate(parse_reason):
        if reason:
            _append_reason(reject_reasons, index, reason)

    bounds = config.get("sample_bounds", {}) or {}
    for ticker_value, indices in frame.groupby("ticker_clean", sort=False).groups.items():
        timezone = _ticker_timezone(str(ticker_value), config)
        earliest = _parse_bound(bounds.get("earliest"), timezone)
        latest = _parse_bound(bounds.get("latest"), timezone)
        for index in indices:
            timestamp = frame.at[index, "timestamp_clean"]
            if pd.isna(timestamp):
                continue
            if earliest is not None and timestamp < earliest:
                _append_reason(reject_reasons, index, "timestamp_before_sample_bound")
            if latest is not None and timestamp > latest:
                _append_reason(reject_reasons, index, "timestamp_after_sample_bound")

    out_of_order = np.zeros(n, dtype=bool)
    for _, group in frame.groupby("ticker_clean", sort=False):
        previous: Optional[pd.Timestamp] = None
        for index in group.sort_values("_processing_input_order").index:
            timestamp = frame.at[index, "timestamp_clean"]
            if pd.isna(timestamp):
                continue
            if previous is not None and timestamp < previous:
                out_of_order[index] = True
                _append_reason(warning_reasons, index, "out_of_order_originally")
            previous = timestamp
    frame["out_of_order_originally"] = out_of_order

    off_grid = np.zeros(n, dtype=bool)
    timestamp_gap = pd.Series(pd.NaT, index=frame.index, dtype="timedelta64[ns]")
    large_positive_gap = np.zeros(n, dtype=bool)
    frequency = config.get("expected_frequency")
    if frequency:
        try:
            step_ns = pd.Timedelta(frequency).value
        except Exception as error:
            raise ValueError(f"invalid expected_frequency: {frequency}") from error
        valid_indices = frame.index[frame["timestamp_valid"]]
        for index in valid_indices:
            timestamp_ns = frame.at[index, "timestamp_clean"].value
            if timestamp_ns % step_ns != 0:
                off_grid[index] = True
                reason_target = reject_reasons if config.get("strict_grid") else warning_reasons
                _append_reason(reason_target, index, "timestamp_off_grid")
        expected_step = pd.Timedelta(frequency)
        for _, group in frame[frame["timestamp_valid"]].groupby(
            "ticker_clean", sort=False
        ):
            previous: Optional[pd.Timestamp] = None
            for index in group.sort_values(
                ["timestamp_clean", "_processing_input_order"]
            ).index:
                current = frame.at[index, "timestamp_clean"]
                if previous is not None:
                    gap = current - previous
                    timestamp_gap.at[index] = gap
                    if gap > expected_step:
                        large_positive_gap[index] = True
                        destination = reject_reasons if config[
                            "timing_gaps"
                        ].get("reject_large_positive_gap", False) else warning_reasons
                        _append_reason(destination, index, "large_positive_gap")
                previous = current
    frame["timestamp_off_grid"] = off_grid
    frame["timestamp_gap"] = timestamp_gap
    frame["large_positive_gap"] = large_positive_gap

    # Exact duplicates compare original supplied values. The first is retained;
    # every later exact copy is represented in the audit and deterministically
    # excluded from the clean output.
    source_columns = [name for name in data.columns]
    exact_duplicate = frame.duplicated(subset=source_columns, keep="first").to_numpy()
    frame["duplicate_exact"] = exact_duplicate
    if bool(config["duplicates"].get("collapse_exact", True)):
        for index in np.flatnonzero(exact_duplicate):
            _append_reason(reject_reasons, int(index), "exact_duplicate_collapsed")
    else:
        for index in np.flatnonzero(exact_duplicate):
            _append_reason(warning_reasons, int(index), "exact_duplicate")

    duplicate_conflict = np.zeros(n, dtype=bool)
    valid_timestamp_frame = frame[frame["timestamp_valid"]]
    for _, group in valid_timestamp_frame.groupby(
        ["ticker_clean", "timestamp_clean"], sort=False, dropna=False
    ):
        if len(group) < 2:
            continue
        supplied = group[source_columns].astype(object)
        unique_count = len(supplied.drop_duplicates())
        if unique_count > 1:
            duplicate_conflict[group.index.to_numpy()] = True
    frame["duplicate_conflict"] = duplicate_conflict
    target = reject_reasons if config["duplicates"].get("reject_conflicting", True) else warning_reasons
    for index in np.flatnonzero(duplicate_conflict):
        _append_reason(target, int(index), "conflicting_duplicate")

    has_bid = _available_column(frame, config, "bid")
    has_ask = _available_column(frame, config, "ask")
    if has_bid != has_ask:
        raise ValueError("bid and ask must either both be present or both be absent")
    has_bid_ask = has_bid and has_ask
    has_mid = _available_column(frame, config, "midpoint")
    if not has_bid_ask and not has_mid:
        raise ValueError("input must contain bid/ask or a configured midpoint")

    bid_ask_valid = np.full(n, False, dtype=object) if has_bid_ask else np.full(n, pd.NA, dtype=object)
    crossed = np.full(n, False, dtype=object) if has_bid_ask else np.full(n, pd.NA, dtype=object)
    locked = np.full(n, False, dtype=object) if has_bid_ask else np.full(n, pd.NA, dtype=object)
    midpoint_consistent = np.full(n, pd.NA, dtype=object)
    if has_bid_ask:
        bid = _coerce_numeric(frame[_column(config, "bid")])
        ask = _coerce_numeric(frame[_column(config, "ask")])
        finite_positive = np.isfinite(bid) & np.isfinite(ask) & (bid > 0) & (ask > 0)
        crossed_bool = finite_positive & (bid > ask)
        locked_bool = finite_positive & (bid == ask)
        valid = finite_positive & ~crossed_bool
        quote_mid = (bid + ask) / 2.0
        valid &= np.isfinite(quote_mid) & (quote_mid > 0)
        bid_ask_valid[:] = valid
        crossed[:] = crossed_bool
        locked[:] = locked_bool
        frame["quote_mid"] = quote_mid
        relative_width = np.full(n, np.nan, dtype=float)
        np.divide(ask - bid, quote_mid, out=relative_width, where=valid)
        frame["relative_quote_width"] = relative_width
        for index in range(n):
            if not np.isfinite(bid[index]):
                _append_reason(reject_reasons, index, "nonfinite_bid")
            elif bid[index] <= 0:
                _append_reason(reject_reasons, index, "nonpositive_bid")
            if not np.isfinite(ask[index]):
                _append_reason(reject_reasons, index, "nonfinite_ask")
            elif ask[index] <= 0:
                _append_reason(reject_reasons, index, "nonpositive_ask")
            if crossed_bool[index]:
                _append_reason(reject_reasons, index, "crossed_quote")
            if locked_bool[index]:
                destination = reject_reasons if config["quotes"].get("reject_locked") else warning_reasons
                _append_reason(destination, index, "locked_quote")

        if has_mid and config["quotes"].get("supplied_midpoint_is_literal", False):
            supplied = _coerce_numeric(frame[_column(config, "midpoint")])
            tolerance = float(config["quotes"]["midpoint_absolute_tolerance"]) + float(
                config["quotes"]["midpoint_relative_tolerance"]
            ) * np.abs(quote_mid)
            consistent = np.isfinite(supplied) & valid & (np.abs(supplied - quote_mid) <= tolerance)
            midpoint_consistent[:] = consistent
            for index in np.flatnonzero(valid & ~consistent):
                destination = reject_reasons if config["quotes"].get(
                    "reject_midpoint_inconsistency", True
                ) else warning_reasons
                _append_reason(destination, int(index), "literal_midpoint_inconsistent")
    else:
        midpoint = _coerce_numeric(frame[_column(config, "midpoint")])
        frame["quote_mid"] = midpoint
        frame["relative_quote_width"] = np.nan
        for index in range(n):
            if not np.isfinite(midpoint[index]):
                _append_reason(reject_reasons, index, "nonfinite_midpoint")
            elif midpoint[index] <= 0:
                _append_reason(reject_reasons, index, "nonpositive_midpoint")

    frame["bid_ask_valid"] = pd.array(bid_ask_valid, dtype="boolean")
    frame["crossed_quote"] = pd.array(crossed, dtype="boolean")
    frame["locked_quote"] = pd.array(locked, dtype="boolean")
    frame["literal_midpoint_consistent"] = pd.array(midpoint_consistent, dtype="boolean")
    frame["_reject_reasons"] = reject_reasons
    frame["_warning_reasons"] = warning_reasons
    availability = {
        "mode": "bid_ask" if has_bid_ask else "midpoint_only",
        "bid_ask_validity": "AVAILABLE" if has_bid_ask else NOT_AVAILABLE_BID_ASK,
        "crossed_quote": "AVAILABLE" if has_bid_ask else NOT_AVAILABLE_BID_ASK,
        "locked_quote": "AVAILABLE" if has_bid_ask else NOT_AVAILABLE_BID_ASK,
        "relative_quote_width": "AVAILABLE" if has_bid_ask else NOT_AVAILABLE_BID_ASK,
        "literal_midpoint_consistency": (
            "AVAILABLE" if has_bid_ask and has_mid and
            config["quotes"].get("supplied_midpoint_is_literal", False)
            else "NOT ENABLED OR INPUT ABSENT"
        ),
    }
    return frame, availability


def _ticker_tick_size(
    group: pd.DataFrame,
    ticker: str,
    metadata: Optional[Mapping[str, Any]],
    config: Mapping[str, Any],
) -> Optional[float]:
    if metadata and ticker in metadata:
        value = metadata[ticker]
        if isinstance(value, Mapping):
            value = value.get("tick_size")
        try:
            numeric = float(value)
            if math.isfinite(numeric) and numeric > 0:
                return numeric
        except (TypeError, ValueError):
            pass
    if _available_column(group, config, "tick_size"):
        values = _coerce_numeric(group[_column(config, "tick_size")])
        values = np.unique(values[np.isfinite(values) & (values > 0)])
        if values.size == 1:
            return float(values[0])
    return None


def _fit_one_ticker(
    group: pd.DataFrame,
    ticker: str,
    metadata: Optional[Mapping[str, Any]],
    config: Mapping[str, Any],
    availability: Mapping[str, Any],
) -> Dict[str, Any]:
    v2 = config["v2"]
    base_ok = group["timestamp_valid"].to_numpy(dtype=bool)
    base_ok &= np.isfinite(group["quote_mid"].to_numpy(dtype=float))
    base_ok &= group["quote_mid"].to_numpy(dtype=float) > 0
    base_ok &= ~group["duplicate_exact"].to_numpy(dtype=bool)
    base_ok &= ~group["duplicate_conflict"].to_numpy(dtype=bool)
    if availability["mode"] == "bid_ask":
        base_ok &= group["bid_ask_valid"].fillna(False).to_numpy(dtype=bool)
    ordered = group.assign(_base_ok=base_ok).sort_values(
        ["timestamp_clean", "_processing_input_order"], na_position="last"
    )
    mids = ordered.loc[ordered["_base_ok"], "quote_mid"].to_numpy(dtype=float)
    minimum = int(v2["minimum_calibration_quotes"])
    enough_mid = mids.size >= minimum
    tick_size = _ticker_tick_size(group, ticker, metadata, config)

    q_max: Optional[float] = None
    median_mid: Optional[float] = float(np.median(mids)) if mids.size else None
    width_components: Dict[str, Any] = {
        "tick_floor": None,
        "empirical_q995": None,
        "robust_envelope": None,
        "median_multiple": None,
    }
    if availability["mode"] == "bid_ask" and enough_mid:
        relative = ordered.loc[ordered["_base_ok"], "relative_quote_width"].to_numpy(dtype=float)
        usable = relative[np.isfinite(relative) & (relative > 0)]
        if usable.size >= minimum:
            log_width = np.log(usable)
            sigma = scaled_mad(log_width)
            robust_envelope = math.exp(
                float(np.median(log_width))
                + float(v2["log_spread_mad_multiplier"]) * (sigma if math.isfinite(sigma) else 0.0)
            )
            median_multiple = float(v2["spread_median_multiple"]) * float(np.median(usable))
            empirical = r_type8_quantile(usable, float(v2["spread_empirical_quantile"]))
            tick_floor = (
                float(v2["tick_floor_multiple"]) * tick_size / float(median_mid)
                if tick_size is not None and median_mid and median_mid > 0 else None
            )
            components = [empirical, min(robust_envelope, median_multiple)]
            if tick_floor is not None:
                components.append(tick_floor)
            q_max = float(max(components))
            width_components = {
                "tick_floor": tick_floor,
                "empirical_q995": empirical,
                "robust_envelope": robust_envelope,
                "median_multiple": median_multiple,
            }

    preliminary = ordered["_base_ok"].to_numpy(dtype=bool)
    if availability["mode"] == "bid_ask" and q_max is not None:
        relative = ordered["relative_quote_width"].to_numpy(dtype=float)
        preliminary &= np.isfinite(relative) & (relative <= q_max)
    accepted_mid = ordered.loc[preliminary, "quote_mid"].to_numpy(dtype=float)
    returns = np.abs(np.diff(np.log(accepted_mid))) if accepted_mid.size >= 2 else np.array([])
    return_sigma = scaled_mad(returns)
    return_quantile = r_type8_quantile(returns, float(v2["price_return_quantile"]))
    h_mid = max(
        float(v2["price_relative_floor"]),
        float(v2["price_return_mad_multiplier"]) *
        (return_sigma if math.isfinite(return_sigma) else 0.0),
        float(v2["price_quantile_multiplier"]) *
        (return_quantile if math.isfinite(return_quantile) else 0.0),
    ) if enough_mid else None

    close_cutoff: Optional[float] = None
    close_calibration_basis: Optional[str] = None
    close_enabled = bool(config["close_mid_consistency"].get("enabled"))
    if close_enabled and availability["mode"] == "bid_ask" and _available_column(group, config, "close"):
        close_values = _coerce_numeric(ordered[_column(config, "close")])
        ordered_mid = ordered["quote_mid"].to_numpy(dtype=float)
        with np.errstate(divide="ignore", invalid="ignore"):
            cm = np.abs(np.log(close_values / ordered_mid))
        cm = cm[preliminary & np.isfinite(close_values) & (close_values > 0) & np.isfinite(cm)]
        candidates: List[float] = []
        if cm.size:
            cm_sigma = scaled_mad(cm)
            candidates.extend([
                r_type8_quantile(cm, float(v2["close_mid_empirical_quantile"])),
                float(np.median(cm)) + float(v2["close_mid_mad_multiplier"]) *
                (cm_sigma if math.isfinite(cm_sigma) else 0.0),
            ])
        if q_max is not None and math.isfinite(float(q_max)):
            # Exact parity with the authoritative R V2 edge case: the
            # relative-width term remains an admissible lower bound even
            # when no usable close-mid evidence exists.
            candidates.append(float(q_max) / 2.0)
        if candidates:
            close_cutoff = float(max(candidates))
            close_calibration_basis = (
                "EMPIRICAL_PLUS_QMAX_FLOOR" if cm.size else "QMAX_FLOOR_ONLY"
            )

    valid_times = ordered.loc[ordered["timestamp_valid"], "timestamp_clean"]
    status = "FITTED" if enough_mid and h_mid is not None else "INSUFFICIENT_CALIBRATION_QUOTES"
    local_rule_status = RULE_AVAILABLE if h_mid is not None else RULE_INSUFFICIENT_CALIBRATION
    if availability["mode"] != "bid_ask":
        width_rule_status = RULE_MISSING_INPUT
    elif q_max is None:
        width_rule_status = RULE_INSUFFICIENT_CALIBRATION
    else:
        width_rule_status = RULE_AVAILABLE
    if not close_enabled:
        close_mid_rule_status = RULE_DISABLED
    elif availability["mode"] != "bid_ask" or not _available_column(group, config, "close"):
        close_mid_rule_status = RULE_MISSING_INPUT
    elif close_cutoff is None:
        close_mid_rule_status = RULE_INSUFFICIENT_CALIBRATION
    else:
        close_mid_rule_status = RULE_AVAILABLE
    result = {
        "ticker": str(ticker),
        "status": status,
        "width_status": width_rule_status,
        "width_rule_status": width_rule_status,
        "local_mid_rule_status": local_rule_status,
        "close_mid_rule_status": close_mid_rule_status,
        "q_max": q_max,
        "h_mid": h_mid,
        "median_mid": median_mid,
        "tick_size": tick_size,
        "tick_size_floor_available": tick_size is not None,
        "width_components": width_components,
        "close_mid_consistency_cutoff": close_cutoff,
        "close_mid_calibration_basis": close_calibration_basis,
        "calibration_valid_mid_count": int(mids.size),
        "calibration_start": valid_times.min().isoformat() if len(valid_times) else None,
        "calibration_end": valid_times.max().isoformat() if len(valid_times) else None,
        "local_history_mid": [],
        "rule_version": V2_FORMULA_VERSION,
    }
    return result


def fit_quote_cleaner(
    calibration_data: pd.DataFrame,
    metadata: Optional[Mapping[str, Any]] = None,
    config: Optional[Mapping[str, Any]] = None,
    ticker: Optional[str] = None,
) -> Dict[str, Any]:
    """Fit ticker-specific cutoffs on a designated calibration sample."""
    cfg = resolved_config(config)
    audit, availability = _base_audit(calibration_data, cfg, ticker)
    ticker_contracts: Dict[str, Any] = {}
    for ticker_value, group in audit.groupby("ticker_clean", sort=True):
        ticker_contracts[str(ticker_value)] = _fit_one_ticker(
            group, str(ticker_value), metadata, cfg, availability
        )
    fitted: Dict[str, Any] = {
        "schema_version": "ticker_quote_cleaner_fitted_v1",
        "utility_version": UTILITY_VERSION,
        "authoritative_formula_version": V2_FORMULA_VERSION,
        "config": cfg,
        "rule_availability": availability,
        "tickers": ticker_contracts,
        "fit_apply_same_sample_warning": False,
    }
    # Obtain a causal tail of accepted calibration midpoints for future apply
    # calls. Thresholds are already frozen, so this cannot modify the fit.
    preliminary = apply_quote_cleaner(
        calibration_data, fitted, ticker=ticker,
        use_calibration_history=False, _building_history=True
    )
    window = int(cfg["v2"]["local_reference_observations"])
    for ticker_value, group in preliminary.audit[preliminary.audit["clean_quote"]].groupby(
        "ticker_clean", sort=False
    ):
        ticker_contracts[str(ticker_value)]["local_history_mid"] = [
            float(value) for value in group.sort_values(
                ["timestamp_clean", "original_input_order"]
            )["quote_mid"].tail(window)
        ]
    fingerprint_payload = copy.deepcopy(fitted)
    fitted["fingerprint"] = cleaner_fingerprint(fingerprint_payload)
    return fitted


@dataclass
class QuoteCleaningResult:
    audit: pd.DataFrame
    clean: pd.DataFrame
    rejected: pd.DataFrame
    summary: pd.DataFrame
    day_summary: pd.DataFrame
    fitted_cleaner: Dict[str, Any]


def _base_rejected(row: pd.Series) -> bool:
    return bool(row["_reject_reasons"])


def apply_quote_cleaner(
    new_data: pd.DataFrame,
    fitted_cleaner: Mapping[str, Any],
    ticker: Optional[str] = None,
    use_calibration_history: bool = True,
    _building_history: bool = False,
) -> QuoteCleaningResult:
    """Apply frozen ticker thresholds without recomputing them."""
    original_fingerprint = fitted_cleaner.get("fingerprint")
    frozen = copy.deepcopy(dict(fitted_cleaner))
    cfg = resolved_config(frozen.get("config"))
    audit, availability = _base_audit(new_data, cfg, ticker)
    n = len(audit)
    audit["quote_width_cutoff"] = np.nan
    audit["local_reference_mid"] = np.nan
    audit["local_mid_displacement"] = np.nan
    audit["local_mid_displacement_cutoff"] = np.nan
    audit["excessive_relative_quote_width"] = pd.array(
        [pd.NA] * n if availability["mode"] != "bid_ask" else [False] * n,
        dtype="boolean"
    )
    audit["implausible_local_midpoint_displacement"] = False
    audit["close_mid_inconsistent"] = pd.array([pd.NA] * n, dtype="boolean")
    audit["identical_quote_run_length"] = 1
    audit["possible_stale_quote"] = False
    audit["cleaning_incomplete"] = False
    audit["calibration_unavailable"] = False
    audit["partial_clean_quote"] = False
    audit["width_rule_status"] = RULE_MISSING_INPUT
    audit["local_mid_rule_status"] = RULE_INSUFFICIENT_CALIBRATION
    audit["close_mid_rule_status"] = (
        RULE_DISABLED if not cfg["close_mid_consistency"].get("enabled")
        else RULE_MISSING_INPUT
    )

    ordered_indices: List[int] = []
    for ticker_value, group in audit.groupby("ticker_clean", sort=True):
        ticker_key = str(ticker_value)
        if ticker_key not in frozen.get("tickers", {}):
            for index in group.index:
                if "ticker_not_in_fitted_cleaner" not in audit.at[index, "_reject_reasons"]:
                    audit.at[index, "_reject_reasons"].append("ticker_not_in_fitted_cleaner")
                audit.at[index, "cleaning_incomplete"] = True
                audit.at[index, "calibration_unavailable"] = True
            ordered_indices.extend(group.index.tolist())
            continue
        contract = frozen["tickers"][ticker_key]
        q_max = contract.get("q_max")
        h_mid = contract.get("h_mid")
        close_cutoff = contract.get("close_mid_consistency_cutoff")
        width_rule_status = (
            RULE_MISSING_INPUT if availability["mode"] != "bid_ask"
            else contract.get("width_rule_status", contract.get("width_status"))
        )
        if width_rule_status not in {
            RULE_AVAILABLE, RULE_MISSING_INPUT, RULE_INSUFFICIENT_CALIBRATION
        }:
            width_rule_status = RULE_AVAILABLE if q_max is not None else RULE_INSUFFICIENT_CALIBRATION
        local_rule_status = contract.get(
            "local_mid_rule_status",
            RULE_AVAILABLE if h_mid is not None else RULE_INSUFFICIENT_CALIBRATION,
        )
        if not cfg["close_mid_consistency"].get("enabled"):
            close_rule_status = RULE_DISABLED
        elif availability["mode"] != "bid_ask" or not _available_column(audit, cfg, "close"):
            close_rule_status = RULE_MISSING_INPUT
        else:
            close_rule_status = contract.get(
                "close_mid_rule_status",
                RULE_AVAILABLE if close_cutoff is not None else RULE_INSUFFICIENT_CALIBRATION,
            )
        incomplete = local_rule_status != RULE_AVAILABLE
        if availability["mode"] == "bid_ask":
            incomplete = incomplete or width_rule_status != RULE_AVAILABLE
        if cfg["close_mid_consistency"].get("enabled"):
            incomplete = incomplete or close_rule_status != RULE_AVAILABLE
        for index in group.index:
            audit.at[index, "width_rule_status"] = width_rule_status
            audit.at[index, "local_mid_rule_status"] = local_rule_status
            audit.at[index, "close_mid_rule_status"] = close_rule_status
            audit.at[index, "cleaning_incomplete"] = bool(incomplete)
            audit.at[index, "calibration_unavailable"] = bool(incomplete)
        history: List[float] = []
        if use_calibration_history:
            history = [
                float(value) for value in contract.get("local_history_mid", [])
                if math.isfinite(float(value)) and float(value) > 0
            ]
        stale_run = 0
        previous_quote: Optional[Tuple[float, ...]] = None
        group_ordered = group.sort_values(
            ["timestamp_clean", "_processing_input_order"], na_position="last"
        )
        ordered_indices.extend(group_ordered.index.tolist())
        for index in group_ordered.index:
            midpoint = float(audit.at[index, "quote_mid"]) if pd.notna(audit.at[index, "quote_mid"]) else math.nan
            if q_max is not None:
                audit.at[index, "quote_width_cutoff"] = float(q_max)
            if h_mid is not None:
                audit.at[index, "local_mid_displacement_cutoff"] = float(h_mid)

            if availability["mode"] == "bid_ask" and not _base_rejected(audit.loc[index]):
                width = float(audit.at[index, "relative_quote_width"])
                excessive = q_max is not None and math.isfinite(width) and width > float(q_max)
                audit.at[index, "excessive_relative_quote_width"] = bool(excessive)
                if excessive:
                    audit.at[index, "_reject_reasons"].append("excessive_relative_quote_width")
                if q_max is None:
                    audit.at[index, "_warning_reasons"].append("quote_width_cutoff_unavailable")
                if not contract.get("tick_size_floor_available", False):
                    audit.at[index, "_warning_reasons"].append("tick_size_floor_unavailable")

            if cfg["close_mid_consistency"].get("enabled"):
                close_column = _column(cfg, "close")
                if availability["mode"] == "bid_ask" and close_column in audit.columns and close_cutoff is not None:
                    close_value = pd.to_numeric(pd.Series([audit.at[index, close_column]]), errors="coerce").iloc[0]
                    inconsistent = not (
                        np.isfinite(close_value) and close_value > 0 and
                        math.isfinite(midpoint) and midpoint > 0 and
                        abs(math.log(float(close_value) / midpoint)) <= float(close_cutoff)
                    )
                    audit.at[index, "close_mid_inconsistent"] = inconsistent
                    if inconsistent:
                        destination = audit.at[index, "_reject_reasons"] if cfg[
                            "close_mid_consistency"
                        ].get("reject", True) else audit.at[index, "_warning_reasons"]
                        destination.append("close_mid_inconsistent")
                else:
                    audit.at[index, "_warning_reasons"].append("close_mid_rule_unavailable")

            if not _base_rejected(audit.loc[index]) and math.isfinite(midpoint) and midpoint > 0:
                minimum = int(cfg["v2"]["local_reference_minimum"])
                window = int(cfg["v2"]["local_reference_observations"])
                if h_mid is not None and len(history) >= minimum:
                    reference = float(np.median(history[-window:]))
                    displacement = abs(math.log(midpoint / reference))
                    audit.at[index, "local_reference_mid"] = reference
                    audit.at[index, "local_mid_displacement"] = displacement
                    if displacement > float(h_mid):
                        audit.at[index, "implausible_local_midpoint_displacement"] = True
                        audit.at[index, "_reject_reasons"].append(
                            "implausible_local_midpoint_displacement"
                        )
                elif h_mid is None:
                    audit.at[index, "_warning_reasons"].append(
                        "local_midpoint_cutoff_unavailable"
                    )

            # Stale runs are chronological diagnostics and do not contaminate
            # the accepted-midpoint history decision above.
            if availability["mode"] == "bid_ask":
                bid_value = pd.to_numeric(pd.Series([audit.at[index, _column(cfg, "bid")]]), errors="coerce").iloc[0]
                ask_value = pd.to_numeric(pd.Series([audit.at[index, _column(cfg, "ask")]]), errors="coerce").iloc[0]
                current_quote = (float(bid_value), float(ask_value)) if np.isfinite(bid_value) and np.isfinite(ask_value) else None
            else:
                current_quote = (midpoint,) if math.isfinite(midpoint) else None
            if current_quote is not None and current_quote == previous_quote:
                stale_run += 1
            else:
                stale_run = 1
            previous_quote = current_quote
            audit.at[index, "identical_quote_run_length"] = stale_run
            if cfg["stale_quote"].get("enabled") and stale_run >= int(cfg["stale_quote"]["run_length"]):
                audit.at[index, "possible_stale_quote"] = True
                destination = audit.at[index, "_reject_reasons"] if cfg[
                    "stale_quote"
                ].get("reject") else audit.at[index, "_warning_reasons"]
                destination.append("possible_stale_quote")

            if not _base_rejected(audit.loc[index]) and math.isfinite(midpoint) and midpoint > 0:
                history.append(midpoint)

            # Apply every available rule first. An incomplete fitted contract
            # is then handled explicitly and cannot silently masquerade as a
            # fully clean quote. Partial mode exposes a separate result only.
            if incomplete:
                available_rules_pass = not _base_rejected(audit.loc[index])
                if cfg.get("allow_partial_cleaning", False):
                    audit.at[index, "partial_clean_quote"] = bool(available_rules_pass)
                    audit.at[index, "_warning_reasons"].append(
                        "calibration_unavailable_partial_cleaning"
                    )
                else:
                    audit.at[index, "_reject_reasons"].append("calibration_unavailable")

    audit["hard_reject"] = audit["_reject_reasons"].map(bool)
    audit["clean_quote"] = (~audit["hard_reject"]) & (~audit["cleaning_incomplete"])
    audit["reject_reasons"] = audit["_reject_reasons"].map(
        lambda values: ";".join(dict.fromkeys(v for v in values if v))
    )
    audit["warning_reasons"] = audit["_warning_reasons"].map(
        lambda values: ";".join(dict.fromkeys(v for v in values if v))
    )
    audit = audit.drop(columns=[
        "_reject_reasons", "_warning_reasons", "_processing_input_order"
    ])
    audit = audit.sort_values(
        ["ticker_clean", "timestamp_clean", "original_input_order"], na_position="last"
    ).reset_index(drop=True)
    clean = audit[audit["clean_quote"]].copy().reset_index(drop=True)
    rejected = audit[audit["hard_reject"]].copy().reset_index(drop=True)
    summary = _summarise(audit, availability)
    day_summary = _day_summary(audit)
    if original_fingerprint is not None and fitted_cleaner.get("fingerprint") != original_fingerprint:
        raise AssertionError("application mutated fitted thresholds")
    return QuoteCleaningResult(audit, clean, rejected, summary, day_summary, frozen)


def _count_or_na(group: pd.DataFrame, column: str) -> Any:
    values = group[column]
    return pd.NA if values.isna().all() else int(values.fillna(False).sum())


def _summarise(audit: pd.DataFrame, availability: Mapping[str, Any]) -> pd.DataFrame:
    rows: List[Dict[str, Any]] = []
    for ticker, group in audit.groupby("ticker_clean", sort=True):
        valid_times = group.loc[group["timestamp_valid"], "timestamp_clean"]
        invalid_price_codes = (
            "nonfinite_bid|nonpositive_bid|nonfinite_ask|nonpositive_ask|"
            "nonfinite_midpoint|nonpositive_midpoint"
        )
        row = {
            "ticker": ticker,
            "input_rows": len(group),
            "parsed_timestamps": int(group["timestamp_valid"].sum()),
            "exact_duplicates": int(group["duplicate_exact"].sum()),
            "conflicting_duplicates": int(group["duplicate_conflict"].sum()),
            "out_of_order_rows": int(group["out_of_order_originally"].sum()),
            "off_grid_rows": int(group["timestamp_off_grid"].sum()),
            "large_positive_gaps": int(group["large_positive_gap"].sum()),
            "invalid_or_nonpositive_prices": int(group["reject_reasons"].str.contains(
                invalid_price_codes, regex=True, na=False
            ).sum()),
            "crossed_quotes": _count_or_na(group, "crossed_quote"),
            "locked_quotes": _count_or_na(group, "locked_quote"),
            "width_failures": _count_or_na(group, "excessive_relative_quote_width"),
            "local_midpoint_failures": int(group["implausible_local_midpoint_displacement"].sum()),
            "reference_price_consistency_failures": _count_or_na(group, "close_mid_inconsistent"),
            "hard_rejected_rows": int(group["hard_reject"].sum()),
            "cleaning_incomplete_rows": int(group["cleaning_incomplete"].sum()),
            "partial_clean_rows": int(group["partial_clean_quote"].sum()),
            "clean_rows": int(group["clean_quote"].sum()),
            "clean_share": float(group["clean_quote"].mean()) if len(group) else math.nan,
            "first_timestamp": valid_times.min().isoformat() if len(valid_times) else None,
            "last_timestamp": valid_times.max().isoformat() if len(valid_times) else None,
            "bid_ask_rules_status": availability["bid_ask_validity"],
            "width_rule_status": group["width_rule_status"].iloc[0],
            "local_mid_rule_status": group["local_mid_rule_status"].iloc[0],
            "close_mid_rule_status": group["close_mid_rule_status"].iloc[0],
        }
        rows.append(row)
    return pd.DataFrame(rows)


def _day_summary(audit: pd.DataFrame) -> pd.DataFrame:
    available = audit[audit["timestamp_valid"]].copy()
    if available.empty:
        return pd.DataFrame(columns=[
            "ticker", "session_date", "raw_rows", "valid_quote_opportunities",
            "clean_quotes", "clean_share"
        ])
    available["session_date"] = available["timestamp_clean"].dt.date.astype(str)
    rows: List[Dict[str, Any]] = []
    for (ticker, date), group in available.groupby(["ticker_clean", "session_date"], sort=True):
        valid = ~group["reject_reasons"].str.contains(
            "timestamp_|nonfinite_|nonpositive_|crossed_quote", regex=True, na=False
        )
        rows.append({
            "ticker": ticker,
            "session_date": date,
            "raw_rows": len(group),
            "valid_quote_opportunities": int(valid.sum()),
            "clean_quotes": int(group["clean_quote"].sum()),
            "clean_share": float(group["clean_quote"].mean()),
        })
    return pd.DataFrame(rows)


def fit_apply_quote_cleaner(
    data: pd.DataFrame,
    metadata: Optional[Mapping[str, Any]] = None,
    config: Optional[Mapping[str, Any]] = None,
    ticker: Optional[str] = None,
) -> QuoteCleaningResult:
    """Exploratory same-sample fit/apply; unsuitable for causal evaluation."""
    fitted = fit_quote_cleaner(data, metadata=metadata, config=config, ticker=ticker)
    fitted["fit_apply_same_sample_warning"] = True
    fitted.pop("fingerprint", None)
    fitted["fingerprint"] = cleaner_fingerprint(fitted)
    return apply_quote_cleaner(
        data, fitted, ticker=ticker, use_calibration_history=False
    )


def save_fitted_cleaner(fitted_cleaner: Mapping[str, Any], path: str | Path) -> Path:
    output = Path(path)
    output.parent.mkdir(parents=True, exist_ok=True)
    output.write_text(
        json.dumps(_jsonable(fitted_cleaner), indent=2, sort_keys=True) + "\n",
        encoding="utf-8",
    )
    return output


def load_fitted_cleaner(path: str | Path) -> Dict[str, Any]:
    value = json.loads(Path(path).read_text(encoding="utf-8"))
    if not isinstance(value, dict) or value.get("schema_version") != "ticker_quote_cleaner_fitted_v1":
        raise ValueError("invalid fitted quote-cleaner file")
    expected = value.get("fingerprint")
    check = copy.deepcopy(value)
    check.pop("fingerprint", None)
    actual = cleaner_fingerprint(check)
    if expected != actual:
        raise ValueError("fitted quote-cleaner fingerprint mismatch")
    return value


def read_quote_table(path: str | Path) -> pd.DataFrame:
    input_path = Path(path)
    suffix = input_path.suffix.lower()
    if suffix == ".csv":
        return pd.read_csv(input_path)
    if suffix in (".parquet", ".pq"):
        try:
            return pd.read_parquet(input_path)
        except ImportError as error:
            raise RuntimeError("Parquet input requires pyarrow or fastparquet") from error
    raise ValueError("quote input must be CSV or Parquet")


def infer_ticker_from_filename(path: str | Path, pattern: Optional[str]) -> Optional[str]:
    if not pattern:
        return None
    match = re.search(pattern, Path(path).name)
    if not match:
        raise ValueError("configured ticker filename rule did not match input")
    if "ticker" in match.groupdict():
        return match.group("ticker")
    if match.groups():
        return match.group(1)
    return match.group(0)


def _safe_output_path(input_path: Optional[str | Path], output_path: Path) -> None:
    if input_path is not None and Path(input_path).resolve() == output_path.resolve():
        raise ValueError("cleaner will not overwrite its input file")


def write_cleaning_outputs(
    result: QuoteCleaningResult,
    output_dir: str | Path,
    output_format: str = "parquet",
    input_path: Optional[str | Path] = None,
) -> Dict[str, Path]:
    directory = Path(output_dir)
    directory.mkdir(parents=True, exist_ok=True)
    output_format = output_format.lower()
    if output_format not in ("parquet", "csv"):
        raise ValueError("output format must be parquet or csv")
    suffix = ".parquet" if output_format == "parquet" else ".csv"
    paths = {
        "clean": directory / f"clean_quotes{suffix}",
        "audit": directory / f"quote_audit{suffix}",
        "rejected": directory / f"rejected_quotes{suffix}",
        "summary": directory / "quote_cleaning_summary.csv",
        "thresholds": directory / "quote_cleaning_thresholds.json",
        "day_summary": directory / "quote_cleaning_by_session.csv",
    }
    for path in paths.values():
        _safe_output_path(input_path, path)
    if output_format == "parquet":
        try:
            result.clean.to_parquet(paths["clean"], index=False)
            result.audit.to_parquet(paths["audit"], index=False)
            result.rejected.to_parquet(paths["rejected"], index=False)
        except ImportError as error:
            raise RuntimeError("Parquet output requires pyarrow or fastparquet") from error
    else:
        result.clean.to_csv(paths["clean"], index=False)
        result.audit.to_csv(paths["audit"], index=False)
        result.rejected.to_csv(paths["rejected"], index=False)
    result.summary.to_csv(paths["summary"], index=False)
    result.day_summary.to_csv(paths["day_summary"], index=False)
    save_fitted_cleaner(result.fitted_cleaner, paths["thresholds"])
    return paths


def jointly_clean_opportunity_counts(
    first_clean_audit: pd.DataFrame,
    second_clean_audit: pd.DataFrame,
) -> pd.DataFrame:
    """Join already-audited legs; report counts without deciding eligibility."""
    required = {"timestamp_clean", "clean_quote"}
    if not required.issubset(first_clean_audit) or not required.issubset(second_clean_audit):
        raise ValueError("both leg audits require timestamp_clean and clean_quote")
    first = first_clean_audit[["timestamp_clean", "clean_quote"]].rename(
        columns={"clean_quote": "first_clean"}
    )
    second = second_clean_audit[["timestamp_clean", "clean_quote"]].rename(
        columns={"clean_quote": "second_clean"}
    )
    joined = first.merge(second, on="timestamp_clean", how="inner")
    return pd.DataFrame({
        "raw_simultaneous_opportunities": [len(joined)],
        "jointly_clean_opportunities": [int((joined["first_clean"] & joined["second_clean"]).sum())],
    })
