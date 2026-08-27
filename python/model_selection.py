#!/usr/bin/env python3
"""Scientific aggregation of exact OU-transition likelihood results.

This module deliberately contains no plotting or report-rendering code.  It
reads validated per-window results, computes within-window information-
criterion support, and writes machine-readable evidence tables.
"""

from __future__ import annotations

import argparse
import math
from itertools import combinations
from pathlib import Path

import numpy as np
import pandas as pd


FAMILY_ORDER = [
    "GAUSSIAN", "NIG", "GHYP_FULL", "VG", "NTS", "BILATERAL_TS",
    "CGMY", "MEIXNER", "SYMMETRIC_ALPHA_STABLE",
]


def _available(frame: pd.DataFrame) -> pd.Series:
    status = frame["fit_status"].astype(str).str.startswith("available")
    if "density_validation_status" in frame:
        status &= frame["density_validation_status"].eq("passed")
    return status


def add_within_window_evidence(frame: pd.DataFrame) -> pd.DataFrame:
    """Add ranks, deltas and normalized weights without changing fit rows."""
    required = {"pair_endpoint_key", "family", "logLik_exact", "cAIC", "cBIC", "fit_status"}
    missing = sorted(required.difference(frame.columns))
    if missing:
        raise ValueError(f"Missing exact-likelihood columns: {missing}")
    result = frame.copy()
    result["available"] = _available(result)
    for criterion in ("logLik_exact", "cAIC", "cBIC"):
        ascending = criterion != "logLik_exact"
        result[f"{criterion}_rank"] = np.nan
        result[f"{criterion}_delta"] = np.nan
        result[f"{criterion}_weight"] = np.nan
        for _, indexes in result.groupby("pair_endpoint_key", sort=False).groups.items():
            indexes = [i for i in indexes if bool(result.at[i, "available"])]
            if not indexes:
                continue
            values = pd.to_numeric(result.loc[indexes, criterion], errors="coerce")
            finite = values.index[np.isfinite(values)]
            if not len(finite):
                continue
            values = values.loc[finite]
            result.loc[finite, f"{criterion}_rank"] = values.rank(
                method="min", ascending=ascending
            )
            best = values.min() if ascending else values.max()
            delta = values - best if ascending else best - values
            result.loc[finite, f"{criterion}_delta"] = delta
            raw_weight = np.exp(-0.5 * delta)
            result.loc[finite, f"{criterion}_weight"] = raw_weight / raw_weight.sum()
    return result


def family_summary(expanded: pd.DataFrame) -> pd.DataFrame:
    """Summarize availability, wins and model support on a fixed denominator."""
    denominator = expanded["pair_endpoint_key"].nunique()
    rows = []
    for family in FAMILY_ORDER:
        subset = expanded.loc[expanded["family"].eq(family)]
        available = subset.loc[subset["available"]]
        rows.append({
            "family": family,
            "complete_pair_window_denominator": denominator,
            "available_pair_windows": int(available["pair_endpoint_key"].nunique()),
            "availability_share": (
                float(available["pair_endpoint_key"].nunique() / denominator)
                if denominator else math.nan
            ),
            "cAIC_wins": int(available["cAIC_rank"].eq(1).sum()),
            "cBIC_wins": int(available["cBIC_rank"].eq(1).sum()),
            "mean_cAIC_weight": float(available["cAIC_weight"].mean()),
            "mean_cBIC_weight": float(available["cBIC_weight"].mean()),
            "median_logLik_exact": float(available["logLik_exact"].median()),
        })
    return pd.DataFrame(rows)


def pairwise_caic_summary(expanded: pd.DataFrame) -> pd.DataFrame:
    """Pairwise cAIC comparisons on each families' common available sample."""
    rows = []
    available = expanded.loc[expanded["available"]]
    wide = available.pivot(index="pair_endpoint_key", columns="family", values="cAIC")
    for left, right in combinations(FAMILY_ORDER, 2):
        if left not in wide or right not in wide:
            continue
        common = wide[[left, right]].dropna()
        difference = common[left] - common[right]
        rows.append({
            "family_left": left,
            "family_right": right,
            "common_pair_windows": len(common),
            "left_lower_cAIC": int((difference < 0).sum()),
            "ties": int((difference == 0).sum()),
            "right_lower_cAIC": int((difference > 0).sum()),
            "median_cAIC_left_minus_right": (
                float(difference.median()) if len(common) else math.nan
            ),
        })
    return pd.DataFrame(rows)


def aggregate(input_csv: Path, output_dir: Path) -> None:
    output_dir.mkdir(parents=True, exist_ok=True)
    expanded = add_within_window_evidence(pd.read_csv(input_csv))
    expanded.to_csv(output_dir / "per_window_exact_likelihood_evidence.csv", index=False)
    family_summary(expanded).to_csv(output_dir / "family_model_support.csv", index=False)
    pairwise_caic_summary(expanded).to_csv(output_dir / "pairwise_caic_summary.csv", index=False)


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--input", required=True, type=Path)
    parser.add_argument("--output-dir", required=True, type=Path)
    args = parser.parse_args()
    aggregate(args.input, args.output_dir)


if __name__ == "__main__":
    main()
