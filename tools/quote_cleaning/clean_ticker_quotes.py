#!/usr/bin/env python3
"""Command-line interface for the standalone ticker quote cleaner."""

from __future__ import annotations

import argparse
import json
import sys
from pathlib import Path

TOOL_ROOT = Path(__file__).resolve().parent
sys.path.insert(0, str(TOOL_ROOT))

from quote_cleaning import (  # noqa: E402
    apply_quote_cleaner,
    fit_apply_quote_cleaner,
    fit_quote_cleaner,
    infer_ticker_from_filename,
    load_config,
    load_fitted_cleaner,
    read_quote_table,
    save_fitted_cleaner,
    write_cleaning_outputs,
)


def parser() -> argparse.ArgumentParser:
    value = argparse.ArgumentParser(
        description=(
            "Fit or apply auditable ticker-level futures quote cleaning. "
            "Input files are never modified."
        )
    )
    value.add_argument("--input", required=True, help="Input CSV or Parquet file")
    value.add_argument("--output-dir", required=True, help="New/generated output directory")
    value.add_argument("--config", required=True, help="JSON or YAML cleaning configuration")
    value.add_argument("--mode", choices=("fit", "apply", "fit-apply"), required=True)
    value.add_argument("--thresholds", help="Fitted threshold JSON required by apply mode")
    value.add_argument("--ticker", help="Ticker for a one-ticker file without a ticker column")
    value.add_argument("--metadata", help="Optional JSON ticker metadata, including tick_size")
    value.add_argument("--output-format", choices=("parquet", "csv"),
                       help="Override configured output format")
    return value


def main(argv: list[str] | None = None) -> int:
    args = parser().parse_args(argv)
    config = load_config(args.config)
    ticker = args.ticker or infer_ticker_from_filename(
        args.input, config.get("ticker_from_filename_regex")
    )
    metadata = None
    if args.metadata:
        metadata = json.loads(Path(args.metadata).read_text(encoding="utf-8"))
    frame = read_quote_table(args.input)
    output_dir = Path(args.output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)
    output_format = args.output_format or config["output"]["format"]

    if args.mode == "fit":
        fitted = fit_quote_cleaner(frame, metadata=metadata, config=config, ticker=ticker)
        output = output_dir / "quote_cleaning_thresholds.json"
        if Path(args.input).resolve() == output.resolve():
            raise ValueError("cleaner will not overwrite its input file")
        save_fitted_cleaner(fitted, output)
        print(f"Fitted thresholds: {output}")
        return 0

    if args.mode == "apply":
        if not args.thresholds:
            raise ValueError("--thresholds is required in apply mode")
        fitted = load_fitted_cleaner(args.thresholds)
        result = apply_quote_cleaner(frame, fitted, ticker=ticker)
    else:
        result = fit_apply_quote_cleaner(
            frame, metadata=metadata, config=config, ticker=ticker
        )
        print(
            "WARNING: fit-apply estimates thresholds from the same sample and "
            "is unsuitable for causal out-of-sample research.",
            file=sys.stderr,
        )
    paths = write_cleaning_outputs(
        result, output_dir, output_format=output_format, input_path=args.input
    )
    print(f"Clean rows: {len(result.clean)} / {len(result.audit)}")
    for name, path in paths.items():
        print(f"{name}: {path}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
