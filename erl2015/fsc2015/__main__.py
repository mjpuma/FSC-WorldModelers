"""Command line for the Python implementation of Puma et al. (2015).

    python -m fsc2015 prepare            # build FAOSTAT extracts in erl2015/data/
    python -m fsc2015 check              # validate extracts and scenario files
    python -m fsc2015 run                # both paper scenarios, default periods
    python -m fsc2015 run --crop wheat --scenario europe_wheat_1816 \\
        --period 1992-1996 --period 2019-2023
"""

from __future__ import annotations

import argparse
import sys

from . import config, data


def _parse_period(text: str):
    try:
        a, b = (int(x) for x in text.split("-"))
    except ValueError:
        raise argparse.ArgumentTypeError(f"period must look like 2019-2023, got {text!r}")
    if a > b:
        raise argparse.ArgumentTypeError(f"period start after end: {text}")
    return text, (a, b)


def main(argv=None) -> int:
    p = argparse.ArgumentParser(prog="python -m fsc2015", description=__doc__,
                                formatter_class=argparse.RawDescriptionHelpFormatter)
    sub = p.add_subparsers(dest="cmd", required=True)

    pr = sub.add_parser("prepare", help="download FAOSTAT bulk files and build extracts")
    pr.add_argument("--force-download", action="store_true")
    pr.add_argument("--skip-trade", action="store_true", help="keep an existing trade extract")

    sub.add_parser("check", help="validate extracts, scenarios and the LDC list")

    fg = sub.add_parser("figures", help="redraw paper figures 2-5 into outputs/erl2015/")
    fg.add_argument("--period", action="append", type=_parse_period,
                    help="periods for figures 4 and 5 (default 1992-1996, 2005-2009, 2019-2023)")
    fg.add_argument("--skip-paleo", action="store_true", help="skip figure 3 (needs NOAA paleo downloads)")

    rn = sub.add_parser("run", help="run export-ban scenarios and write outputs/erl2015/")
    rn.add_argument("--crop", choices=sorted(config.TRADE_ITEMS))
    rn.add_argument("--scenario", help="name in erl2015/scenarios/ or a CSV path")
    rn.add_argument("--period", action="append", type=_parse_period,
                    help="analysis window, repeatable (default: %s)" % ", ".join(config.DEFAULT_PERIODS))
    rn.add_argument("--mirror", default="mean", choices=["mean", "max", "export", "import"],
                    help="how exporter and importer reports are combined (default: mean)")
    rn.add_argument("--min-flow", type=float, default=1.0,
                    help="drop flows below this many tonnes (default 1; see network.py)")
    rn.add_argument("--timeseries-start", type=int, default=1986)
    args = p.parse_args(argv)

    try:
        if args.cmd == "prepare":
            if not args.skip_trade or not (config.DATA_DIR / "trade_wheat_rice.parquet").exists():
                print("Extracting wheat and rice trade (several minutes)...", flush=True)
                print(data.extract_trade())
            print(data.extract_food_balances(args.force_download))
            print(data.extract_gdp(args.force_download))
            return 0

        from . import pipeline

        if args.cmd == "check":
            inp = pipeline.Inputs.load()
            t = inp.trade
            print(f"trade: {len(t):,} rows, {t['year'].min()}-{t['year'].max()}, {len(inp.countries)} countries")
            print(f"food balances: {inp.supply['year'].min()}-{inp.supply['year'].max()}")
            print(f"GDP: {inp.gdp['year'].min()}-{inp.gdp['year'].max()}")
            have = {str(m).zfill(3) for m in inp.countries["m49"]}
            for f in sorted(config.SCENARIO_DIR.glob("*.csv")):
                s = data.load_scenario(f)
                miss = sorted(set(s["m49"]) - have)
                print(f"scenario {f.stem}: {len(s)} exporters" + (f", not in trade: {miss}" if miss else ""))
            print(f"LDC list: {len(inp.ldc)} areas")
            return 0

        if args.cmd == "figures":
            from . import figures
            inp = pipeline.Inputs.load()
            periods = dict(args.period) if args.period else None
            for path in figures.make_all(inp, periods=periods, skip_paleo=args.skip_paleo):
                print("  wrote", path)
            return 0

        from . import report

        periods = dict(args.period) if args.period else config.DEFAULT_PERIODS
        if bool(args.crop) != bool(args.scenario):
            p.error("--crop and --scenario go together")
        jobs = [(args.crop, args.scenario)] if args.crop else [
            ("wheat", "europe_wheat_1816"), ("rice", "asia_rice_1876")]
        inp = pipeline.Inputs.load()
        results = {}
        for crop, scen in jobs:
            print(f"Running {crop} / {scen} ...", flush=True)
            r = pipeline.run(crop, scen, periods, inp=inp, mirror=args.mirror, min_flow_t=args.min_flow)
            print("  wrote", pipeline.write(r))
            results[(crop, scen)] = r
        end = int(inp.trade["year"].max())
        import pandas as pd
        ts = pd.concat([pipeline.timeseries(inp, c, args.timeseries_start, end, args.mirror, args.min_flow)
                        for c in ("wheat", "rice")], ignore_index=True)
        print("  wrote", report.write_report(results, ts, pipeline.ssr_by_period(inp, periods)))
        return 0
    except data.DataError as exc:
        print(f"error: {exc}", file=sys.stderr)
        return 2


if __name__ == "__main__":
    sys.exit(main())
