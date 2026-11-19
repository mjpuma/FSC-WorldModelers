#!/usr/bin/env python3
"""Download the FAOSTAT normalized bulk CSVs used by the FSC input build.

Sheaf's AgriculturalData.downloads.download_und_unzip reads a whole ZIP into
memory. The trade matrix is hundreds of megabytes compressed, so this script
streams the ZIP to disk and extracts the CSV. AgRichter's refresh script is
USDA-only and is not reused.

The public files are:

  https://bulks-faostat.fao.org/production/Production_Crops_Livestock_E_All_Data_(Normalized).zip
  https://bulks-faostat.fao.org/production/Trade_DetailedTradeMatrix_E_All_Data_(Normalized).zip

The trade archive is served from the production folder on that host. A second
URL under /trade/ is tried if the first is refused.

Nothing in sheaf-model or AgRichter2025 is written.

    python3 main/fetch_faostat_bulk.py
    python3 main/fetch_faostat_bulk.py --min-year 2024
"""

from __future__ import annotations

import argparse
import shutil
import zipfile
from pathlib import Path

import pandas as pd
import requests

ROOT = Path(__file__).resolve().parents[1]
CACHE = ROOT / "inputs" / "faostat_bulk_cache"
USER_AGENT = "FSC-WorldModelers faostat-bulk-fetch"

DATASETS = (
    {
        "name": "production",
        "urls": (
            "https://bulks-faostat.fao.org/production/Production_Crops_Livestock_E_All_Data_(Normalized).zip",
        ),
        "dest": ROOT / "inputs" / "Production_Crops_Livestock_E_All_Data_(Normalized).csv",
    },
    {
        "name": "trade",
        "urls": (
            "https://bulks-faostat.fao.org/production/Trade_DetailedTradeMatrix_E_All_Data_(Normalized).zip",
            "https://bulks-faostat.fao.org/trade/Trade_DetailedTradeMatrix_E_All_Data_(Normalized).zip",
        ),
        "dest": Path("/Users/mjp38/GitHub/FoodTradeNetwork/inputs/Trade_DetailedTradeMatrix_E_All_Data_(Normalized).csv"),
    },
)


def parse_args():
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument("--min-year", type=int, default=2024)
    p.add_argument("--cache", type=Path, default=CACHE)
    p.add_argument("--force", action="store_true", help="Download again even if the cache ZIP exists.")
    return p.parse_args()


def stream_download(urls, dest):
    dest.parent.mkdir(parents=True, exist_ok=True)
    partial = dest.with_suffix(dest.suffix + ".partial")
    headers = {"User-Agent": USER_AGENT}
    last_error = None
    for url in urls:
        print(f"Downloading {url}", flush=True)
        try:
            with requests.get(url, stream=True, headers=headers, timeout=120) as response:
                if response.status_code >= 400:
                    last_error = f"{response.status_code} {url}"
                    print(f"  refused: {last_error}", flush=True)
                    continue
                size = 0
                with partial.open("wb") as handle:
                    for chunk in response.iter_content(chunk_size=8 * 1024 * 1024):
                        if not chunk:
                            continue
                        handle.write(chunk)
                        size += len(chunk)
                        if size // (80 * 1024 * 1024) != (size - len(chunk)) // (80 * 1024 * 1024):
                            print(f"  {size / 1e6:.0f} MB", flush=True)
            partial.replace(dest)
            print(f"  saved {dest} ({size / 1e6:.1f} MB)", flush=True)
            return
        except requests.RequestException as exc:
            last_error = str(exc)
            print(f"  failed: {exc}", flush=True)
    raise SystemExit(f"Could not download {dest.name}: {last_error}")


def extract_csv(zip_path, dest):
    with zipfile.ZipFile(zip_path) as archive:
        csvs = [info for info in archive.infolist() if info.filename.lower().endswith(".csv")]
        if not csvs:
            raise SystemExit(f"No CSV inside {zip_path}")
        member = max(csvs, key=lambda info: info.file_size)
        print(f"Extracting {member.filename} ({member.file_size / 1e6:.1f} MB)", flush=True)
        tmp_dir = dest.parent / f".extract_{dest.stem}"
        if tmp_dir.exists():
            shutil.rmtree(tmp_dir)
        tmp_dir.mkdir(parents=True)
        archive.extract(member, path=tmp_dir)
        extracted = tmp_dir / member.filename
        staged = dest.with_suffix(dest.suffix + ".new")
        if staged.exists():
            staged.unlink()
        shutil.move(str(extracted), staged)
        shutil.rmtree(tmp_dir)
        return staged


def max_year(path):
    header = pd.read_csv(path, nrows=0, encoding="latin-1")
    columns = [str(col).lstrip("\ufeff") for col in header.columns]
    year_col = "Year" if "Year" in columns else None
    if year_col is None:
        raise SystemExit(f"{path} has no Year column: {columns}")
    maximum = None
    for chunk in pd.read_csv(path, usecols=["Year"], encoding="latin-1", chunksize=2_000_000):
        value = int(pd.to_numeric(chunk["Year"], errors="coerce").max())
        maximum = value if maximum is None else max(maximum, value)
    return maximum


def install_dataset(spec, cache, min_year, force):
    cache.mkdir(parents=True, exist_ok=True)
    zip_path = cache / f"{spec['name']}.zip"
    if force or not zip_path.exists():
        stream_download(spec["urls"], zip_path)
    else:
        print(f"Using cached {zip_path}", flush=True)
    staged = extract_csv(zip_path, spec["dest"])
    year = max_year(staged)
    print(f"{spec['name']} max year in the new file: {year}", flush=True)
    if year < min_year:
        staged.unlink()
        raise SystemExit(
            f"{spec['name']} only reaches {year}, which is before {min_year}. "
            "The previous CSV was left in place."
        )
    spec["dest"].parent.mkdir(parents=True, exist_ok=True)
    staged.replace(spec["dest"])
    print(f"Installed {spec['dest']}", flush=True)


def main():
    args = parse_args()
    for spec in DATASETS:
        install_dataset(spec, args.cache, args.min_year, args.force)


if __name__ == "__main__":
    main()
