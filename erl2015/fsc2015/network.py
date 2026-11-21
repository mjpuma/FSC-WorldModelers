"""Build annual bilateral trade networks from the FAOSTAT trade extract.

A network for one crop and year is a square matrix ``W`` in tonnes of the
crop's reference unit. ``W[i, j]`` is the flow from exporter ``i`` to
importer ``j``. The diagonal is zero.

Each flow is reported twice in FAOSTAT: once by the exporter and once by
the importer. Following Puma et al. (2015), the default ``mirror="mean"``
takes the mean of the two reports when both exist and the single report
when only one does.

From 2014 the FAOSTAT matrix reports flows below one tonne that earlier
years round away, which adds about 1,200 wheat links in a single year.
Flows below ``min_flow_t`` (default 1 t, after conversion) are therefore
dropped so link counts are comparable across years.
"""

from __future__ import annotations

from dataclasses import dataclass, field

import numpy as np
import pandas as pd

from . import config

MIRROR_RULES = ("mean", "max", "export", "import")
MIN_FLOW_T = 1.0


@dataclass
class TradeNetwork:
    """One crop-year trade network over a fixed node list."""

    crop: str
    year: int
    nodes: np.ndarray            # FAO area codes, sorted
    W: np.ndarray                # flows, tonnes
    names: dict = field(default_factory=dict)
    m49: dict = field(default_factory=dict)

    def __post_init__(self):
        self.nodes = np.asarray(self.nodes, dtype=np.int64)
        self.W = np.asarray(self.W, dtype=float)
        n = len(self.nodes)
        if self.W.shape != (n, n):
            raise ValueError(f"W has shape {self.W.shape}, expected {(n, n)}")
        if not np.all(np.isfinite(self.W)):
            raise ValueError("W has non-finite entries")
        if (self.W < 0).any():
            raise ValueError("W has negative flows")
        if np.any(np.diag(self.W) != 0):
            raise ValueError("W has self-trade on the diagonal")
        if len(np.unique(self.nodes)) != n:
            raise ValueError("duplicate node codes")

    @property
    def index(self) -> dict:
        return {int(c): k for k, c in enumerate(self.nodes)}

    @property
    def exports(self) -> np.ndarray:
        return self.W.sum(axis=1)

    @property
    def imports(self) -> np.ndarray:
        return self.W.sum(axis=0)

    @property
    def total(self) -> float:
        return float(self.W.sum())

    def copy_with(self, W: np.ndarray) -> "TradeNetwork":
        return TradeNetwork(self.crop, self.year, self.nodes.copy(), W, self.names, self.m49)

    def to_edges(self) -> pd.DataFrame:
        i, j = np.nonzero(self.W)
        return pd.DataFrame({
            "year": self.year,
            "exporter": self.nodes[i],
            "importer": self.nodes[j],
            "exporter_name": [self.names.get(int(c), "") for c in self.nodes[i]],
            "importer_name": [self.names.get(int(c), "") for c in self.nodes[j]],
            "flow_t": self.W[i, j],
        })


def convert_flows(trade: pd.DataFrame, crop: str) -> pd.DataFrame:
    """Convert item quantities to the crop's reference unit and sum over items."""
    if crop not in config.TRADE_ITEMS:
        raise ValueError(f"Unknown crop {crop!r}; expected one of {sorted(config.TRADE_ITEMS)}")
    factors = config.TRADE_ITEMS[crop]
    df = trade[trade["item"].isin(factors)].copy()
    df["converted"] = df["value"] * df["item"].map(factors)
    return df.groupby(["year", "element", "reporter", "partner"], as_index=False)["converted"].sum()


def mirror_flows(converted: pd.DataFrame, mirror: str = "mean") -> pd.DataFrame:
    """Combine exporter and importer reports into one flow per pair and year."""
    if mirror not in MIRROR_RULES:
        raise ValueError(f"mirror must be one of {MIRROR_RULES}")
    exp = converted[converted["element"] == config.EXPORT_ELEMENT].rename(
        columns={"reporter": "exporter", "partner": "importer", "converted": "x_report"})
    imp = converted[converted["element"] == config.IMPORT_ELEMENT].rename(
        columns={"reporter": "importer", "partner": "exporter", "converted": "m_report"})
    keys = ["year", "exporter", "importer"]
    both = pd.merge(exp[keys + ["x_report"]], imp[keys + ["m_report"]], on=keys, how="outer")
    x, m = both["x_report"], both["m_report"]
    if mirror == "mean":
        flow = both[["x_report", "m_report"]].mean(axis=1, skipna=True)
    elif mirror == "max":
        flow = both[["x_report", "m_report"]].max(axis=1, skipna=True)
    elif mirror == "export":
        flow = x
    else:
        flow = m
    both["flow"] = flow
    both["source"] = np.select([x.notna() & m.notna(), x.notna()], ["both", "exporter"], "importer")
    both = both[both["flow"].notna() & (both["flow"] > 0) & (both["exporter"] != both["importer"])]
    return both.reset_index(drop=True)


def build_networks(trade: pd.DataFrame, crop: str, years, countries: pd.DataFrame | None = None,
                   mirror: str = "mean", min_flow_t: float = MIN_FLOW_T) -> dict[int, TradeNetwork]:
    """Build one network per year on a node list shared by all those years."""
    if min_flow_t < 0:
        raise ValueError("min_flow_t must be non-negative")
    years = sorted(int(y) for y in years)
    flows = mirror_flows(convert_flows(trade[trade["year"].isin(years)], crop), mirror)
    flows = flows[flows["flow"] >= min_flow_t]
    missing = sorted(set(years) - set(flows["year"]))
    if missing:
        raise ValueError(f"No {crop} trade for years {missing}")
    nodes = np.unique(np.concatenate([flows["exporter"].values, flows["importer"].values]))
    idx = {int(c): k for k, c in enumerate(nodes)}
    names, m49 = {}, {}
    if countries is not None:
        names = dict(zip(countries["fao"].astype(int), countries["name"]))
        m49 = dict(zip(countries["fao"].astype(int), countries["m49"]))
    nets = {}
    for year, grp in flows.groupby("year"):
        W = np.zeros((len(nodes), len(nodes)))
        W[grp["exporter"].map(idx).values, grp["importer"].map(idx).values] = grp["flow"].values
        nets[int(year)] = TradeNetwork(crop, int(year), nodes, W, names, m49)
    return nets
