"""Network metrics from section 2.1 of Puma et al. (2015).

For a directed weighted network W (exporter rows, importer columns):

- degree: k_out = number of export partners, k_in = number of import partners
- strength: s_out = total exports, s_in = total imports
- L = number of directed links, G_total = sum of all flows
- N_out, N_in = number of nodes with any exports or imports

"Active" averages and maxima are over nodes with a nonzero value in that
year. Period values are the mean of the yearly values.

Betweenness is computed on the undirected, unweighted network of active
nodes with the standard normalization 2 / ((N - 1)(N - 2)), so it lies in
[0, 1].
"""

from __future__ import annotations

import networkx as nx
import numpy as np
import pandas as pd

from . import config
from .network import TradeNetwork

MT = 1e6  # tonnes per million tonnes


def node_table(net: TradeNetwork) -> pd.DataFrame:
    A = net.W > 0
    return pd.DataFrame({
        "year": net.year,
        "fao": net.nodes,
        "name": [net.names.get(int(c), "") for c in net.nodes],
        "k_out": A.sum(axis=1),
        "k_in": A.sum(axis=0),
        "s_out_t": net.exports,
        "s_in_t": net.imports,
    })


def _active_mean(x: np.ndarray) -> float:
    x = x[x > 0]
    return float(x.mean()) if x.size else 0.0


def _active_max(x: np.ndarray) -> float:
    return float(x.max()) if x.size and x.max() > 0 else 0.0


def network_summary(net: TradeNetwork) -> dict:
    A = net.W > 0
    k_out, k_in = A.sum(axis=1), A.sum(axis=0)
    s_out, s_in = net.exports, net.imports
    return {
        "year": net.year,
        "G_total_mt": net.total / MT,
        "L": int(A.sum()),
        "N_out": int((s_out > 0).sum()),
        "N_in": int((s_in > 0).sum()),
        "N_active": int(((s_out > 0) | (s_in > 0)).sum()),
        "k_out_mean": _active_mean(k_out),
        "k_in_mean": _active_mean(k_in),
        "s_out_mean_mt": _active_mean(s_out) / MT,
        "s_in_mean_mt": _active_mean(s_in) / MT,
        "k_out_max": _active_max(k_out),
        "k_in_max": _active_max(k_in),
        "s_out_max_mt": _active_max(s_out) / MT,
        "s_in_max_mt": _active_max(s_in) / MT,
    }


def summarize_years(nets: dict[int, TradeNetwork]) -> pd.DataFrame:
    return pd.DataFrame([network_summary(nets[y]) for y in sorted(nets)])


def period_mean(by_year: pd.DataFrame, start: int, end: int) -> pd.Series:
    sel = by_year[(by_year["year"] >= start) & (by_year["year"] <= end)]
    if sel.empty:
        raise ValueError(f"No yearly rows between {start} and {end}")
    return sel.drop(columns="year").mean()


def betweenness(net: TradeNetwork) -> pd.DataFrame:
    """Undirected, unweighted betweenness over active nodes, normalized to [0, 1]."""
    U = (net.W + net.W.T) > 0
    active = np.flatnonzero(U.any(axis=1))
    G = nx.Graph()
    G.add_nodes_from(int(net.nodes[k]) for k in active)
    i, j = np.nonzero(np.triu(U, 1))
    G.add_edges_from(zip(net.nodes[i].tolist(), net.nodes[j].tolist()))
    bc = nx.betweenness_centrality(G, normalized=True)
    out = pd.DataFrame({"fao": list(bc), "betweenness": list(bc.values())})
    out["name"] = out["fao"].map(lambda c: net.names.get(int(c), ""))
    out["year"] = net.year
    return out.sort_values("betweenness", ascending=False).reset_index(drop=True)


def power_law_fit(nodes: pd.DataFrame) -> dict:
    """Least-squares fit of log s_out on log k_out over nodes that export.

    ``nodes`` holds one row per node with period-mean ``k_out`` and ``s_out_t``.
    Returns the exponent alpha, the prefactor, r^2 and the number of points.
    """
    d = nodes[(nodes["k_out"] > 0) & (nodes["s_out_t"] > 0)]
    if len(d) < 3:
        return {"alpha": np.nan, "prefactor_t": np.nan, "r2": np.nan, "n": len(d)}
    x, y = np.log(d["k_out"].to_numpy(float)), np.log(d["s_out_t"].to_numpy(float))
    alpha, logc = np.polyfit(x, y, 1)
    resid = y - (alpha * x + logc)
    r2 = 1 - resid.var() / y.var() if y.var() > 0 else np.nan
    return {"alpha": float(alpha), "prefactor_t": float(np.exp(logc)), "r2": float(r2), "n": int(len(d))}


def period_nodes(nets: dict[int, TradeNetwork], start: int, end: int) -> pd.DataFrame:
    """Per-node mean degree and strength over a period (zeros included)."""
    tabs = [node_table(nets[y]) for y in sorted(nets) if start <= y <= end]
    if not tabs:
        raise ValueError(f"No networks between {start} and {end}")
    t = pd.concat(tabs)
    return t.groupby(["fao", "name"], as_index=False)[["k_out", "k_in", "s_out_t", "s_in_t"]].mean()


# --------------------------------------------------------------------------
# Self-sufficiency from food balances


def staple_supply(fbs: pd.DataFrame) -> pd.DataFrame:
    """Cereal-equivalent production and domestic supply by country and year.

    Starchy roots count at 0.26 t cereal per t roots. Domestic supply is
    production + imports - exports + stock drawdown as published by FAOSTAT.
    """
    el = config.FBS_ELEMENTS
    keep = fbs[fbs["element"].isin([el["production"], el["domestic_supply"]]) &
               fbs["item"].isin([config.FBS_CEREALS, config.FBS_ROOTS])].copy()
    w = np.where(keep["item"] == config.FBS_ROOTS, config.ROOTS_CEREAL_EQUIV, 1.0)
    keep["ce"] = keep["value"].fillna(0) * w
    keep["what"] = np.where(keep["element"] == el["production"], "production_t", "supply_t")
    out = keep.pivot_table(index=["fao", "year"], columns="what", values="ce", aggfunc="sum",
                           fill_value=0.0).reset_index()
    for col in ("production_t", "supply_t"):
        if col not in out:
            out[col] = 0.0
    positive = out["supply_t"] > 0
    out["ssr"] = (out["production_t"] / out["supply_t"].where(positive)).where(positive)
    return out[["fao", "year", "production_t", "supply_t", "ssr"]]


def ssr_classes(ssr: pd.Series, band: float = 0.05) -> pd.Series:
    """Low (< 1 - band), marginal (within band of 1), or high self-sufficiency."""
    return pd.cut(ssr, [-np.inf, 1 - band, 1 + band, np.inf], labels=["low", "marginal", "high"],
                  right=False)
