"""Export bans with static and dynamic (GDP-ordered) accounting.

This is the disturbance step of Puma et al. (2015), section 2.5.

1. Ban. A banned exporter ``i`` with ban fraction ``f_i`` keeps
   ``(1 - f_i)`` of every export link. It still imports. The withheld volume
   ``f_i * s_out_i`` stays in the exporter's domestic supply.

2. Static accounting. No reallocation. Importer ``j`` loses
   ``s_in_j - s_in_after_j``, the imports it used to get from banned
   exporters.

3. Dynamic accounting. The whole volume still traded after the ban,
   ``P = sum(W_after)``, is a single world pool. Importers are served in
   descending GDP order. Each receives up to its full baseline imports
   ``s_in_j`` until the pool is used up; the next importer receives the
   remainder, and later importers receive nothing. Trade links are ignored.
   Importers with no GDP value are served last, largest baseline importer
   first.

Every function checks its own mass balance and raises if it fails.
"""

from __future__ import annotations

from dataclasses import dataclass

import numpy as np
import pandas as pd

from .network import TradeNetwork

TOL = 1e-6  # relative tolerance for balance checks


class BalanceError(AssertionError):
    """Raised when a disturbance result does not conserve tonnes."""


def _check(cond: bool, msg: str) -> None:
    if not cond:
        raise BalanceError(msg)


def ban_vector(net: TradeNetwork, scenario: pd.DataFrame) -> np.ndarray:
    """Ban fraction per node from a scenario table keyed by M49 code."""
    by_m49 = dict(zip(scenario["m49"], scenario["ban_fraction"]))
    f = np.array([by_m49.get(str(net.m49.get(int(c), "")).zfill(3), 0.0) for c in net.nodes], float)
    if ((f < 0) | (f > 1)).any():
        raise ValueError("ban fractions must lie in [0, 1]")
    return f


def apply_ban(net: TradeNetwork, f: np.ndarray) -> TradeNetwork:
    f = np.asarray(f, float)
    if f.shape != (len(net.nodes),):
        raise ValueError("ban vector length does not match the network")
    W_after = net.W * (1.0 - f)[:, None]
    after = net.copy_with(W_after)
    withheld = (net.W - W_after).sum(axis=1)
    scale = max(net.total, 1.0)
    _check(abs(withheld.sum() - (net.total - after.total)) <= TOL * scale,
           "ban: withheld volume does not equal the drop in world trade")
    _check(np.allclose(W_after[f == 0], net.W[f == 0]), "ban: an unbanned exporter changed")
    return after


def gdp_allocation(s_in: np.ndarray, pool: float, gdp: np.ndarray) -> np.ndarray:
    """Serve importers in descending GDP order up to their baseline imports.

    ``gdp`` may contain NaN; those importers go last, larger ``s_in`` first.
    Returns the volume each importer receives.
    """
    s_in = np.asarray(s_in, float)
    gdp = np.asarray(gdp, float)
    if s_in.shape != gdp.shape:
        raise ValueError("s_in and gdp must have the same length")
    if pool < -TOL:
        raise ValueError("pool must be non-negative")
    pool = max(float(pool), 0.0)
    has = np.isfinite(gdp)
    # lexsort: last key is primary. Sort by (has GDP desc, GDP desc, s_in desc, index).
    order = np.lexsort((np.arange(len(s_in)), -s_in, -np.where(has, gdp, 0.0), ~has))
    received = np.zeros_like(s_in)
    remain = pool
    for k in order:
        if s_in[k] <= 0 or remain <= 0:
            continue
        take = min(s_in[k], remain)
        received[k] = take
        remain -= take
    scale = max(pool, s_in.sum(), 1.0)
    _check(abs(received.sum() - min(pool, s_in.sum())) <= TOL * scale,
           "dynamic: allocated volume does not equal min(pool, total demand)")
    _check(bool(np.all(received <= s_in + TOL * scale)), "dynamic: an importer got more than baseline")
    return received


@dataclass
class DisturbanceResult:
    before: TradeNetwork
    after: TradeNetwork
    ban: np.ndarray
    countries: pd.DataFrame   # one row per node, losses in tonnes

    @property
    def lost_trade_t(self) -> float:
        return self.before.total - self.after.total


def disturb(net: TradeNetwork, f: np.ndarray, gdp: pd.Series, supply: pd.Series | None = None
            ) -> DisturbanceResult:
    """Apply a ban and compute static and dynamic losses by country.

    ``gdp`` and ``supply`` are Series indexed by FAO area code for the
    network's year. ``supply`` is staple food supply in cereal-equivalent
    tonnes; when given, losses are also reported as shares of it.
    """
    after = apply_ban(net, f)
    s_in, s_in_after = net.imports, after.imports
    static_loss = s_in - s_in_after
    g = np.array([gdp.get(int(c), np.nan) for c in net.nodes], float)
    received = gdp_allocation(s_in, after.total, g)
    dynamic_loss = s_in - received

    scale = max(net.total, 1.0)
    _check(abs(static_loss.sum() - (net.total - after.total)) <= TOL * scale,
           "static: importer losses do not equal the trade removed by the ban")
    _check(abs(dynamic_loss.sum() - (net.total - after.total)) <= TOL * scale,
           "dynamic: importer losses do not equal the trade removed by the ban")
    _check(bool((static_loss >= -TOL * scale).all()), "static: negative loss")

    tab = pd.DataFrame({
        "year": net.year,
        "fao": net.nodes,
        "name": [net.names.get(int(c), "") for c in net.nodes],
        "m49": [net.m49.get(int(c), "") for c in net.nodes],
        "ban_fraction": f,
        "exports_t": net.exports,
        "withheld_t": net.exports - after.exports,
        "imports_t": s_in,
        "static_loss_t": static_loss,
        "dynamic_received_t": received,
        "dynamic_loss_t": dynamic_loss,
        "gdp_musd2015": g,
    })
    if supply is not None:
        sup = np.array([supply.get(int(c), np.nan) for c in net.nodes], float)
        sup = np.where(sup > 0, sup, np.nan)
        tab["staple_supply_t"] = sup
        tab["static_loss_share"] = static_loss / sup
        tab["dynamic_loss_share"] = dynamic_loss / sup
    return DisturbanceResult(net, after, np.asarray(f, float), tab)
