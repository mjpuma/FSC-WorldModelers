"""Tests for the Python implementation of Puma et al. (2015).

Unit tests use small made-up networks with answers worked by hand. The
integration tests at the end run only when the FAOSTAT extracts exist.

    cd erl2015 && python3 -m pytest -q
"""

from __future__ import annotations

import numpy as np
import pandas as pd
import pytest

from fsc2015 import config, data, metrics
from fsc2015.disturbance import BalanceError, apply_ban, ban_vector, disturb, gdp_allocation
from fsc2015.network import TradeNetwork, build_networks, convert_flows, mirror_flows

E, I = config.EXPORT_ELEMENT, config.IMPORT_ELEMENT


def toy_network() -> TradeNetwork:
    # Nodes 1..4. Node 1 exports 10 to 2 and 5 to 3. Node 4 exports 6 to 2 and 4 to 3.
    # Node 2 exports 1 to 1.
    W = np.array([
        [0, 10, 5, 0],
        [1, 0, 0, 0],
        [0, 0, 0, 0],
        [0, 6, 4, 0],
    ], float)
    return TradeNetwork("wheat", 2000, [1, 2, 3, 4], W,
                        names={1: "A", 2: "B", 3: "C", 4: "D"},
                        m49={1: "001", 2: "002", 3: "003", 4: "004"})


def trade_rows(rows):
    return pd.DataFrame(rows, columns=["year", "reporter", "partner", "item", "element", "value"]).assign(unit="t")


# ---------------------------------------------------------------- network

def test_network_rejects_bad_matrices():
    with pytest.raises(ValueError, match="diagonal"):
        TradeNetwork("wheat", 2000, [1, 2], np.array([[1.0, 0], [0, 0]]))
    with pytest.raises(ValueError, match="negative"):
        TradeNetwork("wheat", 2000, [1, 2], np.array([[0, -1.0], [0, 0]]))
    with pytest.raises(ValueError, match="duplicate"):
        TradeNetwork("wheat", 2000, [1, 1], np.zeros((2, 2)))
    with pytest.raises(ValueError, match="shape"):
        TradeNetwork("wheat", 2000, [1, 2, 3], np.zeros((2, 2)))


def test_conversion_factors_applied_and_summed():
    t = trade_rows([
        (2000, 1, 2, 15, E, 100.0),      # wheat grain
        (2000, 1, 2, 16, E, 78.0),       # flour: 78 * 1.282051 = 100
        (2000, 1, 2, 27, E, 100.0),      # paddy rice, not wheat
    ])
    c = convert_flows(t, "wheat")
    assert len(c) == 1
    assert c["converted"].iloc[0] == pytest.approx(200.0, rel=1e-5)
    r = convert_flows(t, "rice")
    assert r["converted"].iloc[0] == pytest.approx(67.0)


def test_mirror_rule_mean_single_and_alternatives():
    c = pd.DataFrame({
        "year": [2000, 2000, 2000, 2000],
        "element": [E, I, E, I],
        "reporter": [1, 2, 1, 4],
        "partner": [2, 1, 3, 1],
        "converted": [100.0, 80.0, 50.0, 30.0],
    })
    # 1->2 reported by both (100, 80); 1->3 by exporter only; 1->4 by importer only.
    f = mirror_flows(c, "mean").set_index(["exporter", "importer"])["flow"]
    assert f[(1, 2)] == pytest.approx(90.0)
    assert f[(1, 3)] == pytest.approx(50.0)
    assert f[(1, 4)] == pytest.approx(30.0)
    assert mirror_flows(c, "max").set_index(["exporter", "importer"])["flow"][(1, 2)] == 100.0
    assert (1, 4) not in mirror_flows(c, "export").set_index(["exporter", "importer"]).index
    assert mirror_flows(c, "import").set_index(["exporter", "importer"])["flow"][(1, 2)] == 80.0
    with pytest.raises(ValueError):
        mirror_flows(c, "median")


def test_build_networks_shared_nodes_and_no_self_trade():
    t = trade_rows([
        (2000, 1, 2, 15, E, 10.0),
        (2001, 3, 1, 15, E, 5.0),
        (2001, 3, 3, 15, E, 7.0),  # self-trade is dropped
    ])
    nets = build_networks(t, "wheat", [2000, 2001])
    assert list(nets[2000].nodes) == [1, 2, 3] == list(nets[2001].nodes)
    assert nets[2001].total == pytest.approx(5.0)
    with pytest.raises(ValueError, match="No wheat trade"):
        build_networks(t, "wheat", [2000, 2005])


def test_min_flow_threshold_drops_sub_tonne_links():
    t = trade_rows([(2000, 1, 2, 15, E, 0.4), (2000, 1, 3, 15, E, 3.0)])
    assert build_networks(t, "wheat", [2000])[2000].W.astype(bool).sum() == 1
    assert build_networks(t, "wheat", [2000], min_flow_t=0)[2000].W.astype(bool).sum() == 2
    with pytest.raises(ValueError):
        build_networks(t, "wheat", [2000], min_flow_t=-1)


# ---------------------------------------------------------------- ban + accounting

def test_full_ban_zeroes_exports_but_keeps_imports():
    net = toy_network()
    after = apply_ban(net, np.array([1.0, 0, 0, 0]))
    assert after.W[0].sum() == 0
    assert after.W[1, 0] == 1  # the banned country still imports
    assert np.array_equal(after.W[3], net.W[3])


def test_partial_ban_scales_row():
    net = toy_network()
    after = apply_ban(net, np.array([0.4, 0, 0, 0]))
    assert after.W[0, 1] == pytest.approx(6.0)
    assert after.total == pytest.approx(net.total - 0.4 * 15)


def test_static_losses_are_imports_from_banned():
    net = toy_network()
    gdp = pd.Series({1: 10.0, 2: 30.0, 3: 20.0, 4: 5.0})
    res = disturb(net, np.array([1.0, 0, 0, 0]), gdp)
    loss = res.countries.set_index("fao")["static_loss_t"]
    assert loss[2] == pytest.approx(10.0)
    assert loss[3] == pytest.approx(5.0)
    assert loss[1] == 0 and loss[4] == 0
    assert res.lost_trade_t == pytest.approx(15.0)
    assert res.countries["withheld_t"].sum() == pytest.approx(15.0)


def test_dynamic_gdp_order_by_hand():
    # Baseline imports: node1 1, node2 16, node3 9. Ban node 1 -> pool = 1 + 10 = 11.
    # GDP order: node2 (30), node3 (20), node1 (10).
    # node2 needs 16 > 11 -> gets 11, remainder 0; node3 and node1 get 0.
    net = toy_network()
    gdp = pd.Series({1: 10.0, 2: 30.0, 3: 20.0, 4: 5.0})
    res = disturb(net, np.array([1.0, 0, 0, 0]), gdp)
    got = res.countries.set_index("fao")["dynamic_received_t"]
    assert got[2] == pytest.approx(11.0)
    assert got[3] == 0 and got[1] == 0
    lost = res.countries["dynamic_loss_t"].sum()
    assert lost == pytest.approx(res.lost_trade_t)


def test_gdp_allocation_fills_in_order_and_puts_missing_gdp_last():
    s_in = np.array([5.0, 3.0, 4.0, 2.0])
    gdp = np.array([np.nan, 100.0, 50.0, np.nan])
    got = gdp_allocation(s_in, pool=8.0, gdp=gdp)
    # 100 -> 3, 50 -> 4, then missing GDP by size: s_in 5 gets 1, s_in 2 gets 0.
    assert got.tolist() == [1.0, 3.0, 4.0, 0.0]
    assert gdp_allocation(s_in, pool=0.0, gdp=gdp).sum() == 0
    assert gdp_allocation(s_in, pool=100.0, gdp=gdp).tolist() == s_in.tolist()
    with pytest.raises(ValueError):
        gdp_allocation(s_in, pool=-1.0, gdp=gdp)


def test_gdp_allocation_skips_non_importers():
    got = gdp_allocation(np.array([0.0, 5.0]), pool=3.0, gdp=np.array([999.0, 1.0]))
    assert got.tolist() == [0.0, 3.0]


def test_no_ban_means_no_loss():
    net = toy_network()
    res = disturb(net, np.zeros(4), pd.Series({1: 1.0, 2: 2.0, 3: 3.0, 4: 4.0}))
    assert res.countries["static_loss_t"].abs().sum() == 0
    assert res.countries["dynamic_loss_t"].abs().sum() == pytest.approx(0)


def test_loss_share_uses_staple_supply():
    net = toy_network()
    res = disturb(net, np.array([1.0, 0, 0, 0]), pd.Series({2: 30.0, 3: 20.0}),
                  supply=pd.Series({2: 100.0, 3: 0.0}))
    tab = res.countries.set_index("fao")
    assert tab.loc[2, "static_loss_share"] == pytest.approx(0.10)
    assert np.isnan(tab.loc[3, "static_loss_share"])  # zero supply is not a share


def test_ban_vector_from_scenario_and_validation():
    net = toy_network()
    scen = pd.DataFrame({"m49": ["001", "004"], "name": ["A", "D"], "ban_fraction": [1.0, 0.5]})
    assert ban_vector(net, scen).tolist() == [1.0, 0.0, 0.0, 0.5]
    with pytest.raises(ValueError):
        apply_ban(net, np.ones(3))


def test_balance_error_is_raised_on_inconsistent_allocation(monkeypatch):
    import fsc2015.disturbance as d
    monkeypatch.setattr(d, "TOL", -1.0)  # any result now fails its own check
    with pytest.raises(BalanceError):
        gdp_allocation(np.array([1.0]), 1.0, np.array([1.0]))


# ---------------------------------------------------------------- metrics

def test_network_summary_by_hand():
    s = metrics.network_summary(toy_network())
    assert s["L"] == 5
    assert s["G_total_mt"] == pytest.approx(26 / 1e6)
    assert s["N_out"] == 3 and s["N_in"] == 3
    assert s["k_out_mean"] == pytest.approx((2 + 1 + 2) / 3)
    assert s["k_in_max"] == 2
    assert s["s_out_max_mt"] == pytest.approx(15 / 1e6)


def test_betweenness_star_and_path():
    # Star: 1 at the centre of 2, 3, 4.
    W = np.zeros((4, 4))
    W[0, 1:] = 1
    bc = metrics.betweenness(TradeNetwork("rice", 2000, [1, 2, 3, 4], W)).set_index("fao")["betweenness"]
    assert bc[1] == pytest.approx(1.0)
    assert bc[2] == 0
    # Path 1-2-3: node 2 lies on the only path between 1 and 3 -> 1.0.
    W = np.zeros((3, 3)); W[0, 1] = 1; W[2, 1] = 1
    bc = metrics.betweenness(TradeNetwork("rice", 2000, [1, 2, 3], W)).set_index("fao")["betweenness"]
    assert bc[2] == pytest.approx(1.0)


def test_power_law_recovers_exponent():
    k = np.arange(1, 50, dtype=float)
    nodes = pd.DataFrame({"k_out": k, "s_out_t": 3.0 * k ** 1.85})
    fit = metrics.power_law_fit(nodes)
    assert fit["alpha"] == pytest.approx(1.85)
    assert fit["prefactor_t"] == pytest.approx(3.0)
    assert fit["r2"] == pytest.approx(1.0)


def test_staple_supply_weights_roots_and_ssr():
    el = config.FBS_ELEMENTS
    fbs = pd.DataFrame({
        "fao": [1, 1, 1, 1],
        "year": [2000] * 4,
        "item": [config.FBS_CEREALS, config.FBS_CEREALS, config.FBS_ROOTS, config.FBS_ROOTS],
        "element": [el["production"], el["domestic_supply"]] * 2,
        "value": [50.0, 100.0, 100.0, 100.0],
    })
    s = metrics.staple_supply(fbs).iloc[0]
    assert s["production_t"] == pytest.approx(50 + 26)
    assert s["supply_t"] == pytest.approx(100 + 26)
    assert s["ssr"] == pytest.approx(76 / 126)


def test_ssr_classes():
    c = metrics.ssr_classes(pd.Series([0.5, 0.97, 1.0, 1.04, 1.5])).astype(str).tolist()
    assert c == ["low", "marginal", "marginal", "marginal", "high"]


# ---------------------------------------------------------------- shipped tables

def test_shipped_scenarios_and_ldc_list_are_valid():
    for name in ("europe_wheat_1816", "asia_rice_1876"):
        s = data.load_scenario(name)
        assert len(s) > 10 and (s["ban_fraction"] == 1).all()
    ldc = data.load_ldc()
    assert len(ldc) == 46  # 44 current LDCs plus two FAOSTAT predecessor areas


def test_scenario_validation(tmp_path):
    bad = tmp_path / "bad.csv"
    bad.write_text("m49,name,ban_fraction\n001,A,1.5\n")
    with pytest.raises(data.DataError):
        data.load_scenario(bad)
    dup = tmp_path / "dup.csv"
    dup.write_text("m49,name\n1,A\n001,A\n")
    with pytest.raises(data.DataError):
        data.load_scenario(dup)


# ---------------------------------------------------------------- integration

HAVE_DATA = all((config.DATA_DIR / f).exists()
                for f in ("trade_wheat_rice.parquet", "food_balances.parquet", "gdp.parquet"))
needs_data = pytest.mark.skipif(not HAVE_DATA, reason="FAOSTAT extracts not built; run `python -m fsc2015 prepare`")


@pytest.fixture(scope="module")
def inputs():
    from fsc2015.pipeline import Inputs
    return Inputs.load()


@needs_data
def test_scenario_codes_exist_in_trade(inputs):
    have = {str(m).zfill(3) for m in inputs.countries["m49"]}
    for name in ("europe_wheat_1816", "asia_rice_1876"):
        missing = set(data.load_scenario(name)["m49"]) - have
        assert not missing, f"{name}: {missing}"


@needs_data
@pytest.mark.parametrize("crop,scenario", [("wheat", "europe_wheat_1816"), ("rice", "asia_rice_1876")])
def test_full_run_conserves_tonnes(inputs, crop, scenario):
    from fsc2015.pipeline import run
    r = run(crop, scenario, inp=inputs)
    c = r.tables["countries_yearly"]
    lost = r.tables["bans"].set_index("year")["lost_trade_mt"] * 1e6
    by_year = c.groupby("year")[["static_loss_t", "dynamic_loss_t", "withheld_t"]].sum()
    for col in by_year:
        assert np.allclose(by_year[col], lost.loc[by_year.index], rtol=1e-9)
    t1 = r.tables["table1_network"].set_index("metric")
    # Order-of-magnitude guards against unit errors (million tonnes per year).
    lo, hi = {"wheat": (80, 300), "rice": (10, 80)}[crop]
    assert (t1.loc["G_total_mt"].between(lo, hi)).all()


@needs_data
def test_ldc_summary_excludes_transit_ports(inputs):
    from fsc2015.pipeline import run
    r = run("wheat", "europe_wheat_1816", inp=inputs)
    cp = r.tables["countries_by_period"]
    dj = cp[cp["m49"].astype(str).str.zfill(3) == "262"]
    assert len(dj) and dj["transit_flag"].all()
    assert r.tables["ldc_summary"]["excluded_transit"].str.contains("Djibouti").all()


# ---------------------------------------------------------------- figures

def test_top_exporters_reaches_share():
    from fsc2015.figures import top_exporters
    W = np.zeros((4, 4))
    W[0, 3], W[1, 3], W[2, 3] = 60, 30, 10
    net = TradeNetwork("wheat", 2005, [1, 2, 3, 4], W, names={1: "A", 2: "B", 3: "C", 4: "D"})
    top = top_exporters({2005: net}, [2005], share=0.9)
    assert top["name"].tolist() == ["A", "B"]          # 60 + 30 = 90%
    assert top_exporters({2005: net}, [2005], share=0.95)["name"].tolist() == ["A", "B", "C"]


def test_restriction_list_has_expected_columns():
    from fsc2015.figures import load_restrictions
    df = load_restrictions()
    assert {"crop", "m49", "name", "source"} <= set(df.columns)
    assert set(df["crop"]) <= {"wheat", "rice"}


@pytest.mark.skipif(not (config.DATA_DIR / "paleo" / "1816-su.txt").exists(), reason="paleo files not downloaded")
def test_luterbacher_grid_shape_and_cold_1816():
    from fsc2015.figures import LUT_LAT, LUT_LON, summer_1816_anomaly
    a = summer_1816_anomaly()
    assert a.shape == (70, 130) == (len(LUT_LAT), len(LUT_LON))
    # France (around 47N, 2E) was well below its 1971-2000 summer mean in 1816.
    i, j = int(np.argmin(abs(LUT_LAT - 47))), int(np.argmin(abs(LUT_LON - 2)))
    assert a[i, j] < -1.5
