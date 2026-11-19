#!/usr/bin/env python3
"""Build the Food Shock Cascade model brief as a Word document."""

from pathlib import Path

import matplotlib.pyplot as plt
from docx import Document
from docx.enum.text import WD_ALIGN_PARAGRAPH, WD_LINE_SPACING
from docx.oxml import OxmlElement
from docx.oxml.ns import qn
from docx.shared import Cm, Inches, Pt, RGBColor

ROOT = Path(__file__).resolve().parents[1]
FIG = ROOT / "outputs" / "yawei_benchmarks" / "figures"
EQ = FIG / "equations"
OUT = ROOT / "outputs" / "yawei_benchmarks" / "FSC_model_food_security.docx"

NAVY = RGBColor(0x0B, 0x3A, 0x4A)
TEAL = "0B3A4A"
ROW_YES = "E6F2F2"
ROW_NO = "F4F4F4"
HEADER = "0B3A4A"


def shade(cell, hex_color):
    tc = cell._tc
    tcPr = tc.get_or_add_tcPr()
    shd = OxmlElement("w:shd")
    shd.set(qn("w:fill"), hex_color)
    shd.set(qn("w:val"), "clear")
    tcPr.append(shd)


def set_run_font(run, name="Calibri", size=11, bold=False, italic=False, color=None):
    run.font.name = name
    run._element.rPr.rFonts.set(qn("w:eastAsia"), name)
    run.font.size = Pt(size)
    run.bold = bold
    run.italic = italic
    if color is not None:
        run.font.color.rgb = color


def paragraph(doc, text, size=11, bold=False, italic=False, space_after=8, space_before=0, align=None):
    p = doc.add_paragraph()
    p.paragraph_format.space_after = Pt(space_after)
    p.paragraph_format.space_before = Pt(space_before)
    p.paragraph_format.line_spacing = 1.08
    if align is not None:
        p.alignment = align
    run = p.add_run(text)
    set_run_font(run, size=size, bold=bold, italic=italic)
    return p


def add_heading_styled(doc, text, level):
    h = doc.add_heading(text, level=level)
    for run in h.runs:
        set_run_font(run, size=16 if level == 1 else 13, bold=True, color=NAVY)
    h.paragraph_format.space_before = Pt(14 if level == 1 else 10)
    h.paragraph_format.space_after = Pt(6)
    return h


def cell_text(cell, text, bold=False, size=9, color=None, center=False):
    cell.text = ""
    p = cell.paragraphs[0]
    p.paragraph_format.space_after = Pt(2)
    p.paragraph_format.space_before = Pt(2)
    if center:
        p.alignment = WD_ALIGN_PARAGRAPH.CENTER
    run = p.add_run(text)
    set_run_font(run, size=size, bold=bold, color=color)


def set_table_widths(table, widths):
    table.autofit = False
    table.allow_autofit = False
    for row in table.rows:
        for idx, width in enumerate(widths):
            row.cells[idx].width = Inches(width)


def add_table(doc, headers, rows, widths, yes_rows=None):
    table = doc.add_table(rows=1 + len(rows), cols=len(headers))
    table.style = "Table Grid"
    for i, header in enumerate(headers):
        cell_text(table.rows[0].cells[i], header, bold=True, size=8, color=RGBColor(255, 255, 255))
        shade(table.rows[0].cells[i], HEADER)
    for r, row in enumerate(rows):
        for c, value in enumerate(row):
            cell_text(table.rows[r + 1].cells[c], value, size=8)
        if yes_rows is not None:
            shade_color = yes_rows[r]
            for c in range(len(headers)):
                shade(table.rows[r + 1].cells[c], shade_color)
    set_table_widths(table, widths)
    doc.add_paragraph().paragraph_format.space_after = Pt(4)
    return table


def caption(doc, text):
    paragraph(doc, text, size=9, italic=True, space_before=2, space_after=10)


def add_picture(doc, path, width=6.4):
    p = doc.add_paragraph()
    p.alignment = WD_ALIGN_PARAGRAPH.CENTER
    p.paragraph_format.space_after = Pt(2)
    run = p.add_run()
    run.add_picture(str(path), width=Inches(width))


def render_equations():
    EQ.mkdir(parents=True, exist_ok=True)
    specs = {
        "eq_supply": r"$S_i = P_i + \sum_j E_{ji} - \sum_j E_{ij} - \Delta R_i$",
        "eq_shortage": r"$\mathrm{shortage}_i = C_i - S_i$",
        "eq_baseline": r"$C_i^{0} = P_i + \sum_j E_{ji} - \sum_j E_{ij}$",
        "eq_production": r"$P_i \leftarrow \max(P_i + dP_i,\ 0), \qquad dP_i \leq 0,\quad dP_i \geq -P_i$",
        "eq_alpha": r"$R_i \leftarrow \alpha \max(R_i,\ 0), \qquad 0 \leq \alpha \leq 1$",
        "eq_ban": r"$q_i = f_i \sum_j E_{ij}, \quad E_{ij} \leftarrow (1-f_i)E_{ij}, \quad R_i \leftarrow R_i+q_i, \quad \Delta R_i \leftarrow \Delta R_i+q_i$",
        "eq_draw": r"$\mathrm{draw}_i = \min(\max(\mathrm{shortage}_i,0),\ R_i)$",
        "eq_draw2": r"$\Delta R_i \leftarrow \Delta R_i - \mathrm{draw}_i, \qquad R_i \leftarrow R_i - \mathrm{draw}_i$",
        "eq_cut": r"$\Delta C_i = \min(\gamma \max(\mathrm{shortage}_i,0),\ \max(C_i,0)), \qquad \gamma = 0.01$",
        "eq_pta": r"$E_{ij} \leftarrow \max\left(0,\ \left(1 + \mathbf{1}_{i \notin F}\,\pi_j - \pi_i\right) E_{ij}\right)$",
        "eq_pi": r"$\pi_j = \min\left(1,\ \mathrm{need}_j / T_j\right), \quad T_j = \sum_k E_{jk} + \sum_{i \notin F} E_{ij}$",
        "eq_close": r"$C_i = \max(S_i,\ 0)$",
        "eq_price": r"$x_i=\exp\!\left[\beta_s\!\left(\frac{Z_i}{\bar Z_i}-1\right)+\beta_u\ln\!\left(\frac{\mathrm{STU}_i}{\mathrm{STU}^{\mathrm{ref}}_i}\right)\right]$",
        "eq_yawei": r"$\mathrm{export\ cut}_i = D_i \dfrac{X_i}{X_i+U_i}, \qquad \mathrm{domestic\ cut}_i = D_i \dfrac{U_i}{X_i+U_i}$",
    }
    for name, tex in specs.items():
        fig = plt.figure(figsize=(7.6, 0.42))
        fig.patch.set_facecolor("white")
        fig.text(0.01, 0.5, tex, fontsize=14, va="center", ha="left", color="#0B3A4A")
        fig.savefig(EQ / f"{name}.png", dpi=220, bbox_inches="tight", facecolor="white", pad_inches=0.04)
        plt.close(fig)


def build():
    render_equations()
    doc = Document()
    section = doc.sections[0]
    section.top_margin = Inches(0.8)
    section.bottom_margin = Inches(0.75)
    section.left_margin = Inches(0.85)
    section.right_margin = Inches(0.85)
    section.page_width = Inches(8.5)
    section.page_height = Inches(11)
    style = doc.styles["Normal"]
    style.font.name = "Calibri"
    style.font.size = Pt(11)
    style._element.rPr.rFonts.set(qn("w:eastAsia"), "Calibri")

    paragraph(doc, "Food Shock Cascade", size=22, bold=True, space_after=2)
    # The helper doesn't take color. Set the title run color directly.
    doc.paragraphs[-1].runs[0].font.color.rgb = NAVY
    paragraph(
        doc,
        "Model, equations, data, and scenarios for the RIKEN flood-and-trade comparison",
        size=13, italic=True, space_after=2,
    )
    paragraph(
        doc,
        "19 November 2026  ·  Metric tons  ·  2018–2021 mean networks  ·  Preliminary",
        size=10, space_after=8,
    )
    paragraph(
        doc,
        "This note describes the Food Shock Cascade (FSC) as it is run for the collaboration with RIKEN. "
        "It states the equations actually used, the parameters, the input data, and the scenarios. "
        "It then lines those scenarios up with Junshi Xia’s flood-loss construction and with the trade-cascade figures in his 12 November 2026 note. "
        "Junshi’s note is marked preliminary and not for circulation. His totals are quoted here only so the scenarios can be compared with them. They are his results, not FSC output.",
    )

    add_heading_styled(doc, "1. Four pillars of food security", 1)
    paragraph(
        doc,
        "FAO frames food security as four pillars: availability, access, utilization, and stability "
        "(FAO, An Introduction to the Basic Concepts of Food Security, 2008; World Summit on Food Security, 2009). "
        "These runs measure two of them. Availability is the physical balance of production, stocks, trade, and commodity use. "
        "Stability enters as the effect of one shock on that balance. Access and utilization are outside the model.",
    )
    add_table(
        doc,
        ["Pillar", "Role in these runs", "What would count as evidence", "What this document reports"],
        [
            [
                "Availability",
                "Measured",
                "Production, stocks, trade, and use of a commodity, in tonnes",
                "Country and world changes in wheat, milled rice, and maize use",
            ],
            [
                "Access",
                "Outside the model",
                "Prices, incomes, and whether households can obtain food",
                "No price or income result. Junshi’s price checks are empirical and are not FSC output",
            ],
            [
                "Utilization",
                "Outside the model",
                "Diet quality, nutrients, safety, water, and health",
                "No calorie, nutrient, or diet result. A tonne of wheat is not a diet",
            ],
            [
                "Stability",
                "Measured for one shock year",
                "Whether availability holds through a shock, and whether that holds over time",
                "One shock on a fixed 2018–2021 network. The 2002–2024 path is Junshi’s, and it needs his loss table",
            ],
        ],
        [1.15, 1.25, 2.15, 2.25],
        yes_rows=[ROW_YES, ROW_NO, ROW_NO, ROW_YES],
    )
    caption(doc, "Table 1. The four pillars, and which of them these scenarios inform. Shaded rows are the ones the model speaks to.")

    paragraph(
        doc,
        "The statistics below are availability, and they are also a one-year stability test: does use hold when a large exporter closes most of its shipments, or when a placeholder flood cuts production. "
        "The same world total under two trade rules means the grain withheld does not change. What changes is which countries eat less. That is still an availability result. It is not an access result, because the model has no prices and no households.",
    )
    add_table(
        doc,
        ["Availability statistic", "Stocks unused, trade frozen", "Stocks unused, trade can reroute", "Beginning stocks usable"],
        [
            ["Russian wheat ban, world use lost", "33.85 Mt (4.44%)", "33.85 Mt (4.44%)", "0.18 Mt (0.02%)"],
            ["Indian rice ban, world use lost", "13.41 Mt (2.60%)", "13.41 Mt (2.60%)", "0.22 Mt (0.04%)"],
            ["Indian rice ban, GCC use lost", "70%", "15%", "0.9%"],
            ["US maize ban, world use lost", "55.86 Mt (4.86%)", "55.86 Mt (4.86%)", "4.02 Mt (0.35%)"],
            ["US maize placeholder, 10% of US production", "36.4 Mt", "36.4 Mt", "0.12 Mt"],
            ["Access: prices, incomes, household entitlement", "—", "—", "—"],
            ["Utilization: calories, nutrients, diet quality", "—", "—", "—"],
        ],
        [2.35, 1.55, 1.7, 1.2],
        yes_rows=[ROW_YES, ROW_YES, ROW_YES, ROW_YES, ROW_YES, ROW_NO, ROW_NO],
    )
    caption(
        doc,
        "Table 2. Headline availability results on the 2018–2021 mean network. Bans remove 95% of the named exporter’s flows. "
        "The US maize flood row is a placeholder fraction, not Junshi Xia’s estimate. Access and utilization have no entries because the model does not produce them.",
    )
    add_table(
        doc,
        ["Commodity", "Countries", "Directed links ≥ 1 kt", "Trade (Mt/year)", "Commodity use (Mt/year)"],
        [
            ["Wheat", "195", "2,162", "227", "763"],
            ["Rice, milled", "195", "996", "45", "517"],
            ["Maize", "195", "929", "190", "1,149"],
        ],
        [1.5, 1.1, 1.6, 1.4, 1.7],
    )
    caption(doc, "Table 3. Baseline network, 2018–2021 means. Use is production plus imports minus exports. Soybean is in the input files and is not in these scenarios.")

    add_heading_styled(doc, "2. The model", 1)
    paragraph(
        doc,
        "FSC is a country-level balance of one commodity at a time. Countries are the 195 nodes of the published Food Shock Cascade. "
        "There is no household, no income, no substitution between wheat, rice, and maize, and no opening of a trade link that is zero at baseline. "
        "The published model has two allocation rules, both from Marchand et al. (2016) and Heslin et al. (2020). "
        "Proportional trade allocation (PTA) shifts volume along existing links. Reserves-based trade allocation (RTA) lets exporters offer a share of remaining usable stocks to partners that already buy from them.",
    )
    paragraph(
        doc,
        "Heslin et al. reported the cascade in kilocalories. These runs are in metric tons of a single commodity, so a percentage loss is a percentage of that commodity’s use, not of the diet.",
    )
    paragraph(doc, "Supply of country i is production, plus imports, minus exports, minus the stock change. A positive stock change is grain held back.", size=11)
    add_picture(doc, EQ / "eq_supply.png", 5.6)
    caption(doc, "Equation 1. Supply. Eij is the flow from i to j. Imports are the column sum. Exports are the row sum. ΔR is the stock change.")
    paragraph(doc, "Shortage is the gap between current booked use and supply. The cascade stops with an error if shortage is materially negative, so the books cannot create grain.")
    add_picture(doc, EQ / "eq_shortage.png", 3.6)
    caption(doc, "Equation 2. Shortage. A shortage more negative than 10⁻⁸ of the local scale is rejected.")
    paragraph(doc, "Baseline use is the same balance with no stock change.")
    add_picture(doc, EQ / "eq_baseline.png", 4.4)
    caption(doc, "Equation 3. Baseline commodity use on the 2018–2021 mean network.")

    add_heading_styled(doc, "3. Equations of a scenario", 1)
    paragraph(doc, "A production shock is zero or a loss. Losses larger than baseline production are cut back to baseline, so production cannot go negative.", size=11)
    add_picture(doc, EQ / "eq_production.png", 5.8)
    caption(doc, "Equation 4. Production update. dP is negative for a loss.")
    paragraph(doc, "Alpha is the share of USDA beginning stocks that may be drawn. It is applied once, before the cascade.")
    add_picture(doc, EQ / "eq_alpha.png", 4.6)
    caption(doc, "Equation 5. Usable reserves. Alpha outside [0, 1] is clamped.")
    paragraph(
        doc,
        "An export ban removes a fraction of the exporter’s row and records that grain as a stock build. "
        "The rise in ΔR offsets the fall in exports inside Equation 1, so the exporter’s own supply does not jump. "
        "Partners lose the imports. The banned country is then frozen, so later iterations cannot put the withdrawn grain back on the market.",
    )
    add_picture(doc, EQ / "eq_ban.png", 6.4)
    caption(doc, "Equation 6. Export ban. fi is the ban fraction. qi is the tonnes removed.")
    paragraph(doc, "Each iteration draws reserves up to the shortage, then takes a small bite of domestic use, then reallocates trade.")
    add_picture(doc, EQ / "eq_draw.png", 4.8)
    add_picture(doc, EQ / "eq_draw2.png", 4.8)
    caption(doc, "Equation 7. Reserve draw. A draw lowers ΔR and therefore raises supply in Equation 1.")
    add_picture(doc, EQ / "eq_cut.png", 6.2)
    caption(doc, "Equation 8. Domestic absorption. Gamma is 0.01, so each pass takes one percent of the remaining shortage out of use and leaves the rest for trade.")
    paragraph(
        doc,
        "Under PTA, a country with unmet need cuts its own exports and, if the partner is not frozen, raises imports along links that already exist. "
        "F is the set of frozen exporters. In the published code a country is frozen when its usable reserves are zero, and also when a ban has been applied. "
        "At alpha = 0 every country has zero usable reserves, so the published rule freezes the whole network. "
        "The reroute scenarios turn that reserve-empty freeze off and freeze only the banned exporter.",
    )
    add_picture(doc, EQ / "eq_pi.png", 6.2)
    add_picture(doc, EQ / "eq_pta.png", 5.6)
    caption(doc, "Equation 9. PTA update. Need is the shortage after the reserve draw and the domestic cut. Trade cannot go negative, and no new link is created.")
    paragraph(
        doc,
        "RTA, used in Heslin et al. as the comparison rule, first cuts exports in proportion to need. "
        "Exporters then offer 20 percent of remaining usable reserves along existing links. A banned exporter’s offer is set to zero. "
        "With alpha = 0 there are no reserves to offer, so RTA leaves the loss on the importers. That is why the RTA maps resemble the frozen PTA maps.",
    )
    paragraph(doc, "The loop stops when the trade matrix no longer changes, or at 50 iterations. Closing the books sets use to supply, and cuts exports if supply would otherwise be negative.")
    add_picture(doc, EQ / "eq_close.png", 2.8)
    caption(doc, "Equation 10. Closing use. World trade cancels, so the change in world use equals the production change minus the net stock build, plus any grain clipped when supply would have been negative.")
    paragraph(
        doc,
        "A reduced-form rice price index exists in the code and is not used in the figures. "
        "Exportable surplus is production minus domestic use, floored at zero. The index multiplies a surplus shock by an uncalibrated coefficient.",
    )
    add_picture(doc, EQ / "eq_price.png", 6.2)
    caption(
        doc,
        "Equation 11. Uncalibrated rice price factor. Zi is exportable surplus, STU is the stock-to-use ratio, and bars are 2018–2021 means. "
        "The scenarios set βs = −1 and βu = −0.5 so a tighter market raises the index. These are not estimated elasticities. "
        "The world index is the baseline-export-weighted mean of the factors. A pure export ban does not change production, so this index is not comparable to a Pink Sheet regression on flood losses.",
    )

    add_heading_styled(doc, "4. Parameters", 1)
    paragraph(doc, "The figure set uses the corners of a larger grid. The full grid is in outputs/yawei_benchmarks/benchmark_results.csv.")
    add_table(
        doc,
        ["Parameter", "Symbol", "Value in the figures", "Other values in the grid", "Role"],
        [
            ["Reserve access", "α", "0 and 1", "0.25 and 0.5", "Share of beginning stocks that can be drawn"],
            ["Domestic absorption", "γ", "0.01", "—", "Share of remaining shortage taken from use each iteration"],
            ["Iteration cap", "kmax", "50", "—", "Stops the loop if trade has not settled"],
            ["RTA offer", "rexp", "0.20", "—", "Share of remaining reserves an exporter may offer"],
            ["Ban fraction", "f", "0.95", "0.50", "Share of the export row withdrawn into stocks"],
            ["Ban duration", "—", "Full year", "Six months, counted as half", "Six-month cases scale the ban by one half"],
            ["Tiny-shortage tolerance", "ε", "10⁻⁵ of use", "—", "Shortages this small are absorbed rather than traded"],
            ["Price coefficients", "βs, βu", "Not used in figures", "−1 and −0.5 in the results file", "Uncalibrated. Not a Pink Sheet slope"],
            ["Link threshold for counts", "—", "1 kt", "—", "Network-size statistics ignore smaller flows"],
            ["Partner chart floor", "—", "0.1 Mt of baseline use", "—", "Keeps very small markets out of the ranking"],
        ],
        [1.45, 0.7, 1.25, 1.45, 1.95],
    )
    caption(doc, "Table 4. Parameters. Alpha, gamma, and the ban fraction are the ones that move the results.")

    add_heading_styled(doc, "5. Input data", 1)
    paragraph(
        doc,
        "The networks are built from outputs/export_for_RIKEN. Annual files cover 2002–2024. The scenarios average 2018–2021. "
        "They do not rerun a separate network for each flood year. That is the step that waits on Junshi’s country-year losses.",
    )
    add_table(
        doc,
        ["Series", "Source", "Treatment in these files"],
        [
            ["Production", "FAOSTAT, refreshed through 2024", "Wheat, maize, and soybean in tonnes. Rice is FAO item 27 (paddy) times 0.67, so the network is milled rice"],
            ["Bilateral trade", "FAOSTAT detailed trade matrix, importer-reported export quantity, through 2024", "Self-trade dropped. Flows outside the 195-country list dropped. Re-exports netted for Hong Kong, the United Arab Emirates, Singapore, the Netherlands, and Italy"],
            ["Stocks", "USDA PSD beginning stocks", "PSD rice is already milled and is not multiplied by 0.67. Missing PSD rows can be filled with a same-year median stock-to-use times domestic use. Remaining gaps are zero"],
            ["Domestic use", "PSD domestic consumption where it exists", "Otherwise production plus imports minus exports, floored at zero"],
            ["Countries", "FSC 195-country list", "China mainland (FAO 41) and Taiwan (FAO 214) stay separate. EU-28 in the scenarios includes the United Kingdom, matching the existing FSC member list"],
            ["Successor states", "Sudan and Serbia and Montenegro", "Historical FAO units are split with early-year production shares. PSD Sudan stays on Sudan and is not split"],
            ["Flood losses", "Not yet attached", "inputs/yawei_placeholder_flood_shocks.csv holds stand-in fractions. shock_source is placeholder_pending_junshi"],
        ],
        [1.25, 2.15, 3.4],
    )
    caption(doc, "Table 5. Input data behind the 2018–2021 networks. Soybean is exported in the same files and is not shocked in this brief.")
    paragraph(
        doc,
        "PSD market years are joined to FAO calendar years with no shift. One PSD row per country, year, and attribute is used. "
        "The EU is one PSD row and is shared out to member countries with FAO production weights. "
        "Re-export netting removes the overlap of a hub’s imports and exports. It does not reassign that flow to an upstream producer.",
    )

    add_heading_styled(doc, "6. Scenarios, and how they sit against Junshi’s work and the cascade figures", 1)
    paragraph(
        doc,
        "The 12 November 2026 note is signed by Junshi Xia (RIKEN AIP). It has two parts that this project has been treating under two names. "
        "Section 1 is Junshi’s flood-shock construction: flood extent, satellite productivity, and the crop split. "
        "Section 2 and Figures 1, 2, and the appendix are the trade cascade, the reserves comparison, the partner rankings, and the route map. "
        "Those are the figures the benchmarks were written to line up with. The cascade in the note follows Marchand et al. (2016) and Jägermeyr et al. (2020). It is not the same allocation rule as FSC.",
    )
    paragraph(doc, "Junshi’s shock, as described in that note, is built as follows. Flood extent comes from the Global Flood Database (MODIS, 2002–2018) and Sentinel-1 (2015–2024), crossed with cropland. Loss is a satellite productivity anomaly on flooded cropland in the flood year and the next year, on a 0.05 degree grid, converted to tonnes. He reports 422 country-years with a loss. The crop mix is a loss-weighted MapSPAM 2010 harvested-area mix, yield-weighted and capped at national harvested area. Where the flood date is known, only crops in season are counted. Rice is converted to milled with the factor 0.67. The shock in year t is the same-year loss of floods in t plus the following-year loss of floods in t−1.")
    paragraph(
        doc,
        "His cascade uses each year’s own trade network. A deficit country draws stocks first. Whatever remains is split between an export cut and a domestic-use cut in proportion to exports and use. Export cuts are taken from current partners pro rata and become those partners’ deficits. The loop stops below 1 tonne. He runs two cases: all stocks usable, and no stocks. There are no prices, no new links, and no substitution across crops.",
    )
    add_picture(doc, EQ / "eq_yawei.png", 5.8)
    caption(doc, "Equation 12. The residual split in the 12 November note, after stocks. Di is the remaining deficit, Xi exports, Ui domestic use. FSC does not use this split. FSC takes 1 percent of the shortage from domestic use each iteration (Equation 8) and pushes the rest into trade.")
    paragraph(
        doc,
        "That difference is why the two models answer different questions even when both are called a no-reserves case. "
        "In his no-reserves runs, 80–93 percent of a flood loss stays in the flooded country and 7–20 percent is passed on, and no partner loses more than about 7 percent of use. "
        "In FSC, the flooded country’s own use barely moves, because domestic absorption per iteration is 1 percent, and the loss leaves through exports. "
        "His with-reserves runs absorb 95–99 percent of the flood losses. FSC with beginning stocks open absorbs the headline bans down to a few tenths of a percent of world use, except the US maize ban, which still leaves 4.0 Mt.",
    )
    add_table(
        doc,
        ["His reported total, 2002–2024", "Rice", "Wheat", "Maize"],
        [
            ["Flood loss", "30.7 Mt", "27.7 Mt", "26.0 Mt"],
            ["Share of world production, median / max", "0.2% / 0.9%", "0.1% / 0.5%", "0.1% / 0.4%"],
            ["Absorbed by stocks", "96%", "99%", "95%"],
            ["No reserves: kept at home", "93%", "80%", "91%"],
            ["No reserves: passed to partners", "6.9%", "19.6%", "8.6%"],
            ["Largest partner use cut", "6.3% (Georgia, 2012)", "7.0% (Guyana, 2010)", "3.4% (Qatar, 2008)"],
        ],
        [2.5, 1.5, 1.5, 1.5],
    )
    caption(doc, "Table 6. Figures from Junshi Xia’s preliminary 12 November 2026 note. They are not FSC results. The note asks that they not be circulated.")
    paragraph(
        doc,
        "His price section is an empirical check, not a model. He finds no clear response in the flooded country’s producer price or in partners’ food CPI. "
        "World Bank Pink Sheet prices move with global flood losses, and much of that movement is ENSO. "
        "The rice slope that remains is about 0.3 in the log price per 1 percent of world production. "
        "FSC’s uncalibrated index is not that regression, and it is omitted from the figures.",
    )
    add_table(
        doc,
        ["Scenario in this document", "Shock", "What it shares with the 12 Nov note", "What it does not share"],
        [
            [
                "95% export bans, stocks unused, trade frozen",
                "No production change. The ban is a stock build",
                "An upper bound with stocks closed, in the spirit of his no-reserves panel",
                "His loss is a flood, and his partners can still receive rerouted grain. Here every exporter is frozen",
            ],
            [
                "95% export bans, stocks unused, PTA reroute",
                "Same ban",
                "Trade still moves, which is closer to his no-reserves cascade",
                "FSC passes the loss out through exports. His split keeps most of a production loss at home",
            ],
            [
                "Same bans, beginning stocks usable",
                "Same ban, alpha = 1",
                "The with-reserves comparison in his Figure 1a",
                "A ban is not a flood. World use falls only by the grain stocks cannot cover",
            ],
            [
                "PTA versus RTA maps",
                "Same ban, both rules, stocks unused",
                "A network picture of where use falls, in the spirit of his Figure 2 and of Heslin’s maps",
                "His Figure 2 sums real flood years. These maps are one ban on the 2018–2021 mean",
            ],
            [
                "Placeholder floods",
                "Invented fractions, with and without a 95% reactive ban",
                "The experiment his section 1 is meant to feed: a production loss on the trade network",
                "The fractions are not his 422 country-years. The US shock hits the whole United States node",
            ],
        ],
        [1.7, 1.45, 1.85, 1.8],
    )
    caption(doc, "Table 7. Crosswalk from the scenarios in this document to Junshi Xia’s note and to the cascade figures.")
    paragraph(doc, "The placeholder file, until his table replaces it, is:")
    add_table(
        doc,
        ["Event", "Crop", "Node", "Production loss", "Reactive ban"],
        [
            ["Thailand 2011", "Milled rice", "THA", "15%", "95%"],
            ["Pakistan 2022", "Wheat", "PAK", "10%", "95%"],
            ["Pakistan 2022", "Milled rice", "PAK", "10%", "95%"],
            ["Myanmar 2008", "Milled rice", "MMR", "15%", "95%"],
            ["US Midwest 2011", "Maize", "USA, whole country", "10%", "95%"],
            ["US Midwest 2011", "Wheat", "USA, whole country", "5%", "95%"],
        ],
        [1.5, 1.3, 1.6, 1.3, 1.1],
    )
    caption(doc, "Table 8. Placeholder shocks in inputs/yawei_placeholder_flood_shocks.csv. Each is run as production only and as production plus the ban.")

    add_heading_styled(doc, "7. Figures", 1)
    paragraph(
        doc,
        "Every map and bar chart below is an availability result, and a one-year stability result, on the 2018–2021 mean network. "
        "Panel scales differ on purpose. A shared scale hides the with-reserves case, which is the small number.",
    )

    figures = [
        (
            "fig1_rice_reserves_vs_none.png",
            "Figure 1. Milled-rice export bans of 95 percent. Left: published FSC with stocks unused, which freezes every exporter. "
            "Center: stocks unused and other exporters can reroute. Right: beginning stocks usable. "
            "India’s world loss is 13.4 Mt in both no-stock panels and 0.22 Mt when stocks are open. "
            "This is the reserves comparison in his Figure 1, run on bans rather than on flood years.",
        ),
        (
            "figA1_wheat_reserves_vs_none.png",
            "Figure 2. The same three rules for wheat. A Russian ban withholds 33.8 Mt, 4.44 percent of world wheat use, when stocks are closed, and 0.18 Mt when they are open. "
            "EU-28 includes the United Kingdom. This is the wheat counterpart of his appendix figure, again on bans rather than on the 2002–2024 flood series.",
        ),
        (
            "figA2_maize_usa.png",
            "Figure 3. A 95 percent US maize export ban. World use falls 55.9 Mt with stocks closed and 4.0 Mt, 0.35 percent, with beginning stocks open. "
            "This is the reserve case that does not disappear. His maize appendix is a flood series, and the largest partner cut he reports is about 3.4 percent of use.",
        ),
        (
            "fig1c_rice_india_partners.png",
            "Figure 4. Who loses rice use after a 95 percent Indian ban, among countries with at least 0.1 Mt of baseline use. "
            "When trade can reroute, the tonnes sit with other exporters: Thailand 1.58 Mt, Vietnam 1.13, China 1.07, Pakistan 0.89, the United States 0.74. "
            "When trade is frozen, they sit with importers: Iran 1.07 Mt, Saudi Arabia 1.02, Benin 0.86. "
            "His Figure 1c ranks partners of real flood years, and his largest rice partner cut is about 6 percent of use.",
        ),
        (
            "fig1c_wheat_russia_partners.png",
            "Figure 5. The same ranking for a Russian wheat ban. Rerouting puts the loss on the United States (2.72 Mt), Canada (2.25), France (1.87), Ukraine (1.78), and Australia (1.34). "
            "A frozen network puts it on Egypt (6.32 Mt), Iraq, Bangladesh, Yemen, and Nigeria. "
            "His largest wheat receivers, summed over flood years with no reserves, are Indonesia, Japan, Korea, Italy, and the Philippines.",
        ),
        (
            "fig_where_loss_sits.png",
            "Figure 6. Where the lost consumption sits. For a pure ban the shocked country’s own use barely changes, because the withdrawn grain is a stock build. "
            "Rerouting moves that loss onto countries that export more. Freezing the network leaves it on everyone else. "
            "The Thai rice and US maize bars are placeholder production shocks. Even there, FSC keeps only a small share of the loss in the shocked country. "
            "His cascade keeps 80–93 percent at home. Both numbers can be true, because the allocation rules differ.",
        ),
        (
            "fig2_rice_india_network.png",
            "Figure 7. Routes for the Indian rice ban when other exporters can reroute and stocks are unused. "
            "Red arrows are India’s cuts of at least 50 kt. Teal arrows are increases of at least 50 kt on other exporters’ existing links. "
            "Orange circles are consumption losses of at least 0.2 Mt. His Figure 2 uses red circles for flood losses, orange for partners’ use cuts, and curves for export cuts of at least 2 kt, summed over 2002–2024.",
        ),
        (
            "fig2_wheat_russia_network.png",
            "Figure 8. The same route map for the Russian wheat ban. Orange circles here are losses of at least 0.8 Mt, so the map stays readable. "
            "Red arrows leave Russia. Teal arrows are the extra shipments from other exporters that fill part of the gap and, in doing so, reduce their own use.",
        ),
        (
            "fig_heslin_wheat_russia_choropleth.png",
            "Figure 9. Wheat use lost after the Russian ban, as a share of national use. Left: PTA with rerouting and no reserve use. Right: RTA with no reserve use. "
            "The color scale is shared and capped at 35 percent. PTA puts the loss on other exporters, including Canada. RTA, with nothing in reserves to offer, puts it on importers, especially in Africa and the Middle East. "
            "This is the Heslin-style pair of allocation rules, drawn for a ban rather than for the 1930s wheat shock in that paper.",
        ),
        (
            "fig_heslin_rice_india_choropleth.png",
            "Figure 10. The same pair for the Indian rice ban. The scale is capped at 25 percent. "
            "Countries above the cap are drawn at the cap. The left map is the rerouting case. The right map is RTA with stocks unused.",
        ),
        (
            "fig_heslin_wheat_baseline_network.png",
            "Figure 11. The largest baseline wheat flows, 2018–2021 means. This is the network the Russian ban is applied to. "
            "Heslin’s opening figure is a wheat-trade map of the same kind, in kilocalories and for an earlier baseline.",
        ),
        (
            "fig_heslin_wheat_network_diagram.png",
            "Figure 12. Wheat links of at least 0.2 Mt among the twelve largest exporters, before the Russian ban and after it, with PTA rerouting and no reserve use. "
            "Line width uses the baseline maximum in both panels, so a surviving Russian link is drawn at about 5 percent of its original weight. "
            "Argentina and Australia have little trade with this particular set of twelve, so they appear as nodes with no edge above the cutoff.",
        ),
        (
            "fig_heslin_network_metrics.png",
            "Figure 13. Size of the full 195-country network. Wheat has 2,162 directed links of at least 1 kt and 227 Mt of trade. "
            "After the Russian ban with rerouting, 2,134 links and 205 Mt remain. Freezing every exporter drops the link count to 1,583. "
            "Rice falls from 45 Mt of trade to 38 Mt with rerouting and to 30 Mt when trade is frozen. "
            "Heslin’s network table compares node count, degree, and strength before and after a shock. These bars are that comparison for the bans.",
        ),
        (
            "fig3_placeholder_floods.png",
            "Figure 14. Placeholder flood fractions. Orange is the production loss with trade able to reroute. "
            "Red adds a 95 percent export ban. Green is that combined case with beginning stocks usable. "
            "The US maize placeholder is about 36 Mt of world use from the production cut alone, and about 92 Mt once the ban holds a further block of exports as stocks. "
            "With stocks open, the production cut is covered and the ban still leaves about 4 Mt, the same residue as the pure US maize ban. "
            "Replace the placeholder file with Junshi’s fractions before reading this figure as history.",
        ),
    ]
    for filename, text in figures:
        add_picture(doc, FIG / filename, 6.35)
        caption(doc, text)

    add_heading_styled(doc, "8. Mass balance", 1)
    paragraph(
        doc,
        "World trade cancels. For a pure ban, production does not change, and the fall in world use equals the stock build: the grain the exporter held back. "
        "No country increases its use. The loss is a transfer from consumption into stocks, then a reshuffling of who bears the missing imports.",
    )
    add_table(
        doc,
        ["Case", "Rule", "α", "Use lost", "Production change", "Stock change", "Gap"],
        [
            ["Wheat, Russia", "Frozen or reroute", "0", "33.85", "0", "33.85", "0.00"],
            ["Wheat, Russia", "Stocks open", "1", "0.18", "0", "0.18", "0.00"],
            ["Rice, India", "Frozen or reroute", "0", "13.41", "0", "13.41", "0.00"],
            ["Rice, India", "Stocks open", "1", "0.22", "0", "0.22", "0.00"],
            ["Maize, United States", "Stocks unused", "0", "55.86", "0", "55.74", "−0.12"],
            ["Maize, United States", "Stocks open", "1", "4.02", "0", "3.90", "−0.12"],
            ["US maize placeholder", "Stocks unused", "0", "36.37", "−36.25", "0", "−0.12"],
            ["US maize placeholder", "Stocks open", "1", "0.12", "−36.25", "−36.25", "−0.12"],
            ["US maize placeholder plus ban", "Stocks unused", "0", "92.12", "−36.25", "55.74", "−0.12"],
        ],
        [1.7, 1.35, 0.4, 0.85, 1.15, 1.0, 0.7],
    )
    caption(doc, "Table 9. World balance, million tonnes. Gap is the change in use, minus the production change, plus the stock change. Wheat and rice headline runs close within 1 kt.")
    paragraph(
        doc,
        "Every maize run carries the same gap of about 0.12 Mt, including the production shock that reserves cover completely. That residual is in the baseline maize balance. "
        "The largest gap elsewhere is 0.37 Mt, on the EU-28 wheat ban with trade frozen. "
        "A gap of that size does not change the ranking of the scenarios.",
    )

    add_heading_styled(doc, "9. Reading the scenarios", 1)
    paragraph(
        doc,
        "Availability is the pillar these numbers belong to. A 95 percent Russian wheat ban withholds 4.44 percent of world wheat use when stocks cannot be drawn, and 0.02 percent when beginning stocks can. "
        "The Indian rice ban withholds 2.60 percent of world rice use either way trade is allowed to move, and 0.04 percent when stocks are open. "
        "Stability, in this document, means that one-year test. It does not yet mean a 23-year path.",
    )
    paragraph(
        doc,
        "Access is Junshi’s price section, and it is not reproduced here. Utilization would require a conversion to calories or nutrients and a basket of foods. "
        "The original Heslin application was in kilocalories of wheat. These scenarios stay in tonnes of one commodity so they can be compared with his flood losses, which are also in tonnes, with rice on a milled basis.",
    )
    paragraph(
        doc,
        "The next run that would match his Figure 1 directly is a year-by-year cascade on the 2002–2024 networks, with his 422 country-year losses in place of Table 8. "
        "The input files for those years are already built. The shock table is the missing piece.",
    )

    add_heading_styled(doc, "References for the comparison", 1)
    for item in [
        "FAO. 2008. An Introduction to the Basic Concepts of Food Security. EC–FAO Food Security Programme. The four pillars used in Section 1.",
        "FAO. 2009. Declaration of the World Summit on Food Security. WSFS 2009/2. Availability, access, utilization, and stability.",
        "Heslin, A., Puma, M. J., Marchand, P., et al. 2020. Simulating the cascading effects of an extreme agricultural production shock. The PTA and RTA rules, originally in kilocalories. The choropleths and network diagrams in Section 7 follow that paper’s layout.",
        "Marchand, P., Carr, J. A., Dell’Angelo, J., et al. 2016. Reserves and trade jointly determine exposure to food supply shocks. Environmental Research Letters. The allocation rules FSC implements.",
        "Jägermeyr, J., et al. 2020. A regional nuclear conflict would compromise global food security. Proceedings of the National Academy of Sciences. Cited in the 12 November note as the other source of the proportional stock-then-split cascade.",
        "Xia, J. 12 November 2026. Flood-driven crop losses and their transmission through trade. Preliminary note for the RIKEN–Puma discussion. Not for circulation. Source of Table 6 and of the figure design these scenarios are lined up against.",
    ]:
        paragraph(doc, item, size=10, space_after=4)

    doc.save(OUT)
    print(OUT)


if __name__ == "__main__":
    build()
