#!/usr/bin/env python3
"""Draft note to Junshi Xia describing the network files and the next step."""

from pathlib import Path

from docx import Document
from docx.oxml import OxmlElement
from docx.oxml.ns import qn
from docx.shared import Inches, Pt, RGBColor

OUT = Path(__file__).resolve().parents[1] / "outputs" / "export_for_RIKEN" / "Note_for_Junshi_Xia.docx"
NAVY = RGBColor(0x0B, 0x3A, 0x4A)


def font(run, size=11, bold=False, italic=False, color=None):
    run.font.name = "Calibri"
    run._element.rPr.rFonts.set(qn("w:eastAsia"), "Calibri")
    run.font.size = Pt(size)
    run.bold = bold
    run.italic = italic
    if color is not None:
        run.font.color.rgb = color


def para(doc, text, size=11, bold=False, italic=False, space_after=8, space_before=0):
    p = doc.add_paragraph()
    p.paragraph_format.space_after = Pt(space_after)
    p.paragraph_format.space_before = Pt(space_before)
    p.paragraph_format.line_spacing = 1.08
    run = p.add_run(text)
    font(run, size=size, bold=bold, italic=italic)
    return p


def heading(doc, text):
    p = doc.add_paragraph()
    p.paragraph_format.space_before = Pt(12)
    p.paragraph_format.space_after = Pt(4)
    run = p.add_run(text)
    font(run, size=13, bold=True, color=NAVY)
    return p


def main():
    doc = Document()
    section = doc.sections[0]
    section.top_margin = Inches(0.9)
    section.bottom_margin = Inches(0.8)
    section.left_margin = Inches(0.95)
    section.right_margin = Inches(0.95)
    normal = doc.styles["Normal"]
    normal.font.name = "Calibri"
    normal.font.size = Pt(11)

    para(doc, "Note for Junshi Xia", size=20, bold=True, space_after=2)
    doc.paragraphs[-1].runs[0].font.color.rgb = NAVY
    para(doc, "Network files for the flood-loss comparison, and the next step", size=12, italic=True, space_after=2)
    para(doc, "Draft from Michael  ·  19 November 2026  ·  For the RIKEN discussion", size=10, space_after=10)

    para(
        doc,
        "Junshi, thank you for the 12 November note. I read the flood construction and the cascade as a sample of your method, and as the questions you asked about running those shocks on our network. I am not treating that cascade as a request to replace the Food Shock Cascade, and I am not treating the figures as joint results. This note describes the files that answer your question about years, crops, and units, and it proposes how we handle the difference between the two allocation rules.",
    )

    heading(doc, "What is in the package")
    para(
        doc,
        "The attached zip is the network package. README_FSC_Inputs.md inside it is the data note. Everything is metric tons. Country keys are ISO3.",
    )
    para(
        doc,
        "Annual files, 2002 through 2024, are in annual_networks_2002_2024/. There is one production file, one reserves file, and one bilateral-trade file for each of milled rice, wheat, maize, and soybean. The 2018–2021 slice of the same rows is in baseline_2018_2021/. country_lookup_ISO3_FAO.csv maps ISO3 to FAO and USDA codes. COVERAGE.csv has one row per crop, series, and year. In this build every one of those 276 rows is status ok. No FAOSTAT year in 2002–2024 is missing.",
    )
    para(
        doc,
        "Rice production is FAO item 27 (paddy) times 0.67, so the file is milled rice. Rice trade uses 0.67 for paddy, 0.8375 for husked rice, and 1 for milled rice, broken rice, and rice flour. USDA PSD Rice, Milled is already milled and is not multiplied again. Wheat, maize, and soybean use the mass factors in our ancillary table. Soybean is beans only. I included it because the same build produces it. Your note does not use it, and you can ignore those three files.",
    )
    para(
        doc,
        "Trade is the FAOSTAT detailed trade matrix, export quantity, through 2024. Self-trade is dropped. Flows with an endpoint outside our 195-country list are dropped. Re-exports are netted for Hong Kong, the United Arab Emirates, Singapore, the Netherlands, and Italy only: each hub’s export row is scaled down by min(exports, imports). The removed tonnes are in reexport_removed_mt. They are not given back to an upstream origin. trade_mt_net is the column to use. The world sum of that column is a sum of bilateral flows, not a net-trade statistic, so it can sit above the usual headline for world wheat trade.",
    )
    para(
        doc,
        "Stocks are USDA PSD beginning stocks. The PSD market year is joined to the FAO calendar year on the year number. There is no marketing-year shift. beginning_stocks_mt is blank when USDA has no row. PSD_Missing_Flag is 1 only in that case. reserves_imputed_mt is an optional fill: the same-year median stock-to-use of countries with a reported stock, times domestic use. To match the treatment in your note, use beginning_stocks_mt and treat blanks as zero. Our own trial runs used the imputed fill. Both columns are in the file so either choice is explicit.",
    )
    para(
        doc,
        "The country list is the 195-country Food Shock Cascade list. China mainland is CHN (FAO 41). Taiwan is TWN (FAO 214) and is not folded in. Angola is not on that list, so Angola’s PSD rows are not in the package. Réunion is not a separate node. EU stocks in USDA are one row and are shared out to the EU-28 member list, which includes the United Kingdom. Malta is in the 195 and is not in that member list.",
    )

    heading(doc, "What I checked")
    para(
        doc,
        "I checked the built files rather than the script. Production is 195 countries for every crop and every year from 2002 to 2024, with no duplicate country-years, no negative tonnes, and no missing production values. Rice production has a single factor, 0.67. Bilateral files have no self-trade and no negative flows. Magnitudes line up with published crops: 2024 wheat production in the file is 798 Mt, maize 1,215 Mt, soybean 398 Mt, and milled rice 549 Mt. China milled rice in 2023 is 138 Mt. US wheat in 2024 is 53.7 Mt. Milled-rice trade in 2020 sums to 44 Mt.",
    )
    para(
        doc,
        "About a quarter of wheat reserve rows have no USDA beginning stock. Those rows are flagged. They are not silently written as zero.",
    )

    heading(doc, "Two allocation rules")
    para(
        doc,
        "Your cascade draws stocks first and then splits whatever remains between an export cut and a domestic-use cut in proportion to exports and use. Partners receive that export cut pro rata on existing links. That rule is the right main case for the question in your note: how much of a harvest loss stays in the country that lost the harvest. A country that exports a small share of its supply keeps a large share of the loss at home. The rule is transparent, and your figures already use it.",
    )
    para(
        doc,
        "The Food Shock Cascade does something else. Each pass takes 1 percent of the remaining shortage out of domestic use and pushes the rest into trade along existing links. A flooded exporter therefore keeps its own consumption and cuts exports. That behavior is what the published FSC experiments were built to study: how far a shock can travel if countries defend domestic use. It is a poor headline rule for “does the loss stay at home?”, because the answer is largely fixed by the 1 percent step. With no usable reserves, the published code also freezes every exporter, which is harsher than your no-reserves case.",
    )
    para(
        doc,
        "I suggest we use your stock-then-proportional split as the primary result, and report FSC beside it as a sensitivity under a stated rule: domestic use is defended, and transmission is the upper illustration. I do not want to rewrite the published FSC default so that it imitates your split. If we run both on the same shocks and the same network, the difference is the allocation rule and nothing else.",
    )

    heading(doc, "What I need from you, and what I still owe")
    para(
        doc,
        "The file I do not have is the shock table. Your note says a sample with a README was attached separately, 422 country-years, with the loss in year t equal to the same-year loss of floods in t plus the following-year loss of floods in t−1. Please send that xlsx, with crop, ISO3 or FAO code, year, and tonnes or a fraction of production. Rice on a milled basis, with the 0.67 factor already applied or clearly marked.",
    )
    para(
        doc,
        "Three design choices are still open, and I would rather settle them before a year-by-year run. First, whether the y1 loss stays as a second-year shock, as you have it now. I am inclined to keep it that way. Second, whether PSD beginning stocks should be shifted onto a marketing year. The files are unshifted. Third, the share of stocks that can be drawn. Your two cases, all stocks and no stocks, are the right pair. I have not estimated a fraction between them.",
    )
    para(
        doc,
        "On prices: I would not add a model price to this round. Your producer-price and food-CPI checks are the evidence on access, and a reduced-form index I sketched is not calibrated. If we add a rice price step later, it should use an estimated elasticity and sit next to your Pink Sheet regression, not in place of it.",
    )

    heading(doc, "Next steps")
    para(doc, "1. You send the shock table. I confirm the country codes and the rice unit against the lookup.")
    para(doc, "2. We run your split on these annual networks, with blanks in beginning stocks treated as zero, for all-stocks and no-stocks.")
    para(doc, "3. We run FSC on the same shocks and the same networks, and we label it as the sensitivity in which domestic use is defended.")
    para(doc, "4. We compare both with the cascade in your 12 November note. Differences that remain are the network (195 countries, five hubs netted, calendar year) and not the shocks.")
    para(
        doc,
        "5. The framing sentence in your note — losses large locally, mostly absorbed by stocks, mostly staying at home, global transmission small — is the sentence your split is built to test. I would use that as the lead only after it still holds on this network. The FSC sensitivity is there so we can say how much larger transmission becomes if exporters defend domestic consumption.",
        space_after=4,
    )

    para(doc, "Michael", size=11, space_before=8, space_after=0)
    doc.save(OUT)
    print(OUT)


if __name__ == "__main__":
    main()
