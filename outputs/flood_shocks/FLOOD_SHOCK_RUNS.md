# Flood shocks on each year's network

Draft shock table dated 2026-11-20. Rows for 2015-2024 are provisional and are expected to be replaced. Losses are metric tons. Rice is already milled. The applied shock is shock_total_t, the same-year loss plus the second-year loss from the previous year's floods.

Each crop-year uses that year's production, trade_mt_net, and USDA beginning stocks. Blank beginning stocks are zero. Imputed stocks are not used. Alpha 0 draws no stocks. Alpha 1 may draw the reported beginning stock. There is no price step and no export ban.

Stock-then-split is the primary rule. Modified PTA, with the empty-reserve freeze off, is the sensitivity in which domestic use is defended. The published FSC default is unchanged.

Runs: 276. Converged: 208. New bilateral links: 0. Negative quantities: 0. Reserve overdraws: 0. Largest absolute mass gap: 0.016 Mt.

Figures below are physical availability. They are not household consumption or nutritional outcomes. The share is the part of the consumption decline that sits in the countries hit by the flood. With stocks open, the stock share is the part of the production loss taken from beginning stocks.

| Crop | Production loss | Split, no stocks, share in shocked countries | Split, stocks open, share of the loss drawn from stocks | Split, stocks open, remaining consumption loss | Modified PTA, no stocks, share in shocked countries |
|---|---:|---:|---:|---:|---:|
| rice_milled | 30.67 | 90.1% | 96.5% | 1.09 | 44.4% |
| wheat | 27.66 | 75.6% | 99.9% | 0.03 | 29.5% |
| maize | 26.04 | 89.8% | 96.0% | 1.04 | 32.6% |

Across all crop-years, with no stocks, stock-then-split puts 85.2% of the consumption decline in the directly shocked countries. Modified PTA puts 34.6% there. With beginning stocks open, stock-then-split draws 82.22 Mt of a 84.37 Mt production loss, and world consumption falls by 2.16 Mt.

With beginning stocks open, the loss is mostly absorbed by stocks for all three crops, and what remains stays in the flooded countries. With no stocks, rice and maize keep about 90 percent of the decline in the flooded countries. Wheat keeps 76 percent. Wheat in 2010 and 2011 keeps about half, so the claim that the loss mostly stays at home is weaker for wheat when stocks cannot be drawn.

Modified PTA with no stocks reached the 50-iteration cap in 68 of 69 crop-years, so its shares are approximate. The stock-then-split runs all converged and opened no new links. The largest mass gap in the split runs is 81 tonnes. The larger gap in the line above is from modified PTA.

The three rows capped at production are Papua New Guinea 2003 rice and maize, and Turkmenistan 2016 maize. The cap was already in the table.

