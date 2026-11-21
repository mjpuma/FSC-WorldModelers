# Original MATLAB code (2012–2015)

These are the MATLAB scripts behind Puma, M.J., Bose, S., Chon, S.Y., and Cook, B.I. (2015), *Assessing the evolving fragility of the global food system*, Environmental Research Letters 10, 024007, [doi:10.1088/1748-9326/10/2/024007](https://doi.org/10.1088/1748-9326/10/2/024007). They are kept exactly as they were last saved in 2014–2015, as a record of the code that produced the published results. Nothing has been edited, including the hard-coded local paths.

The maintained implementation is the Python package in [`../fsc2015/`](../fsc2015/). It is a new implementation of the same rules, not a port of these scripts; see [`../README.md`](../README.md) for how it compares with the paper.

## Files

| File | Last saved | What it does (from its header comments) |
|---|---|---|
| `Global_Trade_network_twocommodities.m` | Dec 2014 | Builds the annual bilateral trade networks for two commodities (wheat and rice) and computes complex-network statistics. M.J. Puma, 2012–2014. |
| `Global_VWground_network_twocommodities.m` | Feb 2015 | The same script as above; the only difference is which local `rootdir` line is commented out. |
| `GTN_climatedisturbances.m` | Dec 2014 | Targeted attacks on the network (export bans), self-sufficiency ratios, and the bar plots and maps of losses in the ERL article. M.J. Puma, 2013–2014. |
| `GTN_totalproduction.m` | Sep 2014 | Percentage of world production affected by a targeted attack. M.J. Puma, 2014. |
| `ComplexNetworkExtract_climatedisturbances.m` | Oct 2014 | Network files for plotting in Cytoscape, and network statistics with and without an attack. M.J. Puma, 2013–2014. |
| `GlobalFoodReserves.m` | May 2015 | Global food reserves (stocks) from FAO, IGC, USDA and national data compiled in November 2014. M.J. Puma, 2014. |
| `cross_simulation_boxplots_FIXvsVAR_SSTs.m` | Jun 2014 | Box plots comparing climate-model simulations with fixed and varying sea surface temperatures. It reads from an irrigation and SST manuscript folder and appears to belong to a different study; it is kept because it was archived with these scripts. |

## Running them

They will not run as they are. They read FAOSTAT and World Bank extracts downloaded in 2012–2014 from local Dropbox folders that are not part of this repository (country lists, GDP and population spreadsheets, bilateral trade matrices, shapefiles), and they call helper functions that are not included here, among them the FAOSTAT loaders named in their headers (`FAO_bilateral_trade_loadCE_*.m`), `colormap_function`, `logfit_v2`, graph utilities (`GraphLoadFromFile`, `GraphRemoveUnlinkedNodes`), and the m_map mapping toolbox (`m_proj`, `m_patch`, `m_grid`).
