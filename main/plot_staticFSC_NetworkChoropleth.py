#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Apr 20 04:05:36 2020

Modified from "Interactive Choropleth Maps with Bokeh"
https://cbouy.github.io/2019/06/09/interactive-map.html

Note: Older version of Bokeh needed (conda install bokeh==1.3.0)

@author: puma
"""

# others
import pandas as pd
import geopandas as gpd
import textwrap
import networkx as nx
import numpy as np
from scipy import stats
import matplotlib as plt
import matplotlib.pyplot as pyplot
from mpl_toolkits.axes_grid1 import make_axes_locatable
import seaborn as sns
import matplotlib.pylab as pylab
params = {'legend.fontsize': 'x-large',
          'figure.figsize': (15, 5),
         'axes.labelsize': 'x-large',
         'axes.titlesize':'x-large',
         'xtick.labelsize':'x-large',
         'ytick.labelsize':'x-large'}
pylab.rcParams.update(params)


# Specify  crop
crop_name = 'Wheat'
#crop_name = 'Rice'
#crop_name = 'Maize'

ACToday_iso3 = ["BGD","COL","GTM","SEN","VNM","ETH"]
ACToday_country = ["Bangladesh","Colombia","Guatemala","Senegal","Vietnam","Ethiopia"]

IASC_iso3 = ["BDI","DJI","ERI","ETH","KEN","RWA","SOM","SSD","TZA","UGA","HTI","MOZ"]
IASC_country = ["Burundi","Djibouti","Eritrea","Ethiopia","Kenya","Rwanda","Somalia","South Sudan","Tanzania","Uganda","Haiti","Mozambique"]

NorthKorea_iso3 = ["BDI","DJI","ERI","ETH","KEN","RWA","SOM","SSD","TZA","UGA","HTI","MOZ"]
IASC_country = ["Burundi","Djibouti","Eritrea","Ethiopia","Kenya","Rwanda","Somalia","South Sudan","Tanzania","Uganda","Haiti","Mozambique"]


# specify directory
dir_data ='/Users/puma/GitHub_mjpuma/FSC-WorldModelers/COVID-19_data/data_network/GlobalMetrics/'

#Read shapefile for countries using Geopandas
shapefile = 'data/countries_110m/ne_110m_admin_0_countries.shp'
gdf = gpd.read_file(shapefile)[['ADMIN', 'ADM0_A3', 'geometry']]
#Rename columns.
gdf.columns = ['country', 'country_code', 'geometry']
#Drop row corresponding to 'Antarctica'
#print(gdf[gdf['country'] == 'Antarctica'])
#gdf = gdf.drop(gdf.index[159])
gdf = gdf.loc[~(gdf['country'] == 'Antarctica')]


#Read csv file using pandas
datafile = (dir_data+ crop_name + '_Avg20152017Gdeg_in_kcal.csv')
data_degreein = pd.read_csv(datafile)
data_degreein.rename(columns={"iso3": "Code"}, inplace=True)
data_degreein.rename(columns={"Gdeg_in": "Value"}, inplace=True)
data_degreein.Value=data_degreein.Value # Convert from kcal to 10^12 kcal
data_degreein["Value"].describe()# stats

#Read csv file using pandas
datafile = (dir_data+ crop_name + '_Avg20152017Gdeg_out_kcal.csv')
data_degreeout = pd.read_csv(datafile)
data_degreeout.rename(columns={"iso3": "Code"}, inplace=True)
data_degreeout.rename(columns={"Gdeg_out": "Value"}, inplace=True)
data_degreeout.Value=data_degreeout.Value # Convert from kcal to 10^12 kcal
data_degreeout["Value"].describe()# stats


#Plot In Degree
df_plot = data_degreein
merged = gdf.merge(df_plot, left_on = 'country_code', right_on = 'Code', how = 'left')
title = (crop_name+', Number of Import Partners (In Degree)')
ax = merged.plot(column="Value",cmap='OrRd',legend=True,scheme="EqualInterval",
                  figsize=(15, 10),missing_kwds={'color': 'lightgrey'},
            );
description = '''
Based on FAOSTAT Data'''.strip()
ax.set_title(title, fontdict={'fontsize': 20}, loc='left')
ax.annotate(description, xy=(0.1, 0.1), size=12, xycoords='figure fraction')
ax.set_axis_off()
ax.get_legend().set_bbox_to_anchor((.12, .4))
ax.get_figure()
plt.pyplot.savefig(('figs/Degree_in_Avg20152017_'+ crop_name + '.png'))
plt.pyplot.savefig(('figs/Degree_in_Avg20152017_'+ crop_name + '.svg'))

#Plot Out Degree
df_plot = data_degreeout
merged = gdf.merge(df_plot, left_on = 'country_code', right_on = 'Code', how = 'left')
title = (crop_name+', Number of Export Partners (Out Degree)')
ax = merged.plot(column="Value",cmap='OrRd',legend=True,scheme="EqualInterval",
                  figsize=(15, 10),missing_kwds={'color': 'lightgrey'},
            );
description = '''
Based on FAOSTAT Data'''.strip()
ax.set_title(title, fontdict={'fontsize': 20}, loc='left')
ax.annotate(description, xy=(0.1, 0.1), size=12, xycoords='figure fraction')
ax.set_axis_off()
ax.get_legend().set_bbox_to_anchor((.12, .4))
ax.get_figure()
pyplot.savefig(('figs/Degree_out_Avg20152017_'+ crop_name + '.png'))
pyplot.savefig(('figs/Degree_out_Avg20152017_'+ crop_name + '.svg'))


#-----

# Create a sample dataframe with an text index
title = (crop_name+', Number of Import Partners (In Degree)')
ACToday = data_degreein[data_degreein.Code.isin(ACToday_iso3)]
fig, ax = pyplot.subplots(figsize=(10,7))
ax.set_title(title, fontdict={'fontsize': 20}, loc='left')
pyplot.bar(ACToday.Code, ACToday.Value, align='center', alpha=0.5, color='orange')
# Save Plot as image
fig.savefig(('figs/Degree_in_Avg20152017_ACToday' + crop_name+'.png'), dpi=100, bbox_inches='tight')
# Show Plot
pyplot.show()

# Create a sample dataframe with an text index
title = (crop_name+', Number of Export Partners (Out Degree)')
ACToday = data_degreeout[data_degreeout.Code.isin(ACToday_iso3)]
fig, ax = pyplot.subplots(figsize=(10,7))
ax.set_title(title, fontdict={'fontsize': 20}, loc='left')

pyplot.bar(ACToday.Code, ACToday.Value, align='center', alpha=0.5, color='orange')
# Save Plot as image
fig.savefig(('figs/Degree_out_Avg20152017_ACToday' + crop_name+'.png'), dpi=100, bbox_inches='tight')
# Show Plot
pyplot.show()


# Create a sample dataframe with an text index
title = (crop_name+', Number of Import Partners (In Degree)')
IASC = data_degreein[data_degreein.Code.isin(IASC_iso3)]
fig, ax = pyplot.subplots(figsize=(10,7))
ax.set_title(title, fontdict={'fontsize': 20}, loc='left')
pyplot.bar(IASC.Code, IASC.Value, align='center', alpha=0.5, color='orange')
# Save Plot as image
fig.savefig(('figs/Degree_in_Avg20152017_IASC' + crop_name+'.png'), dpi=100, bbox_inches='tight')
# Show Plot
pyplot.show()

# Create a sample dataframe with an text index
title = (crop_name+', Number of Export Partners (Out Degree)')
IASC = data_degreeout[data_degreeout.Code.isin(IASC_iso3)]
fig, ax = pyplot.subplots(figsize=(10,7))
ax.set_title(title, fontdict={'fontsize': 20}, loc='left')

pyplot.bar(IASC.Code, IASC.Value, align='center', alpha=0.5, color='orange')
# Save Plot as image
fig.savefig(('figs/Degree_out_Avg20152017_IASC' + crop_name+'.png'), dpi=100, bbox_inches='tight')
# Show Plot
pyplot.show()


