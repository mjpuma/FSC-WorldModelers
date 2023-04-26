#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Apr 20 04:05:36 2020

Modified from "Interactive Choropleth Maps with Bokeh"
https://cbouy.github.io/2019/06/09/interactive-map.html
Map classifiers:
https://pysal.org/mapclassify/_modules/mapclassify/classifiers.html

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
from mpl_toolkits.axes_grid1 import make_axes_locatable

dir_main = '/Users/puma/GitHub_mjpuma/FSC-WorldModelers/'

# Specify  crop and scenario
i_scenario = 34 # See list below 

if (i_scenario == 1):
   crop_name = 'Wheat'
   timewindow ='_Avg20152017'
   outfolder ='COVID-19_data/data_network/'
   dir_exp = '/Users/puma/Coronavirus/GitHub_codes_data/'
  
elif (i_scenario == 2):
   crop_name = 'Maize'
   timewindow ='_Avg20152017'
   outfolder ='COVID-19_data/data_network/'
   dir_exp = '/Users/puma/Coronavirus/GitHub_codes_data/'

elif (i_scenario == 3):
   crop_name = 'Rice'
   timewindow ='_Avg20152017'
   outfolder ='COVID-19_data/data_network/'
   dir_exp = '/Users/puma/Coronavirus/GitHub_codes_data/'

# Wheat scenarios (2021) for breadbasket failure and locust declines 
elif (i_scenario == 11):
   crop_name = 'Wheat'
   timewindow ='_breadbasket_Year2008'
   outfolder ='outputs/data_network/'
   dir_exp = '/Users/puma/GitHub_mjpuma/FSC-WorldModelers/'
 
elif (i_scenario == 12):
   crop_name = 'Wheat'
   timewindow ='_locust_Year2020'
   outfolder ='outputs/data_network/'
   dir_exp = '/Users/puma/GitHub_mjpuma/FSC-WorldModelers/'

elif (i_scenario == 13):
   crop_name = 'Wheat'
   timewindow ='_locust_Year2020_breadbasket_Year2008'
   outfolder ='outputs/data_network/'
   dir_exp = '/Users/puma/GitHub_mjpuma/FSC-WorldModelers/' 

elif (i_scenario == 14):
   crop_name = 'Wheat'
   timewindow ='_ExportRestrictionFraction_Year2008'
   outfolder ='outputs/data_network/'
   dir_exp = '/Users/puma/GitHub_mjpuma/FSC-WorldModelers/'

elif (i_scenario == 15):
   crop_name = 'Wheat'
   timewindow ='_ExportRestrictionFraction_Year2008with_RUS_25'
   outfolder ='outputs/data_network/'
   dir_exp = '/Users/puma/GitHub_mjpuma/FSC-WorldModelers/'

elif (i_scenario == 16):
   crop_name = 'Wheat'
   timewindow ='_ExportRestrictionFraction_Year2008with_RUS_50'
   outfolder ='outputs/data_network/'
   dir_exp = '/Users/puma/GitHub_mjpuma/FSC-WorldModelers/'

# Maize scenarios (2021) for breadbasket failure and locust declines 
elif (i_scenario == 21):
   crop_name = 'Maize'
   timewindow ='_breadbasket_Year2008'
   outfolder ='outputs/data_network/'
   dir_exp = '/Users/puma/GitHub_mjpuma/FSC-WorldModelers/'
 
elif (i_scenario == 22):
   crop_name = 'Maize'
   timewindow ='_locust_Year2020'
   outfolder ='outputs/data_network/'
   dir_exp = '/Users/puma/GitHub_mjpuma/FSC-WorldModelers/'

elif (i_scenario == 23):
   crop_name = 'Maize'
   timewindow ='_locust_Year2020_breadbasket_Year2008'
   outfolder ='outputs/data_network/'
   dir_exp = '/Users/puma/GitHub_mjpuma/FSC-WorldModelers/' 

elif (i_scenario == 24):
   crop_name = 'Maize'
   timewindow ='_ExportRestrictionFraction_Year2008'
   outfolder ='outputs/data_network/'
   dir_exp = '/Users/puma/GitHub_mjpuma/FSC-WorldModelers/'

elif (i_scenario == 25):
   crop_name = 'Maize'
   timewindow ='_ExportRestrictionFraction_Year2008with_ARG_25'
   outfolder ='outputs/data_network/'
   dir_exp = '/Users/puma/GitHub_mjpuma/FSC-WorldModelers/'

elif (i_scenario == 26):
   crop_name = 'Maize'
   timewindow ='_ExportRestrictionFraction_Year2008with_ARG_50'
   outfolder ='outputs/data_network/'
   dir_exp = '/Users/puma/GitHub_mjpuma/FSC-WorldModelers/'

elif (i_scenario == 31):
   crop_name = 'Rice'
   timewindow ='_breadbasket_Year2008'
   outfolder ='outputs/data_network/'
   dir_exp = '/Users/puma/GitHub_mjpuma/FSC-WorldModelers/'

elif (i_scenario == 32):
   crop_name = 'Rice'
   timewindow ='_locust_Year2020'
   outfolder ='outputs/data_network/'
   dir_exp = '/Users/puma/GitHub_mjpuma/FSC-WorldModelers/'

elif (i_scenario == 33):
   crop_name = 'Rice'
   timewindow ='_locust_Year2020_breadbasket_Year2008'
   outfolder ='outputs/data_network/'
   dir_exp = '/Users/puma/GitHub_mjpuma/FSC-WorldModelers/'

elif (i_scenario == 34):
   crop_name = 'Rice'
   timewindow ='_ExportRestrictionFraction_Year2008'
   outfolder ='outputs/data_network/'
   dir_exp = '/Users/puma/GitHub_mjpuma/FSC-WorldModelers/'


#Read shapefile for countries using Geopandas
shapefile = (dir_main+'data/countries_110m/ne_110m_admin_0_countries.shp')
gdf = gpd.read_file(shapefile)[['ADMIN', 'ADM0_A3', 'geometry']]
#Rename columns.
gdf.columns = ['country', 'country_code', 'geometry']
#Drop row corresponding to 'Antarctica'
#print(gdf[gdf['country'] == 'Antarctica'])
#gdf = gdf.drop(gdf.index[159])
gdf = gdf.loc[~(gdf['country'] == 'Antarctica')]

#Read csv file using pandas
datafile = (dir_exp + outfolder + crop_name + timewindow + 'ProductionStatic.csv')
data_production = pd.read_csv(datafile)
data_production.rename(columns={"iso3": "Code"}, inplace=True)
data_production["Value"].describe()# stats
data_production.Value=data_production.Value/1e12 # Convert from kcal to 10^12 kcal
#stats.describe(data_production.Value)

#Read csv file using pandas
datafile = (dir_exp+ outfolder + crop_name + timewindow + 'ReserveStatic.csv')
data_reserve = pd.read_csv(datafile)
data_reserve.rename(columns={"iso3": "Code"}, inplace=True)
data_reserve["Value"].describe()# stats
data_reserve.Value=data_reserve.Value/1e12 # Convert from kcal to 10^12 kcal

#Read csv file using pandas
datafile = (dir_exp+ outfolder + crop_name + timewindow + 'ShortageStatic.csv')
data_shortage = pd.read_csv(datafile)
data_shortage.rename(columns={"iso3": "Code"}, inplace=True)
data_shortage.Value=data_shortage.Value/1e12 # Convert from kcal to 10^12 kcal
data_shortage["Value"].describe()# stats

#Read csv file using pandas
datafile = (dir_exp+ outfolder + crop_name + timewindow + 'ExportStatic.csv')
data_export = pd.read_csv(datafile, header = 0, index_col = 0)
num_yrs = int(data_export.shape[1]/data_export.shape[0])
data_export_2darr = data_export.values 

#Read csv file using pandas
datafile = (dir_exp+ outfolder + crop_name + timewindow + 'C1C0Static.csv')
data_C1toC0 = pd.read_csv(datafile)
data_C1toC0.rename(columns={"iso3": "Code"}, inplace=True)
data_C1toC0["Value"].describe()# stats

#Read csv file using pandas
datafile = (dir_exp+ outfolder + crop_name + timewindow + 'C2C0Static.csv')
data_C2toC0 = pd.read_csv(datafile)
data_C2toC0.rename(columns={"iso3": "Code"}, inplace=True)
data_C2toC0["Value"].describe()# stats

# #Read csv file using pandas
# datafile = (dir_exp+'COVID-19_data/data_network/GlobalMetrics/'+ crop_name + timewindow + 'Gdeg_in_kcal.csv')
# data_degreein = pd.read_csv(datafile)
# data_degreein.rename(columns={"iso3": "Code"}, inplace=True)
# data_degreein.rename(columns={"Gdeg_in": "Value"}, inplace=True)
# data_degreein.Value=data_degreein.Value # Convert from kcal to 10^12 kcal
# data_degreein["Value"].describe()# stats

#Plot C1C0Static
df_plot = data_C1toC0
merged = gdf.merge(df_plot, left_on = 'country_code', right_on = 'Code', how = 'left')
title = (crop_name+', Ratio of Domestic Supply to Initial Supply Including Production Losses')
ax = merged.plot(column="Value",cmap='OrRd_r',legend=True,scheme="natural_breaks",
                  figsize=(15, 10),missing_kwds={'color': 'lightgrey'},
            );
description = '''
Based on FAOSTAT Data'''.strip()
ax.set_title(title, fontdict={'fontsize': 14}, loc='left')
ax.annotate(description, xy=(0.1, 0.1), size=12, xycoords='figure fraction')
ax.set_axis_off()
ax.get_legend().set_bbox_to_anchor((.12, .4))
ax.get_figure()
plt.pyplot.savefig((dir_exp+'figures/C1toC0_'+ crop_name + timewindow + '.png'))
#plt.pyplot.savefig((dir_exp+'figures/C1toC0_'+ crop_name + timewindow + '.svg'))


#Plot C2C0Static
df_plot = data_C2toC0
merged = gdf.merge(df_plot, left_on = 'country_code', right_on = 'Code', how = 'left')
title = (crop_name+', Ratio of Domestic Supply to Initial Supply Including Production Loss & Export Restrictions ')
ax = merged.plot(column="Value",cmap='OrRd_r',legend=True,scheme="natural_breaks",
                  figsize=(15, 10),missing_kwds={'color': 'lightgrey'},
            );
description = '''
Based on FAOSTAT Data'''.strip()
ax.set_title(title, fontdict={'fontsize': 14}, loc='left')
ax.annotate(description, xy=(0.1, 0.1), size=12, xycoords='figure fraction')
ax.set_axis_off()
ax.get_legend().set_bbox_to_anchor((.12, .4))
ax.get_figure()
plt.pyplot.savefig((dir_exp+'figures/C2toC0_'+ crop_name + timewindow + '.png'))
#plt.pyplot.savefig((dir_exp+'figures/C2toC0_'+ crop_name + timewindow + '.svg'))
