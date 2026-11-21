% GlobalFoodReserves.m

% M.J. Puma, Winter 2014
clear;  clc; close all;

%% Run options
flag_map = 1;
ScenarioNum = 1;
plot_log =1;
flag_mean = 1;

%% Folders
rootdir = '/Users/mpuma/';
%rootdir = '/Users/michaeljpuma/';
folder_feature = [rootdir 'MapFeatures/'];
basefold = [rootdir 'Dropbox/FoodReserves/Parameters/'];
figure_dir=[rootdir 'Dropbox/FoodReserves/Figures/'];


%% Open the text file.
% NOTE: First row with titles is not included in first row of data; code
% recognizes this row as 'titles'.  SO no need to offset row indices below
filename = [rootdir 'Dropbox/FoodReserves/Data_AMIS/dataFAOall_Nov2014.txt']; src='FAO';
%filename = [rootdir 'Dropbox/FoodReserves/Data_AMIS/dataIGCall_Nov2014.txt']; src='IGC';
%filename = [rootdir 'Dropbox/FoodReserves/Data_AMIS/dataNATIONALall_Nov2014.txt']; src='national';
%filename = [rootdir 'Dropbox/FoodReserves/Data_AMIS/dataUSDAall_Nov2014.txt'];  src='usda';
delimiter = '\t';
startRow = 2;
formatSpec = '%s%f%s%f%s%f%s%s%s%f%[^\n\r]';
fileID = fopen(filename,'r');
dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'EmptyValue' ,NaN,'HeaderLines' ,startRow-1, 'ReturnOnError', false);
fclose(fileID);
dataFAOallNov2014 = dataset(dataArray{1:end-1}, 'VarNames', {'DataSource','CountryRegionCode','CountryRegionName','ProductCode','ProductName','ElementCode','ElementName','Units','Year','Value'});
clearvars filename delimiter startRow formatSpec fileID dataArray ans;


%% Time data: includes two years but adjust to the latter year
% [3] "Trade" refers to the following export seasons: - July/June for wheat and maize -
% October/September for soybean - January/December (calendar) for rice
% (corresponding to second year of the split year shown).
code_temp   = dataFAOallNov2014(1:end,9); 
for i = 1:length(code_temp)
    dataYearvector(i,1) = str2double(code_temp{i,1}(1:4))+1;
end

% Years
year_start = 2004;%min(dataYearvector); 
year_end =   2013;%max(dataYearvector); 
years = year_start:year_end;
num_years = year_end-year_start+1;
i_fullperiod=find(years>=year_start & years<=year_end);


%% Scenarios combining commodity and disturbance
if ScenarioNum == 1
    %Scenario 1
    disturbance_type = 2; disturb_title = 'European '; disturb_title2 = 'No Summer';
    commod_title='wheat'; commod_titleb='Wheat'; commod_print = 'Wheat';
    scale_losses = .15; scale_production = 30; scale_consumption = 30;
    commodityNum = 1;
elseif ScenarioNum == 2
    %Scenario 2
    disturbance_type = 3; disturb_title = 'Asian '; disturb_title2 = 'GV Drought';
    commod_title='rice'; commod_titleb='RiceMilled'; commod_print = 'Rice';
    scale_losses = .05; scale_production = 30; scale_consumption = 30; 
    commodityNum = 2;
elseif ScenarioNum == 3
    %Scenario 3
    disturbance_type = 4; disturb_title = 'US Disturbance'; disturb_title2 = 'US Drought';
    commod_title='maize'; commod_titleb='Maize'; commod_print = 'Maize';
    scale_losses = .1; scale_production = 30; scale_consumption = 30;
    commodityNum = 3;
elseif ScenarioNum == 4
    %Scenario 4
    disturbance_type = 4; disturb_title = 'US Disturbance'; disturb_title2 = 'US Drought';
    commod_title='wheat'; commod_titleb='Wheat'; commod_print = 'Wheat';
    scale_losses = .05; scale_production = 30; scale_consumption = 30;
    commodityNum = 1;
elseif ScenarioNum == 5
    %Scenario 5
    disturbance_type = 5; disturb_title = 'Ukraine '; disturb_title2 = 'Ukraine';
    commod_title='wheat'; commod_titleb='Wheat'; commod_print = 'Wheat';
    scale_losses = .15; scale_production = 30; scale_consumption = 30; 
    commodityNum = 1;
elseif ScenarioNum == 6
    %Scenario 6
    disturbance_type = 6; disturb_title = 'UkraineRussia '; disturb_title2 = 'UkraineRussia';
    commod_title='wheat'; commod_titleb='Wheat'; commod_print = 'Wheat';
    scale_losses = .15; scale_production = 30; scale_consumption = 30; 
    commodityNum = 1;
elseif ScenarioNum == 7
    %Scenario 7
    disturbance_type = 2; disturb_title = 'European '; disturb_title2 = 'No Summer';
    commod_title='Total Cereals'; commod_titleb='TotalCereals'; commod_print = 'TotalCereals';
    scale_losses = .15; scale_production = 30; scale_consumption = 30;
    commodityNum = 6;
end


%% PLOTTING SUPPORT CODE% Resolution Flag
flag_high = 1;
% Colormap Selection
colormap_number =30; cmap=colormap_function(colormap_number);
%colormap_number =26; cmap=colormap_function(colormap_number);
%colormap_number =29; cmapdiff=colormap_function(colormap_number);
colormap_number =35; cmapdiff=colormap_function(colormap_number);
%colormap_number =33; cmaploss=colormap_function(colormap_number);
colormap_number =34; cmaploss=colormap_function(colormap_number);
colormap_number =21; cmapStock=colormap_function(colormap_number);
colormap_number =211; cmapStocklog=colormap_function(colormap_number);
%colormap_number =38; cmapbar1=flipud(colormap_function(colormap_number));
%colormap_number =41; cmapbar2=flipud(colormap_function(colormap_number));

if flag_high == 1
    country = shape_read([folder_feature '10m_cultural/10m_cultural/ne_10m_admin_0_countries.shp']);
    provinces = shape_read([folder_feature '10m_cultural/10m_cultural/ne_10m_admin_1_states_provinces_lines_shp.shp']);
    rivers_lakes = shape_read([folder_feature '10m_physical/ne_10m_rivers_lake_centerlines.shp']);
    ocean = shape_read([folder_feature '10m_physical/ne_10m_ocean.shp']);
    land = shape_read([folder_feature '10m_physical/ne_10m_land.shp']);
    coastline = shape_read([folder_feature '10m_physical/ne_10m_coastline.shp']);
elseif flag_high == 0
    country = shape_read([folder_feature '50m_cultural/ne_50m_admin_0_countries.shp']);
    provinces = shape_read([folder_feature '50m_cultural/ne_50m_admin_1_states_provinces_lines.shp']);
    rivers_lakes = shape_read([folder_feature '50m_physical/ne_50m_rivers_lake_centerlines.shp']);
    ocean = shape_read([folder_feature '50m_physical/ne_50m_ocean.shp']);
    land = shape_read([folder_feature '50m_physical/ne_50m_land.shp']);
    coastline = shape_read([folder_feature '50m_physical/ne_50m_coastline.shp']);
end


%% DATASET:  Read Country-Level Boundries
%filename='/Users/michaeljpuma/10m-admin-0-countries/10m_admin_0_countries.shp';
filename=[rootdir 'MapFeatures/10m_cultural/10m_cultural/ne_10m_admin_0_countries.shp'];

countryshapefile = shaperead(filename);
for itemp = 1:length(countryshapefile)
    patch_codes(itemp,1) = str2double(countryshapefile(itemp,1).iso_n3);
end


%% Projection information
map_proj_name='Equidistant Cylindrical';  % Flat proj, usually use
%map_proj_name='Lambert Conformal Conic';  % Good for midlatitudes, about size of US
%map_proj_name='Mollweide';                % shrinks poles, good for global
%map_proj_name='Hammer-Aitoff'; latlim=[-90 90];               % shrinks poles, good for global

%% Plot vectors
%CS = {'k','b','r','g','m','c'};%,'y'
CS =[0.1059,0.6196,0.4667;...
  0.8510,0.3725,0.0078;...
  0.4588,0.4392,0.7020;...
  0.9059,0.1608,0.5412;...
  0.4000,0.6510,0.1176;...
  0.9020,0.6706,0.0078;...
  0.6510,0.4627,0.1137;...
  0.4000,0.4000,0.4000;...
  0.1059,0.6196,0.4667;...
  0.8510,0.3725,0.0078;...
  0.4588,0.4392,0.7020;...
  0.9059,0.1608,0.5412;...
  0.4000,0.6510,0.1176;...
  0.9020,0.6706,0.0078;...
  0.6510,0.4627,0.1137;...
  0.4000,0.4000,0.4000;...
  0.1059,0.6196,0.4667];
MS={'o','s','d','+','*','x','^','>','+','*','x','^','>','<','p','h','s'};
LS = {'-.','--','-',':'};

SSRlonmin = -180; SSRlonmax = 180; SSRlatmin = -60; SSRlatmax = 90;
SSRlonlim=[SSRlonmin SSRlonmax];
SSRlatlim=[SSRlatmin SSRlatmax];
scale_SSR = .1;

%% AMIS codes for different commodities
code_wheat = 1; % Wheat: includes soft and durum wheat.
code_ricemilled = 4; 
code_maize = 5; % Maize: includes yellow and white maize.
code_soybeans = 6;
code_coarsegrains = 7;
% Cereals: include wheat, maize (corn), millet, oats, sorghum and other 
% minor coarse grains, plus rice in milled equivalent
code_totalcereals = 8; 
code_populationA = 9; 


%% AMIS codes for different elements
code_populationB=1;	
code_AreaHarvested=2;	
code_ExtractionRate=3;	
code_Yield=4;	
code_Production=5;	
code_Imports_NMY=7;	
code_Imports_ITY=8;	
code_Exports_NMY=10;	
code_Exports_ITY=12;	
code_FeedUse=13;	
code_FoodUse=14;	
code_OtherUses=15;	
code_ClosingStocks=16;
code_GovernmentStocks=17;	
code_OpeningStocks=18;	
code_TotalSupply=19; 
code_DomesticUtilization=20;	
code_PerCapitaFoodUse=25;	
code_DomesticSupply=27;	
code_TotalUtilization=35;	
code_Crush=36;	
code_Production_1=38;




%% Conversion factors
mmt_convert = 1000000;  % tons to million metric tons
grams_per_metricton = 1000000;


%%  TARGETED ATTACK type
%disturbance_type = 1; disturb_title = 'Wheat Rust'; disturb_title2 = 'Wheat Rust';
%disturbance_type = 2; disturb_title = '1816''s Year without a Summer'; disturb_title2 = 'No Summer';
%disturbance_type = 3; disturb_title = 'Great Victorian Drought'; disturb_title2 = 'GV Drought';
%disturbance_type = 4; disturb_title = '2012 US Drought'; disturb_title2 = 'US Drought';


%% DATASET: Open FULL country list with **FAOSTAT** country codes
[country_tradecodes,country_tradenames,country_tradeinfo] = xlsread([basefold 'countrylist_partialv3.xls']);
index_tradecountry = 1:length(country_tradenames);


%%  DATASET: Open SUBSET country list with **FAOSTAT** country codes
%subsetcountry_title= 'G8 Countries'; subsetcountry_print = 'G8'; [subsetcountry_codes,subsetcountry_names,subsetcountry_info] = xlsread([basefold 'countrylist_StrongEcon.xls']); 
%subsetcountry_title= 'Plus 5 Countries'; subsetcountry_print = 'Plus5'; [subsetcountry_codes,subsetcountry_names,subsetcountry_info] = xlsread([basefold 'countrylist_plus5.xls']); 
%subsetcountry_title= 'Least Developed Countries'; subsetcountry_print = 'LDC'; [subsetcountry_codes,subsetcountry_names,subsetcountry_info] = xlsread([basefold 'countrylist_leastDC.xls']);
%subsetcountry_title= 'Top Rice Exporters'; subsetcountry_print = 'RiceExporters'; [subsetcountry_codes,subsetcountry_names,subsetcountry_info] = xlsread([basefold 'countrylist_toprice.xls']); 
subsetcountry_title= 'Top Wheat Exporters'; subsetcountry_print = 'WheatExporters'; [subsetcountry_codes,subsetcountry_names,subsetcountry_info] = xlsread([basefold 'countrylist_topwheat.xls']); 


%% DATASET: Conversion table for FAOSTAT-based full list
[~, ~, countrycodeCONVERTv3] = xlsread([basefold 'country_code_CONVERTv3.xls'],'Sheet1');
countrycodeCONVERTv3 = countrycodeCONVERTv3(2:end,2:end);
countrycodeCONVERTv3(cellfun(@(x) ~isempty(x) && isnumeric(x) && isnan(x),countrycodeCONVERTv3)) = {''};
%Index = cell2mat(countrycodeCONVERTv3(:,1));
FAOSTATcode = cell2mat(countrycodeCONVERTv3(:,2));
ISOcode = cell2mat(countrycodeCONVERTv3(:,4));
FAOmaybe = cell2mat(countrycodeCONVERTv3(:,5));
GAULcode = cell2mat(countrycodeCONVERTv3(:,6));
WBletcode = countrycodeCONVERTv3(:,7);
ISO3letcode = countrycodeCONVERTv3(:,8);
clearvars countrycodeCONVERTv3; % Clear temporary variables

% *** Identify country codes used in the AMIS & World Bank datasets ***
for i = 1:length(country_tradecodes)
   index = find(FAOSTATcode==country_tradecodes(i));
   CountryFullList_GAUL(i,1) = GAULcode(index);
   country_WBtradecodes{i,1}  = WBletcode{index,1};
end

for i = 1:length(subsetcountry_codes)
   index = find(FAOSTATcode==subsetcountry_codes(i));
   CountrySubsetList_GAUL(i,1) = GAULcode(index);
   CountrySubsetList_WB{i,1}  = WBletcode{index,1};
end


%% DATASET: Open disturbance data (files assume use of countrylist_partialv2.xls)
[~, ~, raw] = xlsread([basefold 'global_disruptionv2.xls'],'Sheet1');
raw = raw(2:end,2:end);
data = reshape([raw{:}],size(raw));% Create output variable
% Allocate imported array to column variable names
WheatStemRust = data(:,1);
Summer1816 = data(:,2);
GreatVictorianDrought = data(:,3);
Drought2012 = data(:,4);
disturb_location(:,1) = data(:,disturbance_type);
clearvars data raw; % Clear temporary variables

%% Number of disturbed countries and subset countries
number_subsetCountries = length(subsetcountry_codes);
number_disturbed = sum(disturb_location>=1);

%% DATASET:  Read County-Level Boundaries
filename= [rootdir 'MapFeatures/10m_cultural/10m_cultural/ne_10m_admin_0_countries.shp'];

%% DATASET: Import GDP and Population data 
% Source: http://databank.worldbank.org/data/home.aspx
% Downloaded on March 11, 2015; Years 1960 to 2013
[~, ~, GDPPopulationWBaccessed03112015dataonly] = xlsread([basefold 'GDPPopulation_WBaccessed03112015dataonly.xls'],'Sheet1');
GDPPopulationWBaccessed03112015dataonly = GDPPopulationWBaccessed03112015dataonly(2:end,:);
GDPPopulationWBaccessed03112015dataonly(cellfun(@(x) ~isempty(x) && isnumeric(x) && isnan(x),GDPPopulationWBaccessed03112015dataonly)) = {''};
%%NEED TO REPLACE -9999 values with NaN
%GDPPopulationWBaccessed03112015dataonly(find(GDPPopulationWBaccessed03112015dataonly==-9999))=NaN;

% Years
yearsGDP = 1960-2013;
offset_yrGDPpopulation = year_start-1960;

% Population
temp = strcmpi('Population, total', GDPPopulationWBaccessed03112015dataonly);
pop_index=find(temp(:,1)==1); clear temp;
Population_countrycodes= GDPPopulationWBaccessed03112015dataonly(pop_index,4); 
Population = GDPPopulationWBaccessed03112015dataonly(pop_index,5:end); 

% GDP
temp = strcmpi('GDP (constant 2005 US$)', GDPPopulationWBaccessed03112015dataonly);
GDP_index=find(temp(:,1)==1); clear temp;
GDP_countrycodes= GDPPopulationWBaccessed03112015dataonly(GDP_index,4); 
GDPbycountryyearUSD2005 = GDPPopulationWBaccessed03112015dataonly(GDP_index,5:end);  

% GDP per capita
temp = strcmpi('GDP per capita (constant 2005 US$)', GDPPopulationWBaccessed03112015dataonly);
GDPcapita_index=find(temp(:,1)==1); clear temp;
GDPcapita_countrycodes= GDPPopulationWBaccessed03112015dataonly(GDPcapita_index,4); 
GDPcapitabycountryyearUSD2005 = GDPPopulationWBaccessed03112015dataonly(GDPcapita_index,5:end);  

% Country codes to find appropriate row
GDPandPopulation_countrycodes = GDPPopulationWBaccessed03112015dataonly(:,4);


%% TOTAL CEREALS in AMIS data in "dataFAOallNov2014" code_totalcereals
% Assign indices
AMIS_countryvector(:,1)   = double(dataFAOallNov2014(1:end,2));
AMIS_commodityvector(:,1) = double(dataFAOallNov2014(1:end,4));
AMIS_elementvector(:,1)   = double(dataFAOallNov2014(1:end,6));

% Element
index_Production = find(AMIS_elementvector==code_Production);
index_Import = find(AMIS_elementvector==code_Imports_ITY);
index_Export = find(AMIS_elementvector==code_Exports_ITY);
index_OpeningStocks = find(AMIS_elementvector==code_OpeningStocks);
index_ClosingStocks = find(AMIS_elementvector==code_ClosingStocks);
index_DomesticUtilization = find(AMIS_elementvector==code_DomesticUtilization);

% Commodity
index_wheat = find(AMIS_commodityvector==code_wheat);
index_ricemilled = find(AMIS_commodityvector==code_ricemilled);
index_maize = find(AMIS_commodityvector==code_maize);
index_soybeans = find(AMIS_commodityvector==code_soybeans);
index_coarsegrains = find(AMIS_commodityvector==code_coarsegrains);
index_totalcereals = find(AMIS_commodityvector==code_totalcereals);
%%index_populationA = find(AMIS_commodityvector==code_populationA);

if commodityNum == 1
    index_commodity = index_wheat;
elseif commodityNum == 2
    index_commodity = index_ricemilled;
elseif commodityNum == 3
    index_commodity = index_maize;
elseif commodityNum == 4
    index_commodity = index_soybeans;
elseif commodityNum == 5
    index_commodity = index_coarsegrains;
elseif commodityNum == 6
    index_commodity = index_totalcereals;
end


%% Create matrices that include data on full country list 
% (ADD 1 to COLUMN INDEX becase first column is reserved for country codes)
Population_all=nan(length(CountryFullList_GAUL),num_years+1);
GDP_all=nan(length(CountryFullList_GAUL),num_years+1);
GDPcapita_all=nan(length(CountryFullList_GAUL),num_years+1);
Production_all=nan(length(CountryFullList_GAUL),num_years+1);
Import_all=nan(length(CountryFullList_GAUL),num_years+1);
Export_all=nan(length(CountryFullList_GAUL),num_years+1);
OpeningStocks_all=nan(length(CountryFullList_GAUL),num_years+1);
ClosingStocks_all=nan(length(CountryFullList_GAUL),num_years+1);
DomesticUtilization_all=nan(length(CountryFullList_GAUL),num_years+1);

%% Loop over countries and years to save food balance data
for i_country = 1:length(CountryFullList_GAUL)
    index_country = find(AMIS_countryvector==CountryFullList_GAUL(i_country,1));
    
    if ~isempty(index_country)
        
        % find this country_WBtradecodes{i_country,1} in pop_countrycodes and GDP_countrycodes        
        IndexC = strfind(Population_countrycodes, country_WBtradecodes{i_country,1});
        index_population = find(not(cellfun('isempty', IndexC)));

        IndexC = strfind(GDP_countrycodes, country_WBtradecodes{i_country,1});
        index_GDP = find(not(cellfun('isempty', IndexC)));
        
        IndexC = strfind(GDPcapita_countrycodes, country_WBtradecodes{i_country,1});
        index_GDPcapita = find(not(cellfun('isempty', IndexC)));
        
       
        % Add country codes to first column
        Population_all(i_country,1)=CountryFullList_GAUL(i_country);
        GDP_all(i_country,1)=CountryFullList_GAUL(i_country);
        GDPcapita_all(i_country,1)=CountryFullList_GAUL(i_country);
        Production_all(i_country,1)=CountryFullList_GAUL(i_country);
        Import_all(i_country,1)=CountryFullList_GAUL(i_country);
        Export_all(i_country,1)=CountryFullList_GAUL(i_country);
        OpeningStocks_all(i_country,1)=CountryFullList_GAUL(i_country);
        DomesticUtilization_all(i_country,1)=CountryFullList_GAUL(i_country);
        
        i_year = 0; % Initialize
        for year = year_start:year_end
            i_year = i_year + 1; % year index
            AMIS_yearvector = find(year==dataYearvector);
            i_commod = 1; % only one commodity
            
            % Population data
            Population_all(i_country,i_year+1)= Population{index_population,i_year+offset_yrGDPpopulation};

            % GDP data
            GDP_all(i_country,i_year+1)= GDPbycountryyearUSD2005{index_GDP,i_year+offset_yrGDPpopulation};
            GDPcapita_all(i_country,i_year+1)= GDPcapitabycountryyearUSD2005{index_GDPcapita,i_year+offset_yrGDPpopulation};

            % Domestic Utilization data
            temp = intersect(index_country,index_DomesticUtilization);   % select country and then element (e.g. Production, Exports)
            temp2 = intersect(temp,index_commodity);            % select commodity (e.g. wheat, rice)
            temp3 = intersect(temp2,AMIS_yearvector);           % select year
            if ~isempty(temp3)
                DomesticUtilization_all(i_country,i_year+1) =dataFAOallNov2014(temp3,10);
            else
                DomesticUtilization_all(i_country,i_year+1) = 0;
            end
            clear temp temp2 temp3 
            
            
            % Production data
            temp = intersect(index_country,index_Production);   % select country and then element (e.g. Production, Exports)
            temp2 = intersect(temp,index_commodity);            % select commodity (e.g. wheat, rice)
            temp3 = intersect(temp2,AMIS_yearvector);           % select year
            if ~isempty(temp3)
                Production_all(i_country,i_year+1) =dataFAOallNov2014(temp3,10);
            else
                Production_all(i_country,i_year+1) = 0;
            end
            clear temp temp2 temp3
            
            
            % Import data
            temp = intersect(index_country,index_Import);   % select country and then element (e.g. Production, Exports)
            temp2 = intersect(temp,index_commodity);            % select commodity (e.g. wheat, rice)
            temp3 = intersect(temp2,AMIS_yearvector);           % select year
            if ~isempty(temp3)
                Import_all(i_country,i_year+1) =dataFAOallNov2014(temp3,10);
            else
                Import_all(i_country,i_year+1) = 0;
            end
            clear temp temp2 temp3
            
            
            % Export data
            temp = intersect(index_country,index_Export);   % select country and then element (e.g. Production, Exports)
            temp2 = intersect(temp,index_commodity);            % select commodity (e.g. wheat, rice)
            temp3 = intersect(temp2,AMIS_yearvector);           % select year
            if ~isempty(temp3)
                Export_all(i_country,i_year+1) =dataFAOallNov2014(temp3,10);
            else
                Export_all(i_country,i_year+1) = 0;
            end
            clear temp temp2 temp3
            
            
            % Opening stocks data
            temp = intersect(index_country,index_OpeningStocks);   % select country and then element (e.g. Production, Exports)
            temp2 = intersect(temp,index_commodity);            % select commodity (e.g. wheat, rice)
            temp3 = intersect(temp2,AMIS_yearvector);           % select year
            if ~isempty(temp3)
                OpeningStocks_all(i_country,i_year+1) =dataFAOallNov2014(temp3,10);
            else
                OpeningStocks_all(i_country,i_year+1) = 0;
            end
            clear temp temp2 temp3
            
            
            % Closing stocks data
            temp = intersect(index_country,index_ClosingStocks);   % select country and then element (e.g. Production, Exports)
            temp2 = intersect(temp,index_commodity);            % select commodity (e.g. wheat, rice)
            temp3 = intersect(temp2,AMIS_yearvector);           % select year
            if ~isempty(temp3)
                ClosingStocks_all(i_country,i_year+1) =dataFAOallNov2014(temp3,10);
            else
                ClosingStocks_all(i_country,i_year+1) = 0;
            end
            clear temp temp2 temp3
            

            
        end
    end
end


%%%%% REPEAT BUT ONLY ON SUBSET LIST OF COUNTRIES **********************

%% Create matrices that include data on SUBSET country list 
% (ADD 1 to COLUMN INDEX becase first column is reserved for country codes)
Population_subset=nan(length(CountrySubsetList_GAUL),num_years+1);
GDP_subset=nan(length(CountrySubsetList_GAUL),num_years+1);
GDPcapita_subset=nan(length(CountrySubsetList_GAUL),num_years+1);
Production_subset=nan(length(CountrySubsetList_GAUL),num_years+1);
Import_subset=nan(length(CountrySubsetList_GAUL),num_years+1);
Export_subset=nan(length(CountrySubsetList_GAUL),num_years+1);
OpeningStocks_subset=nan(length(CountrySubsetList_GAUL),num_years+1);
ClosingStocks_subset=nan(length(CountrySubsetList_GAUL),num_years+1);
DomesticUtilization_subset=nan(length(CountrySubsetList_GAUL),num_years+1);

% Loop over countries and years to save food balance data
for i_country = 1:length(CountrySubsetList_GAUL)
    index_country = find(AMIS_countryvector==CountrySubsetList_GAUL(i_country,1));


    if ~isempty(index_country) 
        
        % find this country_WBtradecodes{i_country,1} in pop_countrycodes and GDP_countrycodes        
        IndexC = strfind(Population_countrycodes, CountrySubsetList_WB{i_country,1});
        index_population = find(not(cellfun('isempty', IndexC)));

        IndexC = strfind(GDP_countrycodes, CountrySubsetList_WB{i_country,1});
        index_GDP = find(not(cellfun('isempty', IndexC)));
        
        IndexC = strfind(GDPcapita_countrycodes, CountrySubsetList_WB{i_country,1});
        index_GDPcapita = find(not(cellfun('isempty', IndexC)));
        
        
        % Add country codes to first column
        Population_subset(i_country,1)=CountrySubsetList_GAUL(i_country);
        GDP_subset(i_country,1)=CountrySubsetList_GAUL(i_country);
        GDPcapita_subset(i_country,1)=CountrySubsetList_GAUL(i_country);
        Production_subset(i_country,1)=CountrySubsetList_GAUL(i_country);
        Import_subset(i_country,1)=CountrySubsetList_GAUL(i_country);
        Export_subset(i_country,1)=CountrySubsetList_GAUL(i_country);
        OpeningStocks_subset(i_country,1)=CountrySubsetList_GAUL(i_country);
        DomesticUtilization_subset(i_country,1)=CountrySubsetList_GAUL(i_country);
        
        i_year = 0; % Initialize
        for year = year_start:year_end
            i_year = i_year + 1; % year index
            AMIS_yearvector = find(year==dataYearvector);
            i_commod = 1; % only one commodity
            
            % Population data
            Population_subset(i_country,i_year+1)= Population{index_population,i_year+offset_yrGDPpopulation};

            % GDP data
            GDP_subset(i_country,i_year+1)= GDPbycountryyearUSD2005{index_GDP,i_year+offset_yrGDPpopulation};
            GDPcapita_subset(i_country,i_year+1)= GDPcapitabycountryyearUSD2005{index_GDPcapita,i_year+offset_yrGDPpopulation};

            % Domestic Utilization data
            temp = intersect(index_country,index_DomesticUtilization);   % select country and then element (e.g. Production, Exports)
            temp2 = intersect(temp,index_commodity);            % select commodity (e.g. wheat, rice)
            temp3 = intersect(temp2,AMIS_yearvector);           % select year
            if ~isempty(temp3)
                DomesticUtilization_subset(i_country,i_year+1) =dataFAOallNov2014(temp3,10);
            else
                DomesticUtilization_subset(i_country,i_year+1) = 0;
            end
            clear temp temp2 temp3 
            
            
            % Production data
            temp = intersect(index_country,index_Production);   % select country and then element (e.g. Production, Exports)
            temp2 = intersect(temp,index_commodity);            % select commodity (e.g. wheat, rice)
            temp3 = intersect(temp2,AMIS_yearvector);           % select year
            if ~isempty(temp3)
                Production_subset(i_country,i_year+1) =dataFAOallNov2014(temp3,10);
            else
                Production_subset(i_country,i_year+1) = 0;
            end
            clear temp temp2 temp3
            
            
            % Import data
            temp = intersect(index_country,index_Import);   % select country and then element (e.g. Production, Exports)
            temp2 = intersect(temp,index_commodity);            % select commodity (e.g. wheat, rice)
            temp3 = intersect(temp2,AMIS_yearvector);           % select year
            if ~isempty(temp3)
                Import_subset(i_country,i_year+1) =dataFAOallNov2014(temp3,10);
            else
                Import_subset(i_country,i_year+1) = 0;
            end
            clear temp temp2 temp3
            
            
            % Export data
            temp = intersect(index_country,index_Export);   % select country and then element (e.g. Production, Exports)
            temp2 = intersect(temp,index_commodity);            % select commodity (e.g. wheat, rice)
            temp3 = intersect(temp2,AMIS_yearvector);           % select year
            if ~isempty(temp3)
                Export_subset(i_country,i_year+1) =dataFAOallNov2014(temp3,10);
            else
                Export_subset(i_country,i_year+1) = 0;
            end
            clear temp temp2 temp3
            
            
            % Opening stocks data
            temp = intersect(index_country,index_OpeningStocks);   % select country and then element (e.g. Production, Exports)
            temp2 = intersect(temp,index_commodity);            % select commodity (e.g. wheat, rice)
            temp3 = intersect(temp2,AMIS_yearvector);           % select year
            if ~isempty(temp3)
                OpeningStocks_subset(i_country,i_year+1) =dataFAOallNov2014(temp3,10);
            else
                OpeningStocks_subset(i_country,i_year+1) = 0;
            end
            clear temp temp2 temp3
            
            
            % Closing stocks data
            temp = intersect(index_country,index_ClosingStocks);   % select country and then element (e.g. Production, Exports)
            temp2 = intersect(temp,index_commodity);            % select commodity (e.g. wheat, rice)
            temp3 = intersect(temp2,AMIS_yearvector);           % select year
            if ~isempty(temp3)
                ClosingStocks_subset(i_country,i_year+1) =dataFAOallNov2014(temp3,10);
            else
                ClosingStocks_subset(i_country,i_year+1) = 0;
            end
            clear temp temp2 temp3
            

            
        end
    end
end


%%  Maps of Stocks
if flag_map == 1
    figure
    hold on;
    m_proj(map_proj_name, 'lon', SSRlonlim, 'lat', SSRlatlim); shading flat;
    %m_patch(ocean.x,ocean.y,color_ocean,'FaceAlpha',1);
    %m_patch(lon_grid,lat_grid,color_ocean,'EdgeColor',color_ocean);
    %h=m_pcolor(lon_basin_new-nanmean(diff(lon_basin_new))/2, lat_basin-nanmean(diff(lat_basin))/2, basin_new);
    %set(h,'LineStyle','none')
    
    
                    
    % Data range
    if flag_mean == 1
        upper = ceil(nanmax(nanmax(ClosingStocks_all(:,i_fullperiod+1))));
    else
        upper = ceil(nanmax(nanmax(ClosingStocks_all(:,end))));
    end

    
    if plot_log == 0
        num_colors = (0.5*length(cmapStock)); % length of colormap
        start_value = 0; end_value=num_colors-1;
        scale_Stock = ceil(upper/num_colors-1);
        colorlims = fliplr([start_value:end_value])*scale_Stock;
    elseif plot_log == 1
        num_colors = (0.5*length(cmapStocklog)); % length of colormap
        start_value = 0; end_value=num_colors-1;
        scale_Stock = ceil(upper/num_colors-1);
        colorlims =fliplr([0.001 0.01 0.1 1.0]*100);
    end

 
    % Loop over country_info content
    i_sub =1;
    for i_country = 1:length(country_tradecodes)
        flag_noplot = 0;
        index_country = find(FAOSTATcode==country_tradecodes(i_country,1));
        if ~isempty(index_country)
            index_patch = find(patch_codes==ISOcode(index_country,1));
            country_tradenames(i_country,1);
            
            % Variable to plot: mean of final year
            if flag_mean==1 % Mean Variable
                variable_A = nanmean(ClosingStocks_all(i_country,i_fullperiod+1));
                missingA = sum(isnan(ClosingStocks_all(i_country,i_fullperiod+1)));
            else            % Final year
                variable_A = nanmean(ClosingStocks_all(i_country,end));
                missingA = sum(isnan(ClosingStocks_all(i_country,end)));              
            end
            
            if missingA>2; variable_A=nan; end;
            variable_temp = variable_A;
            
            if isempty(index_country)
                ['FAOSTAT missing, ' country_tradenames(i_country)];
                variable_temp = -99;
            elseif isempty(index_patch)
                ['Patch missing,  ' country_tradenames(i_country)];
                variable_temp = -99;
                flag_noplot = 1;
            end
            
            % Plot if country patch/shapefile exists
            if flag_noplot == 0
                if plot_log == 0
                    % Below assumes cmapStock has size [16,3]
                    if variable_temp == -99 || isnan(variable_temp) % MISSING
                        m_patch(countryshapefile(index_patch(i_sub),1).X,countryshapefile(index_patch(i_sub),1).Y,'w');
                    elseif variable_temp>colorlims(1)
                        m_patch(countryshapefile(index_patch(i_sub),1).X,countryshapefile(index_patch(i_sub),1).Y,cmapStock(16,:));
                    elseif variable_temp>colorlims(2) && variable_temp<=colorlims(1)
                        m_patch(countryshapefile(index_patch(i_sub),1).X,countryshapefile(index_patch(i_sub),1).Y,cmapStock(14,:));
                    elseif variable_temp>colorlims(3) && variable_temp<=colorlims(2)
                        m_patch(countryshapefile(index_patch(i_sub),1).X,countryshapefile(index_patch(i_sub),1).Y,cmapStock(12,:));
                    elseif variable_temp>colorlims(4) && variable_temp<=colorlims(3)
                        m_patch(countryshapefile(index_patch(i_sub),1).X,countryshapefile(index_patch(i_sub),1).Y,cmapStock(10,:));
                    elseif variable_temp>colorlims(5) && variable_temp<=colorlims(4)
                        m_patch(countryshapefile(index_patch(i_sub),1).X,countryshapefile(index_patch(i_sub),1).Y,cmapStock(8,:));
                    elseif variable_temp>colorlims(6) && variable_temp<=colorlims(5)
                        m_patch(countryshapefile(index_patch(i_sub),1).X,countryshapefile(index_patch(i_sub),1).Y,cmapStock(6,:));
                    elseif variable_temp>colorlims(7) && variable_temp<=colorlims(6)
                        m_patch(countryshapefile(index_patch(i_sub),1).X,countryshapefile(index_patch(i_sub),1).Y,cmapStock(4,:));
                    elseif variable_temp>colorlims(8) && variable_temp<=colorlims(7)
                        m_patch(countryshapefile(index_patch(i_sub),1).X,countryshapefile(index_patch(i_sub),1).Y,cmapStock(2,:));
                    elseif variable_temp<=colorlims(8) % if zero
                        m_patch(countryshapefile(index_patch(i_sub),1).X,countryshapefile(index_patch(i_sub),1).Y,[.6 .6 .6]);
                    end
                else 
                    % Below assumes cmapStocklog has size [8,3]
                    if variable_temp == -99 || isnan(variable_temp) % MISSING
                        m_patch(countryshapefile(index_patch(i_sub),1).X,countryshapefile(index_patch(i_sub),1).Y,'w');
                    elseif variable_temp>colorlims(1)
                        m_patch(countryshapefile(index_patch(i_sub),1).X,countryshapefile(index_patch(i_sub),1).Y,cmapStocklog(8,:));
                    elseif variable_temp>colorlims(2) && variable_temp<=colorlims(1)
                        m_patch(countryshapefile(index_patch(i_sub),1).X,countryshapefile(index_patch(i_sub),1).Y,cmapStocklog(6,:));
                    elseif variable_temp>colorlims(3) && variable_temp<=colorlims(2)
                        m_patch(countryshapefile(index_patch(i_sub),1).X,countryshapefile(index_patch(i_sub),1).Y,cmapStocklog(4,:));
                    elseif variable_temp>colorlims(4) && variable_temp<=colorlims(3)
                        m_patch(countryshapefile(index_patch(i_sub),1).X,countryshapefile(index_patch(i_sub),1).Y,cmapStocklog(2,:));
                    elseif variable_temp<=colorlims(4) % if zero
                        m_patch(countryshapefile(index_patch(i_sub),1).X,countryshapefile(index_patch(i_sub),1).Y,[.6 .6 .6]);
                    end
                    
                    
                end
            end
        end
    end
    
    
    % Grey for South Sudan
    index_patch = find(patch_codes==728);
    m_patch(countryshapefile(index_patch(i_sub),1).X,countryshapefile(index_patch(i_sub),1).Y,[.6 .6 .6]);
    
    if plot_log == 0
        colormap(cmapStock);
    else
        colormap(cmapStocklog);
    end
    
    m_plot(country.x,country.y,'color',[.2 .2 .2]);
    %m_line(lon_data_cut,lat_data_cut,'line','none','marker','square','markersize',3,'color','k','MarkerFaceColor','r');
    m_grid('linestyle', 'none','fontsize',16,'fontname','helvetica','FontWeight','bold',...
        'xticklabels',[],'yticklabels',[]);
    if flag_mean == 1
        title(['Closing Stocks (million metric tons), ' commod_title ' ' num2str(year_start) '-' num2str(year_end)],'fontsize',24);
    else
        title(['Closing Stocks (million metric tons), ' commod_title ' ' num2str(year_end)],'fontsize',24);
    end
    
    
    if plot_log == 0
        colorbar; colorbar1 = colorbar(...%'FontName','Times',...
            'FontSize',18,...
            'FontWeight','bold',...
            'LOCATION','EastOutside','fontname','helvetica');
        for i = 1:length(colorlims)
            j = length(colorlims)-i+1;
            cbarlabel{j} = num2str(colorlims(i));
        end
        cbarlabel{length(colorlims)+1}=['>' num2str(colorlims(1))];
        set(colorbar1,'YTick',[0:1/length(colorlims):1],'YTicklabel',cbarlabel)
        %         caxis([min(L) max(L)]);
        %         colorbar1 = colorbar('FontName','Times',...
        %             'FontSize',14,...
        %             'FontWeight','bold',...
        %             'XLim',[-0.5 1.5],'LOCATION','EastOutside','Ytick',L(1:length(L)),'YTicklabel',L(1:length(L)),'fontname','helvetica');
    else
         colorbar; colorbar1 = colorbar(...%'FontName','Times',...
            'FontSize',18,...
            'FontWeight','bold',...
            'LOCATION','EastOutside','fontname','helvetica');
        for i = 1:length(colorlims)
            j = length(colorlims)-i+1;
            cbarlabel{j} = num2str(colorlims(i));
        end
        cbarlabel{length(colorlims)+1}=['>' num2str(colorlims(1))];
        set(colorbar1,'YTick',[0:1/length(colorlims):1],'YTicklabel',cbarlabel)       
 
        %         myscale = [min(L) max(L)];
        %         h=colorbar('EastOutside',...
        %             'FontSize',14,...
        %             'FontWeight','bold');
        %         caxis(log10(myscale))
        %         ticks_wanted=L;
        %         set(h,'YTick',log10(ticks_wanted));
        %         set(h,'YTickLabel',ticks_wanted);
    end

    hold off
    
    % Whole figure commands
    set(gcf,'Renderer','painters')
    set(gcf,'OuterPosition',[200   999   800   600])
    set(gcf,'PaperPositionMode','auto')
    if flag_mean == 1
        print('-depsc','-painters','-r600',[figure_dir 'ClosingStocks_' num2str(year_start) '_' num2str(year_end) '_' commod_print src '.eps' ])
        
    else
        print('-depsc','-painters','-r600',[figure_dir 'ClosingStocks_' num2str(year_end) '_' commod_print src '.eps' ])
    end
    
    
end


%%  Maps of Exports
if flag_map == 1
    figure
    hold on;
    m_proj(map_proj_name, 'lon', SSRlonlim, 'lat', SSRlatlim); shading flat;
    %m_patch(ocean.x,ocean.y,color_ocean,'FaceAlpha',1);
    %m_patch(lon_grid,lat_grid,color_ocean,'EdgeColor',color_ocean);
    %h=m_pcolor(lon_basin_new-nanmean(diff(lon_basin_new))/2, lat_basin-nanmean(diff(lat_basin))/2, basin_new);
    %set(h,'LineStyle','none')
    
    
                    
    % Data range
    if flag_mean == 1
        upper = ceil(nanmax(nanmax(Export_all(:,i_fullperiod+1))));
    else
        upper = ceil(nanmax(nanmax(Export_all(:,end))));
    end

    
    if plot_log == 0
        num_colors = (0.5*length(cmapStock)); % length of colormap
        start_value = 0; end_value=num_colors-1;
        scale_Stock = ceil(upper/num_colors-1);
        colorlims = fliplr([start_value:end_value])*scale_Stock;
    elseif plot_log == 1
        num_colors = (0.5*length(cmapStocklog)); % length of colormap
        start_value = 0; end_value=num_colors-1;
        scale_Stock = ceil(upper/num_colors-1);
        colorlims =fliplr([0.001 0.01 0.1 1.0]*100);
    end

 
    % Loop over country_info content
    i_sub =1;
    for i_country = 1:length(country_tradecodes)
        flag_noplot = 0;
        index_country = find(FAOSTATcode==country_tradecodes(i_country,1));
        if ~isempty(index_country)
            index_patch = find(patch_codes==ISOcode(index_country,1));
            country_tradenames(i_country,1);
            
            % Variable to plot: mean of final year
            if flag_mean==1 % Mean Variable
                variable_A = nanmean(Export_all(i_country,i_fullperiod+1));
                missingA = sum(isnan(Export_all(i_country,i_fullperiod+1)));
            else            % Final year
                variable_A = nanmean(Export_all(i_country,end));
                missingA = sum(isnan(Export_all(i_country,end)));              
            end
            
            if missingA>2; variable_A=nan; end;
            variable_temp = variable_A;
            
            if isempty(index_country)
                ['FAOSTAT missing, ' country_tradenames(i_country)];
                variable_temp = -99;
            elseif isempty(index_patch)
                ['Patch missing,  ' country_tradenames(i_country)];
                variable_temp = -99;
                flag_noplot = 1;
            end
            
            % Plot if country patch/shapefile exists
            if flag_noplot == 0
                if plot_log == 0
                    % Below assumes cmapStock has size [16,3]
                    if variable_temp == -99 || isnan(variable_temp) % MISSING
                        m_patch(countryshapefile(index_patch(i_sub),1).X,countryshapefile(index_patch(i_sub),1).Y,'w');
                    elseif variable_temp>colorlims(1)
                        m_patch(countryshapefile(index_patch(i_sub),1).X,countryshapefile(index_patch(i_sub),1).Y,cmapStock(16,:));
                    elseif variable_temp>colorlims(2) && variable_temp<=colorlims(1)
                        m_patch(countryshapefile(index_patch(i_sub),1).X,countryshapefile(index_patch(i_sub),1).Y,cmapStock(14,:));
                    elseif variable_temp>colorlims(3) && variable_temp<=colorlims(2)
                        m_patch(countryshapefile(index_patch(i_sub),1).X,countryshapefile(index_patch(i_sub),1).Y,cmapStock(12,:));
                    elseif variable_temp>colorlims(4) && variable_temp<=colorlims(3)
                        m_patch(countryshapefile(index_patch(i_sub),1).X,countryshapefile(index_patch(i_sub),1).Y,cmapStock(10,:));
                    elseif variable_temp>colorlims(5) && variable_temp<=colorlims(4)
                        m_patch(countryshapefile(index_patch(i_sub),1).X,countryshapefile(index_patch(i_sub),1).Y,cmapStock(8,:));
                    elseif variable_temp>colorlims(6) && variable_temp<=colorlims(5)
                        m_patch(countryshapefile(index_patch(i_sub),1).X,countryshapefile(index_patch(i_sub),1).Y,cmapStock(6,:));
                    elseif variable_temp>colorlims(7) && variable_temp<=colorlims(6)
                        m_patch(countryshapefile(index_patch(i_sub),1).X,countryshapefile(index_patch(i_sub),1).Y,cmapStock(4,:));
                    elseif variable_temp>colorlims(8) && variable_temp<=colorlims(7)
                        m_patch(countryshapefile(index_patch(i_sub),1).X,countryshapefile(index_patch(i_sub),1).Y,cmapStock(2,:));
                    elseif variable_temp<=colorlims(8) % if zero
                        m_patch(countryshapefile(index_patch(i_sub),1).X,countryshapefile(index_patch(i_sub),1).Y,[.6 .6 .6]);
                    end
                else 
                    % Below assumes cmapStocklog has size [8,3]
                    if variable_temp == -99 || isnan(variable_temp) % MISSING
                        m_patch(countryshapefile(index_patch(i_sub),1).X,countryshapefile(index_patch(i_sub),1).Y,'w');
                    elseif variable_temp>colorlims(1)
                        m_patch(countryshapefile(index_patch(i_sub),1).X,countryshapefile(index_patch(i_sub),1).Y,cmapStocklog(8,:));
                    elseif variable_temp>colorlims(2) && variable_temp<=colorlims(1)
                        m_patch(countryshapefile(index_patch(i_sub),1).X,countryshapefile(index_patch(i_sub),1).Y,cmapStocklog(6,:));
                    elseif variable_temp>colorlims(3) && variable_temp<=colorlims(2)
                        m_patch(countryshapefile(index_patch(i_sub),1).X,countryshapefile(index_patch(i_sub),1).Y,cmapStocklog(4,:));
                    elseif variable_temp>colorlims(4) && variable_temp<=colorlims(3)
                        m_patch(countryshapefile(index_patch(i_sub),1).X,countryshapefile(index_patch(i_sub),1).Y,cmapStocklog(2,:));
                    elseif variable_temp<=colorlims(4) % if zero
                        m_patch(countryshapefile(index_patch(i_sub),1).X,countryshapefile(index_patch(i_sub),1).Y,[.6 .6 .6]);
                    end
                    
                    
                end
            end
        end
    end
    
    
    % Grey for South Sudan
    index_patch = find(patch_codes==728);
    m_patch(countryshapefile(index_patch(i_sub),1).X,countryshapefile(index_patch(i_sub),1).Y,[.6 .6 .6]);
    
    if plot_log == 0
        colormap(cmapStock);
    else
        colormap(cmapStocklog);
    end
    
    m_plot(country.x,country.y,'color',[.2 .2 .2]);
    %m_line(lon_data_cut,lat_data_cut,'line','none','marker','square','markersize',3,'color','k','MarkerFaceColor','r');
    m_grid('linestyle', 'none','fontsize',16,'fontname','helvetica','FontWeight','bold',...
        'xticklabels',[],'yticklabels',[]);
    if flag_mean == 1
        title(['Exports (million metric tons), ' commod_title ' ' num2str(year_start) '-' num2str(year_end)],'fontsize',24);
    else
        title(['Exports (million metric tons), ' commod_title ' ' num2str(year_end)],'fontsize',24);
    end
    
    
    if plot_log == 0
        colorbar; colorbar1 = colorbar(...%'FontName','Times',...
            'FontSize',18,...
            'FontWeight','bold',...
            'LOCATION','EastOutside','fontname','helvetica');
        for i = 1:length(colorlims)
            j = length(colorlims)-i+1;
            cbarlabel{j} = num2str(colorlims(i));
        end
        cbarlabel{length(colorlims)+1}=['>' num2str(colorlims(1))];
        set(colorbar1,'YTick',[0:1/length(colorlims):1],'YTicklabel',cbarlabel)
        %         caxis([min(L) max(L)]);
        %         colorbar1 = colorbar('FontName','Times',...
        %             'FontSize',14,...
        %             'FontWeight','bold',...
        %             'XLim',[-0.5 1.5],'LOCATION','EastOutside','Ytick',L(1:length(L)),'YTicklabel',L(1:length(L)),'fontname','helvetica');
    else
         colorbar; colorbar1 = colorbar(...%'FontName','Times',...
            'FontSize',18,...
            'FontWeight','bold',...
            'LOCATION','EastOutside','fontname','helvetica');
        for i = 1:length(colorlims)
            j = length(colorlims)-i+1;
            cbarlabel{j} = num2str(colorlims(i));
        end
        cbarlabel{length(colorlims)+1}=['>' num2str(colorlims(1))];
        set(colorbar1,'YTick',[0:1/length(colorlims):1],'YTicklabel',cbarlabel)       
 
        %         myscale = [min(L) max(L)];
        %         h=colorbar('EastOutside',...
        %             'FontSize',14,...
        %             'FontWeight','bold');
        %         caxis(log10(myscale))
        %         ticks_wanted=L;
        %         set(h,'YTick',log10(ticks_wanted));
        %         set(h,'YTickLabel',ticks_wanted);
    end

    hold off
    
    % Whole figure commands
    set(gcf,'Renderer','painters')
    set(gcf,'OuterPosition',[200   999   800   600])
    set(gcf,'PaperPositionMode','auto')
    if flag_mean == 1
        print('-depsc','-painters','-r600',[figure_dir 'Exports_' num2str(year_start) '_' num2str(year_end) '_' commod_print src '.eps' ])
        
    else
        print('-depsc','-painters','-r600',[figure_dir 'Exports_' num2str(year_end) '_' commod_print src '.eps' ])
    end
    
    
    
end

%% LINE PLOTS
figure
clear Countrylegend_WB
% Loop over country_info content
i_sub =1;
for i_country = 1:length(subsetcountry_codes)
    CountrySubsetList_WB{i_country,1}
    index_country = find(FAOSTATcode==subsetcountry_codes(i_country,1));
    if ~isempty(index_country)
        variable_a = DomesticUtilization_subset(i_country,i_fullperiod+1);
        variable_b = ClosingStocks_subset(i_country,i_fullperiod+1);
        variable_c = GDPcapita_subset(i_country,i_fullperiod+1);
        if sum(isnan(variable_a))+sum(isnan(variable_b))+sum(isnan(variable_c)) ==0
            plot(years,variable_b./variable_a,'LineStyle','-','LineWidth',2,'Marker',MS{i_sub},'MarkerSize',10,...
                'MarkerFaceColor',CS(i_sub,:),'Color',CS(i_sub,:))
            set(gca,'FontSize',20)
            hold on;
            Countrylegend_WB{i_sub,1}=CountrySubsetList_WB{i_country,1};
            i_sub = i_sub +1;
        end
    end
end
xlim([min(years) max(years)])
ylim([0 1.2])
plot(xlim, [1 1]*0.2, '--k')
title(['Stocks-to-Use Ratio for ' commod_print],'fontsize',24);
xlabel('Year');
ylabel('Stocks-to-Use Ratio (-)');
legend(Countrylegend_WB{:,1}, 'location','best')
set(gca,'DefaultTextFontSize',20)
print('-depsc','-painters','-r600',[figure_dir 'StocksUseTime' commod_print src '_' num2str(year_start) '_' num2str(year_end)  '.eps' ])


figure
clear Countrylegend_WB
% Loop over country_info content
i_sub =1;
for i_country = 1:length(subsetcountry_codes)
    
    index_country = find(FAOSTATcode==subsetcountry_codes(i_country,1));
    if ~isempty(index_country)
        variable_a = nansum([DomesticUtilization_subset(i_country,i_fullperiod+1); Export_subset(i_country,i_fullperiod+1)]);
        variable_b = ClosingStocks_subset(i_country,i_fullperiod+1);
        variable_c = GDP_subset(i_country,i_fullperiod+1);
        f_inline = inline('sqrt(y2)','y2');
        if sum(isnan(variable_a))+sum(isnan(variable_b))+sum(isnan(variable_c)) ==0
            plot(years,variable_b./variable_a,'LineStyle','-','LineWidth',2,'Marker',MS{i_sub},'MarkerSize',10,...
                'MarkerFaceColor',CS(i_sub,:),'Color',CS(i_sub,:))
            set(gca,'FontSize',20)
            hold on;
            Countrylegend_WB{i_sub,1}=CountrySubsetList_WB{i_country,1};
            i_sub = i_sub +1;
        end
    end
end
title(['Stocks-to-Disappearance Ratio for ' commod_print],'fontsize',24);
xlim([min(years) max(years)])
ylim([0 1])
plot(xlim, [1 1]*0.2, '--k')
xlabel('Year','FontSize',20);
ylabel('Stocks-to-Disappearance Ratio (year)','FontSize',20);
legend(Countrylegend_WB{:,1}, 'location','NortheastOutside')

print('-depsc','-painters','-r600',[figure_dir 'StocksDisappearTime' commod_print src '_' num2str(year_start) '_' num2str(year_end)  '.eps' ])


return



figure
% Loop over country_info content
i_sub =1;
for i_country = 1:length(subsetcountry_codes)
    
    index_country = find(FAOSTATcode==subsetcountry_codes(i_country,1));
    if ~isempty(index_country)
        variable_a = DomesticUtilization_subset(i_country,i_fullperiod+1);
        variable_b = ClosingStocks_subset(i_country,i_fullperiod+1);
        variable_c = GDP_subset(i_country,i_fullperiod+1);
        semilogx(variable_c,variable_b./variable_a,'LineStyle','-','LineWidth',2,'Marker',MS{i_sub},'MarkerSize',10,...
            'MarkerFaceColor',CS(i_sub,:),'Color',CS(i_sub,:))
        labelpoints(nanmax(variable_c), nanmax((variable_b./variable_a)), CountrySubsetList_WB{i_country,1}, 'NE');
        i_sub = i_sub +1;
        hold on;
        %missingA = sum(isnan(Export_all(i_country,i_fullperiod+1)));
        %quiver(variable_a,variable_b,DomesticUtilization_all(i_country,i_fullperiod+1),ClosingStocks_all(i_country,i_fullperiod+1));
        
    end
end
plot(xlim, [1 1]*0.2, '-r')
xlabel('GDP (Constant 2005 US$ per person)','FontSize',18);
ylabel('Stocks-to-Use Ratio (-)','FontSize',18);

print('-depsc','-painters','-r600',[figure_dir 'StocksUseGDP' commod_print src '_' num2str(year_start) '_' num2str(year_end)  '.eps' ])

    
figure
% Loop over country_info content
i_sub =1;
for i_country = 1:length(country_tradecodes)
    
    index_country = find(FAOSTATcode==country_tradecodes(i_country,1));
    if ~isempty(index_country)
        variable_a = DomesticUtilization_all(i_country,i_fullperiod+1);
        variable_b = ClosingStocks_all(i_country,i_fullperiod+1);
        variable_c = GDP_all(i_country,i_fullperiod+1);
        semilogx(variable_c,variable_b./variable_a,'LineStyle','none','LineWidth',2,'Marker','*','MarkerSize',10,...
            'MarkerFaceColor','b','Color','b')
        labelpoints(nanmax(variable_c), nanmax((variable_b./variable_a)), country_WBtradecodes{i_country,1}, 'NE');
        i_sub = i_sub +1;
        hold on;
        %missingA = sum(isnan(Export_all(i_country,i_fullperiod+1)));
        %quiver(variable_a,variable_b,DomesticUtilization_all(i_country,i_fullperiod+1),ClosingStocks_all(i_country,i_fullperiod+1));
        
    end
end
plot(xlim, [1 1]*0.2, '-r')
xlabel('GDP (Constant 2005 US$ per person)','FontSize',18);
ylabel('Stocks-to-Use Ratio (-)','FontSize',18);

print('-depsc','-painters','-r600',[figure_dir 'StocksUseGDPall' commod_print src '_' num2str(year_start) '_' num2str(year_end)  '.eps' ])


figure
% Loop over country_info content
i_sub =1;
for i_country = 1:length(country_tradecodes)
    
    index_country = find(FAOSTATcode==country_tradecodes(i_country,1));
    if ~isempty(index_country)
        variable_a = nansum([DomesticUtilization_all(i_country,i_fullperiod+1); Export_all(i_country,i_fullperiod+1)]);
        variable_b = ClosingStocks_all(i_country,i_fullperiod+1);
        variable_c = GDP_all(i_country,i_fullperiod+1);
        semilogx(variable_c,variable_b./variable_a,'LineStyle','-','LineWidth',1,'Marker','*','MarkerSize',10,...
            'MarkerFaceColor','b','Color','b')
        labelpoints(nanmax(variable_c), nanmax((variable_b./variable_a)), country_WBtradecodes{i_country,1}, 'NE');
        i_sub = i_sub +1;
        hold on;
        %missingA = sum(isnan(Export_all(i_country,i_fullperiod+1)));
        %quiver(variable_a,variable_b,DomesticUtilization_all(i_country,i_fullperiod+1),ClosingStocks_all(i_country,i_fullperiod+1));
        
    end
end
plot(xlim, [1 1]*0.2, '-r')
xlabel('GDP (Constant 2005 US$ per person)','FontSize',18);
ylabel('Stocks-to-Disappearance Ratio (-)','FontSize',18);

print('-depsc','-painters','-r600',[figure_dir 'StocksDisappearGDPall' commod_print src '_' num2str(year_start) '_' num2str(year_end)  '.eps' ])


figure
% Loop over country_info content
i_sub =1;
for i_country = 1:length(subsetcountry_codes)
    
    index_country = find(FAOSTATcode==subsetcountry_codes(i_country,1));
    if ~isempty(index_country)
        variable_a = nansum([DomesticUtilization_subset(i_country,i_fullperiod+1); Export_subset(i_country,i_fullperiod+1)]);
        variable_b = ClosingStocks_subset(i_country,i_fullperiod+1);
        variable_c = GDP_subset(i_country,i_fullperiod+1);
        semilogx(variable_c,variable_b./variable_a,'LineStyle','-','LineWidth',2,'Marker',MS{i_sub},'MarkerSize',10,...
            'MarkerFaceColor',CS(i_sub,:),'Color',CS(i_sub,:))
        labelpoints(nanmax(variable_c), nanmax((variable_b./variable_a)), CountrySubsetList_WB{i_country,1}, 'NE');
        i_sub = i_sub +1;
        hold on;
        %missingA = sum(isnan(Export_all(i_country,i_fullperiod+1)));
        %quiver(variable_a,variable_b,DomesticUtilization_all(i_country,i_fullperiod+1),ClosingStocks_all(i_country,i_fullperiod+1));
        
    end
end
plot(xlim, [1 1]*0.2, '-r')
xlabel('GDP (Constant 2005 US$ per person)','FontSize',18);
ylabel('Stocks-to-Disappearance Ratio (-)','FontSize',18);

print('-depsc','-painters','-r600',[figure_dir 'StocksDisappearGDP' commod_print src '_' num2str(year_start) '_' num2str(year_end)  '.eps' ])


return


%% NOTES FROM AMIS
% NMY = National Marketing Year
% ITY = International Trade Year
%--------------------------------------------------------
% [1] "Production" (except for soybeans) refers to the first year of the
% split year shown. Rice production is expressed in milled terms. 
% [2] "Domestic Supply" equals production plus opening stocks. 
% [3] "Trade" refers to the following export seasons: - July/June for wheat and maize -
% October/September for soybean - January/December (calendar) for rice
% (corresponding to second year of the split year shown).
% 
% For the forecast year, world exports and imports are always assumed to be
% equal. 
% [4] "Closing Stocks" may not equal the difference between supply
% and utilization due to differences in individual national marketing
% years.
%--------------------------------------------------------
% Estimates of world imports and exports may not always match, mainly
% because shipments and deliveries do not necessarily occur in the same
% marketing year.

% Estimates for China also include those for the Taiwan Province, Hong Kong
% SAR and Macao SAR, unless otherwise stated. FAOSTAT: A new dissemination
% policy regarding data for China was adopted in 2013. Data is disseminated
% separately for China, mainland (code 41), China, Hong Kong SAR (code 96),
% China, Macao SAR (code 128) and China, Taiwan Province of (code 214), and
% is aggregated under China (code 351). Previously in the Trade domain
% China (code 357) referred to the aggregate of China, mainland (code 41)
% and China, Taiwan Province of (code 214) while China, Hong Kong SAR (code
% 96) and China, Macao SAR (code 128) were disseminated separately. 

%  Up to 2006 or 2006/07, the European Union includes 25 member states. From 2007
% or 2007/08 onwards, the European Union includes 27 member states.
%--------------------------------------------------------
% Total Supply= Domestic Availability + Imports
% Domestic Availability = Opening (or carry-in) Stocks + Production
% Opening (or carry-in) Stocks Quantity of stocks at the beginning of the
% marketing year (carried-in from the previous marketing year). Opening
% stocks consist of supplies held at all levels within the food system;
% both by governments and the private sector (incl. farm holdings). Opening
% stocks of a given marketing year are always identical to the closing
% stocks of the preceding year.
% 
% Production Production refers to the full amount of the harvest before any
% deductions are made for post-harvest losses, seed use, etc. For cereals,
% production data refer to the calendar year in which the entire harvest or
% the bulk of it took place. For soybeans, production refers to a split
% year that brings together crops harvested in the latter part of the first
% year shown (normally applying to the northern hemisphere) and crops
% harvested in the early part of the second year shown (normally applying
% to the southern hemisphere). Production of wheat, maize and soybeans is
% in terms of product weight. For rice, paddy is converted to milled
% equivalent.
% 
% Imports Imports include commercial purchases from abroad as well as food
% aid deliveries. They are estimated/reported on the basis of: National
% Marketing Year (or National Marketing Season): a twelve month period
% starting with the harvest of the main crop of the season. Global Trade
% Year (or Global Trade Season): Wheat and maize imports are calculated
% using a July/June trade year (conventionally referred to as the marketing
% season). Rice imports are calculated using January/December over the
% second year shown. Soybeans imports are calculated using
% October/September. Wheat import data include wheat flour converted into
% wheat equivalent. The National Marketing Year import data may also
% include other processed products which are directly consumed as a ?basic
% food?, such as semolina and bulgar. Nevertheless, these products are
% excluded from the Global Trade Year imports. Rice imports are in milled
% terms.
% 
% Total Utilization= Domestic Utilization + Exports + Closing Stocks
% 
% Domestic Utilization Includes food use, feed use and other uses. Food
% use: refers to direct human consumption. Feed use: refers to the
% quantities fed to livestock. Other uses: include seed, industrial use and
% post-harvest losses . Seed: quantity used for the planting of the
% following production cycle. Industrial use: refers to products intended
% neither for direct human consumption nor for feed. It includes the
% manufacture of secondary food products such as starch, sweeteners and
% alcohol. Post-Harvest Losses: include losses incurred after harvest, from
% sorting, waste, storage, transport, packing, etc. For soybeans, the
% breakdown differs from that used for cereals; to reflect this, the
% relevant balance sheets are currently under modification.
% 
% Exports Exports refer to all shipments outside the country, including
% food aid. They are estimated/reported on the basis of: National Marketing
% Year (or National Marketing Season): a twelve month period starting with
% the harvest of the main crop of the season. Global Trade Year (or Global
% Trade Season): Wheat and maize exports are calculated using a July/June
% trade year (conventionally referred to as the marketing season). Rice
% exports are calculated using January/December over the second year shown.
% Soybeans exports are calculated using October/September. Wheat export
% data include wheat flour converted into wheat equivalent. The National
% Marketing Year export data may also include other processed products
% which are directly consumed as a ?basic food?, such as semolina and
% bulgar. Nevertheless, these products are excluded from the Global Trade
% Year imports. Rice exports are in milled terms.
% 
% Closing (or ending or carry-over) Stocks Quantity of stocks at the end of
% the marketing year (before the following year?s harvest) held at all
% levels within the food system, both by governments and the private sector
% (incl. farm holdings and households). Closing stocks of a given marketing
% year are always identical to the opening stocks of the following year.
% 
% Per Caput Food Use Calculated as food use divided by population and
% expressed in kg per year.
% 
% Auxiliary Data
% 
% Planted Area (000 ha): refers to the area seeded over the crop year.
% Harvested Area (000 ha): refers to the area harvested. Yields (Kg/ha):
% refers to the whole grain yield obtained at harvest time net of losses
% incurred during harvesting and threshing. Usually calculated per unit of
% area harvested. Population (million): population within the geographical
% boundaries of a country at the mid-point of the reference period. Refers
% to the number of individuals actually partaking of the food supplies
% during the reference period, including refugees.
