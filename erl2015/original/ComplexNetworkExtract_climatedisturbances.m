% Global_Trade_network_attack.m
% Code to 1) Create network files for plotting with Cytoscape
%         2) Compute network statistics with and without network attack
%         
% INPUT: Reads in formatted annual bilateral trade data and data from FAO Food Balance
% Sheets.  These files format the data:
%     1) FAO_bilateral_trade_loadCE_vfaostat3.m for post-2013 FAOSTAT website.
%     2) FAO_bilateral_trade_loadCE_v10162013.m for pre-2014 FAOSTAT website.

% M.J. Puma, Spring 2013 to Winter 2014
clear all; close all; clc;
rootdir = '/Users/michaeljpuma/';
%rootdir = '/Users/mpuma/';
rootdropbox =[rootdir 'Dropbox/FragileFoodTrade/'];

%% Extract Year for network plot
year_extract = 2009;

%% Plot flags
flag_removesubset = 1; % USE THIS FLAG TO REMOVE SUBSET COUNTRIES
flag_map = 0; % SSR plots
flag_bar = 1; % Bar plots of lost consumption
flag_maplosses = 0; % Maps of lost consumption
flag_subset_plot = 1;% Flag to plot barplots of subset countries separated into categories; currently only for LDCs

%% Minimum dietary energy requirements FAO (2008) 
ER_low = 1690;
ER_high = 2000;

%% Conversion factors
factor_Roots_CE = 0.26; % Cereal equivalent from starchy roots 
mmt_convert = 1000000;  % tons to million metric tons
grams_per_metricton = 1000000;
%% FAOSTAT codes for different elements and commodities
code_wheat = 2511;
code_maize = 2514;
code_rice = 2804; % Rice (Paddy equivalent)
%%code_rice_milled = 2805; % Rice (Milled Equivalent):27, 28, 29, 31, 32, 34, 38 and nutrient data only 33 and 35
code_totalfood = 2901;

code_FoodSupplykcal=664;
code_Production = 5510;
code_Import = 5610;
code_Export = 5910;
code_StockVariation = 5071;
code_DomesticSupply = 5300;
code_Seed=5525;
code_Waste=5120;
code_Processing=5130;
code_Food=5141;
code_OtherUtil=5153;

%% Resolution Flag
flag_high = 1;

%% Colormap Selection
colormap_number =30; cmap=colormap_function(colormap_number);
%colormap_number =26; cmap=colormap_function(colormap_number);
%colormap_number =29; cmapdiff=colormap_function(colormap_number);
colormap_number =35; cmapdiff=colormap_function(colormap_number);
%colormap_number =33; cmaploss=colormap_function(colormap_number);
colormap_number =34; cmaploss=colormap_function(colormap_number);
colormap_number =13; cmapSSR=colormap_function(colormap_number);
%colormap_number =38; cmapbar1=flipud(colormap_function(colormap_number));
%colormap_number =41; cmapbar2=flipud(colormap_function(colormap_number));


%% DATASET:  Read County-Level Boundries
%filename='/Users/michaeljpuma/10m-admin-0-countries/10m_admin_0_countries.shp';
filename=[rootdir 'MapFeatures/10m_cultural/10m_cultural/ne_10m_admin_0_countries.shp'];

countryv2 = shaperead(filename);
for itemp = 1:length(countryv2)
    %patch_codes(itemp,1) = countryv2(itemp,1).ISO_N3;
    patch_codes(itemp,1) = str2num(countryv2(itemp,1).iso_n3);
end

%% Folders
folder_feature = [rootdir 'MapFeatures/'];
basefold = [rootdir 'ComplexNetworkFood/'];
outfold = [rootdropbox 'Matrices/Network_Cytoscape/'];
figure_dir= [rootdir 'ComplexNetworkFood/Figures/Fragile/'];

%% Projection information
map_proj_name='Equidistant Cylindrical';  % Flat proj, usually use
%map_proj_name='Lambert Conformal Conic';  % Good for midlatitudes, about size of US
%map_proj_name='Mollweide';                % shrinks poles, good for global
%map_proj_name='Hammer-Aitoff'; latlim=[-90 90];               % shrinks poles, good for global


%% Years to plot
year_start = 1992; year_end = 2009;
years = year_start:year_end;
num_years = year_end-year_start+1;
offset_yr = year_start-1986; % because data starts in 1986
i_periodA=find(years>=1992 & years<=1996);
i_periodB=find(years>=2005 & years<=2009);
periodlabel{1} = '1992-1996';
periodlabel{2} = '2005-2009';

%% Drought reduction factor for production
reduction_factor = 1;

%% Scenarios combining commodity and disturbance
%Scenario 1
disturbance_type = 2; disturb_title = 'Europe Disturbance'; disturb_title2 = 'NoSummer';
commod_title='wheat'; commod_titleb='Wheat'; commod_print = 'Wheat'; 
scale_losses = .15; scale_production = 30; scale_consumption = 30; scale_SSR = .1;
lonmin = -25; lonmax = 80; latmin = -38; latmax = 62;
color1 = [0.7490,0.5059,0.1765]; cmapbar1 = [color1*.6; color1]; cmapbar2 = [color1*.6; color1; 0.8 0.8 0.8]; cmapbar2(cmapbar2>1)=1;
kcalpergram = 3.64*0.75; % pp. 19 and 21 assuming 67% extraction rate from FAO (2001) "Food balance sheets: a handbook"

% % % Scenario 2
% disturbance_type = 3; disturb_title = 'Asia Disturbance'; disturb_title2 = 'GreatDrought';
% commod_title='rice'; commod_titleb='RicePaddy'; commod_print = 'Rice'; 
% scale_losses = .05; scale_production = 30; scale_consumption = 30; scale_SSR = .1;
% lonmin = -20; lonmax = 150; latmin = -20; latmax = 60;%lonmin = -180; lonmax = 180; latmin = -60; latmax = 90;
% color1 = [0.2078,0.5922,0.5608]; cmapbar1 = [color1*.6; color1];cmapbar1 = [color1*.6; color1; 0.8 0.8 0.8]; cmapbar1(cmapbar1>1)=1;
% kcalpergram = 3.60*0.67; % pp. 20 and 22  assuming 67% extraction rate from FAO (2001) "Food balance sheets: a handbook"

% %Scenario 3
% disturbance_type = 4; disturb_title = 'US Disturbance'; disturb_title2 = 'USDrought';
% commod_title='maize'; commod_titleb='Maize'; commod_print = 'Maize';
% scale_losses = .1; scale_production = 30; scale_consumption = 30;

% %Scenario 4
% disturbance_type = 4; disturb_title = 'US Disturbance'; disturb_title2 = 'USDrought';
% commod_title='wheat'; commod_titleb='Wheat'; commod_print = 'Wheat';
% scale_losses = .05; scale_production = 30; scale_consumption = 30;
% lonmin = -25; lonmax = 80; latmin = -38; latmax = 62;

% %Scenario 5
% disturbance_type = 5; disturb_title = 'Ukraine Disturbance'; disturb_title2 = 'Ukraine';
% commod_title='wheat'; commod_titleb='Wheat'; commod_print = 'Wheat'; 
% scale_losses = .15; scale_production = 30; scale_consumption = 30; scale_SSR = .1;
% lonmin = -25; lonmax = 80; latmin = -38; latmax = 62;
% color1 = [0.7490,0.5059,0.1765]; cmapbar1 = [color1*.6; color1]; cmapbar2 = [color1*.6; color1; 0.8 0.8 0.8]; cmapbar2(cmapbar2>1)=1;
% kcalpergram = 3.64*0.75; % pp. 19 and 21 assuming 67% extraction rate from FAO (2001) "Food balance sheets: a handbook"



%% Open commodity list
%commod_title='wheatonly'; commod_titleb='Wheat'; commod_print = 'Wheat';
%commod_title='maizeonly'; commod_titleb='Maize'; commod_print = 'Maize';
%commod_title='riceonly'; commod_titleb='Rice'; commod_print = 'Paddy Rice'; % commod_print1 = 'Rice (Paddy equivalent)';

%%  TARGETED ATTACK type
%disturbance_type = 1; disturb_title = 'Wheat Rust'; disturb_title2 = 'Wheat Rust';
%disturbance_type = 2; disturb_title = 'No Summer in Europe'; disturb_title2 = 'No Summer';
%disturbance_type = 3; disturb_title = 'Great Victorian Drought'; disturb_title2 = 'GV Drought';
%disturbance_type = 4; disturb_title = '2012 US Drought'; disturb_title2 = 'US Drought';

%%  DATASET: Open country list for various UN categories
%country_title= 'Annex I Countries'; country_print = 'AnnexI'; [country_codes,country_names,country_info] = xlsread([basefold 'Input_data/countrylist_annex1.xls']); 
%country_title= 'Least Developed Countries'; country_print = 'LDC'; [country_codes,country_names,country_info] = xlsread([basefold 'Input_data/countrylist_leastDCnoDjibouti.xls']);
%country_title= 'Low-Income Food-Deficit Countries'; country_print = 'LIFDC'; [country_codes,country_names,country_info] = xlsread([basefold 'Input_data/countrylist_lowincomefooddeficit.xls']); 
%country_title= 'Non-Annex I Countries'; country_print = 'nonAnnexI';[country_codes,country_names,country_info] = xlsread([basefold 'Input_data/countrylist_nonannex1.xls']);
%country_title= 'G8 Countries'; country_print = 'G8'; [country_codes,country_names,country_info] = xlsread([basefold 'Input_data/countrylist_G8.xls']); 
%country_title= 'G8 plus China'; country_print = 'G8China'; [country_codes,country_names,country_info] = xlsread([basefold 'Input_data/countrylist_G8China.xls']); 
%country_title= 'G20 Countries'; country_print = 'G20'; [country_codes,country_names,country_info] = xlsread([basefold 'Input_data/countrylist_G20.xls']); 
%country_title= 'G8+5 Countries'; country_print = 'G8Plus5'; [country_codes,country_names,country_info] = xlsread([basefold 'Input_data/countrylist_G8plus5.xls']); 
%country_title= 'G8 Countries'; country_print = 'G8'; [country_codes,country_names,country_info] = xlsread([basefold 'Input_data/countrylist_G8noUSSR.xls']); 
%country_title= 'Plus 5 Countries'; country_print = 'Plus5'; [country_codes,country_names,country_info] = xlsread([basefold 'Input_data/countrylist_plus5.xls']); 


% CONTINENTS
% country_title= 'Africa'; country_print = 'Africa'; [country_codes,country_names,country_info] = xlsread([basefold 'Input_data/countrylist_Africa.xls']); lonmin = -25; lonmax = 60; latmin = -42; latmax = 42;
% country_title= 'Asia'; country_print = 'Asia'; [country_codes,country_names,country_info] = xlsread([basefold 'Input_data/countrylist_Asia.xls']); lonmin = 45; lonmax = 180; latmin = 0; latmax = 80;
% country_title= 'Europe'; country_print = 'Europe'; [country_codes,country_names,country_info] = xlsread([basefold 'Input_data/countrylist_Europe.xls']); lonmin = -30; lonmax = 50; latmin = 30; latmax = 75;
% country_title= 'North America'; country_print = 'NAmerica'; [country_codes,country_names,country_info] = xlsread([basefold 'Input_data/countrylist_NorthAmerica.xls']); lonmin = -170; lonmax = -45; latmin = 0; latmax = 90;
% country_title= 'Central America'; country_print = 'CAmerica'; [country_codes,country_names,country_info] = xlsread([basefold 'Input_data/countrylist_CentralAmericaCarib.xls']); lonmin = -120; lonmax = -45; latmin = 0; latmax = 30;
% country_title= 'South America'; country_print = 'SAmerica'; [country_codes,country_names,country_info] = xlsread([basefold 'Input_data/countrylist_SouthAmerica.xls']); lonmin = -90; lonmax = -30; latmin = -60; latmax = 40;
country_title= 'Global'; country_print = 'World'; [country_codes,country_names,country_info] = xlsread([basefold 'Input_data/countrylist_World.xls']); 

% COUNTRIES
% country_title= 'Democratic Republic of the Congo'; country_print = 'DemCongo'; [country_codes,country_names,country_info] = xlsread([basefold 'Input_data/countrylist_DemocraticRepublicCongo.xls']); lonmin = -25; lonmax = 60; latmin = -42; latmax = 42;
% country_title= 'Zimbabwe'; [country_codes,country_names,country_info] = xlsread([basefold 'Input_data/countrylist_Zimbabwe.xls']); lonmin = -25; lonmax = 60; latmin = -42; latmax = 42;
% country_title= 'Ethiopia'; [country_codes,country_names,country_info] = xlsread([basefold 'Input_data/countrylist_Ethiopia.xls']); lonmin = -25; lonmax = 60; latmin = -42; latmax = 42;
% country_title= 'USA';  country_print = 'USA'; [country_codes,country_names,country_info] = xlsread([basefold 'Input_data/countrylist_USA.xls']);
% country_title= 'South Korea'; country_print = 'ROK'; [country_codes,country_names,country_info] = xlsread([basefold 'Input_data/countrylist_KoreaSouth.xls']);
% country_title= 'Japan'; [country_codes,country_names,country_info] = xlsread([basefold 'Input_data/countrylist_Japan.xls']);
% country_title= 'China'; [country_codes,country_names,country_info] = xlsread([basefold 'Input_data/countrylist_China.xls']);
% country_title= 'Panama'; country_print = 'Panama'; [country_codes,country_names,country_info] = xlsread([basefold 'Input_data/countrylist_Panama.xls']); lonmin = -120; lonmax = -45; latmin = 0; latmax = 30;

%% Place plotting limits into vector
lonlim=[lonmin lonmax];
latlim=[latmin latmax];

SSRlonmin = -180; SSRlonmax = 180; SSRlatmin = -60; SSRlatmax = 90;
SSRlonlim=[SSRlonmin SSRlonmax];
SSRlatlim=[SSRlatmin SSRlatmax];

%% Colors
color_ocean = rgb('White');
color_land = rgb('Cornsilk');
color_basin = rgb('DarkGreen');

%% Physical and cultural features for plots
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

new_lon(:,1)=[0.5:.25:360]; new_lon=new_lon-180;
new_lat(:,1)=-89.5:.25:89.5;
lon_grid=repmat(new_lon',[length(new_lat) 1]);
lat_grid=repmat(new_lat,[1 length(new_lon)]);

clear new_lon new_lat;

%% Simulation/analysis flags
save_figure = 1;

%% Flow Type (i.e. virtual water, tons of commoditites, etc)
%flow_type = 'tons'; flow_title = 'Weight'; flow_units = 'metric tons';
flow_type = 'tonscropeq'; flow_title = 'Weight'; flow_units = 'metric tons';


%% DATASET: Conversion table for country codes
[~, ~, raw] = xlsread([basefold 'Input_data/country_code_CONVERTv2.xls'],'Sheet1');
raw = raw(2:end,2:end);
data = reshape([raw{:}],size(raw)); %% Create output variable

% Allocate imported array to column variable names
Index = data(:,1);
FAOSTATcode = data(:,2);
ISOcode = data(:,4);

% Clear temporary variables
clearvars data raw;

%% Trade element to plot
i_element = 4; 
element_vec = {...
    'Import_Value'
    'Export_Value'
    'Import_Quantity'
    'Export_Quantity'};
element = element_vec{i_element};


%% DATASET: Open country list
[country_tradecodes,country_tradenames,country_tradeinfo] = xlsread([basefold 'Input_data/countrylist_partialv2.xls']);
index_tradecountry = 1:length(country_tradenames);

%% DATASET: Open list of ISO3 codes (3 letter codes)
[~,~,country_ISOinfo] = xlsread([basefold 'Input_data/countrylist_partialv2_ISO3.xls']);
country_ISOinfo(:,1)=[];


%% DATASET: Open disturbance data (files assume use of countrylist_partialv2.xls)
[~, ~, raw] = xlsread([basefold 'Input_data/global_disruptionv2.xls'],'Sheet1');
raw = raw(2:end,2:end);

% Create output variable
data = reshape([raw{:}],size(raw));

% Allocate imported array to column variable names
WheatStemRust = data(:,1);
Summer1816 = data(:,2);
GreatVictorianDrought = data(:,3);
Drought2012 = data(:,4);
Ukraine = data(:,5);
UkraineRussia = data(:,6);
disturb_location(:,1) = data(:,disturbance_type);

% Clear temporary variables
clearvars data raw;

%% Number of disturbed countries and subset countries
number_subsetCountries = length(country_codes);
number_disturbed = sum(disturb_location>=1);


%%  DATASET: Import GDP data (1970 to 2011)
offset_GDPyr = year_start-1970;
[~, names, raw] = xlsread([basefold 'Input_data/GDPbycountryyear_USD2005.xls'],'Sheet1');
raw = raw(:,2:end);
raw(cellfun(@(x) ~isempty(x) && isnumeric(x) && isnan(x),raw)) = {''};

% Replace non-numeric cells with NaN
R = cellfun(@(x) ~isnumeric(x) && ~islogical(x),raw); % Find non-numeric cells
raw(R) = {NaN}; % Replace non-numeric cells
code_country   = cell2mat(raw(2:end,1));

% Create output variable
temp = reshape([raw{:}],size(raw));
yearsGDP = temp(1,2:end);
GDP_countrycodes = temp(2:end,1); 
GDPbycountryyearUSD2005 = temp(2:end,2:end);
GDP_all=nan(length(country_tradecodes),num_years+1);


%% Format GDP data
for i_country = 1:length(country_tradecodes)
    % Check FAOSTAT crop (wheat, maize, rice) data
    index_country = find(code_country==country_tradecodes(i_country,1))+1;
    index_GDP = find(GDP_countrycodes==country_tradecodes(i_country,1));
    
    if ~isempty(index_country)        
        % Add country codes to first column
        GDP_all(i_country,1)=country_tradecodes(i_country);
        
        i_year = 0; % Initialize
        for year = year_start:year_end
            i_year = i_year + 1; % year index
            i_commod = 1; % only one commodity
            
            % GDP data
            GDP_all(i_country,i_year+1)= GDPbycountryyearUSD2005(index_GDP,i_year+offset_GDPyr);
        end
    end
end


% Clear temporary variables
clearvars raw R temp;


%% TRADE ANALYSIS WITH TARGETED ATTACK
% Intilize disturbance matrices
%Production_lost = zeros(size(Production_all));
%Export_lost = zeros(size(Export_all));
%Import_lost = zeros(size(Import_all));

% Initialize correlation matrix
Graph = GraphLoadFromFile([basefold 'Matrices/Trade_Graph_' commod_title '_' flow_type '_' element '_' num2str(year_start) '_' num2str(year_start) '.mat']);
rank_matrix =zeros(length(Graph.Data), size(year_start:year_end,2));
clear Graph;

%Subset country MATRICES and their exports, imports
export_trade = nan(number_subsetCountries,length(FAOSTATcode),size(year_start:year_end,2));
import_trade = nan(number_subsetCountries,length(FAOSTATcode),size(year_start:year_end,2));

export_trade_disturbed = nan(number_subsetCountries,length(FAOSTATcode),size(year_start:year_end,2));
import_trade_disturbed = nan(number_subsetCountries,length(FAOSTATcode),size(year_start:year_end,2));


% START TIME LOOP HERE FOR TRADE DATA
i_year = 0;
for year = year_start:year_end
    clear WDegreeOut_temp
    
    year
    i_year = i_year + 1; % year index
    
    %% Read in network from file
    Graph = GraphLoadFromFile([basefold 'Matrices/Trade_Graph_' commod_title '_' flow_type '_' element '_' num2str(year) '_' num2str(year) '.mat']);
    
    %% Identify the links
    LinkID = (1:length(Graph.Data))';
    
    %% Place data in a matrix containing all years
    rank_matrix(:,i_year)=Graph.Data(:,3);
    
    %% WD is the directed trade matrix: virtual water trade network forms a
    % weighted, directed network; WD is not symmetric and W(i, i) = 0; that is,
    % a country cannot trade with itself.
    WD = zeros(max(Graph.Index.Properties.NodeIDs));
    
    for i_link = 1: length(Graph.Data)
        WD(Graph.Data(i_link,1),Graph.Data(i_link,2))=Graph.Data(i_link,3);
    end
        
    %% Check diagonal is zero
    if sum(diag(WD)) >0
        'error in diagonal 1' %#ok<NOPTS>
        return
    end
    
    %% Remove non-existing links (i.e., links with zero weight)
    LinkID(Graph.Data(:,3)==0,:)=[];
    Graph.Data(Graph.Data(:,3)==0,:)=[];
    
    %% Remove inactive nodes
    Graph=GraphRemoveUnlinkedNodes(Graph);
    
    %% Weighted, undirected network (WU): created by symmetrizing the directed,
    % weighted network on the basis of the sum of the link weights between
    % two nodes. This symmetrization creates an undirected network
    % with at most a single link between any two nodes
    L = tril(WD,-1); U = triu(WD,1);
    WU = WD+transpose(L)+transpose(U); % Weighted, undirected network (WU)
    AD = WD>0; % Unweighted, directed network (AD)
    AU = WU>0; % Unweighted, undirected network (AU)
    
    %% Compute the number of links in the network
    NumLinks(i_year) = sum(AD(:));
    disp(sprintf('Number of Links in network: %d',GraphCountNumberOfLinks(Graph)))
    
    %% Number of nodes in graph (sum rows or columns of undirected network WU, to identify active nodes)
    NumNodes(i_year) = nnz(sum(WU,1));
    disp(sprintf('Number of Nodes in network: %d',GraphCountNumberOfNodes(Graph)));
    NumNodes_out(i_year) = nnz(sum(WD,2)); % number of exporting nodes
    NumNodes_in(i_year) = nnz(sum(WD,1));  % number of importing nodes
    
    %% Active nodes
    Active_nodes(i_year,:) = sum(WU,1)>0;
    
    %% Remove inactive nodes
    index_inactive=(~any(WD,2)+~any(WD,1)')==2;%
    index_active(:,1) = (1:length(WD))'; index_active(index_inactive,:)=[];
    WD_active = WD; WU_active = WU;
    AD_active = AD; AU_active = AU;
    
    WD_active(index_inactive,:)=[]; WD_active(:,index_inactive)=[];
    WU_active(index_inactive,:)=[]; WU_active(:,index_inactive)=[];    
    AD_active(index_inactive,:)=[]; AD_active(:,index_inactive)=[];
    AU_active(index_inactive,:)=[]; AU_active(:,index_inactive)=[];
    
    country_ISOinfo_alpha = country_ISOinfo(:,1);
    country_ISOinfo_alpha_active = country_ISOinfo_alpha;
    country_ISOinfo_alpha_active(find(index_inactive==1)) = [];
    
    %% Amount traded globally
    TotTradeGlobal(i_year) = sum(WD(:));
    
    %% Matrix tests
%     S = issimple(WD);
%     S = isdirected(WD);
%     S = issymmetric(WD);
%     S = isregular(WD);
    
    %% Strength, si, is calculated as the sum of all the exporting fluxes from
    % that node (i.e., as the row sum in the trade matrix)
    [strength,instrength,outstrength]=degrees(WD);% mit code
    strength = strength'; instrength = instrength'; outstrength= outstrength'; 
    s_D_out(:,i_year) = outstrength;
    s_D_in(:,i_year) = instrength;
    s_D(:,i_year) = strength;
    clear strength instrength outstrength;
    % BELOW IS EQUIVALENT
    % s_D_out(:,i_year) = nansum(WD,2); %exporting strength
    % s_D_in(:,i_year) = nansum(WD,1); %importing strength
    
    
    %% Compute degrees in network for exports in directed network
    % Also use k=average_degree(AD);
    [deg,indeg,outdeg]=degrees(AD);% mit code
    deg = deg'; indeg = indeg'; outdeg=outdeg';
    k_D_out(:,i_year) = outdeg;
    k_D_in(:,i_year) = indeg;
    k_D(:,i_year) = deg;
    
    % BELOW IS EQUIVALENT
    %k_D_out(:,i_year) = nansum(AD,2);  % sum each row for export degree
    %k_D_in(:,i_year) = nansum(AD,1);  % sum each row for export degree
    
    %% Betweenness Centrality
    % ComplexNetwork_BCT
    % Computation using "Brain Connectivity Toolbox" 
    % (https://sites.google.com/site/bctnet/Home/functions)
    %Unweighted, Undirected -- confirmed to work with FUGA
    BC_U_unweighted_temp=betweenness_wei(AU_active);
    N = length(AU_active);
    BC_U_unweighted_temp = BC_U_unweighted_temp / ( ((N-1)*(N-2))/2 );% Undirected
    BC_U_unweighted(index_active,i_year)=BC_U_unweighted_temp;
    
    %Unweighted, Directed -- reasonable but not verified
    BC_D_unweighted_temp=betweenness_wei(AD_active);
    BC_D_unweighted(index_active,i_year)=BC_D_unweighted_temp;
    clear BC_D_unweighted_temp BC_U_unweighted_temp
     
    %Weighted, Undirected -- not verified
    BC_U_weighted_temp=betweenness_wei(WU_active);  
    BC_U_weighted(index_active,i_year)=BC_U_weighted_temp;

    %Weighted, Undirected -- not verified
    BC_D_weighted_temp=betweenness_wei(WD_active);
    BC_D_weighted(index_active,i_year)=BC_D_weighted_temp;
    clear BC_D_weighted_temp BC_U_weighted_temp


%     % FUGA (you must type install_fuga in directory and put at top of path)
%     % Computation using Biological network analysis with FUGA 
%     % Functional Genomics Assistant
%     % http://www.code.google.com/p/fuga
%     %Unweighted
%     adj_AD = adjacency(AD_active,country_ISOinfo_alpha_active,WD_active);
%     BC_D_unweighted_temp2=betweenness(adj_AD);
%     BC_D_unweighted2(index_active,i_year)=BC_D_unweighted_temp2;
%     adj_AU = adjacency(AU_active);%,lbls,weights)
%     BC_U_unweighted_temp2=betweenness(adj_AU);
%     BC_U_unweighted2(index_active,i_year)=BC_U_unweighted_temp2;
%     clear BC_D_unweighted_temp2 BC_U_unweighted_temp2
    
%     % ComplexNetwork_MIT2011
%     %Unweighted: computed values are not correct!!
%     BC_D_unweighted_temp3=node_betweenness_slow(WD_active);
%     BC_U_unweighted_temp3=node_betweenness_slow(WU_active);
%     BC_D_unweighted3(index_active,i_year)=BC_D_unweighted_temp3;
%     BC_U_unweighted3(index_active,i_year)=BC_U_unweighted_temp3;  
%     clear BC_D_unweighted_temp3 BC_U_unweighted_temp3
    
    % ComplexNetwork_LevMuchnik...Doesn't work
    %Graphsq=mexGraphSqueeze(Graph); Graphtest = ObjectCreateGraph(Graph.Data);
    %[BetweennessCentrality, Nodes] = mexGraphBetweennessCentrality(Graph,[],'direct',1);
    %[A  B]= mexGraphAllNodeShortestPasses(Graphtest,1)
    
    %% Simulated Disturbance
    % Create temporary matrices
    WD_temp=WD;
    AD_temp=AD;
    WU_temp=WU;
    
    WDegreeOut_temp(:,1) = index_tradecountry;
    WDegreeOut_temp(:,2) = k_D_out(:,i_year);
    
    % Initialize
    NodesToRemovePerStep =1;
    NumbersOfNodes = [];
    NumbersOfLinks = [];
    Amount_lost =[];
    NumbersOfComponents = [];
    LargestClusterSizes = [];
    SecondLargestClusterSizes = [];
    index_full=WDegreeOut_temp(:,1);
    
    RemoveVector = find(disturb_location>=1);
    
    for i_remove = 1:length(RemoveVector)
        nnz(sum(WD_temp,1)); % number of nodes
        sum(AD_temp(:));     % number of links
        
        NodesToRemove = RemoveVector(i_remove);
        ['Removing ' country_tradenames{NodesToRemove}]
        
        if ~isempty(find(index_tradecountry==NodesToRemove))
            Amount_lost(end+1,1) = sum(WD_temp(find(index_tradecountry==NodesToRemove),:));
            Amount_lost(end,1); %#ok<NOPTS>
                       
            % Remove country as an exporter
            WD_temp(find(index_tradecountry==NodesToRemove),:)=0; % remove country as an exporter...
            AD_temp(find(index_tradecountry==NodesToRemove),:)=0; % remove country as an exporter...
            %WD_temp(:,find(index_tradecountry==NodesToRemove))=0; % remove country as an importer...
            %AD_temp(:,find(index_tradecountry==NodesToRemove))=0; % remove country as an importer...
            
            % Print out
            nnz(sum(WD_temp,1)); % number of nodes
            sum(AD_temp(:));     % number of links
            
            NumbersOfNodes(end+1,1) = nnz(sum(WD_temp,1));
            NumbersOfLinks(end+1,1) = sum(sum(WD_temp>0));
            WDegreeOut_temp(find(WDegreeOut_temp(:,1)==NodesToRemove),:)=[];
        end
        
        %% Sums over all countries
        %Production_lost(NodesToRemove,i_year+1)= reduction_factor * Production_all(NodesToRemove,i_year+1);
        %Import_lost(NodesToRemove,i_year+1)= reduction_factor * Import_all(NodesToRemove,i_year+1);
        %Export_lost(NodesToRemove,i_year+1)= reduction_factor * Export_all(NodesToRemove,i_year+1);

    end
    
    
    %% Strength of disturbed network
    s_D_out_disturbed(:,i_year) = nansum(WD_temp,2); %exporting strength
    s_D_in_disturbed(:,i_year) = nansum(WD_temp,1); %importing strength
    
    %% Degree of disturbed network
    k_D_out_disturbed(:,i_year) = nansum(AD_temp,2);  % sum each row for export degree
    k_D_in_disturbed(:,i_year) = nansum(AD_temp,1);  % sum each row for export degree
    
    % Imports lost by country
    s_D_in_LOST(:,i_year) = s_D_in(:,i_year) - s_D_in_disturbed(:,i_year);
    k_D_in_LOST(:,i_year) = k_D_in(:,i_year) - k_D_in_disturbed(:,i_year);
    
    %% Extract the trade for the specified countries 
    for i_country = 1:number_subsetCountries
        index_country = find(FAOSTATcode==country_codes(i_country,1));
        %index_country2 = find(index_tradecountry==countryexist_codes(i_country,1));
        
        if ~isempty(index_country)
            % Exports: find rows in WD that correspond to the given country
            export_trade(i_country,:,i_year)=WD(index_country,:);
            
            % Imports: find columns in WD that correspond to the given country
            import_trade(i_country,:,i_year)=WD(:,index_country)';
            
            % Exports: find rows in WD that correspond to the given country
            export_trade_disturbed(i_country,1:length(WD_temp),i_year)=WD_temp(index_country,:);
            
            % Imports: find columns in WD that correspond to the given country
            import_trade_disturbed(i_country,1:length(WD_temp),i_year)=WD_temp(:,index_country)';
        end
        
    end
    
    
    
    %% Volume traded globally after disturbance
    TotTradeGlobal_after(i_year) = sum(WD_temp(:));
    
%     TotProductionGlobal(i_year) = nansum(Production_all(:,i_year+1));
%     TotProductionGlobal_disturbed(i_year) = nansum(Production_all(:,i_year+1))-...
%         nansum(Production_lost(:,i_year+1));
    
%     Output(:,i_year) = [
%         TotProductionGlobal(i_year);... % Total Global Production
%         TotProductionGlobal_disturbed(i_year);... % Total Global Production with Disturbance
%         TotProductionGlobal(i_year)-TotProductionGlobal_disturbed(i_year);... %Total Vulnerable Production
%         TotTradeGlobal(i_year);... % Total Global Trade
%         sum(Amount_lost);...       % Total Lost Trade
%         sum(Amount_lost)/TotTradeGlobal(i_year);... vulnerable trade as fraction of total global trade
%         NumNodes(i_year);...
%         NumbersOfNodes(end);...
%         NumLinks(i_year);...
%         NumbersOfLinks(end);...
%         ];
    

    %% Allocate remaining trade volume to countries based on sorted GDP
    % Sort GDP
    GDP_single = GDP_all(:,i_year+1);
    GDP_single(isnan(GDP_single))=-99;
    [country_sortGDP,icountry_sortGDP]  = sortrows(GDP_single,1);
    icountry_sortGDP=flipud(icountry_sortGDP); country_sortGDP=flipud(country_sortGDP);
    
    % Allocate based on GDP   
    TradeRemain = TotTradeGlobal_after(i_year);
    flag_temp = 0;
    for i_country = 1:length(index_tradecountry)
        i_allocate = icountry_sortGDP(i_country);

        % Only consider for allocation if imports exist (i.e. s_D_in of
        % node is nonzero)
        if s_D_in(i_allocate,i_year)>0
            if (TradeRemain-s_D_in(i_allocate,i_year))>=0
                s_D_in_disturbed_GDP(i_allocate,i_year)=s_D_in(i_allocate,i_year);
                TradeRemain = TradeRemain-s_D_in(i_allocate,i_year);
            elseif flag_temp == 0
                s_D_in_disturbed_GDP(i_allocate,i_year)=TradeRemain;
                TradeRemain=0;
                flag_temp = 1;
            elseif flag_temp == 1
                s_D_in_disturbed_GDP(i_allocate,i_year)=0;
            end
        else
            s_D_in_disturbed_GDP(i_allocate,i_year)=0;
        end
    end
    
    if year == year_extract
        Network = WD;
        Network_disrupted = WD_temp;
    end
    
    % Clear variables
    clear WD_active WU_active AD_active AU_active
    clear WD_temp WU_temp AD_temp AU_temp  
    clear WD WU AD AU 
    clear index_inactive index_active 

end

% Percentage lost
s_D_in_LOSTfraction = nansum(s_D_in_LOST,1)./nansum(s_D_in,1);
k_D_in_LOSTfraction = nansum(k_D_in_LOST,1)./nansum(k_D_in,1);

%% TABLE: losse


%% Create 4 column cell arrray (with columns corresponding to Source, Destination, Weight, and Weight -- repeated for plotting)
Network3Col = cell(length(Network)^2,6);
Network3Col_disrupted = cell(length(Network_disrupted)^2,6);
Network3Col_LOST= cell(length(Network_disrupted)^2,6);

% Assign country codes to all possible links
list_all = 1:length(Network);
max_connects = length(Network); %%%-1;
counter = 1;

%% Keep only destination countries specified in "country_codes"
[C,ia,ib] = intersect(country_codes,country_tradecodes);

for i_node = 1:length(Network)
    % Indices counter
    indices = 1+max_connects*(counter-1):max_connects*counter;

    temp = list_all;
    %temp(temp==i_node)=[];
    
    % Undisturbed Network
    Network3Col(indices,1)=country_ISOinfo(i_node,1);%%%i_node;
    Network3Col(indices,2)=country_ISOinfo(:,1);%%% temp;
    Network3Col(indices,3)= num2cell(Network(i_node,:)); % export from source to destination
    Network3Col(indices,4)= num2cell(Network(i_node,:)); % (repeat) export from source to destination
    Network3Col(indices,5)= num2cell(ones(1,length(indices))*nansum(Network(i_node,:))); %total exports of source county
    Network3Col(indices,6)= num2cell(nansum(Network,1)); % Sum columns to get total imports of destination country

    
    %Disturbed Network
    Network3Col_disrupted(indices,1)=country_ISOinfo(i_node,1);%%%i_node;
    Network3Col_disrupted(indices,2)=country_ISOinfo(:,1);%%% temp;
    Network3Col_disrupted(indices,3)= num2cell(Network_disrupted(i_node,:));
    Network3Col_disrupted(indices,4)= num2cell(Network_disrupted(i_node,:));
    Network3Col_disrupted(indices,5)= num2cell(ones(1,length(indices))*nansum(Network_disrupted(i_node,:)));
    Network3Col_disrupted(indices,6)= num2cell(nansum(Network_disrupted,1)); % Sum columns to get imports
    
    %Lost Network connections
    Network3Col_LOST(indices,1)=country_ISOinfo(i_node,1);%%%i_node;
    Network3Col_LOST(indices,2)=country_ISOinfo(:,1);%%% temp;
    Network3Col_LOST(indices,3)= num2cell(Network(i_node,:)-Network_disrupted(i_node,:));
    Network3Col_LOST(indices,4)= num2cell(Network(i_node,:)-Network_disrupted(i_node,:));
    Network3Col_LOST(indices,5)= num2cell(cell2mat(Network3Col(indices,5))-cell2mat(Network3Col_disrupted(indices,5)));
    Network3Col_LOST(indices,6)= num2cell(cell2mat(Network3Col(indices,6))-cell2mat(Network3Col_disrupted(indices,6)));   
    
    if flag_removesubset == 1
        % Remove destination countries not specified in "country_codes"
        % Identify indices that should be removed
        remove_countries = indices;
        remove_countries(list_all(ib))=[];
        
        Network3Col(remove_countries,3)={0};
        Network3Col_disrupted(remove_countries,3)={0};
        Network3Col_LOST(remove_countries,3)={0};
        
        Network3Col(remove_countries,4)={0};
        Network3Col_disrupted(remove_countries,4)={0};
        Network3Col_LOST(remove_countries,4)={0};
    end
    counter= counter+1;
end
clear temp;


%% Remove links with NO trade
rm_rows =  find(cell2mat(Network3Col(:,3))==0);
Network3Col(rm_rows,:)=[]; clear rm_rows;

rm_rows =  find(cell2mat(Network3Col_disrupted(:,3))==0);
Network3Col_disrupted(rm_rows,:)=[]; clear rm_rows;

rm_rows =  find(cell2mat(Network3Col_LOST(:,3))==0);
Network3Col_LOST(rm_rows,:)=[]; clear rm_rows;

%% Save Networks to file for use by visualization software Cytoscape
yrname = num2str(year_extract);
dlmcell([outfold 'CytoscapeData_Network' '_' country_print commod_title '_tons_' element '_' disturb_title2 '_' yrname '.txt'],Network3Col);
dlmcell([outfold 'CytoscapeData_NetworkDisrupted' '_' country_print commod_title '_tons_' element '_' disturb_title2 '_' yrname '.txt'],Network3Col_disrupted);
dlmcell([outfold 'CytoscapeData_NetworkLost' '_' country_print commod_title '_tons_' element '_' disturb_title2 '_' yrname '.txt'],Network3Col_LOST);

%% PLOT NETWROK STATISTICS
figure
hold on
plot(year_start:year_end,NumLinks(:), 'LineStyle','-','LineWidth',3,'Marker','s','MarkerSize',7,...
    'MarkerFaceColor',color1,'MarkerEdgeColor',color1,'Color',color1)
set(get(gcf,'CurrentAxes'),'FontSize',24)
ylabel('Number of Links, {\itL}','FontSize',24);
%xlabel('Year','FontSize',18);
xlim([1991 2010])
%ylim([0 7])
%title(['Network Links'],'FontSize',28)% ' to ' num2str(year1_end)])
legend(commod_print,'Location','Northwest')
box on
%axis square
hold off
set(gcf,'Renderer','painters');
set(gcf,'OuterPosition',[100   500 400 400]);
set(gcf,'PaperPositionMode','auto');
if save_figure>0
    print('-depsc','-painters','-r600',[figure_dir 'numberlinks_' flow_type '_' commod_title '_'  num2str(year_start) '_' num2str(year_end) '.eps' ])
end


%% Total flows in the network 
figure
hold on
plot(year_start:year_end,TotTradeGlobal(1:end)/mmt_convert,'LineStyle','-','LineWidth',3,'Marker','s','MarkerSize',7,...
    'MarkerFaceColor',color1,'Color',color1)
set(get(gcf,'CurrentAxes'),'FontSize',24)
xlabel('Year','FontSize',24);
ylabel(['Trade flow, \Sigma_{\iti }{\its} (mmt)'],'FontSize',24);
legend(commod_print,'Location','Northwest')
xlim([1991 2010])
%ylim([0 200])
hold off
box on
%axis square
set(gcf,'Renderer','painters')
set(gcf,'OuterPosition',[100 500 400 400])
set(gcf,'PaperPositionMode','auto')
if save_figure>0
    print('-depsc','-painters','-r600',[figure_dir 'actual_' flow_type '_' commod_title '_' num2str(year_start) '_' num2str(year_end)  '.eps' ])
end




%% Exceedance probability plots (aka complementary CDF, survivor function, "flow duration curve")
% Plotting position using Weibull Formula
iyear = length(year_start:year_end); %PLOT FINAL YEAR 
A_values = k_D_out(:,iyear);
number_data = length(A_values);
rankmax_A= zeros(number_data,5);
rankmax_A(:,1) = 1:number_data;
rankmax_A(:,2) = flipud(sortrows(A_values(:)));
rankmax_A(:,3) = rankmax_A(:,1)/(number_data+1);
rankmax_A(:,4) = (number_data+1)./rankmax_A(:,1);
[muhat_A, muci_A] = expfit(A_values);
k_hat = 0:0.1:200;
Acumprob = exppdf(k_hat,muhat_A);

B_values = k_D_in(:,iyear);
number_data = length(B_values);
rankmax_B= zeros(number_data,5);
rankmax_B(:,1) = 1:number_data; % Rank
rankmax_B(:,2) = flipud(sortrows(B_values(:))); % Values but ordered from greatest to least
rankmax_B(:,3) = rankmax_B(:,1)/(number_data+1);  % Exceedance Probability
rankmax_B(:,4) = (number_data+1)./rankmax_B(:,1); % Return Period
[muhat_B, muci_B] = expfit(B_values);
Bcumprob = exppdf(k_hat,muhat_B);

periodlabel{1} = 'k_{out}';
periodlabel{2} = 'k_{in}';

%% Colors http://www.mathworks.com/matlabcentral/fileexchange/screenshots/3140/original.jpg
c_blue=rgb('DodgerBlue');
c_violet=rgb('BlueViolet');
c_black=rgb('Black');
c_red=rgb('Crimson');

%inverse CDF P(X>x)
figure
plot(rankmax_A(:,2),rankmax_A(:,3),'LineStyle','none','LineWidth',1.5,...
    'Marker','*','MarkerSize',8,...
    'MarkerFaceColor',[c_blue],'Color',[c_blue])
hold on
%plot(k_hat,Acumprob,'LineStyle','-','LineWidth',1.5,'Color','k')

plot(rankmax_B(:,2),rankmax_B(:,3),'LineStyle','none','LineWidth',1.5,...
    'Marker','*','MarkerSize',8,...
    'MarkerFaceColor', [c_violet],'Color', [c_violet])
%plot(k_hat,Bcumprob,'LineStyle','-','LineWidth',1.5,'Color','k')

set(get(gcf,'CurrentAxes'),'FontSize',18)
xlabel('k','FontSize',18);
ylabel('P(K>k)','FontSize',16);
%title(['\fontname{helvetica} \fontsize{18}' vartxt ' (' seas '), ' area_txt]);
legend(periodlabel)
box on
set(gcf,'PaperPositionMode','auto')
set(gcf,'OuterPosition',[100 500 400 400])
set(gcf,'Renderer','painters')
%print('-depsc','-painters','-r600',[figure_dir area_txt '_Pexceed_' varname 'IrrigONLY_' seas '_150yrs.eps' ])

%% Amount traded (i.e. node strength) versus number of trade partners (node degree)
figure
loglog(k_D_out(:,iyear),s_D_out(:,iyear), 'LineStyle','none','LineWidth',3,'Marker','s','MarkerSize',7,'MarkerFaceColor',color1,'Color',color1);
hold on
set(get(gcf,'CurrentAxes'),'FontSize',24)
xlabel('Number of trading partners, {\itk}_{out}','FontSize',17);
ylabel(['Trade flow, \Sigma_{\iti }{\its} (mmt)'],'FontSize',24);
%title(['Total Trade'],'FontSize',28)
legend(commod_print,'Location','Northwest')
xlim([1 250])
%ylim([0 200])
hold off
box on
%axis square
set(gcf,'Renderer','painters')
set(gcf,'OuterPosition',[100 500 400 400])
set(gcf,'PaperPositionMode','auto')
