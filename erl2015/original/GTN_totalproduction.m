% GTN_totalproduction.m
% Code to 1) compute the percentage of world production impacted by a
% targeted attack.
% Reads in annual bilateral trade data and data from FAO Food Balance
% Sheets from FAOSTAT

% M.J. Puma, Winter 2014
clear all; close all; clc;
rootdir = '/Users/mpuma/';
%rootdir = '/Users/michaeljpuma/';

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
filename= [rootdir 'MapFeatures/10m_cultural/10m_cultural/ne_10m_admin_0_countries.shp'];

countryv2 = shaperead(filename);
for itemp = 1:length(countryv2)
    %patch_codes(itemp,1) = countryv2(itemp,1).ISO_N3;
    patch_codes(itemp,1) = str2num(countryv2(itemp,1).iso_n3);
end

%% Folders
folder_feature = [rootdir 'MapFeatures/'];
basefold = [rootdir 'ComplexNetworkFood/'];
figure_dir=[rootdir 'ComplexNetworkFood/Figures/Fragile/'];

%% Projection information
map_proj_name='Equidistant Cylindrical';  % Flat proj, usually use
%map_proj_name='Lambert Conformal Conic';  % Good for midlatitudes, about size of US
%map_proj_name='Mollweide';                % shrinks poles, good for global
%map_proj_name='Hammer-Aitoff'; latlim=[-90 90];               % shrinks poles, good for global


%% Years to plot
year_start = 1986; year_end = 2009;
years = year_start:year_end;
num_years = year_end-year_start+1;
offset_yr = year_start-1986; % because data starts in 1986
i_periodA=find(years>=1992 & years<=1996);
i_periodB=find(years>=2005 & years<=2009);
periodlabel{1} = '1992-1996';
periodlabel{2} = '2005-2009';

%% Drought reduction factor for production
reduction_factor = 1;

% Scenarios combining commodity and disturbance
 %Scenario 1
disturbance_type = 2; disturb_title = 'European '; disturb_title2 = 'No Summer';
commod_title='wheat'; commod_titleb='Wheat'; commod_print = 'Wheat'; 
scale_losses = .15; scale_production = 30; scale_consumption = 30; scale_SSR = .1;
lonmin = -25; lonmax = 80; latmin = -38; latmax = 62;
color1 = [0.7490,0.5059,0.1765]; cmapbar1 = [color1*.6; color1]; cmapbar2 = [color1*.6; color1; 0.8 0.8 0.8]; cmapbar2(cmapbar2>1)=1;
kcalpergram = 3.64*0.75; % pp. 19 and 21 assuming 67% extraction rate from FAO (2001) "Food balance sheets: a handbook"
cmappie = [color1; [0.7,0.7,0.7]];

% %Scenario 2
% disturbance_type = 3; disturb_title = 'Asian '; disturb_title2 = 'GV Drought';
% commod_title='rice'; commod_titleb='RiceMilled'; commod_print = 'Rice'; 
% scale_losses = .05; scale_production = 30; scale_consumption = 30; scale_SSR = .1;
% lonmin = -20; lonmax = 150; latmin = -20; latmax = 60;%lonmin = -180; lonmax = 180; latmin = -60; latmax = 90;
% color2 = [0.2078,0.5922,0.5608]; cmapbar1 = [color2*.6; color2];cmapbar2 = [color2*.6; color2; 0.8 0.8 0.8]; cmapbar2(cmapbar2>1)=1;
% kcalpergram = 3.60*0.67; % pp. 20 and 22  assuming 67% extraction rate from FAO (2001) "Food balance sheets: a handbook"
% cmappie = [color2; [0.7,0.7,0.7]];

% %Scenario 3
% disturbance_type = 4; disturb_title = 'US Disturbance'; disturb_title2 = 'US Drought';
% commod_title='maize'; commod_titleb='Maize'; commod_print = 'Maize';
% scale_losses = .1; scale_production = 30; scale_consumption = 30;

% %Scenario 4
% disturbance_type = 4; disturb_title = 'US Disturbance'; disturb_title2 = 'US Drought';
% commod_title='wheat'; commod_titleb='Wheat'; commod_print = 'Wheat';
% scale_losses = .05; scale_production = 30; scale_consumption = 30;
% lonmin = -25; lonmax = 80; latmin = -38; latmax = 62;

%  %Scenario 5
% disturbance_type = 5; disturb_title = 'Ukraine '; disturb_title2 = 'Ukraine';
% commod_title='wheat'; commod_titleb='Wheat'; commod_print = 'Wheat'; 
% scale_losses = .15; scale_production = 30; scale_consumption = 30; scale_SSR = .1;
% lonmin = -25; lonmax = 80; latmin = -38; latmax = 62;
% color1 = [0.7490,0.5059,0.1765]; cmapbar1 = [color1*.6; color1]; cmapbar2 = [color1*.6; color1; 0.8 0.8 0.8]; cmapbar2(cmapbar2>1)=1;
% kcalpergram = 3.64*0.75; % pp. 19 and 21 assuming 67% extraction rate from FAO (2001) "Food balance sheets: a handbook"
% cmappie = [color1; [0.7,0.7,0.7]];

%  %Scenario 6
% disturbance_type = 6; disturb_title = 'UkraineRussia '; disturb_title2 = 'UkraineRussia';
% commod_title='wheat'; commod_titleb='Wheat'; commod_print = 'Wheat'; 
% scale_losses = .15; scale_production = 30; scale_consumption = 30; scale_SSR = .1;
% lonmin = -25; lonmax = 80; latmin = -38; latmax = 62;
% color1 = [0.7490,0.5059,0.1765]; cmapbar1 = [color1*.6; color1]; cmapbar2 = [color1*.6; color1; 0.8 0.8 0.8]; cmapbar2(cmapbar2>1)=1;
% kcalpergram = 3.64*0.75; % pp. 19 and 21 assuming 67% extraction rate from FAO (2001) "Food balance sheets: a handbook"
% cmappie = [color1; [0.7,0.7,0.7]];

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
% SOCIOECONOMIC GROUPS
country_title= 'G8 Countries'; country_print = 'G8'; [country_codes,country_names,country_info] = xlsread([basefold 'Input_data/countrylist_G8noUSSR.xls']); 
%country_title= 'Plus 5 Countries'; country_print = 'Plus5'; [country_codes,country_names,country_info] = xlsread([basefold 'Input_data/countrylist_plus5.xls']); 
% country_title= 'Least Developed Countries'; country_print = 'LDC'; [country_codes,country_names,country_info] = xlsread([basefold 'Input_data/countrylist_leastDC.xls']);
% %SUBSETS OF LDCs -- LDC with data available
% index_WAfrica = [4 6 17 18 19 24 27 35 36 42];
% index_Africa = [2 7 9 10 13 15 16 23 25 26 28 29 32 33 44 45 47 48];
% index_Asia = [3 8 22 30 31];
% index_Islands = [11 20 21 34 37 41 46];
% country_title= 'Least Developed Countries'; country_print = 'LDC'; [country_codes,country_names,country_info] = xlsread([basefold 'Input_data/countrylist_leastDCnoDjibouti.xls']);
% %SUBSETS OF LDCs -- LDC without Djoubuti
% index_WAfrica = [4 6 16 17 18 23 26 34 35 41];
% index_Africa = [2 7 9 10 14 15 22 24 25 27 28 31 32 43 44 46 47];   
% index_Asia = [3 8 21 29 30];    
% index_Islands = [11 19 20 33 36 40 45];

%country_title= 'Low-Income Food-Deficit Countries'; country_print = 'LIFDC'; [country_codes,country_names,country_info] = xlsread([basefold 'Input_data/countrylist_lowincomefooddeficit.xls']); 
%country_title= 'Annex I Countries'; country_print = 'AnnexI'; [country_codes,country_names,country_info] = xlsread([basefold 'Input_data/countrylist_annex1.xls']); 
%country_title= 'Non-Annex I Countries'; country_print = 'nonAnnexI';[country_codes,country_names,country_info] = xlsread([basefold 'Input_data/countrylist_nonannex1.xls']);
%country_title= 'G8 Countries'; country_print = 'G8'; [country_codes,country_names,country_info] = xlsread([basefold 'Input_data/countrylist_G8.xls']); 
%country_title= 'G8 plus China'; country_print = 'G8China'; [country_codes,country_names,country_info] = xlsread([basefold 'Input_data/countrylist_G8China.xls']); 
%country_title= 'G20 Countries'; country_print = 'G20'; [country_codes,country_names,country_info] = xlsread([basefold 'Input_data/countrylist_G20.xls']); 
% country_title= 'G8+5 Countries'; country_print = 'G8Plus5'; [country_codes,country_names,country_info] = xlsread([basefold 'Input_data/countrylist_G8plus5.xls']); 

% CONTINENTS
% country_title= 'Africa'; country_print = 'Africa'; [country_codes,country_names,country_info] = xlsread([basefold 'Input_data/countrylist_Africa.xls']); lonmin = -25; lonmax = 60; latmin = -42; latmax = 42;
% country_title= 'Asia'; country_print = 'Asia'; [country_codes,country_names,country_info] = xlsread([basefold 'Input_data/countrylist_Asia.xls']); lonmin = 45; lonmax = 180; latmin = 0; latmax = 80;
% country_title= 'Europe'; country_print = 'Europe'; [country_codes,country_names,country_info] = xlsread([basefold 'Input_data/countrylist_Europe.xls']); lonmin = -30; lonmax = 50; latmin = 30; latmax = 75;
% country_title= 'North America'; country_print = 'NAmerica'; [country_codes,country_names,country_info] = xlsread([basefold 'Input_data/countrylist_NorthAmerica.xls']); lonmin = -170; lonmax = -45; latmin = 0; latmax = 90;
% country_title= 'Central America'; country_print = 'CAmerica'; [country_codes,country_names,country_info] = xlsread([basefold 'Input_data/countrylist_CentralAmericaCarib.xls']); lonmin = -120; lonmax = -45; latmin = 0; latmax = 30;
% country_title= 'South America'; country_print = 'SAmerica'; [country_codes,country_names,country_info] = xlsread([basefold 'Input_data/countrylist_SouthAmerica.xls']); lonmin = -90; lonmax = -30; latmin = -60; latmax = 40;
% country_title= 'Global'; country_print = 'World'; [country_codes,country_names,country_info] = xlsread([basefold 'Input_data/countrylist_World.xls']); 

% COUNTRIES
% country_title= 'Democratic Republic of the Congo'; country_print = 'DemCongo'; [country_codes,country_names,country_info] = xlsread([basefold 'Input_data/countrylist_DemocraticRepublicCongo.xls']); lonmin = -25; lonmax = 60; latmin = -42; latmax = 42;
% country_title= 'Zimbabwe'; [country_codes,country_names,country_info] = xlsread([basefold 'Input_data/countrylist_Zimbabwe.xls']); lonmin = -25; lonmax = 60; latmin = -42; latmax = 42;
% country_title= 'Ethiopia'; [country_codes,country_names,country_info] = xlsread([basefold 'Input_data/countrylist_Ethiopia.xls']); lonmin = -25; lonmax = 60; latmin = -42; latmax = 42;
% country_title= 'USA';  country_print = 'USA'; [country_codes,country_names,country_info] = xlsread([basefold 'Input_data/countrylist_USA.xls']);
% country_title= 'South Korea'; [country_codes,country_names,country_info] = xlsread([basefold 'Input_data/countrylist_KoreaSouth.xls']);
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
%flow_type = 'green'; flow_title = 'Green Virtual Water'; flow_units = 'm^3';
%flow_type = 'blue'; flow_title = 'Blue Virtual Water'; flow_units = 'm^3';
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
disturb_location(:,1) = data(:,disturbance_type);

% Clear temporary variables
clearvars data raw;

%% Number of disturbed countries and subset countries
number_subsetCountries = length(country_codes);
number_disturbed = sum(disturb_location>=1);

%% DATASET:  Import population data (1961 to 2050)
offset_populationyr = year_start-1961;
convert_population = 1000; %convert population as it is stored in thousands
[~, ~, raw] = xlsread([basefold 'Input_data/Population_Totalinthousands_Code511_Oct2013.xls'],'Sheet1');
raw = raw(:,2:end);
raw(cellfun(@(x) ~isempty(x) && isnumeric(x) && isnan(x),raw)) = {''};

% Replace non-numeric cells with NaN
R = cellfun(@(x) ~isnumeric(x) && ~islogical(x),raw); % Find non-numeric cells
raw(R) = {NaN}; % Replace non-numeric cells

% Create output variable
temp = reshape([raw{:}],size(raw));
years = temp(1,2:end);
pop_countrycodes = temp(2:end,1); 
Population = temp(2:end,2:end);

% Clear temporary variables
clearvars raw R temp;

%%  DATASET: Import GDP data (1970 to 2009)
offset_GDPyr = year_start-1970;
[~, ~, raw] = xlsread([basefold 'Input_data/GDPbycountryyear_USD2005.xls'],'Sheet1');
raw = raw(:,2:end);
raw(cellfun(@(x) ~isempty(x) && isnumeric(x) && isnan(x),raw)) = {''};

% Replace non-numeric cells with NaN
R = cellfun(@(x) ~isnumeric(x) && ~islogical(x),raw); % Find non-numeric cells
raw(R) = {NaN}; % Replace non-numeric cells

% Create output variable
temp = reshape([raw{:}],size(raw));
yearsGDP = temp(1,2:end);
GDP_countrycodes = temp(2:end,1); 
GDPbycountryyearUSD2005 = temp(2:end,2:end);

% Clear temporary variables
clearvars raw R temp;


%%  DATASET: Import FOOD BALANCE SHEETS for Wheat AND Rice
% Read columns of data as strings:
formatSpec = '%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%[^\n\r]';
num_commodities=1;

%Default composition: 15 Wheat, 16 Flour of Wheat, 18 Macaroni, 20 Bread, 21 Bulgur, 22 Pastry, 23 Starch of Wheat, 41 Breakfast Cereals, 110 Wafers; nutrient data only: 17 Bran of Wheat, 19 Germ of Wheat, 24 Gluten of Wheat, 114 Mixes and Doughs, 115 Food Prep,Flour,Malt Extract
%Default composition: 27 Rice, paddy, 28 Rice Husked, 29 Milled/Husked Rice, 31 Rice Milled, 32 Rice Broken, 34 Starch of Rice, 38 Rice Flour; nutrient data only: 33 Rice gluten, 35 Bran of Rice
for i_commod = 1:num_commodities
    % Open the text file
    %filename = [basefold 'Input_data/' commod_title{i_commod} '_CommodityBalance_1986_2009_tonnes.txt'];
    filename = [basefold 'Input_data/' commod_titleb '_CommodityBalance_1986_2009_tonnes.txt'];
    
    fileID = fopen(filename,'r');
    
    % Read columns of data according to format string.
    delimiter = '\t';
    dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter,  'ReturnOnError', false);
    
    %% Close the text file.
    fclose(fileID);
    
    %% Convert the contents of columns containing numeric strings to numbers.
    % Replace non-numeric strings with NaN.
    raw = repmat({''},length(dataArray{1}),length(dataArray)-1);
    for col=1:length(dataArray)-1
        raw(1:length(dataArray{col}),col) = dataArray{col};
    end
    numericData = NaN(size(dataArray{1},1),size(dataArray,2));
    
    for col=[2,4,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30]
        % Converts strings in the input cell array to numbers. Replaced non-numeric
        % strings with NaN.
        rawData = dataArray{col};
        for row=1:size(rawData, 1);
            % Create a regular expression to detect and remove non-numeric prefixes and
            % suffixes.
            regexstr = '(?<prefix>.*?)(?<numbers>([-]*(\d+[\,]*)+[\.]{0,1}\d*[eEdD]{0,1}[-+]*\d*[i]{0,1})|([-]*(\d+[\,]*)*[\.]{1,1}\d+[eEdD]{0,1}[-+]*\d*[i]{0,1}))(?<suffix>.*)';
            try
                result = regexp(rawData{row}, regexstr, 'names');
                numbers = result.numbers;
                
                % Detected commas in non-thousand locations.
                invalidThousandsSeparator = false;
                if any(numbers==',');
                    thousandsRegExp = '^\d+?(\,\d{3})*\.{0,1}\d*$';
                    if isempty(regexp(thousandsRegExp, ',', 'once'));
                        numbers = NaN;
                        invalidThousandsSeparator = true;
                    end
                end
                % Convert numeric strings to numbers.
                if ~invalidThousandsSeparator;
                    numbers = textscan(strrep(numbers, ',', ''), '%f');
                    numericData(row, col) = numbers{1};
                    raw{row, col} = numbers{1};
                end
            catch me
            end
        end
    end
    
    %% Split data into numeric and cell columns.
    rawNumericColumns = raw(:, [2,4,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30]);
    rawCellColumns = raw(:, [1,3,5]);
    
    
    %% Replace non-numeric cells with NaN
    R = cellfun(@(x) ~isnumeric(x) && ~islogical(x),rawNumericColumns); % Find non-numeric cells
    rawNumericColumns(R) = {NaN}; % Replace non-numeric cells
    
    %% Create output variable
    CommodityBalance.commod{i_commod} = rawNumericColumns;%raw
    % Clear temporary variables
    clearvars filename delimiter fileID dataArray ans raw col numericData rawData row regexstr result numbers invalidThousandsSeparator thousandsRegExp me rawNumericColumns rawCellColumns R;
end

%% PROCESS food balance data for wheat and rice
% Assign indices
i_commod = 1;
code_country   = cell2mat(CommodityBalance.commod{i_commod}(2:end,1));
code_commodity = cell2mat(CommodityBalance.commod{i_commod}(2:end,2));
code_element   = cell2mat(CommodityBalance.commod{i_commod}(2:end,3));

index_Production = find(code_element==code_Production)+1;
index_Import = find(code_element==code_Import)+1;
index_Export = find(code_element==code_Export)+1;
index_StockVariation = find(code_element==code_StockVariation)+1;
index_FoodHuman = find(code_element==code_Food)+1;

%% ALL Countries: Food Balance Year Loop for Wheat, Rice, etc
Population_all=nan(length(country_tradecodes),num_years+1);
GDP_all=nan(length(country_tradecodes),num_years+1);
Production_all=nan(length(country_tradecodes),num_years+1);
Import_all=nan(length(country_tradecodes),num_years+1);
Export_all=nan(length(country_tradecodes),num_years+1);
StockVariation_all=nan(length(country_tradecodes),num_years+1);
FoodHuman_all=nan(length(country_tradecodes),num_years+1);
offset_col = 3+offset_yr;  %% depends on this line above:CommodityBalance.commod{i_commod} = rawNumericColumns;%raw

for i_country = 1:length(country_tradecodes)
    % Check FAOSTAT crop (wheat, maize, rice) data
    index_country = find(code_country==country_tradecodes(i_country,1))+1;
    index_population = find(pop_countrycodes==country_tradecodes(i_country,1));
    index_GDP = find(GDP_countrycodes==country_tradecodes(i_country,1));
    
    if ~isempty(index_country)        
        % Add country codes to first column

        Population_all(i_country,1)=country_tradecodes(i_country);
        GDP_all(i_country,1)=country_tradecodes(i_country);
        Production_all(i_country,1)=country_tradecodes(i_country);
        Import_all(i_country,1)=country_tradecodes(i_country);
        Export_all(i_country,1)=country_tradecodes(i_country);
        StockVariation_all(i_country,1)=country_tradecodes(i_country);
        FoodHuman_all(i_country,1)=country_tradecodes(i_country);
        
        i_year = 0; % Initialize
        for year = year_start:year_end
            i_year = i_year + 1; % year index
            i_commod = 1; % only one commodity
            
            % Population data
            Population_all(i_country,i_year+1)= Population(index_population,i_year+offset_populationyr)*convert_population;

            % GDP data
            GDP_all(i_country,i_year+1)= GDPbycountryyearUSD2005(index_GDP,i_year+offset_GDPyr);
            
            % Food data (total amount of the commodity available as human
            % food during the reference period)
            temp_index_FoodHuman = intersect(index_country,index_FoodHuman);
            if ~isempty(temp_index_FoodHuman)
                FoodHuman_all(i_country,i_year+1) =cell2mat(CommodityBalance.commod{i_commod}(temp_index_FoodHuman,offset_col+i_year));
            else
                FoodHuman_all(i_country,i_year+1) = 0;
            end                        
            
            % Production data
            temp_index_Production = intersect(index_country,index_Production);
            if ~isempty(temp_index_Production)
                Production_all(i_country,i_year+1) =cell2mat(CommodityBalance.commod{i_commod}(temp_index_Production,offset_col+i_year));
            else
                Production_all(i_country,i_year+1) = 0;
            end
            
            % Import data
            temp_index_Import = intersect(index_country,index_Import);
            if ~isempty(temp_index_Import)
                Import_all(i_country,i_year+1) =cell2mat(CommodityBalance.commod{i_commod}(temp_index_Import,offset_col+i_year));
            else
                Import_all(i_country,i_year+1) = 0;
            end
            
            % Export data
            temp_index_Export = intersect(index_country,index_Export);
            if ~isempty(temp_index_Export)
                Export_all(i_country,i_year+1) =cell2mat(CommodityBalance.commod{i_commod}(temp_index_Export,offset_col+i_year));
            else
                Export_all(i_country,i_year+1) = 0;
            end
            
            
            % Stock Variation data
            temp_index_StockVariation = intersect(index_country,index_StockVariation);
            if ~isempty(temp_index_StockVariation)
                StockVariation_all(i_country,i_year+1) =cell2mat(CommodityBalance.commod{i_commod}(temp_index_StockVariation,offset_col+i_year));
            else
                StockVariation_all(i_country,i_year+1) = 0;
            end
        end
    end
end


% %% Subset of Countries: Food Balance Year Loop),num_years+1);
Population_sub=nan(length(country_codes),num_years+1);
GDP_sub=nan(length(country_codes),num_years+1);
Production_sub=nan(length(country_codes),num_years+1);
Import_sub_sub=nan(length(country_codes),num_years+1);
Export_sub=nan(length(country_codes),num_years+1);
StockVariation_sub=nan(length(country_codes),num_years+1);
FoodHuman_sub=nan(length(country_codes),num_years+1);
offset_col = 3+offset_yr;  %% depends on this line above:CommodityBalance.commod{i_commod} = rawNumericColumns;%raw

for i_country = 1:length(country_codes)
    % Check FAOSTAT crop (wheat, maize, rice) data
    index_country = find(code_country==country_codes(i_country,1))+1;
    index_population = find(pop_countrycodes==country_codes(i_country,1));
    index_GDP = find(GDP_countrycodes==country_codes(i_country,1));
    
    if ~isempty(index_country)        
        % Add country codes to first column

        Population_sub(i_country,1)=country_codes(i_country);
        GDP_sub(i_country,1)=country_codes(i_country);
        Production_sub(i_country,1)=country_codes(i_country);
        Import_sub(i_country,1)=country_codes(i_country);
        Export_sub(i_country,1)=country_codes(i_country);
        StockVariation_sub(i_country,1)=country_codes(i_country);
        FoodHuman_sub(i_country,1)=country_codes(i_country);
        
        i_year = 0; % Initialize
        for year = year_start:year_end
            i_year = i_year + 1; % year index
            i_commod = 1; % only one commodity
            
            % Population data
            Population_sub(i_country,i_year+1)= Population(index_population,i_year+offset_populationyr)*convert_population;

            % GDP data
            GDP_sub(i_country,i_year+1)= GDPbycountryyearUSD2005(index_GDP,i_year+offset_GDPyr);
            
            % Food data (total amount of the commodity available as human
            % food during the reference period)
            temp_index_FoodHuman = intersect(index_country,index_FoodHuman);
            if ~isempty(temp_index_FoodHuman)
                FoodHuman_sub(i_country,i_year+1) =cell2mat(CommodityBalance.commod{i_commod}(temp_index_FoodHuman,offset_col+i_year));
            else
                FoodHuman_sub(i_country,i_year+1) = 0;
            end                        
            
            % Production data
            temp_index_Production = intersect(index_country,index_Production);
            if ~isempty(temp_index_Production)
                Production_sub(i_country,i_year+1) =cell2mat(CommodityBalance.commod{i_commod}(temp_index_Production,offset_col+i_year));
            else
                Production_sub(i_country,i_year+1) = 0;
            end
            
            % Import data
            temp_index_Import = intersect(index_country,index_Import);
            if ~isempty(temp_index_Import)
                Import_sub(i_country,i_year+1) =cell2mat(CommodityBalance.commod{i_commod}(temp_index_Import,offset_col+i_year));
            else
                Import_sub(i_country,i_year+1) = 0;
            end
            
            % Export data
            temp_index_Export = intersect(index_country,index_Export);
            if ~isempty(temp_index_Export)
                Export_sub(i_country,i_year+1) =cell2mat(CommodityBalance.commod{i_commod}(temp_index_Export,offset_col+i_year));
            else
                Export_sub(i_country,i_year+1) = 0;
            end
            
            
            % Stock Variation data
            temp_index_StockVariation = intersect(index_country,index_StockVariation);
            if ~isempty(temp_index_StockVariation)
                StockVariation_sub(i_country,i_year+1) =cell2mat(CommodityBalance.commod{i_commod}(temp_index_StockVariation,offset_col+i_year));
            else
                StockVariation_sub(i_country,i_year+1) = 0;
            end
        end
    end
end



%% Food Balance data in text file for CEAREALS and STARCHY ROOTS
num_commodities = 2;
FBcommod_title{1}='Cereals'; FBcommod_print1 = 'Cereals';
FBcommod_title{2}='StarchyRoots'; FBcommod_print2 = 'Starchy Roots';

% Import files commodity balance files
% Read columns of data as strings:
formatSpec = '%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%[^\n\r]';

for i_commod = 1:num_commodities
    % Open the text file
    filename = [basefold 'Input_data/' FBcommod_title{i_commod} '_CommodityBalance_1986_2009_tonnes.txt'];
    fileID = fopen(filename,'r');
    
    % Read columns of data according to format string.
    delimiter = '\t';
    dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter,  'ReturnOnError', false);
    
    % Close the text file.
    fclose(fileID);
    
    % Convert the contents of columns containing numeric strings to numbers.
    % Replace non-numeric strings with NaN.
    raw = repmat({''},length(dataArray{1}),length(dataArray)-1);
    for col=1:length(dataArray)-1
        raw(1:length(dataArray{col}),col) = dataArray{col};
    end
    numericData = NaN(size(dataArray{1},1),size(dataArray,2));
    
    for col=[2,4,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30]
        % Converts strings in the input cell array to numbers. Replaced non-numeric
        % strings with NaN.
        rawData = dataArray{col};
        for row=1:size(rawData, 1);
            % Create a regular expression to detect and remove non-numeric prefixes and
            % suffixes.
            regexstr = '(?<prefix>.*?)(?<numbers>([-]*(\d+[\,]*)+[\.]{0,1}\d*[eEdD]{0,1}[-+]*\d*[i]{0,1})|([-]*(\d+[\,]*)*[\.]{1,1}\d+[eEdD]{0,1}[-+]*\d*[i]{0,1}))(?<suffix>.*)';
            try
                result = regexp(rawData{row}, regexstr, 'names');
                numbers = result.numbers;
                
                % Detected commas in non-thousand locations.
                invalidThousandsSeparator = false;
                if any(numbers==',');
                    thousandsRegExp = '^\d+?(\,\d{3})*\.{0,1}\d*$';
                    if isempty(regexp(thousandsRegExp, ',', 'once'));
                        numbers = NaN;
                        invalidThousandsSeparator = true;
                    end
                end
                % Convert numeric strings to numbers.
                if ~invalidThousandsSeparator;
                    numbers = textscan(strrep(numbers, ',', ''), '%f');
                    numericData(row, col) = numbers{1};
                    raw{row, col} = numbers{1};
                end
            catch me
            end
        end
    end
    
    % Split data into numeric and cell columns.
    rawNumericColumns = raw(:, [2,4,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30]);
    rawCellColumns = raw(:, [1,3,5]);
    
    
    % Replace non-numeric cells with NaN
    R = cellfun(@(x) ~isnumeric(x) && ~islogical(x),rawNumericColumns); % Find non-numeric cells
    rawNumericColumns(R) = {NaN}; % Replace non-numeric cells
    
    % Create output variable
    FBCommodityBalance.commod{i_commod} = rawNumericColumns;%raw
    % Clear temporary variables
    clearvars filename delimiter fileID dataArray ans raw col numericData rawData row regexstr result numbers invalidThousandsSeparator thousandsRegExp me rawNumericColumns rawCellColumns R;
end

%% Process Food Balance Sheet data for cereals and starchy roots
% Cereals
i_commod = 1;
FBcode_country   = cell2mat(FBCommodityBalance.commod{i_commod}(2:end,1));
FBcode_commodity = cell2mat(FBCommodityBalance.commod{i_commod}(2:end,2));
FBcode_element   = cell2mat(FBCommodityBalance.commod{i_commod}(2:end,3));

FBindex_Production = find(FBcode_element==code_Production)+1;
FBindex_Import = find(FBcode_element==code_Import)+1;
FBindex_Export = find(FBcode_element==code_Export)+1;
FBindex_StockVariation = find(FBcode_element==code_StockVariation)+1;

% Starchy Roots
i_commod = 2;
FBcode_country2   = cell2mat(FBCommodityBalance.commod{i_commod}(2:end,1));
FBcode_commodity2 = cell2mat(FBCommodityBalance.commod{i_commod}(2:end,2));
FBcode_element2   = cell2mat(FBCommodityBalance.commod{i_commod}(2:end,3));

FBindex_Production2 = find(FBcode_element2==code_Production)+1;
FBindex_Import2 = find(FBcode_element2==code_Import)+1;
FBindex_Export2 = find(FBcode_element2==code_Export)+1;
FBindex_StockVariation2 = find(FBcode_element2==code_StockVariation)+1;

% All country loop
FBSSR=nan(length(country_tradecodes),num_years+1);
FBIDR=nan(length(country_tradecodes),num_years+1);
FBProduction_all=nan(length(country_tradecodes),num_years+1);
FBConsumption=nan(length(country_tradecodes),num_years+1);

FBoffset_yr = year_start-1986; % because data starts in 1986
FBoffset_col = 3+FBoffset_yr;  %% depends on this line above:CommodityBalance.commod{i_commod} = rawNumericColumns;%raw


for i_country = 1:length(country_tradecodes)
    % Check FAOSTAT cereal data
    FBindex_country = find(FBcode_country==country_tradecodes(i_country,1))+1;
    % Check mapping data
    FBindex_country2 = find(FBcode_country2==country_tradecodes(i_country,1))+1;
    
    FBSSR(i_country,1)=country_tradecodes(i_country);
    FBIDR(i_country,1)=country_tradecodes(i_country);
    FBProduction_all(i_country,1)=country_tradecodes(i_country);
    FBConsumption(i_country,1)=country_tradecodes(i_country);
    
    if ~isempty(FBindex_country) || ~isempty(FBindex_country2)

        i_year = 0; % Initialize
        for year = year_start:year_end
            i_year = i_year + 1; % year index
            
            % Production data
            i_commod = 1;
            temp_index_Production = intersect(FBindex_country,FBindex_Production);
            if ~isempty(temp_index_Production)
                FBProduction =cell2mat(FBCommodityBalance.commod{i_commod}(temp_index_Production,FBoffset_col+i_year));
            else
                FBProduction = 0;
            end
            
            i_commod = 2;
            temp_index_Production2 = intersect(FBindex_country2,FBindex_Production2);
            if ~isempty(temp_index_Production2)
                FBProduction2 =cell2mat(FBCommodityBalance.commod{i_commod}(temp_index_Production2,FBoffset_col+i_year));
            else
                FBProduction2 = 0;
            end
               
            % Import data
            i_commod = 1;
            temp_index_Import = intersect(FBindex_country,FBindex_Import);
            if ~isempty(temp_index_Import)
                FBImport =cell2mat(FBCommodityBalance.commod{i_commod}(temp_index_Import,FBoffset_col+i_year));
            else
                FBImport = 0;
            end
            
            i_commod = 2;
            temp_index_Import2 = intersect(FBindex_country2,FBindex_Import2);
            if ~isempty(temp_index_Import2)
                FBImport2 =cell2mat(FBCommodityBalance.commod{i_commod}(temp_index_Import2,FBoffset_col+i_year));
            else
                FBImport2 = 0;
            end
            
            % Export data
            i_commod = 1;
            temp_index_Export = intersect(FBindex_country,FBindex_Export);
            if ~isempty(temp_index_Export)
                FBExport =cell2mat(FBCommodityBalance.commod{i_commod}(temp_index_Export,FBoffset_col+i_year));
            else
                FBExport = 0;
            end   
            
            i_commod = 2;
            temp_index_Export2 = intersect(FBindex_country2,FBindex_Export2);
            if ~isempty(temp_index_Export2)
                FBExport2 =cell2mat(FBCommodityBalance.commod{i_commod}(temp_index_Export2,FBoffset_col+i_year));
            else
                FBExport2 = 0;
            end  

            
            % Stock Variation data
            i_commod = 1;
            temp_index_StockVariation = intersect(FBindex_country,FBindex_StockVariation);
            if ~isempty(temp_index_StockVariation)
                FBStockVariation =cell2mat(FBCommodityBalance.commod{i_commod}(temp_index_StockVariation,FBoffset_col+i_year));
            else
                FBStockVariation = 0;
            end     
            
            i_commod = 2;
            temp_index_StockVariation2 = intersect(FBindex_country2,FBindex_StockVariation2);
            if ~isempty(temp_index_StockVariation2)
                FBStockVariation2 =cell2mat(FBCommodityBalance.commod{i_commod}(temp_index_StockVariation2,FBoffset_col+i_year));
            else
                FBStockVariation2 = 0;
            end  

            
            % Production array
            if (FBProduction+FBProduction2) > 0
                FBProduction_all(i_country,i_year+1)=FBProduction+factor_Roots_CE*FBProduction2;
            else
                FBProduction_all(i_country,i_year+1)=NaN;
            end
            
            % Commodity self sufficiency ratio
            if (FBProduction+FBImport-FBExport) > 0
                FBSSR(i_country,i_year+1)=(FBProduction+factor_Roots_CE*FBProduction2)./(FBProduction+FBImport-FBExport+FBStockVariation+factor_Roots_CE*(FBProduction2+FBImport2-FBExport2+FBStockVariation2));
            else
                FBSSR(i_country,i_year+1)=NaN;
            end
            
            % Commodity Import Dependency Ratio
            if (FBProduction+FBImport-FBExport) > 0
                FBIDR(i_country,i_year+1)=(FBImport+factor_Roots_CE*FBImport2)./(FBProduction+FBImport-FBExport+FBStockVariation+factor_Roots_CE*(FBProduction2+FBImport2-FBExport2+FBStockVariation2));
            else
                FBIDR(i_country,i_year+1)=NaN;
            end
            
            % Consumption
            if (FBProduction+FBImport-FBExport+FBStockVariation) > 0
                FBConsumption(i_country,i_year+1)=FBProduction+FBImport-FBExport+FBStockVariation +...
                    factor_Roots_CE*(FBProduction2+FBImport2-FBExport2+FBStockVariation2);
            else
                FBConsumption(i_country,i_year+1)=NaN;
            end
        end
        
    end
end



%% DATASET:  Food supply (kcal/capita/day) for wheat, rice, and all comodities
filename = [basefold 'Input_data/WheatRiceAll_FoodSupply_1986_2009_kcalcapitaday.txt'];
delimiter = '\t';

% Read columns of data as strings:
formatSpec = '%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%[^\n\r]';

%%Open the text file.
fileID = fopen(filename,'r');

% Read columns of data according to format string.
dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter,  'ReturnOnError', false);

% Close the text file.
fclose(fileID);

% Convert the contents of columns containing numeric strings to numbers.
% Replace non-numeric strings with NaN.
raw = repmat({''},length(dataArray{1}),length(dataArray)-1);
for col=1:length(dataArray)-1
    raw(1:length(dataArray{col}),col) = dataArray{col};
end
numericData = NaN(size(dataArray{1},1),size(dataArray,2));

for col=[2,4,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30]
    % Converts strings in the input cell array to numbers. Replaced non-numeric
    % strings with NaN.
    rawData = dataArray{col};
    for row=1:size(rawData, 1);
        % Create a regular expression to detect and remove non-numeric prefixes and
        % suffixes.
        regexstr = '(?<prefix>.*?)(?<numbers>([-]*(\d+[\,]*)+[\.]{0,1}\d*[eEdD]{0,1}[-+]*\d*[i]{0,1})|([-]*(\d+[\,]*)*[\.]{1,1}\d+[eEdD]{0,1}[-+]*\d*[i]{0,1}))(?<suffix>.*)';
        try
            result = regexp(rawData{row}, regexstr, 'names');
            numbers = result.numbers;
            
            % Detected commas in non-thousand locations.
            invalidThousandsSeparator = false;
            if any(numbers==',');
                thousandsRegExp = '^\d+?(\,\d{3})*\.{0,1}\d*$';
                if isempty(regexp(thousandsRegExp, ',', 'once'));
                    numbers = NaN;
                    invalidThousandsSeparator = true;
                end
            end
            % Convert numeric strings to numbers.
            if ~invalidThousandsSeparator;
                numbers = textscan(strrep(numbers, ',', ''), '%f');
                numericData(row, col) = numbers{1};
                raw{row, col} = numbers{1};
            end
        catch me
        end
    end
end

% Split data into numeric and cell columns.
rawNumericColumns = raw(:, [2,4,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30]);
rawCellColumns = raw(:, [1,3,5]);

% Replace non-numeric cells with NaN
R = cellfun(@(x) ~isnumeric(x) && ~islogical(x),rawNumericColumns); % Find non-numeric cells
rawNumericColumns(R) = {NaN}; % Replace non-numeric cells

% Create output variable
WheatRiceAll_Calories = raw;
% Clear temporary variables
clearvars filename delimiter formatSpec fileID dataArray ans raw col numericData rawData row regexstr result numbers invalidThousandsSeparator thousandsRegExp me rawNumericColumns rawCellColumns R;

% Assign codes to vectors for indices
kcalcode_country   = cell2mat(WheatRiceAll_Calories(2:end,2));
kcalcode_commodity = cell2mat(WheatRiceAll_Calories(2:end,4));
kcalcode_element   = cell2mat(WheatRiceAll_Calories(2:end,6));

index_FoodSupplykcal = find(kcalcode_element==code_FoodSupplykcal)+1;
index_totalFoodSupplykcal = find(kcalcode_commodity==code_totalfood)+1;


% ALL Countries: Food Balance Year Loop for Wheat, Rice, etc
FoodSupplykcal_all=nan(length(country_tradecodes),num_years+1);
offset_col = 6+offset_yr;  %% depends on this line above:CommodityBalance.commod{i_commod} = rawNumericColumns;%raw

for i_country = 1:length(country_tradecodes)
    index_country = find(kcalcode_country==country_tradecodes(i_country,1))+1;
    
    if ~isempty(index_country)        

        % Add country codes to first column
        FoodSupplykcal_all(i_country,1)=country_tradecodes(i_country);
        
        i_year = 0; % Initialize
        for year = year_start:year_end
            i_year = i_year + 1; % year index
            i_commod = 1; % only one commodity

            % Food Supply in kcal per capita per day
            temp_index_FoodSupply = intersect(index_country,index_FoodSupplykcal);
            temp_totalkcal = intersect(index_totalFoodSupplykcal,temp_index_FoodSupply);
            
            if ~isempty(temp_totalkcal) && ~isempty(cell2mat((WheatRiceAll_Calories(temp_totalkcal,offset_col+i_year))))
                FoodSupplykcal_all(i_country,i_year+1) =cell2mat(WheatRiceAll_Calories(temp_totalkcal,offset_col+i_year));
            else
                FoodSupplykcal_all(i_country,i_year+1) = NaN;
            end                        
            
        end
    end
end

% %% Subset of Countries: Food Balance Year Loop... initialize
kcalcode_country   = cell2mat(WheatRiceAll_Calories(2:end,2));
kcalcode_commodity = cell2mat(WheatRiceAll_Calories(2:end,4));
kcalcode_element   = cell2mat(WheatRiceAll_Calories(2:end,6));
index_FoodSupplykcal = find(kcalcode_element==code_FoodSupplykcal)+1;
index_totalFoodSupplykcal = find(kcalcode_commodity==code_totalfood)+1;

FoodSupplykcal_sub=nan(length(country_codes),num_years+1);

for i_country = 1:length(country_codes)
    index_country = find(kcalcode_country==country_codes(i_country,1))+1;
    
    if ~isempty(index_country)
        % Add country codes to first column
        FoodSupplykcal_sub(i_country,1)=country_codes(i_country);
        
        i_year = 0; % Initialize
        for year = year_start:year_end
            i_year = i_year + 1; % year index
            
            % Food Supply in kcal per capita per day
            temp_index_FoodSupply = intersect(index_country,index_FoodSupplykcal);
            temp_totalkcal = intersect(index_totalFoodSupplykcal,temp_index_FoodSupply);
            if ~isempty(temp_totalkcal) && ~isempty(cell2mat((WheatRiceAll_Calories(temp_totalkcal,offset_col+i_year))))
                FoodSupplykcal_sub(i_country,i_year+1) =cell2mat(WheatRiceAll_Calories(temp_totalkcal,offset_col+i_year));
            else
                FoodSupplykcal_sub(i_country,i_year+1) = NaN;
            end
            
        end
    end
end



%% NETORK ANALYSIS WITH TARGETED ATTACK
% Intilize disturbance matrices
Production_lost = zeros(size(Production_all));

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
    
    %% Active nodes
    Active_nodes(i_year,:) = sum(WU,1)>0;
    Active_nodes(i_year,:) = sum(WU,1)>0;
    
    %% Amount traded globally
    TotTradeGlobal(i_year) = sum(WD(:));
    
    %% Strength, si, is calculated as the sum of all the exporting fluxes from
    % that node (i.e., as the row sum in the trade matrix)
    s_D_out(:,i_year) = sum(WD,2); %exporting strength
    s_D_in(:,i_year) = sum(WD,1); %importing strength
    
    %% Compute degrees in network for exports in directed network
    k_D_out(:,i_year) = nansum(AD,2);  % sum each row for export degree
    k_D_in(:,i_year) = nansum(AD,1);  % sum each row for export degree
    
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
        Production_lost(NodesToRemove,i_year+1)= reduction_factor * Production_all(NodesToRemove,i_year+1);

    end
    
    
    %% Strength of disturbed network
    s_D_out_disturbed(:,i_year) = nansum(WD_temp,2); %exporting strength
    s_D_in_disturbed(:,i_year) = nansum(WD_temp,1); %importing strength
    
    %% Degree of disturbed network
    k_D_out_disturbed(:,i_year) = nansum(AD_temp,2);  % sum each row for export degree
    k_D_in_disturbed(:,i_year) = nansum(AD_temp,1);  % sum each row for export degree
    
    % Imports lost by country
    s_D_in_LOST(:,i_year) = s_D_in(:,i_year) - s_D_in_disturbed(:,i_year);
    
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
    
    TotProductionGlobal(i_year) = nansum(Production_all(:,i_year+1));
    TotProductionGlobal_disturbed(i_year) = nansum(Production_all(:,i_year+1))-...
        nansum(Production_lost(:,i_year+1));
    
    Output(:,i_year) = [
        TotProductionGlobal(i_year);... % Total Global Production
        TotProductionGlobal_disturbed(i_year);... % Total Global Production with Disturbance
        TotProductionGlobal(i_year)-TotProductionGlobal_disturbed(i_year);... %Total Vulnerable Production
        TotTradeGlobal(i_year);... % Total Global Trade
        sum(Amount_lost);...       % Total Vulnerable Trade
        sum(Amount_lost)/TotTradeGlobal(i_year);... vulnerable trade as fraction of total global trade
        NumNodes(i_year);...
        NumNodes(i_year)-NumbersOfNodes(end);...
        NumLinks(i_year);...
        NumLinks(i_year)-NumbersOfLinks(end);...
        ];
    

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
            TradeRemain = TradeRemain-s_D_in(i_allocate,i_year);
            if TradeRemain>=0
                import_GDPallocated(i_allocate,i_year)=s_D_in(i_allocate,i_year);
                TradeRemain = TradeRemain-s_D_in(i_allocate,i_year);
            elseif flag_temp == 0
                import_GDPallocated(i_allocate,i_year)=TradeRemain;
                TradeRemain=0;
                flag_temp = 1;
            elseif flag_temp == 1
                import_GDPallocated(i_allocate,i_year)=0;
            end
        else
            import_GDPallocated(i_allocate,i_year)=0;
        end
    end
    
    
end



%% Compute groupings for bar plots
%Production
Prod(1) = nanmean(Output(2,i_periodA))/mmt_convert; % Rest of World (i.e., production with attack)
Prod(2) = nanmean(Output(3,i_periodA))/mmt_convert; % Production threatened

Prod(3) = nanmean(Output(2,i_periodB))/mmt_convert; % Rest of World (i.e., production with attack) 
Prod(4) = nanmean(Output(3,i_periodB))/mmt_convert; % Production threatened

Title1A = num2str(nanmean(TotProductionGlobal(i_periodA))/mmt_convert);
Title1B = num2str(nanmean(TotProductionGlobal(i_periodB))/mmt_convert);

% Trade
Export(1)=nanmean(TotTradeGlobal_after(i_periodA))/mmt_convert; % Rest of World (i.e., exports with attack) 
Export(2)=nanmean(Output(5,i_periodA))/mmt_convert; % Total Vulnerable Trade

Export(3)=nanmean(TotTradeGlobal_after(i_periodB))/mmt_convert; % Rest of World (i.e., exports with attack) 
Export(4)=nanmean(Output(5,i_periodB))/mmt_convert; % Total Vulnerable Trade

Title2A = num2str(nanmean(TotTradeGlobal(i_periodA))/mmt_convert);
Title2B = num2str(nanmean(TotTradeGlobal(i_periodB))/mmt_convert);

%% Losses from Targeted Attacks (pie charts)
figure;
explode = [0 1]; components={'Rest of World';'Targeted Production'};
pie(Prod(1:2),explode); 
%pie(Prod(1:2),explode,components)
%pie3s(Prod(1:2),'Explode',explode)
colormap(cmappie)

title(['Susceptible ' commod_print ' Supply, ' Title1A(1:3) ' mmt'],'FontSize',24)
set(gcf,'PaperPositionMode','auto')
set(gcf,'PaperPositionMode','auto')
set(gcf,'OuterPosition',[100   500   400 400])
set(gcf,'Renderer','painters')
print('-depsc','-painters','-r600',[figure_dir 'Bar_vulnerableprod_1992_1996_'  commod_titleb '.eps' ])

figure;
pie(Prod(3:4),explode)
colormap(cmappie)
title(['Susceptible ' commod_print ' Supply, ' Title1B(1:3) ' mmt'],'FontSize',24)
set(gcf,'PaperPositionMode','auto')
set(gcf,'PaperPositionMode','auto')
set(gcf,'OuterPosition',[100   500   400 400])
set(gcf,'Renderer','painters')
print('-depsc','-painters','-r600',[figure_dir 'Bar_vulnerableprod_2005_2009_'  commod_titleb '.eps' ])

figure;
pie(Export(1:2),explode)
colormap(cmappie)
title(['Susceptible ' commod_print ' Exports, ' Title2A(1:4) ' mmt'],'FontSize',24)
set(gcf,'PaperPositionMode','auto')
set(gcf,'PaperPositionMode','auto')
set(gcf,'OuterPosition',[100   500   400 400])
set(gcf,'Renderer','painters')
print('-depsc','-painters','-r600',[figure_dir 'Bar_vulnerableexport_1992_1996_'  commod_titleb '.eps' ])

figure;
pie(Export(3:4),explode)
colormap(cmappie)
title(['Susceptible ' commod_print ' Exports, ' Title2B(1:4) ' mmt'],'FontSize',24)
set(gcf,'PaperPositionMode','auto')
set(gcf,'PaperPositionMode','auto')
set(gcf,'OuterPosition',[100   500   400 400])
set(gcf,'Renderer','painters')
print('-depsc','-painters','-r600',[figure_dir 'Bar_vulnerableexport_2005_2009_'  commod_titleb '.eps' ])


