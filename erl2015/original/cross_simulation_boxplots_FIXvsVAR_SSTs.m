clear all; close all; clc;
% ASSUMES SAME NUMBER OF YRS AND ENSEMBLES FOR EACH RUN

%% Select Site
i_site = 0;

%% Variables
variable_flag = 0; % 0 => precipitation 1=>temperature
flag_obs = 1;      % 0 -> no observations; 1 -> observations
province_flag = 1;
flag_high = 0;     % Map Features Resolution 
shade_flag = 0;    % 0 for flat and 1 for interp
save_figure=1;     % 0 => no; 1 => yes

%% Select months/years to composite
%mon_comp = [12 1 2]; overwint=1;
%mon_comp = [6 7 8]; overwint=0;
%mon_comp = [3 4 5]; overwint=0;
%mon_comp = [9 10 11]; overwint=0;
%mon_comp = [1]; overwint=0;
%mon_comp = [6]; overwint=0;
mon_comp = [7]; overwint=0;
%mon_comp = [10 11 12 1 2 3]; overwint=1;
%mon_comp = [4 5 6 7 8 9]; overwint=0;
%mon_comp = 1:12; overwint=0;

%% Simulation Comparisons
%F40 balance
anomaly_years1  = 1960:2009; simname1='E_AR5_I01fixSST'; subfolder1 = 'E_AR5_I01fixSST/';simname1_print='IRRIG';
anomaly_years2  = 1960:2009; simname2='E_AR5_C01fixSST'; subfolder2 = 'E_AR5_C01fixSST/';simname2_print='CTRL';
anomaly_years3  = 1880:1929; simname3='E01ISF40nint'; subfolder3 = 'E01ISF40nint/';simname3_print='IRRIG/SST';
anomaly_years4  = 1880:1929; simname4='E01OSF40nint'; subfolder4 = 'E01OSF40nint/';simname4_print='CTRL/SST';

if flag_obs == 1
    cru_years  = 1980:2004;
    cmap_years = 1980:2004;
    gpcp_years = 1980:2004;
end

%% Identify Base Directory
%rootdir = '/Users/michaeljpuma/';
rootdir = '/Users/mpuma/';
rootdirDROPBOX = [rootdir 'Dropbox/IRRIG_SSTmanuscript/'];

%% Identify input/output folders 
folder_feature = [rootdir 'MapFeatures/'];
figure_dir=[rootdirDROPBOX 'figures/SSTonly/'];
data_dir=  [rootdirDROPBOX 'dataprocessed/'];
cru_folder = [rootdir 'CRU_data/'];

if variable_flag == 0
    cmap_folder = [rootdir 'CMAP_data/'];
    gpcp_folder = [rootdir 'GPCP_data/'];
end

%% Variable name
varname='prec'; vartxt='Precip.'; unit_txt='mm/d'; L=[-4 -3 -2 -1 0 1 2 3 4]/4; c_flag=10; variable_bins = 0:2:60; precip_flag=0;
%varname='tsurf'; vartxt='Mean Surf. Temp'; unit_txt='\circC'; L=[-5.5 -4.5 -3.5 -2.5 -1.5 -0.5 0.5 1.5 2.5 3.5 4.5 5.5]; c_flag=19; variable_bins = -20:5:50;
%varname='TMAXC'; vartxt='Max. Surf. Temp'; unit_txt='\circC'; L=[-2 -1.6 -1.2 -.8 -.4 0 .4 .8 1.2 1.6 2]*2; c_flag=11; variable_bins = -20:5:50;
%varname='dtdiurn'; vartxt='DTR'; unit_txt='\circC'; L=[-5.5 -3.5 -1.5 0 1.5 3.5 5.5]; c_flag=19; variable_bins = -20:5:50;
%varname='TMNMX'; vartxt='Surf Temp, Lowest Diurnal High'; unit_txt='\circC'; L=[-2 -1.6 -1.2 -.8 -.4 0 .4 .8 1.2 1.6 2]*2; c_flag=11; variable_bins = -20:5:50;
%varname='irrig_w'; vartxt='Total Irrig'; unit_txt='mm/yr'; L=[0 0.5 1 1.5 2.0]*200; c_flag=4; irrig_minus_control=1;  variable_bins = 0:.2:10; scale = 365;
%varname='sst'; vartxt='Sea Surf. Temp.'; unit_txt='\circC'; L=[-2 -1.6 -1.2 -.8 -.4 0 .4 .8 1.2 1.6 2]*2; c_flag=11; variable_bins = -20:5:50;

%% Map Projection 
map_proj_name='Equidistant Cylindrical';  % Flat proj, usually use
%map_proj_name='Lambert Conformal Conic';  % Good for midlatitudes, about size of US
%map_proj_name='Mollweide';                % shrinks poles, good for global
%map_proj_name='Hammer-Aitoff'; latlim=[-90 90];               % shrinks poles, good for global

%% Colors
color_ocean = rgb('LightSlateGrey');
color_land = rgb('Cornsilk');
color_basin = rgb('DarkGreen');



%% Specifiy Area of Interest for analysis  
% Set flag_basin_shpfile
% 0 - Grid-based dataset (UNH/GRDC basins)
% 1 - Shapefile (uses inpologon)
% 2 - Latitude-Longitude Box
if i_site == 0
    % 0. Global
    area_txt = 'World'; basin_name = 'World'; flag_basin_shpfile = 2;
    lon_region_min = -180; lon_region_max = 180; lat_region_min = -90; lat_region_max = 90; lonmin = -180; lonmax = 180; latmin = -90; latmax = 90;
    basinID =-99; flag_wri = 0;  
elseif i_site == 1
    % 1. Niger River
    area_txt = 'Niger River'; basin_name = 'NigerRiver'; flag_basin_shpfile = 1; lonmin = -20; lonmax = 20; latmin = 0; latmax = 30;
    basinID =115; flag_wri = 1;  file_name = 'wri_basins/wribasin.shp';
elseif i_site == 2
    % % 2. Lake Chad
    area_txt = 'Lake Chad'; basin_name = 'LakeChad'; file_name = 'chadbasin_gen/chadbasin_gen.shp'; flag_basin_shpfile = 1; lonmin = 0; lonmax = 30; latmin = 0; latmax = 30;
    basinID =244; flag_wri = 0;
elseif i_site == 3
    % 3. Nile River
    area_txt = 'Nile River'; basin_name = 'NileRiver'; file_name = 'wri_basins/wribasin.shp'; flag_basin_shpfile = 1; lonmin = 18; lonmax = 48; latmin = -9; latmax = 32;
    basinID =108; flag_wri = 1;
elseif i_site == 4
    % 4. Congo River
    area_txt = 'Congo River'; basin_name = 'CongoRiver'; file_name = 'wri_basins/wribasin.shp'; flag_basin_shpfile = 1; lonmin = 0; lonmax = 50; latmin = -18; latmax = 12;
    basinID =156; flag_wri = 1;
elseif i_site == 5
    % 5. Okavango River
    area_txt = 'Okavango River'; basin_name = 'OkavangoRiver'; file_name = 'Okavango_megabasin/Okavango_basin.shp'; flag_basin_shpfile = 1; lonmin = 6; lonmax = 36; latmin = -36; latmax = -11;
    basinID =209; flag_wri = 0;
elseif i_site == 6
    % 6. Orange-Sengu River
    area_txt = 'Orange-Sengu River'; basin_name = 'OrangeSenguRiver'; file_name = 'wri_basins/wribasin.shp'; flag_basin_shpfile = 1; lonmin = 6; lonmax = 36; latmin = -36; latmax = -11;
    basinID =227; flag_wri = 1;
elseif i_site == 7
    % 7. Dnipro River
    area_txt = 'Dnipro River'; basin_name = 'DniproRiver';file_name = 'wri_basins/wribasin.shp';  flag_basin_shpfile = 1; lonmin = 20; lonmax = 45; latmin = 40; latmax = 60;
    basinID =24; flag_wri = 1;
elseif i_site == 8
    % 8. Tigris-Euphrates River
    area_txt = 'Tigris-Euphrates River'; basin_name = 'TigrisEuphratesRiver'; file_name = 'wri_basins/wribasin.shp'; flag_basin_shpfile = 1; lonmin = 30; lonmax = 60; latmin = 24; latmax = 48;
    basinID =83; flag_wri = 1;
elseif i_site == 9
    % 9. Kura-Aras River
    area_txt = 'Kura-Aras River'; basin_name = 'KuraArasRiver'; flag_basin_shpfile = 1; lonmin = 40; lonmax = 55; latmin = 36; latmax = 48;
    basinID =69; flag_wri = 1; file_name = 'wri_basins/wribasin.shp';
elseif i_site == 10
    % 10. Lake Baikal
    area_txt = 'Lake Baikal'; basin_name = 'LakeBaikal'; flag_basin_shpfile = 1; lonmin = 90; lonmax = 120; latmin = 42; latmax = 62;
    basinID =22; flag_wri = 1; file_name = 'wri_basins/wribasin.shp';
elseif i_site == 11 
    % 11. Lake Titicaca-Rio Mauri
    area_txt = 'Lake Titicaca-Rio Mauri'; basin_name = 'LakeTiticaca'; flag_basin_shpfile = 1; lonmin = -76; lonmax = -64; latmin = -22; latmax = -12;
    basinID =214; flag_wri = 1; file_name = 'wri_basins/wribasin.shp';
elseif i_site == 12
    % 12. Lake Tanganyika **lat-lon box
    area_txt = 'Lake Tanganyika'; basin_name = 'LakeTanganyika'; flag_basin_shpfile = 2;
    lonmin = 20; lonmax = 40; latmin = -14; latmax = 4; lon_region_min = 28; lon_region_max = 33; lat_region_min = -10; lat_region_max = -2; 
    flag_wri = 0; basinID =-99;%156; 
elseif i_site == 13
    % 13. Drin River **lat-lon box
    area_txt = 'Drin River'; basin_name = 'DrinRiver'; flag_basin_shpfile = 2; 
    lon_region_min = 18; lon_region_max = 23; lat_region_min = 40.5; lat_region_max = 43.5; 
    lonmin = 16-2; lonmax = 24+2; latmin = 40; latmax = 45;
    basinID =65; flag_wri = 0;
elseif i_site == 14
    % 14. Lake Prespa **lat-lon box
    area_txt = 'Lake Prespa'; basin_name = 'LakePrespa'; flag_basin_shpfile = 2; 
    lon_region_min = 19.5; lon_region_max = 22.5; lat_region_min = 39.5; lat_region_max = 42.5; 
    lonmin = 16; lonmax = 24; latmin = 38; latmax = 44;
    basinID =-99; flag_wri = 0;
elseif i_site == 15
    % 15. Artibonite River **lat-lon box
    area_txt = 'Artibonite River'; basin_name = 'ArtiboniteRiver'; flag_basin_shpfile = 2; 
    lon_region_min = -73.5; lon_region_max = -70.5; lat_region_min = 17.5; lat_region_max = 20.5; 
    lonmin = -76; lonmax = -68; latmin = 16; latmax = 22;
    basinID =-99; flag_wri = 0;
elseif i_site == 16
    % 16. Caspian Sea ** gridded dataset
    area_txt = 'Caspian Sea'; basin_name='CaspianSea'; flag_basin_shpfile = 0;  lonmin = 32; lonmax = 68; latmin = 32; latmax = 62; basinID = [17 54 69 85 201 215 230 258 307 319 355 359 365 453 479 647 803 867 948 951 971 1215 1287 1315 1443 1476 1482 1506 1521 1527 1529 1534 1538 1570 2098 2102 2111 2127 2140 2152 2164 2174 2183 2191 2227 2268 2314 3863 3864 3865 3891 3930 3949 3978 4063 4079 4086 4116 4145 4146 4174 4234 4315 4316 4340 4361 4404];  %%% CaspianSea: Volga - 17;  Ural - 69; Kura -> 85; Atrek -> 54
    flag_wri = 0;
elseif i_site == 17
    % 17. Mississippi River
    area_txt = 'Mississippi River'; basin_name = 'Mississippi'; file_name = 'wri_basins/wribasin.shp'; flag_basin_shpfile = 1; 
    lonmin = -130; lonmax = -65; latmin = 25; latmax = 52;
    basinID =46; flag_wri = 1;
elseif i_site == 18
    % 18. High Plains Region 1
    area_txt = 'High Plains Region 1'; basin_name = 'HP_Region1'; flag_basin_shpfile = 2;
    %lonmin = -104; lonmax = -98; latmin = 33; latmax = 44; 
    %lon_region_min = -130; lon_region_max = -65; lat_region_min = 25; lat_region_max = 50;
    lon_region_min = -104; lon_region_max = -98; lat_region_min = 33; lat_region_max = 44; 
    lonmin = -130; lonmax = -65; latmin = 25; latmax = 50;
    basinID =-99; flag_wri = 0;
elseif i_site == 19
    % 19. High Plains Region 2
    area_txt = 'High Plains Region 2'; basin_name = 'HP_Region2'; flag_basin_shpfile = 2;
    lon_region_min = -98; lon_region_max = -92; lat_region_min = 36; lat_region_max = 45; lonmin = -130; lonmax = -65; latmin = 25; latmax = 50;
    basinID =-99; flag_wri = 0;
elseif i_site == 20
    % 20. High Plains Region 3
    area_txt = 'High Plains Region 3'; basin_name = 'HP_Region3'; flag_basin_shpfile = 2;
    lon_region_min = -92; lon_region_max = -85; lat_region_min = 36; lat_region_max = 45; 
    lonmin = -130; lonmax = -65; latmin = 25; latmax = 50;
    basinID =-99; flag_wri = 0;
elseif i_site == 21
    % 21. High Plains Region 4
    area_txt = 'High Plains Region 4'; basin_name = 'HP_Region4'; flag_basin_shpfile = 2;
    lon_region_min = -98; lon_region_max = -85; lat_region_min = 30; lat_region_max = 36; 
    lonmin = -130; lonmax = -65; latmin = 25; latmax = 50;
    basinID =-99; flag_wri = 0;
elseif i_site == 22
    % 22. Yangtze River
    area_txt = 'Yangtze River'; basin_name = 'Yangtze'; file_name = 'wri_basins/wribasin.shp'; flag_basin_shpfile = 1; 
    lonmin = 85; lonmax = 125; latmin = 20; latmax = 40;
    basinID =97; flag_wri = 1;
elseif i_site == 23
    % 23. Colorado River
    area_txt = 'Colorado River'; basin_name = 'Colorado'; file_name = 'wri_basins/wribasin.shp'; flag_basin_shpfile = 1; 
    lonmin = -130; lonmax = -65; latmin = 25; latmax = 52;
    basinID =59; flag_wri = 1;
elseif i_site == 24
    % 24. Ganges-Brahmaputra River
    area_txt = 'Ganges-Brahmaputra River'; basin_name = 'Ganges-Brahmaputra'; file_name = 'wri_basins/wribasin.shp'; flag_basin_shpfile = 1; 
    lonmin = 70; lonmax = 105; latmin = 5; latmax = 35;
    basinID =107; flag_wri = 1;    
elseif i_site == 25
    % 25. Hwang Ho River
    area_txt = 'Yellow River (Huang He)'; basin_name = 'Yellow'; file_name = 'wri_basins/wribasin.shp'; flag_basin_shpfile = 1; 
    lonmin = 90; lonmax = 125; latmin = 30; latmax = 50;
    basinID =72; flag_wri = 1;    
 
end


%% Place plotting limits into vector
lonlim=[lonmin lonmax];
latlim=[latmin latmax];


%% Physical and cultural features for plots
if flag_high == 1
    country = shape_read([folder_feature '10m_cultural/10m_cultural/ne_10m_admin_0_countries.shp']);
    provinces = shape_read([folder_feature '10m_cultural/10m_cultural/ne_10m_admin_1_states_provinces_lines_shp.shp']);
    rivers_lakes = shape_read([folder_feature '10m_physical/ne_10m_rivers_lake_centerlines.shp']);
    lakes = shape_read([folder_feature '10m_physical/ne_10m_lakes.shp']);
    ocean = shape_read([folder_feature '10m_physical/ne_10m_ocean.shp']);
    land = shape_read([folder_feature '10m_physical/ne_10m_land.shp']);
    coastline = shape_read([folder_feature '10m_physical/ne_10m_coastline.shp']);
elseif flag_high == 0
    country = shape_read([folder_feature '50m_cultural/ne_50m_admin_0_countries.shp']);
    provinces = shape_read([folder_feature '50m_cultural/ne_50m_admin_1_states_provinces_lines.shp']);
    rivers_lakes = shape_read([folder_feature '50m_physical/ne_50m_rivers_lake_centerlines.shp']);
    lakes = shape_read([folder_feature '50m_physical/ne_50m_lakes.shp']);
    ocean = shape_read([folder_feature '50m_physical/ne_50m_ocean.shp']);
    land = shape_read([folder_feature '50m_physical/ne_50m_land.shp']);
    coastline = shape_read([folder_feature '50m_physical/ne_50m_coastline.shp']);
end

%% Define Watershed
if (flag_basin_shpfile == 0) % Grid-based dataset (UNH/GRDC basins)
    % Allowed offset from grid cell centers
    basin_off=0.25; % count model cell if center +/- one quarter of model resolution is in watershed
    %basin_off=0;    % center MUST be in watershed
    
    % load basin data and create
    basins=flipud(load([folder_feature 'watershed/basin.grd'])); basins(find(basins<=-900))=nan;
    lat_basin=[-55.25:0.5:82.75]; lon_basin=[-179.75:0.5:179.75];
    
    % DISABLE:Change map orientation to match CMIP5 models
    basin_new=basins;%[basins(:,361:720) basins(:,1:360)];
    lon_basin_new=lon_basin;%+180;
    
    % Mask anything outside chosen basin
    basin_temp = zeros(size(basin_new));
    for i_basin =1:size(basinID,2)
        basin_temp(find(basin_new==basinID(i_basin)))=1;
    end
    basin_new(find(basin_temp~=1))=nan;
    basin_new(find(basin_temp==1))=1;
    
  
    %% Plot river basin
    figure;
    hold on;
    m_proj(map_proj_name, 'lon', lonlim, 'lat', latlim); shading flat;
    if i_site ~=4 && i_site ~=8 && i_site ~= 7
        %m_patch(ocean.x,ocean.y,color_ocean,'FaceAlpha',1);
    end
    %m_patch(land.x,land.y,color_land,'FaceAlpha',1);hold off
    hold on
    m_plot(country.x,country.y,'color',[.2 .2 .2]);
    m_pcolor(lon_basin_new,lat_basin,basin_new); shading flat; cmap=colormap_function(4); colormap(cmap);%m_patch(basin_shp.X,basin_shp.Y,color_basin,'FaceAlpha',1,'line','none');
    m_line(rivers_lakes.x,rivers_lakes.y);
    m_grid('box','on','linestyle','none','fontsize',16,'fontname','helvetica','FontWeight','bold');
    title(['\fontname{helvetica} \fontsize{18} \bf{' area_txt '}']);
    hold off
    print('-depsc','-painters','-r600',[figure_dir area_txt '_Basin_Map.eps'])

    
elseif(flag_basin_shpfile == 1) % Shapefile (uses inpologon)
    if flag_wri == 1
        basin_shp_all = shaperead([folder_feature  file_name]);
        basin_shp = basin_shp_all(basinID);
    elseif flag_wri == 0
        basin_shp = shaperead([folder_feature  file_name]);
    end
    
    %% Plot river basin
    figure;
    hold on;
    m_proj(map_proj_name, 'lon', lonlim, 'lat', latlim); shading flat;
    if i_site ~=4 && i_site ~=8 && i_site ~= 7
        %m_patch(ocean.x,ocean.y,color_ocean,'FaceAlpha',1);
    end
    m_patch(land.x,land.y,color_land,'FaceAlpha',1);
    m_plot(country.x,country.y,'color',[.2 .2 .2]);
    m_patch(basin_shp.X,basin_shp.Y,color_basin,'FaceAlpha',1,'line','none');
    m_line(rivers_lakes.x,rivers_lakes.y);
    m_grid('box','on','linestyle','none','fontsize',16,'fontname','helvetica','FontWeight','bold');
    title(['\fontname{helvetica} \fontsize{18} \bf{' area_txt '}']);
    hold off
    print('-depsc','-painters','-r600',[figure_dir area_txt '_Basin_Map.eps'])

elseif(flag_basin_shpfile == 2) % Latitude-Longitude Box
    bndry_lat = [lat_region_min; lat_region_max;lat_region_max;lat_region_min;lat_region_min];
    bndry_lon = [lon_region_min;lon_region_min;lon_region_max;lon_region_max;lon_region_min];
    
    % Figure: Basin check 1
    %figure; pcolor(lon_data,lat_data,landfrac.*basin_mask),title('model land fraction, basin mask'),colorbar, grid off;
    % Figure: Basin check 2
    figure; 
    hold on; 
    m_proj(map_proj_name, 'lon', lonlim, 'lat', latlim); shading flat;
    m_plot(country.x,country.y,'color',[.2 .2 .2]);
    if i_site~=0
        m_patch(bndry_lon,bndry_lat,[0.1294,0.4431,0.7098],'FaceColor','none','EdgeColor',[0.1294,0.4431,0.7098],'LineWidth',4);
    end
    m_grid('box','on','linestyle','none','fontsize',16,'fontname','helvetica','FontWeight','bold');
    title(['\fontname{helvetica} \fontsize{18} \bf{' area_txt '}']);
    hold off
    print('-depsc','-painters','-r600',[figure_dir area_txt '_BoxRegion_Map.eps'])
    
end

%% Plot vectors
%CS = {'k','b','r','g','m','c'};%,'y'
CS =[0.1059,0.6196,0.4667;...
  0.8510,0.3725,0.0078;...
  0.4588,0.4392,0.7020;...
  0.9059,0.1608,0.5412;...
  0.4000,0.6510,0.1176;...
  0.9020,0.6706,0.0078;...
  0.6510,0.4627,0.1137;...
  0.4000,0.4000,0.4000];
MS={'o','s','d','+','*','s','x','o'};%'+','*','x','^','>','<','p','h'};
LS = {'-.','--','-',':'};

%% Get colormap for plots
cmap=colormap_function(c_flag);
%cmapdiff=colormap_function(c_flagdiff);

% Create a monthly name vector--------------------------------------------
monthName = {...
'JAN'
'FEB'
'MAR'	
'APR'	
'MAY'	
'JUN'	
'JUL'	
'AUG'	
'SEP'	
'OCT'	
'NOV'		
'DEC'};

%% Names and titles
for s=1:length(mon_comp)
   tempMon=monthName{mon_comp(s)};
   seas(s)=tempMon(1);
end

if length(mon_comp)==12
    seas = 'ANNUAL';
elseif length(mon_comp)==1
    seas =monthName{mon_comp};
end

%% Load simulations and set missing data to NaN
% Fixed SSTs
filename=[varname '_' simname1 '_' simname2 '_' num2str(anomaly_years1(1)) num2str(anomaly_years1(end)) '_' seas '.mat'];
fixedSST=load([data_dir filename]);
fixedSST.outIRRI.anom_irrig(fixedSST.outIRRI.anom_irrig<-1e29)=NaN;
fixedSST.outCTRL.anom_ctrl(fixedSST.outCTRL.anom_ctrl<-1e29)=NaN;

% Variable SSTs
filename=[varname '_' simname3 '_' simname4 '_' num2str(anomaly_years3(1)) num2str(anomaly_years3(end)) '_' seas '.mat'];
varSST=load([data_dir filename]);
varSST.outIRRI.anom_irrig(varSST.outIRRI.anom_irrig<-1e29)=NaN;
varSST.outCTRL.anom_ctrl(varSST.outCTRL.anom_ctrl<-1e29)=NaN;

%% Latitude and Longitude data
lon_data=fixedSST.outCTRL.lon; longitude = fixedSST.outCTRL.lon; num_lon = length(longitude);
lat_data=fixedSST.outCTRL.lat; latitude  = fixedSST.outCTRL.lat; num_lat = length(latitude);
        
% Offset by half a grid point
delta_plot_lon=nanmean(diff(lon_data))/2;
delta_plot_lat=nanmean(diff(lat_data))/2;

%% Create Basin Mask
if flag_basin_shpfile == 0 % Gridded basin dataset
    
    % Create New Dummy Mask of ones
    basin_mask=nan(length(lat_data),length(lon_data));
    
    % find lat/lons of basin cells at original basin resolution.
    [lat_b,lon_b]=find(isnan(basin_new)==0);
    
    % Allowed offset (if zero, no allowed offset of model grid cell center
    lat_half_mod=nanmean(diff(lat_data)).*basin_off;
    lon_half_mod=nanmean(diff(lon_data)).*basin_off;
    
    for i_basin_cell=1:length(lon_b)
        % Find overlapping model grid cells
        lon_loc=find((lon_data+lon_half_mod)>=(lon_basin_new(lon_b(i_basin_cell))-.25) & (lon_data-lon_half_mod)<=(lon_basin_new(lon_b(i_basin_cell))+.25));
        lat_loc=find((lat_data+lat_half_mod)>=(lat_basin(lat_b(i_basin_cell))-.25) & (lat_data-lat_half_mod)<=(lat_basin(lat_b(i_basin_cell))+.25));
        
        if (isempty(lon_loc)==0 && isempty(lat_loc)==0 )
            
            for i_lon_fill=1:length(lon_loc)
                for i_lat_fill=1:length(lat_loc)
                    basin_mask(lat_loc(i_lat_fill),lon_loc(i_lon_fill))=1;
                end
            end
        end
    end
    
    
elseif (flag_basin_shpfile == 1) % Shapefile of basin
    % Create New Dummy Mask of ones
    basin_mask=nan(length(lat_data),length(lon_data));
    
    % Clear variables from previous model
    clear lonlat lonlat_loc lonlat_loc_ind lat_data_cut lon_data_cut
    clear lonlat_loc_ind lat_data_cut lon_data_cut
    
    % Place lat-lon in array for inpolygon
    jj = 1;
    for i=1:length(lon_data)
        for j=1:length(lat_data)
            lonlat(jj,1) = lon_data(i);
            lonlat(jj,2) = lat_data(j);
            jj = jj+1;
        end
    end
    
    % Find grid cells within basin polygon
    [IN ON] = inpolygon(lonlat(:,1),lonlat(:,2),basin_shp.X,basin_shp.Y);
    lonlat_loc = IN+ON; clear IN ON;
    lonlat_loc_ind=find(lonlat_loc>0); lonlat_loc_ind(isnan(lonlat_loc_ind))=[];
    lon_data_cut(:,1) = lonlat(lonlat_loc_ind,1);
    lat_data_cut(:,1) = lonlat(lonlat_loc_ind,2);
    for i = 1:size(lonlat_loc_ind,1)
        basin_mask(find(lat_data == lat_data_cut(i)),find(lon_data == lon_data_cut(i)))=1;
    end   
    
    
elseif (flag_basin_shpfile == 2) % lat-lon
    % Create New Dummy Mask of ones
    basin_mask=nan(length(lat_data),length(lon_data));
    
    % Clear variables from previous model
    clear lonlat lonlat_loc lonlat_loc_ind lat_data_cut lon_data_cut
    clear lonlat_loc_ind lat_data_cut lon_data_cut
    
    % Place lat-lon in array for inpolygon
    jj = 1;
    for i=1:length(lon_data)
        for j=1:length(lat_data)
            lonlat(jj,1) = lon_data(i);
            lonlat(jj,2) = lat_data(j);
            jj = jj+1;
        end
    end
    
    % Find grid cells within basin polygon
    [IN ON] = inpolygon(lonlat(:,1),lonlat(:,2),bndry_lon,bndry_lat);
    lonlat_loc = IN+ON; clear IN ON;
    lonlat_loc_ind=find(lonlat_loc>0); lonlat_loc_ind(isnan(lonlat_loc_ind))=[];
    lon_data_cut(:,1) = lonlat(lonlat_loc_ind,1);
    lat_data_cut(:,1) = lonlat(lonlat_loc_ind,2);
    for i = 1:size(lonlat_loc_ind,1)
        basin_mask(find(lat_data == lat_data_cut(i)),find(lon_data == lon_data_cut(i)))=1;
    end
    
end

%% Compute number of years, ensembles, and grid cells
totalnumber_ens = size(fixedSST.outCTRL.anom_ctrl,1);
totalnumber_yrs = size(fixedSST.outCTRL.anom_ctrl,2);
totalnumber_cells = size(fixedSST.outCTRL.anom_ctrl,3)*size(fixedSST.outCTRL.anom_ctrl,4); %lat*lon

%% Mask diagnostic data
for iens = 1:totalnumber_ens
    for iyr = 1:totalnumber_yrs
        fixedSST.outCTRL.anom_ctrl(iens,iyr,:,:)=squeeze(fixedSST.outCTRL.anom_ctrl(iens,iyr,:,:)).*basin_mask;
        fixedSST.outIRRI.anom_irrig(iens,iyr,:,:)=squeeze(fixedSST.outIRRI.anom_irrig(iens,iyr,:,:)).*basin_mask;
        varSST.outCTRL.anom_ctrl(iens,iyr,:,:)=squeeze(varSST.outCTRL.anom_ctrl(iens,iyr,:,:)).*basin_mask;
        varSST.outIRRI.anom_irrig(iens,iyr,:,:)=squeeze(varSST.outIRRI.anom_irrig(iens,iyr,:,:)).*basin_mask;
    end
end


%% Averages: 1) average of all years for each ensemble (i.e., one map for each ensemble)
for ilat = 1:num_lat
    for ilon = 1:num_lon
        % Fixed SST, no irrigation
        temp = squeeze(fixedSST.outCTRL.anom_ctrl(:,:,ilat,ilon));
        CTRL_spatialavg_allyrs_fixedSST(ilat,ilon)=nanmean(temp(:)); 
        CTRL_spatialvar_allyrs_fixedSST(ilat,ilon)=nanvar(temp(:)); 
        clear temp;
        
        % Fixed SST, irrigation        
        temp = squeeze(fixedSST.outIRRI.anom_irrig(:,:,ilat,ilon));
        IRRI_spatialavg_allyrs_fixedSST(ilat,ilon)=nanmean(temp(:));
        IRRI_spatialvar_allyrs_fixedSST(ilat,ilon)=nanvar(temp(:));  
        clear temp;
        
        % Variable SST, no irrigation       
        temp = squeeze(varSST.outCTRL.anom_ctrl(:,:,ilat,ilon));
        CTRL_spatialavg_allyrs_varSST(ilat,ilon)=nanmean(temp(:));
        CTRL_spatialvar_allyrs_varSST(ilat,ilon)=nanvar(temp(:));
        clear temp;
        
        % Variable SST, irrigation        
        temp = squeeze(varSST.outIRRI.anom_irrig(:,:,ilat,ilon));
        IRRI_spatialavg_allyrs_varSST(ilat,ilon)=nanmean(temp(:));
        IRRI_spatialvar_allyrs_varSST(ilat,ilon)=nanvar(temp(:));
        clear temp;
        
    end
end


%% Averages: 2) average only in space (i.e. lat-lon) to get a single spatial value (i.e. do not average years; do not average ensembles)
% RESHAPE and compute mean values
for iens = 1:totalnumber_ens
    for iyr = 1:totalnumber_yrs
        
        temp = squeeze(fixedSST.outCTRL.anom_ctrl(iens,iyr,:,:));
        CTRL_timeavg_allyrs_fixedSST(iens,iyr)=nanmean(temp(:)); clear temp;
        temp = squeeze(fixedSST.outIRRI.anom_irrig(iens,iyr,:,:));
        IRRI_timeavg_allyrs_fixedSST(iens,iyr)=nanmean(temp(:));  clear temp;
        temp = squeeze(varSST.outCTRL.anom_ctrl(iens,iyr,:,:));
        CTRL_timeavg_allyrs_varSST(iens,iyr)=nanmean(temp(:));  clear temp;
        temp = squeeze(varSST.outIRRI.anom_irrig(iens,iyr,:,:));
        IRRI_timeavg_allyrs_varSST(iens,iyr)=nanmean(temp(:));  clear temp;
        
    end
end


%% Figures: Timeseries
figure;
hold on;
plot(1:length(anomaly_years1), CTRL_timeavg_allyrs_fixedSST,'LineStyle','-','LineWidth',1.5,'Marker','none','MarkerSize',10,...
    'MarkerFaceColor','k','Color','k')
plot(1:length(anomaly_years1), CTRL_timeavg_allyrs_varSST,'LineStyle','-','LineWidth',1.5,'Marker','none','MarkerSize',10,...
    'MarkerFaceColor','r','Color','r')
%hy = graph2d.constantline(0, 'Color','k','LineStyle','--');  changedependvar(hy,'y')  %# horizontal line
set(get(gcf,'CurrentAxes'),'FontSize',18)
xlabel('Year','FontSize',16);
ylabel([unit_txt],'FontSize',16);
title(['\fontname{helvetica} \fontsize{18}' vartxt ' (' seas ')']);
%legend( {simname2_print,simname4_print},'Location','Eastoutside','FontSize',12)
box on
hold off
set(gcf,'PaperPositionMode','auto')
set(gcf,'OuterPosition',[100   500   800 400])
set(gcf,'Renderer','painters')
if save_figure>0
   print('-depsc','-painters','-r600',[figure_dir area_txt '_' varname 'FixedVaribleSST_timeseries.' seas '_50yrs.eps' ])
   %print('-dpdf','-painters',[figure_dir varname 'FixedSST_timeseries.' seas '_50yrs.pdf' ])
end

% BOXPLOT
%color1 = [0.7490,0.5059,0.1765]; cmapbar1 = [color1*.6; color1]; 
%cmapbar2 = [color1*.6; color1; 0.8 0.8 0.8]; cmapbar2(cmapbar2>1)=1;
%boxcolor=flipud(cmapbar1);
periodlabel{1} = 'Fixed';
periodlabel{2} = 'Variable';
figure;
hold on;
my_boxvalues = [CTRL_timeavg_allyrs_fixedSST(:), CTRL_timeavg_allyrs_varSST(:)];
hl = boxplot(my_boxvalues,'Colors',['k','r'],'notch','off');
set(get(gcf,'CurrentAxes'),'FontSize',18)
for ih=1:6
    set(hl(ih,:),'LineWidth',3);
end
set(gca,'XLim',[0 3],'XTick',[1 2],'XTickLabel',periodlabel)
ylabel([unit_txt],'FontSize',18);
%legend(findobj(gca,'Tag','Box'),'IRRIG','CTRL')%%note sure why this is flipped...
title(['\fontname{helvetica} \fontsize{18}' vartxt ' (' seas ')']);
set(gcf,'Renderer','painters')
set(gcf,'PaperPositionMode','auto')
hold off
set(gcf,'PaperPositionMode','auto')
set(gcf,'OuterPosition',[100   500   400 400])
set(gcf,'Renderer','painters')
if save_figure>0
   print('-depsc','-painters','-r600',[figure_dir area_txt '_' varname 'boxplot_fixedvar_' seas '_50yrs.eps' ])
   %print('-dpdf','-painters',[figure_dir varname 'VarSST_timeseries.' seas '_50yrs.pdf' ])
end

%% Figures: Maps 
% Mean difference maps
diffmap = CTRL_spatialavg_allyrs_varSST-CTRL_spatialavg_allyrs_fixedSST;
figure
colormap(cmap)
m_proj(map_proj_name, 'lon', lonlim, 'lat', latlim);   
hold on
m_pcolor(longitude-delta_plot_lon/2, latitude-delta_plot_lat/2, diffmap)
if shade_flag == 0
    shading flat
elseif shade_flag == 1
    shading interp
end

if province_flag == 1
    m_plot(provinces.x,provinces.y,'color',[.5 .5 .5]); 
end
%m_patch(lakes.x,lakes.y,'w','FaceAlpha',1,'EdgeColor', [.5 .5 .5]);
m_plot(country.x,country.y,'color',[.5 .5 .5]);

caxis([min(L) max(L)]);
m_grid('linestyle', 'none','fontsize',16,'fontname','helvetica','FontWeight','bold');
colorbar1 = colorbar('FontName','Times',...
 'FontSize',16,...
 'FontWeight','bold',...  
  'XLim',[-0.5 1.5],'LOCATION','EastOutside','Ytick',L(1:length(L)),'YTicklabel',L(1:length(L)),'fontname','helvetica');
title(['\fontname{helvetica} \fontsize{18}' vartxt ', Diff. in Mean (VAR - FIX)']);
set(gcf,'Renderer','painters')
%set(gcf,'OuterPosition',[100   100   800   400]) %left edge botton edge; width height
set(gcf,'PaperPositionMode','auto')

if save_figure>0
   print('-depsc','-painters','-r600',[figure_dir area_txt '_' varname 'mean_FixedMinusVarSST_diffmap_' seas '_50yrs.eps' ])
end

% Variance difference maps
diffmap = CTRL_spatialvar_allyrs_varSST-CTRL_spatialvar_allyrs_fixedSST;
figure
colormap(cmap)
m_proj(map_proj_name, 'lon', lonlim, 'lat', latlim);   
hold on
m_pcolor(longitude-delta_plot_lon/2, latitude-delta_plot_lat/2, diffmap)
if shade_flag == 0
    shading flat
elseif shade_flag == 1
    shading interp
end

if province_flag == 1
    m_plot(provinces.x,provinces.y,'color',[.5 .5 .5]); 
end
%m_patch(lakes.x,lakes.y,'w','FaceAlpha',1,'EdgeColor', [.5 .5 .5]);
m_plot(country.x,country.y,'color',[.5 .5 .5]);

caxis([min(L) max(L)]);
m_grid('linestyle', 'none','fontsize',16,'fontname','helvetica','FontWeight','bold');
colorbar1 = colorbar('FontName','Times',...
 'FontSize',16,...
 'FontWeight','bold',...  
  'XLim',[-0.5 1.5],'LOCATION','EastOutside','Ytick',L(1:length(L)),'YTicklabel',L(1:length(L)),'fontname','helvetica');
title(['\fontname{helvetica} \fontsize{18}' vartxt ', Diff. in Variance (VAR - FIX)']);
set(gcf,'Renderer','painters')
%set(gcf,'OuterPosition',[100   100   800   400]) %left edge botton edge; width height
set(gcf,'PaperPositionMode','auto')

if save_figure>0
   print('-depsc','-painters','-r600',[figure_dir area_txt '_' varname 'variance_FixedMinusVarSST_diffmap_' seas '_50yrs.eps' ])
end
return;







