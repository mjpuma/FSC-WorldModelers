% Global_trade_network_twocommodities.m
% THIS CODE COMPUTES STATISTICS FOR TWO COMMODITIES
% Reads in annual bilateral trade network and computes complex network
% statistics after converting to matrix
% M. J. Puma, 2012 to 2014

clear all; close all; clc;

%% Folder specifications
%rootdir = '/Users/michaeljpuma/';
rootdir = '/Users/mpuma/';

basefold = [rootdir 'ComplexNetworkFood/'];
figure_dir=[rootdir 'ComplexNetworkFood/Figures/Fragile/'];

%% Simulation/analysis flags
save_figure = 1;
mmt_convert = 1000000;

%% Years to plot
year_start = 1986; year_end = 2009;
years = year_start:year_end;
num_years = year_end-year_start+1;
offset_yr = 1992-1986; % because data starts in 1986
i_periodA=find(years>=1992 & years<=1996);
i_periodB=find(years>=2005 & years<=2009);
periodlabel{1} = '1992-1996';
periodlabel{2} = '2005-2009';
 
%% Open commodity list
commod_title='wheat'; commod_print = 'Wheat';
commod_title2='rice'; commod_print2 = 'Rice';
 
%% Flow Type (i.e. virtual water, tons of commoditites, etc)
%flow_type = 'green'; flow_title = 'Green Virtual Water'; flow_units = 'm^3';
%flow_type = 'blue'; flow_title = 'Blue Virtual Water'; flow_units = 'm^3';
flow_type = 'tonscropeq'; flow_title = 'Weight'; flow_units = 'metric tons';

%% Element to plot
i_element = 4; 
element_vec = {...
'Import_Value'
'Export_Value'
'Import_Quantity'
'Export_Quantity'};
element = element_vec{i_element};


%% Open country list
[country_codes,country_names,country_info] = xlsread([basefold 'Input_data/countrylist_partialv2.xls']);


%% Open country list for various UN categories
% SOCIOECONOMIC GROUPS
country_title1= 'Least Developed Countries'; country_print1 = 'LDC'; [countrysubset_codes1,countrysubset_names1,countrysubset_info1] = xlsread([basefold 'Input_data/countrylist_leastDCnoDjibouti.xls']);
country_title2= 'Annex I Countries'; country_print2 = 'AnnexI'; [countrysubset_codes2,countrysubset_names2,countrysubset_info2] = xlsread([basefold 'Input_data/countrylist_annex1.xls']); 
%country_title1= 'G8 Countries'; country_print1 = 'G8'; [countrysubset_codes1,countrysubset_names1,countrysubset_info1] = xlsread([basefold 'Input_data/countrylist_G8noUSSR.xls']); 
%country_title2= 'Plus 5 Countries'; country_print2 = 'Plus5'; [countrysubset_codes2,countrysubset_names2,countrysubset_info2] = xlsread([basefold 'Input_data/countrylist_plus5.xls']); 
%country_title3= 'Low-Income Food-Deficit Countries'; country_print3 = 'LIFDC'; [countrysubset_codes3,countrysubset_names3,countrysubset_info3] = xlsread([basefold 'Input_data/countrylist_lowincomefooddeficit.xls']); 

number_subsetCountries1 = length(countrysubset_codes1);
number_subsetCountries2 = length(countrysubset_codes2);

%% Conversion table for country codes
[~, ~, raw] = xlsread([basefold 'Input_data/country_code_CONVERTv2.xls'],'Sheet1');
raw = raw(2:end,2:end);
data = reshape([raw{:}],size(raw)); %% Create output variable

% Allocate imported array to column variable names
Index = data(:,1);
FAOSTATcode = data(:,2);
ISOcode = data(:,4);

% Clear temporary variables
clearvars data raw;


%% Initialize correlation matrix
Graph_g = GraphLoadFromFile([basefold 'Matrices/Trade_Graph_' commod_title '_' flow_type '_' element '_' num2str(year_start) '_' num2str(year_start) '.mat']);
Graph_b = GraphLoadFromFile([basefold 'Matrices/Trade_Graph_' commod_title2 '_' flow_type '_' element '_' num2str(year_start) '_' num2str(year_start) '.mat']);
 
rank_matrix_g =zeros(length(Graph_g.Data), size(year_start:year_end,2));
rank_matrix_b =zeros(length(Graph_b.Data), size(year_start:year_end,2));
clear Graph_g Graph_b;
 
%% Year Loop
i_year = 0;
for year = year_start:year_end
    year
    i_year = i_year + 1; % year index
    
    %% Read in network from file
    Graph_g = GraphLoadFromFile([basefold 'Matrices/Trade_Graph_' commod_title '_' flow_type '_' element '_' num2str(year) '_' num2str(year) '.mat']);
    Graph_b = GraphLoadFromFile([basefold 'Matrices/Trade_Graph_' commod_title2 '_' flow_type '_' element '_' num2str(year) '_' num2str(year) '.mat']);
 
    %% Identify the links
    LinkID_g = (1:length(Graph_g.Data))';
    LinkID_b = (1:length(Graph_b.Data))';
    
    %% Place data in a matrix containing all years
    rank_matrix_g(:,i_year)=Graph_g.Data(:,3);
    rank_matrix_b(:,i_year)=Graph_b.Data(:,3);    
    
    %% WD is the directed trade matrix: virtual water trade network forms a
    % weighted, directed network; WD is not symmetric and W(i, i) = 0; that is,
    % a country cannot trade with itself.
    WD_g = zeros(max(Graph_g.Index.Properties.NodeIDs));
    WD_b = zeros(max(Graph_b.Index.Properties.NodeIDs));
    
    % goes through
    for i_link = 1: length(Graph_g.Data)
        WD_g(Graph_g.Data(i_link,1),Graph_g.Data(i_link,2))=Graph_g.Data(i_link,3);
    end
 
    for i_link = 1: length(Graph_b.Data)
        WD_b(Graph_b.Data(i_link,1),Graph_b.Data(i_link,2))=Graph_b.Data(i_link,3);
    end
    
    
    %% Check diagonal is zero
    if sum(diag(WD_g)) >0
        'error in diagonal 1' %#ok<NOPTS>
        return   
    elseif sum(diag(WD_b)) >0
        'error in diagonal 1' %#ok<NOPTS>
        return
    end
    
    %% Remove non-existing links (i.e., links with zero weight)
    LinkID_g(Graph_g.Data(:,3)==0,:)=[];
    Graph_g.Data(Graph_g.Data(:,3)==0,:)=[];
    LinkID_b(Graph_b.Data(:,3)==0,:)=[];
    Graph_b.Data(Graph_b.Data(:,3)==0,:)=[];
    
    %% Remove inactive nodes
    Graph_g=GraphRemoveUnlinkedNodes(Graph_g);
    Graph_b=GraphRemoveUnlinkedNodes(Graph_b);
    
    %% Weighted, undirected network (WU): created by symmetrizing the directed,
    % weighted network on the basis of the sum of the link weights between
    % two nodes. This symmetrization creates an undirected network
    % with at most a single link between any two nodes
    L_g = tril(WD_g,-1); U_g = triu(WD_g,1); 
    WU_g = WD_g+transpose(L_g)+transpose(U_g); % Weighted, undirected network (WU)
    AD_g = WD_g>0; % Unweighted, directed network (AD)
    AU_g = WU_g>0; % Unweighted, undirected network (AU)
    
    L_b = tril(WD_b,-1); U_b = triu(WD_b,1); 
    WU_b = WD_b+transpose(L_b)+transpose(U_b); % Weighted, undirected network (WU)
    AD_b = WD_b>0; % Unweighted, directed network (AD)
    AU_b = WU_b>0; % Unweighted, undirected network (AU)
    
    %% Compute the number of links in the network (import or export)
    NumLinks_g(i_year) = sum(AD_g(:));
    NumLinks_b(i_year) = sum(AD_b(:));
    
    %% Number of nodes in graph (sum rows of undirected network WU, to identify active nodes)
    NumNodes_g(i_year) = nnz(sum(WU_g,1));
    NumNodes_b(i_year) = nnz(sum(WU_b,1));
    
    NumNodes_g_out(i_year) = nnz(sum(WD_g,2)); % number of exporting nodes
    NumNodes_g_in(i_year) = nnz(sum(WD_g,1));  % number of importing nodes
    NumNodes_b_out(i_year) = nnz(sum(WD_b,2)); % number of exporting nodes
    NumNodes_b_in(i_year) = nnz(sum(WD_b,1));  % number of importing nodes
     
    %% Total amount export globally 
    TotTradeGlobal_g(i_year) = sum(WD_g(:));
    TotTradeGlobal_b(i_year) = sum(WD_b(:));

     %% Strength, si, is calculated as the sum of all the IMPORTING fluxes from
    % that node (i.e., as the row sum in the trade matrix)
    strength_g(:,i_year) = nansum(WU_g,1);
    strength_b(:,i_year) = nansum(WU_b,1);  

 %% Strength, si, is calculated as the sum of all the EXPORTING fluxes from
    % that node (i.e., as the row sum in the trade matrix)
    strengthEXPORT_g(:,i_year) = nansum(WD_g,2);
    strengthEXPORT_b(:,i_year) = nansum(WD_b,2); 
  
    %% Strength, si, is calculated as the sum of all the IMPORTING fluxes from
    % that node (i.e., as the row sum in the trade matrix)
    strengthIMPORT_g(:,i_year) = nansum(WD_g,1);
    strengthIMPORT_b(:,i_year) = nansum(WD_b,1);  
    
    %% Compute degrees in network for exports in directed network
    k_D_out_g(:,i_year) = nansum(AD_g,2);  % sum each row for export
    k_D_out_b(:,i_year) = nansum(AD_b,2);  
       
    k_D_in_g(:,i_year) = nansum(AD_g,1);  % sum each column for import
    k_D_in_b(:,i_year) = nansum(AD_b,1);  

    k_D_g(:,i_year) = nansum(AU_g,1);  % sum each row or column
    k_D_b(:,i_year) = nansum(AU_b,1);  
    if nansum(AU_g,1) ~= nansum(AU_g,2)'
        ['Error with the undirected network'] %#ok<NBRAK,NOPTS>
        break
    end
    
   %% Average nearest neighbor degree
   % First dataset
   for i =1:size(k_D_out_g,1)
       if sum(AD_g(i,:)==1)>0
           % Find indices of active trade partners
           j_indices = find(AD_g(i,:)==1);
           
           %%%% remove j_indices == 1
           %%j_indices(find(j_indices==i))=[];
           
           sum_kj = sum(k_D_out_g(j_indices,i_year));
           knn_D_out_g(i,i_year) = sum_kj/k_D_out_g(i);
       else
           knn_D_out_g(i,i_year) = 0;
       end
   end
   

   
    %% Concentration index(for non fractions, >1 if similar weights... approaches 1 as you hav a donimant node
    Iconcentrate_g(:,i_year) =  (nansum(WD_g,1).^2)'./(nansum(WD_g.^2,1))'; % sum each column for import  
    Iconcentrate_b(:,i_year) =  (nansum(WD_b,1).^2)'./(nansum(WD_b.^2,1))'; % sum each column for import
 
    % Sort Concentration index
    temp = Iconcentrate_g(:,i_year); temp(isnan(temp))=-9999; [Bg,IXg] = sort(temp,'descend');
    temp = Iconcentrate_b(:,i_year); temp(isnan(temp))=-9999; [Bb,IXb] = sort(temp,'descend');
 
    % Save Concentration index and country names
    for i = 1:length(country_names)
        Iconcentrate_rank_g.name{i,i_year} = country_names{IXg(i)};
        Iconcentrate_rank_g.value(i,i_year) = Bg(i);
        
        Iconcentrate_rank_b.name{i,i_year} = country_names{IXb(i)};
        Iconcentratel_rank_b.value(i,i_year) = Bb(i);
    end    
       
    %% Control index
    for i = 1:size(WD_g,1) % rows
        for j = 1:size(WD_g,2) % columns
%            Measures the number of important outgoing edges of the
%            vertices. For a given vertex i, with a destination vertex j, we first define a measure which reflects
            Hfcontrol_g(i,j)=WD_g(i,j).^2./(nansum(WD_g(:,j).^2));
            Hfcontrol_b(i,j)=WD_b(i,j).^2./(nansum(WD_b(:,j).^2))';
        end
        hcontrol_g(i,i_year) = nansum(Hfcontrol_g(i,:));
        hcontrol_b(i,i_year) = nansum(Hfcontrol_b(i,:));
    end
    
    hcontrol_g_mean(i_year,1)=nanmean(hcontrol_g(:,i_year));
    hcontrol_b_mean(i_year,1)=nanmean(hcontrol_b(:,i_year));
    
    % Sort control index
    temp = hcontrol_g(:,i_year); temp(isnan(temp))=-9999; [Bg,IXg] = sort(temp,'descend');
    temp = hcontrol_b(:,i_year); temp(isnan(temp))=-9999; [Bb,IXb] = sort(temp,'descend');
 
    % Save  control index and country names
    for i = 1:length(country_names)
        hcontrol_rank_g.name{i,i_year} = country_names{IXg(i)};
        hcontrol_rank_g.value(i,i_year) = Bg(i);
        
        hcontrol_rank_b.name{i,i_year} = country_names{IXb(i)};
        hcontrol_rank_b.value(i,i_year) = Bb(i);
        
    end
 
     %% Remove non-existing links (i.e., links with zero weight)
    LinkID_g(Graph_g.Data(:,3)==0,:)=[];
    LinkID_b(Graph_b.Data(:,3)==0,:)=[]; 
    
    Graph_g.Data(Graph_g.Data(:,3)==0,:)=[];
    Graph_b.Data(Graph_b.Data(:,3)==0,:)=[];   
 
    %% Remove inactive nodes
    Graph_g=GraphRemoveUnlinkedNodes(Graph_g);
    Graph_b=GraphRemoveUnlinkedNodes(Graph_b);
 
    Graph_g = mexGraphSqueeze(Graph_g);
    NearestNeighborData_g = mexGraphAverageNearestNeighborDegree(Graph_g,[],'direct');
    Graph_b = mexGraphSqueeze(Graph_b);
    NearestNeighborData_b = mexGraphAverageNearestNeighborDegree(Graph_b,[],'direct'); 

   
   %% SUBSET 1: Extract network properties the specified countries
   for i_country = 1:number_subsetCountries1
       index_country = find(FAOSTATcode==countrysubset_codes1(i_country,1));
       
       if ~isempty(index_country)
           
           % Degree (undirected) 
           ksubset_g(i_country,:)=AU_g(index_country,:);
           ksubset_b(i_country,:)=AU_b(index_country,:);           
           
           % Export Degree: find rows in WD that correspond to the given country
           kexport_g(i_country,:)=AD_g(index_country,:);
           kexport_b(i_country,:)=AD_b(index_country,:);
           
           % Import Degree: find columns in WD that correspond to the given country
           kimport_g(i_country,:)=AD_g(:,index_country)';
           kimport_b(i_country,:)=AD_b(:,index_country)';
           
           % Traded export flow: find rows in WD that correspond to the given country
           total_trade_g(i_country,:)=WU_g(index_country,:);
           total_trade_b(i_country,:)=WU_b(index_country,:);
                 
           % Traded export flow: find rows in WD that correspond to the given country
           export_trade_g(i_country,:)=WD_g(index_country,:);
           export_trade_b(i_country,:)=WD_b(index_country,:);
           
           % Imports: find columns in WD that correspond to the given country
           import_trade_g(i_country,:)=WD_g(:,index_country)';
           import_trade_b(i_country,:)=WD_b(:,index_country)';
           
       end
       
   end
    
   % Node degree
   koutexportsubset_g1(:,i_year) = nansum(kexport_g,2);  % sum each row for export
   koutexportsubset_b1(:,i_year) = nansum(kexport_b,2);  
   
   kinimportsubset_g1(:,i_year) = nansum(kimport_g,2);  % also sum each row because rotated above
   kinimportsubset_b1(:,i_year) = nansum(kimport_g,2);  
   
   kbothsubset_g1(:,i_year) = nansum(ksubset_g,2);  % sum each row or column
   kbothsubset_b1(:,i_year) = nansum(ksubset_b,2); 
 
   % Strength, si, is calculated as the sum of all the EXPORTING fluxes from
   % that node (i.e., as the row sum in the trade matrix)
   strengthsubset_g1(:,i_year) = nansum(total_trade_g,2);
   strengthsubset_b1(:,i_year) = nansum(total_trade_b,2);   
   
   % Strength, si, is calculated as the sum of all the EXPORTING fluxes from
   % that node (i.e., as the row sum in the trade matrix)
   strengthEXPORTsubset_g1(:,i_year) = nansum(export_trade_g,2);
   strengthEXPORTsubset_b1(:,i_year) = nansum(export_trade_b,2);
   
   % Strength, si, is calculated as the sum of all the IMPORTING fluxes from
   % that node (i.e., as the row sum in the trade matrix)
   strengthIMPORTsubset_g1(:,i_year) = nansum(import_trade_g,2);
   strengthIMPORTsubset_b1(:,i_year) = nansum(import_trade_b,2);
   
   % Rest subset variables
   clear kexport_g kexport_b kimport_g kimport_g ksubset_g ksubset_b
   clear total_trade_g total_trade_b export_trade_g export_trade_b
   clear import_trade_g import_trade_b
   
   
   %% SUBSET 2: Extract network properties the specified countries
   for i_country = 1:number_subsetCountries2
       index_country = find(FAOSTATcode==countrysubset_codes2(i_country,1));
       
       if ~isempty(index_country)
           
           % Degree (undirected) 
           ksubset_g(i_country,:)=AU_g(index_country,:);
           ksubset_b(i_country,:)=AU_b(index_country,:);           
           
           % Export Degree: find rows in WD that correspond to the given country
           kexport_g(i_country,:)=AD_g(index_country,:);
           kexport_b(i_country,:)=AD_b(index_country,:);
           
           % Import Degree: find columns in WD that correspond to the given country
           kimport_g(i_country,:)=AD_g(:,index_country)';
           kimport_b(i_country,:)=AD_b(:,index_country)';
           
           % Traded export flow: find rows in WD that correspond to the given country
           total_trade_g(i_country,:)=WU_g(index_country,:);
           total_trade_b(i_country,:)=WU_b(index_country,:);
                 
           % Traded export flow: find rows in WD that correspond to the given country
           export_trade_g(i_country,:)=WD_g(index_country,:);
           export_trade_b(i_country,:)=WD_b(index_country,:);
           
           % Imports: find columns in WD that correspond to the given country
           import_trade_g(i_country,:)=WD_g(:,index_country)';
           import_trade_b(i_country,:)=WD_b(:,index_country)';
           
       end
       
   end
    
   % Node degree
   koutexportsubset_g2(:,i_year) = nansum(kexport_g,2);  % sum each row for export
   koutexportsubset_b2(:,i_year) = nansum(kexport_b,2);  
   
   kinimportsubset_g2(:,i_year) = nansum(kimport_g,2);  % also sum each row because rotated above
   kinimportsubset_b2(:,i_year) = nansum(kimport_g,2);  
   
   kbothsubset_g2(:,i_year) = nansum(ksubset_g,2);  % sum each row or column
   kbothsubset_b2(:,i_year) = nansum(ksubset_b,2); 
 
   % Strength, si, is calculated as the sum of all the EXPORTING fluxes from
   % that node (i.e., as the row sum in the trade matrix)
   strengthsubset_g2(:,i_year) = nansum(total_trade_g,2);
   strengthsubset_b2(:,i_year) = nansum(total_trade_b,2);   
   
   % Strength, si, is calculated as the sum of all the EXPORTING fluxes from
   % that node (i.e., as the row sum in the trade matrix)
   strengthEXPORTsubset_g2(:,i_year) = nansum(export_trade_g,2);
   strengthEXPORTsubset_b2(:,i_year) = nansum(export_trade_b,2);
   
   % Strength, si, is calculated as the sum of all the IMPORTING fluxes from
   % that node (i.e., as the row sum in the trade matrix)
   strengthIMPORTsubset_g2(:,i_year) = nansum(import_trade_g,2);
   strengthIMPORTsubset_b2(:,i_year) = nansum(import_trade_b,2);
   
   % Rest subset variables
   clear kexport_g kexport_b kimport_g kimport_g ksubset_g ksubset_b
   clear total_trade_g total_trade_b export_trade_g export_trade_b
   clear import_trade_g import_trade_b
   

end
 
%% Statistics for table in paper
% Compute average (or max) of all node values during period
temp = k_D_out_g;
%temp(temp==0)=NaN; 
k_D_out_g_mean_periodA = nanmean(nanmean(temp(:,i_periodA),1));
k_D_out_g_mean_periodB = nanmean(nanmean(temp(:,i_periodB),1));
k_D_out_g_max_periodA = nanmean(nanmax(temp(:,i_periodA)));
k_D_out_g_max_periodB = nanmean(nanmax(temp(:,i_periodB)));
clear temp

temp = k_D_out_b;
%temp(temp==0)=NaN; 
k_D_out_b_mean_periodA = nanmean(nanmean(temp(:,i_periodA),1));
k_D_out_b_mean_periodB = nanmean(nanmean(temp(:,i_periodB),1));
k_D_out_b_max_periodA = nanmean(nanmax(temp(:,i_periodA)));
k_D_out_b_max_periodB = nanmean(nanmax(temp(:,i_periodB)));
clear temp

temp = k_D_in_g;
%temp(temp==0)=NaN; 
k_D_in_g_mean_periodA = nanmean(nanmean(temp(:,i_periodA),1));
k_D_in_g_mean_periodB = nanmean(nanmean(temp(:,i_periodB),1));
k_D_in_g_max_periodA = nanmean(nanmax(temp(:,i_periodA)));
k_D_in_g_max_periodB = nanmean(nanmax(temp(:,i_periodB)));
clear temp

temp = k_D_in_b;
%temp(temp==0)=NaN; 
k_D_in_b_mean_periodA = nanmean(nanmean(temp(:,i_periodA),1));
k_D_in_b_mean_periodB = nanmean(nanmean(temp(:,i_periodB),1));
k_D_in_b_max_periodA = nanmean(nanmax(temp(:,i_periodA)));
k_D_in_b_max_periodB = nanmean(nanmax(temp(:,i_periodB)));
clear temp

temp = strengthEXPORT_g;
%temp(temp==0)=NaN; 
s_D_out_g_mean_periodA = nanmean(nanmean(temp(:,i_periodA),1));
s_D_out_g_mean_periodB = nanmean(nanmean(temp(:,i_periodB),1));
s_D_out_g_max_periodA = nanmean(nanmax(temp(:,i_periodA)));
s_D_out_g_max_periodB = nanmean(nanmax(temp(:,i_periodB)));
clear temp

temp = strengthEXPORT_b;
%temp(temp==0)=NaN;  
s_D_out_b_mean_periodA = nanmean(nanmean(temp(:,i_periodA),1));
s_D_out_b_mean_periodB = nanmean(nanmean(temp(:,i_periodB),1));
s_D_out_b_max_periodA = nanmean(nanmax(temp(:,i_periodA)));
s_D_out_b_max_periodB = nanmean(nanmax(temp(:,i_periodB)));
clear temp

temp = strengthIMPORT_g;
%temp(temp==0)=NaN; 
s_D_in_g_mean_periodA = nanmean(nanmean(temp(:,i_periodA),1));
s_D_in_g_mean_periodB = nanmean(nanmean(temp(:,i_periodB),1));
s_D_in_g_max_periodA = nanmean(nanmax(temp(:,i_periodA)));
s_D_in_g_max_periodB = nanmean(nanmax(temp(:,i_periodB)));
clear temp

temp = strengthIMPORT_b;
%temp(temp==0)=NaN; 
s_D_in_b_mean_periodA = nanmean(nanmean(temp(:,i_periodA),1));
s_D_in_b_mean_periodB = nanmean(nanmean(temp(:,i_periodB),1));
s_D_in_b_max_periodA = nanmean(nanmax(temp(:,i_periodA)));
s_D_in_b_max_periodB = nanmean(nanmax(temp(:,i_periodB)));
clear temp

table_networkstat = [...% first four rows are avg global measures
    nanmean(TotTradeGlobal_g(i_periodA))/mmt_convert nanmean(TotTradeGlobal_g(i_periodB))/mmt_convert ...
    nanmean(TotTradeGlobal_b(i_periodA))/mmt_convert nanmean(TotTradeGlobal_b(i_periodB))/mmt_convert;...
%%  Number of links
    nanmean(NumLinks_g(i_periodA)) nanmean(NumLinks_g(i_periodB))...
    nanmean(NumLinks_b(i_periodA)) nanmean(NumLinks_b(i_periodB));...
%%  Number of active export nodes
    nanmean(NumNodes_g_out(i_periodA)) nanmean(NumNodes_g_out(i_periodB))...
    nanmean(NumNodes_b_out(i_periodA)) nanmean(NumNodes_b_out(i_periodB));...
%%  Number of active import nodes
    nanmean(NumNodes_g_in(i_periodA)) nanmean(NumNodes_g_in(i_periodB))...
    nanmean(NumNodes_b_in(i_periodA)) nanmean(NumNodes_b_in(i_periodB));...   
%%  Average export degree
    k_D_out_g_mean_periodA k_D_out_g_mean_periodB k_D_out_b_mean_periodA k_D_out_b_mean_periodB;...
%%  Average import degree
    k_D_in_g_mean_periodA k_D_in_g_mean_periodB k_D_in_b_mean_periodA k_D_in_b_mean_periodB;...
%%  Average export strength
    s_D_out_g_mean_periodA/mmt_convert s_D_out_g_mean_periodB/mmt_convert s_D_out_b_mean_periodA/mmt_convert s_D_out_b_mean_periodB/mmt_convert;...
%%  Average import strength
    s_D_in_g_mean_periodA/mmt_convert s_D_in_g_mean_periodB/mmt_convert s_D_in_b_mean_periodA/mmt_convert s_D_in_b_mean_periodB/mmt_convert;...   
%%  Max. export degree
    k_D_out_g_max_periodA k_D_out_g_max_periodB k_D_out_b_max_periodA k_D_out_b_max_periodB;...
%%  Max. import degree
    k_D_in_g_max_periodA k_D_in_g_max_periodB k_D_in_b_max_periodA k_D_in_b_max_periodB;...
%%  Max. export strength
    s_D_out_g_max_periodA/mmt_convert s_D_out_g_max_periodB/mmt_convert s_D_out_b_max_periodA/mmt_convert s_D_out_b_max_periodB/mmt_convert;...
%%  Max. import strength
    s_D_in_g_max_periodA/mmt_convert s_D_in_g_max_periodB/mmt_convert s_D_in_b_max_periodA/mmt_convert s_D_in_b_max_periodB/mmt_convert];

%% Figure with number of links in network as a function of time
color1 = [0.7490,0.5059,0.1765];
color2 = [0.2078,0.5922,0.5608];

figure
hold on
plot(year_start+offset_yr:year_end,NumLinks_g(offset_yr+1:end), 'LineStyle','-','LineWidth',2.5,'Marker','o','MarkerSize',7,...
    'MarkerFaceColor',color1,'MarkerEdgeColor',color1,'Color',color1)
set(get(gcf,'CurrentAxes'),'FontSize',24)
ylabel('Number of Trade Links, {\itL}','FontSize',24);
xlabel('Year','FontSize',24);
xlim([1991 2010])
%ylim([0 7])
title([commod_print],'FontSize',24)% ' to ' num2str(year1_end)])
legend(commod_print,'Location','Northwest')
box on
%axis square
hold off
set(gcf,'Renderer','painters');
set(gcf,'OuterPosition',[100   500 500 400]);
set(gcf,'PaperPositionMode','auto');
if save_figure>0
    print('-depsc','-painters','-r600',[figure_dir 'numberlinks_' flow_type '_' commod_title '_'  num2str(year_start) '_' num2str(year_end) '.eps' ])
end

figure
hold on
plot(year_start+offset_yr:year_end,NumLinks_b(offset_yr+1:end),'LineStyle','-','LineWidth',2.5,'Marker','s','MarkerSize',7,...
     'MarkerFaceColor',color2,'MarkerEdgeColor',color2,'Color',color2) 
set(get(gcf,'CurrentAxes'),'FontSize',24)
ylabel('Number of Trade Links, {\itL}','FontSize',24);
xlabel('Year','FontSize',24);
xlim([1991 2010])
%ylim([0 7])
title([commod_print2],'FontSize',24)% ' to ' num2str(year1_end)])
legend(commod_print2,'Location','Northwest')
box on
%axis square
hold off
set(gcf,'Renderer','painters');
set(gcf,'OuterPosition',[100 500 500 400]);
set(gcf,'PaperPositionMode','auto');
if save_figure>0
    print('-depsc','-painters','-r600',[figure_dir 'numberlinks_' flow_type '_' commod_title2   '_' num2str(year_start) '_' num2str(year_end) '.eps' ])
end


%% Total flows in the network 
figure
hold on
plot(year_start+offset_yr:year_end,TotTradeGlobal_g(offset_yr+1:end)/mmt_convert,'LineStyle','-','LineWidth',2,'Marker','o','MarkerSize',7,...
    'MarkerFaceColor',color1,'Color',color1)
set(get(gcf,'CurrentAxes'),'FontSize',24)
xlabel('Year','FontSize',24);
ylabel('Trade flow, \Sigma_{\iti }{\its} (mmt)','FontSize',24);
title([commod_print],'FontSize',28)
%legend(commod_print,'Location','Northwest')
xlim([1991 2010])
%ylim([0 200])
hold off
box on
%axis square
set(gcf,'Renderer','painters')
set(gcf,'OuterPosition',[100 500 500 400])
set(gcf,'PaperPositionMode','auto')
if save_figure>0
    print('-depsc','-painters','-r600',[figure_dir 'actual_' flow_type '_' commod_title '_' num2str(year_start) '_' num2str(year_end)  '.eps' ])
end

figure
hold on
plot(year_start+offset_yr:year_end,TotTradeGlobal_b(offset_yr+1:end)/mmt_convert,'LineStyle','-','LineWidth',2,'Marker','o','MarkerSize',7,...
    'MarkerFaceColor',color2,'Color',color2)
set(get(gcf,'CurrentAxes'),'FontSize',24)
xlabel('Year','FontSize',24);
ylabel('Trade flow, \Sigma_{\iti }{\its} (mmt)','FontSize',24);
title([commod_print2],'FontSize',24)
%legend(commod_print2,'Location','Northwest')
xlim([1991 2010])
%ylim([0 200])
hold off
box on
%axis square
set(gcf,'Renderer','painters')
set(gcf,'OuterPosition',[100 500 500 400])
set(gcf,'PaperPositionMode','auto')
if save_figure>0
    print('-depsc','-painters','-r600',[figure_dir 'actual_' flow_type '_' commod_title2 '_' num2str(year_start) '_' num2str(year_end)  '.eps' ])
end

%%  THE TWO SECTIONS BELOW PLOT THE IMPORT RELATIONSHIP BETWEEN STRENGTH 
%   AND NODE DEGREE FOR !!EXPORTS!!
%% WHEAT
% Period A
xvariable = nanmean(k_D_out_g(:,i_periodA),2); 
yvariable = nanmean(strengthEXPORT_g(:,i_periodA),2);
[slope, intercept, MSE, R2, S] = logfit_v2(xvariable,yvariable,'loglog');
xApprox = [.01 .1 1 10 100 1000 10000 100000 1000000 10000000 100000000 10000000000];
yApprox = (10^intercept)*xApprox.^(slope);

figure
xvariable = nanmean(k_D_out_g(:,i_periodA),2); 
yvariable = nanmean(strengthEXPORT_g(:,i_periodA),2);
loglog(xApprox,yApprox,'Line','-','Color','k','Marker','none');
text(10,50,['\it{\beta}=' num2str(slope,3)],'FontSize',24)

hold on
loglog(xvariable,yvariable,'Line','none','Color',color1,'Marker','o','MarkerSize',6,'MarkerFaceColor',color1);
%-----
% Code to find centroid of cluster of points
% In a power-law distribution, most of values are below the mean.  
% The median is below the mean, and the mode is below the median.
X = [xvariable yvariable];
opts = statset('Display','final');
[idx,ctrs] = kmeans(X,1,'Distance','sqEuclidean','Replicates',5,'Options',opts);
loglog(ctrs(:,1),ctrs(:,2),'LineWidth',2,'Color','k','Marker','o','MarkerSize',18,'MarkerFaceColor','none');
loglog(ctrs(:,1),ctrs(:,2),'LineWidth',2,'Color','k','Marker','x','MarkerSize',18,'MarkerFaceColor','none');
%-----
ylabel('Export strength, {\its}_{out} (tons)','FontSize',24);
xlabel('Number of export connections, {\itk}_{out}','FontSize',24);
title(['Wheat, ' periodlabel{1}],'FontSize',28)
axis([10^-1 10^2.5 10^-1 10^8]);     
axis square
%set(gca,'XTick',[1.5 3.5 5.5 7.5])
set(gca,'Box','on')
%set(gca,'XTickLabel',{'NE';'S';'W';'FW'})
set(get(gcf,'CurrentAxes'),'FontSize',18)
set(gca,'XMinorTick','off')  % sets Minor X Ticks to display
set(gcf,'Renderer','painters') %only vector graphics that matlab has- painters
%set(gcf,'PaperOrientation','landscape')
set(gcf,'OuterPosition',[170 1200 500 500]) %left edge botton edge; width height
set(gcf,'PaperPositionMode','auto')
if save_figure>0
    print('-depsc','-painters','-r600',[figure_dir 'DegreeStrength_EXPORT_' flow_type '_' commod_title '_' periodlabel{1}  '.eps' ])
end

% Period B
xvariable = nanmean(k_D_out_g(:,i_periodB),2); 
yvariable = nanmean(strengthEXPORT_g(:,i_periodB),2);
[slope, intercept, MSE, R2, S] = logfit_v2(xvariable,yvariable,'loglog');
xApprox = [.01 .1 1 10 100 1000 10000 100000 1000000 10000000 100000000 10000000000];
yApprox = (10^intercept)*xApprox.^(slope);

figure
xvariable = nanmean(k_D_out_g(:,i_periodB),2); 
yvariable = nanmean(strengthEXPORT_g(:,i_periodB),2);
loglog(xApprox,yApprox,'Line','-','Color','k','Marker','none');
text(10,50,['\it{\beta}=' num2str(slope,3)],'FontSize',24)

hold on
loglog(xvariable,yvariable,'Line','none','Color',color1,'Marker','o','MarkerSize',6,'MarkerFaceColor',color1);
%-----
% Code to find centroid of cluster of points
X = [xvariable yvariable];
opts = statset('Display','final');
[idx,ctrs] = kmeans(X,1,'Distance','sqEuclidean','Replicates',5,'Options',opts);
loglog(ctrs(:,1),ctrs(:,2),'LineWidth',2,'Color','k','Marker','o','MarkerSize',18,'MarkerFaceColor','none');
loglog(ctrs(:,1),ctrs(:,2),'LineWidth',2,'Color','k','Marker','x','MarkerSize',18,'MarkerFaceColor','none');
%-----
ylabel('Export strength, {\its}_{out} (tons)','FontSize',24);
xlabel('Number of export connections, {\itk}_{out}','FontSize',24);
title(['Wheat, ' periodlabel{2}],'FontSize',28)
axis([10^-1 10^2.5 10^-1 10^8]);     
axis square
%set(gca,'XTick',[1.5 3.5 5.5 7.5])
set(gca,'Box','on')
%set(gca,'XTickLabel',{'NE';'S';'W';'FW'})
set(get(gcf,'CurrentAxes'),'FontSize',18)
set(gca,'XMinorTick','off')  % sets Minor X Ticks to display
set(gcf,'Renderer','painters') %only vector graphics that matlab has- painters
%set(gcf,'PaperOrientation','landscape')
set(gcf,'OuterPosition',[170 1200 500 500]) %left edge botton edge; width height
set(gcf,'PaperPositionMode','auto')
if save_figure>0
    print('-depsc','-painters','-r600',[figure_dir 'DegreeStrength_EXPORT_' flow_type '_' commod_title '_' periodlabel{2}  '.eps' ])
end

%% RICE
% Period A
xvariable = nanmean(k_D_out_b(:,i_periodA),2); 
yvariable = nanmean(strengthEXPORT_b(:,i_periodA),2);
[slope, intercept, MSE, R2, S] = logfit_v2(xvariable,yvariable,'loglog');
xApprox = [.01 .1 1 10 100 1000 10000 100000 1000000 10000000 100000000 10000000000];
yApprox = (10^intercept)*xApprox.^(slope);

figure
xvariable = nanmean(k_D_out_b(:,i_periodA),2); 
yvariable = nanmean(strengthEXPORT_b(:,i_periodA),2);
loglog(xApprox,yApprox,'Line','-','Color','k','Marker','none');
text(10,50,['\it{\beta}=' num2str(slope,3)],'FontSize',24)

hold on
loglog(xvariable,yvariable,'Line','none','Color',color2,'Marker','s','MarkerSize',6,'MarkerFaceColor',color2);
%-----
% Code to find centroid of cluster of points
X = [xvariable yvariable];
opts = statset('Display','final');
[idx,ctrs] = kmeans(X,1,'Distance','sqEuclidean','Replicates',5,'Options',opts);
loglog(ctrs(:,1),ctrs(:,2),'LineWidth',2,'Color','k','Marker','o','MarkerSize',18,'MarkerFaceColor','none');
loglog(ctrs(:,1),ctrs(:,2),'LineWidth',2,'Color','k','Marker','x','MarkerSize',18,'MarkerFaceColor','none');
%-----
ylabel('Export strength, {\its}_{out} (tons)','FontSize',24);
xlabel('Number of export connections, {\itk}_{out}','FontSize',24);
title(['Rice, ' periodlabel{1}],'FontSize',28)
axis([10^-1 10^2.5 10^-1 10^8]);     
axis square
%set(gca,'XTick',[1.5 3.5 5.5 7.5])
set(gca,'Box','on')
%set(gca,'XTickLabel',{'NE';'S';'W';'FW'})
set(get(gcf,'CurrentAxes'),'FontSize',18)
set(gca,'XMinorTick','off')  % sets Minor X Ticks to display
set(gcf,'Renderer','painters') %only vector graphics that matlab has- painters
%set(gcf,'PaperOrientation','landscape')
set(gcf,'OuterPosition',[170 1200 500 500]) %left edge botton edge; width height
set(gcf,'PaperPositionMode','auto')
if save_figure>0
    print('-depsc','-painters','-r600',[figure_dir 'DegreeStrength_EXPORT_' flow_type '_' commod_title2 '_' periodlabel{1}  '.eps' ])
end

% Period B
xvariable = nanmean(k_D_out_b(:,i_periodB),2); 
yvariable = nanmean(strengthEXPORT_b(:,i_periodB),2); 
[slope, intercept, MSE, R2, S] = logfit_v2(xvariable,yvariable,'loglog');
xApprox = [.01 .1 1 10 100 1000 10000 100000 1000000 10000000 100000000 10000000000];
yApprox = (10^intercept)*xApprox.^(slope);

figure
xvariable = nanmean(k_D_out_b(:,i_periodB),2); 
yvariable = nanmean(strengthEXPORT_b(:,i_periodB),2); 
loglog(xApprox,yApprox,'Line','-','Color','k','Marker','none');
text(10,50,['\it{\beta}=' num2str(slope,3)],'FontSize',24)

hold on
loglog(xvariable,yvariable,'Line','none','Color',color2,'Marker','s','MarkerSize',6,'MarkerFaceColor',color2);
%-----
% Code to find centroid of cluster of points
X = [xvariable yvariable];
opts = statset('Display','final');
[idx,ctrs] = kmeans(X,1,'Distance','sqEuclidean','Replicates',5,'Options',opts);
loglog(ctrs(:,1),ctrs(:,2),'LineWidth',2,'Color','k','Marker','o','MarkerSize',18,'MarkerFaceColor','none');
loglog(ctrs(:,1),ctrs(:,2),'LineWidth',2,'Color','k','Marker','x','MarkerSize',18,'MarkerFaceColor','none');
%-----
ylabel('Export strength, {\its}_{out} (tons)','FontSize',24);
xlabel('Number of export connections, {\itk}_{out}','FontSize',24);
title(['Rice, ' periodlabel{2}],'FontSize',28)
axis([10^-1 10^2.5 10^-1 10^8]);     
axis square
%set(gca,'XTick',[1.5 3.5 5.5 7.5])
set(gca,'Box','on')
%set(gca,'XTickLabel',{'NE';'S';'W';'FW'})
set(get(gcf,'CurrentAxes'),'FontSize',18)
set(gca,'XMinorTick','off')  % sets Minor X Ticks to display
set(gcf,'Renderer','painters') %only vector graphics that matlab has- painters
%set(gcf,'PaperOrientation','landscape')
set(gcf,'OuterPosition',[170 1200 500 500]) %left edge botton edge; width height
set(gcf,'PaperPositionMode','auto')
if save_figure>0
    print('-depsc','-painters','-r600',[figure_dir 'DegreeStrength_EXPORT_' flow_type '_' commod_title2 '_' periodlabel{2}  '.eps' ])
end


return;
%%  THE TWO SECTIONS BELOW PLOT THE IMPORT RELATIONSHIP BETWEEN STRENGTH 
%   AND NODE DEGREE FOR !!IMPORTS!!
%% WHEAT
% Period A
xvariable = nanmean(k_D_in_g(:,i_periodA),2); 
yvariable = nanmean(strengthIMPORT_g(i_periodA,:),1)';
[slope, intercept, MSE, R2, S] = logfit_v2(xvariable,yvariable,'loglog');
xApprox = [.01 .1 1 10 100 1000 10000 100000 1000000 10000000 100000000 10000000000];
yApprox = (10^intercept)*xApprox.^(slope);

figure
xvariable = nanmean(k_D_in_g(:,i_periodA),2); 
yvariable = nanmean(strengthIMPORT_g(i_periodA,:),1)';
loglog(xApprox,yApprox,'Line','-','Color','k','Marker','none');
text(10,50,['\it{\beta}=' num2str(slope,3)],'FontSize',24)

hold on
loglog(xvariable,yvariable,'Line','none','Color',color1,'Marker','.','MarkerSize',20);
ylabel('Amount imported (tons)','FontSize',24);
xlabel('Number of import connections','FontSize',24);
title(['Wheat, ' periodlabel{1}],'FontSize',28)
axis([10^-1 10^2.5 10^-1 10^8]);     
axis square
%set(gca,'XTick',[1.5 3.5 5.5 7.5])
set(gca,'Box','on')
%set(gca,'XTickLabel',{'NE';'S';'W';'FW'})
set(get(gcf,'CurrentAxes'),'FontSize',18)
set(gca,'XMinorTick','off')  % sets Minor X Ticks to display
set(gcf,'Renderer','painters') %only vector graphics that matlab has- painters
%set(gcf,'PaperOrientation','landscape')
set(gcf,'OuterPosition',[170 1200 500 500]) %left edge botton edge; width height
set(gcf,'PaperPositionMode','auto')
if save_figure>0
    print('-depsc','-painters','-r600',[figure_dir 'DegreeStrength_subset_' flow_type '_' commod_title '_' periodlabel{1}  '.eps' ])
end

% Period B
xvariable = nanmean(k_D_in_g(:,i_periodB),2); 
yvariable = nanmean(strengthIMPORT_g(i_periodB,:),1)';
[slope, intercept, MSE, R2, S] = logfit_v2(xvariable,yvariable,'loglog');
xApprox = [.01 .1 1 10 100 1000 10000 100000 1000000 10000000 100000000 10000000000];
yApprox = (10^intercept)*xApprox.^(slope);

figure
xvariable = nanmean(k_D_in_g(:,i_periodB),2); 
yvariable = nanmean(strengthIMPORT_g(i_periodB,:),1)';
loglog(xApprox,yApprox,'Line','-','Color','k','Marker','none');
text(10,50,['\it{\beta}=' num2str(slope,3)],'FontSize',24)

hold on
loglog(xvariable,yvariable,'Line','none','Color',color1,'Marker','.','MarkerSize',20);
ylabel('Amount imported (tons)','FontSize',24);
xlabel('Number of import connections','FontSize',24);
title(['Wheat, ' periodlabel{2}],'FontSize',28)
axis([10^-1 10^2.5 10^-1 10^8]);     
axis square
%set(gca,'XTick',[1.5 3.5 5.5 7.5])
set(gca,'Box','on')
%set(gca,'XTickLabel',{'NE';'S';'W';'FW'})
set(get(gcf,'CurrentAxes'),'FontSize',18)
set(gca,'XMinorTick','off')  % sets Minor X Ticks to display
set(gcf,'Renderer','painters') %only vector graphics that matlab has- painters
%set(gcf,'PaperOrientation','landscape')
set(gcf,'OuterPosition',[170 1200 500 500]) %left edge botton edge; width height
set(gcf,'PaperPositionMode','auto')
if save_figure>0
    print('-depsc','-painters','-r600',[figure_dir 'DegreeStrength_subset_' flow_type '_' commod_title '_' periodlabel{2}  '.eps' ])
end

%% RICE
% Period A
xvariable = nanmean(k_D_in_b(:,i_periodA),2); 
yvariable = nanmean(strengthIMPORT_b(i_periodA,:),1)';
[slope, intercept, MSE, R2, S] = logfit_v2(xvariable,yvariable,'loglog');
xApprox = [.01 .1 1 10 100 1000 10000 100000 1000000 10000000 100000000 10000000000];
yApprox = (10^intercept)*xApprox.^(slope);

figure
xvariable = nanmean(k_D_in_b(:,i_periodA),2); 
yvariable = nanmean(strengthIMPORT_b(i_periodA,:),1)';
loglog(xApprox,yApprox,'Line','-','Color','k','Marker','none');
text(10,50,['\it{\beta}=' num2str(slope,3)],'FontSize',24)

hold on
loglog(xvariable,yvariable,'Line','none','Color',color2,'Marker','.','MarkerSize',20);
ylabel('Amount imported (tons)','FontSize',24);
xlabel('Number of import connections','FontSize',24);
title(['Rice, ' periodlabel{1}],'FontSize',28)
axis([10^-1 10^2.5 10^-1 10^8]);     
axis square
%set(gca,'XTick',[1.5 3.5 5.5 7.5])
set(gca,'Box','on')
%set(gca,'XTickLabel',{'NE';'S';'W';'FW'})
set(get(gcf,'CurrentAxes'),'FontSize',18)
set(gca,'XMinorTick','off')  % sets Minor X Ticks to display
set(gcf,'Renderer','painters') %only vector graphics that matlab has- painters
%set(gcf,'PaperOrientation','landscape')
set(gcf,'OuterPosition',[170 1200 500 500]) %left edge botton edge; width height
set(gcf,'PaperPositionMode','auto')
if save_figure>0
    print('-depsc','-painters','-r600',[figure_dir 'DegreeStrength_subset_' flow_type '_' commod_title2 '_' periodlabel{1}  '.eps' ])
end

% Period B
xvariable = nanmean(k_D_in_b(:,i_periodB),2); 
yvariable = nanmean(strengthIMPORT_b(i_periodB,:),1)';
[slope, intercept, MSE, R2, S] = logfit_v2(xvariable,yvariable,'loglog');
xApprox = [.01 .1 1 10 100 1000 10000 100000 1000000 10000000 100000000 10000000000];
yApprox = (10^intercept)*xApprox.^(slope);

figure
xvariable = nanmean(k_D_in_b(:,i_periodB),2); 
yvariable = nanmean(strengthIMPORT_b(i_periodB,:),1)';
loglog(xApprox,yApprox,'Line','-','Color','k','Marker','none');
text(10,50,['\it{\beta}=' num2str(slope,3)],'FontSize',24)

hold on
loglog(xvariable,yvariable,'Line','none','Color',color2,'Marker','.','MarkerSize',20);
ylabel('Amount imported (tons)','FontSize',24);
xlabel('Number of import connections','FontSize',24);
title(['Rice, ' periodlabel{2}],'FontSize',28)
axis([10^-1 10^2.5 10^-1 10^8]);     
axis square
%set(gca,'XTick',[1.5 3.5 5.5 7.5])
set(gca,'Box','on')
%set(gca,'XTickLabel',{'NE';'S';'W';'FW'})
set(get(gcf,'CurrentAxes'),'FontSize',18)
set(gca,'XMinorTick','off')  % sets Minor X Ticks to display
set(gcf,'Renderer','painters') %only vector graphics that matlab has- painters
%set(gcf,'PaperOrientation','landscape')
set(gcf,'OuterPosition',[170 1200 500 500]) %left edge botton edge; width height
set(gcf,'PaperPositionMode','auto')
if save_figure>0
    print('-depsc','-painters','-r600',[figure_dir 'DegreeStrength_subset_' flow_type '_' commod_title2 '_' periodlabel{2}  '.eps' ])
end

return;



%% WHEAT
% Period A
figure
xvariable = nanmean(k_D_in_g(:,i_periodA),2); 
yvariable = nanmean(strengthIMPORT_g(i_periodA,:),1)';
[slope, intercept, MSE, R2, S] = logfit_v2(xvariable,yvariable,'loglog');
xApprox = [.01 .1 1 10 100 1000 10000 100000 1000000 10000000 100000000 10000000000];
yApprox = (10^intercept)*xApprox.^(slope);

figure
xvariable = nanmean(k_D_in_g(:,i_periodA),2); 
yvariable = nanmean(strengthIMPORT_g(i_periodA,:),1)';
loglog(xApprox,yApprox,'Line','-','Color','k','Marker','none');
text(10,50,['\it{\beta}=' num2str(slope,3)],'FontSize',24)

hold on
% %Subset 3
% xvariable = nanmean(kinimportsubset_g3(:,i_periodA),2); 
% yvariable = nanmean(strengthIMPORTsubset_g3(i_periodA,:),1)';
% loglog(xvariable,yvariable,'Line','none','Color',color1,'Marker','s','MarkerSize',6,'MarkerFaceColor',color1);
%Subset 1
xvariable = nanmean(kinimportsubset_g1(:,i_periodA),2); 
yvariable = nanmean(strengthIMPORTsubset_g1(i_periodA,:),1)';
loglog(xvariable,yvariable,'Line','none','Color',color1,'Marker','.','MarkerSize',20);
%Subset 2
xvariable = nanmean(kinimportsubset_g2(:,i_periodA),2); 
yvariable = nanmean(strengthIMPORTsubset_g2(i_periodA,:),1)';
loglog(xvariable,yvariable,'Line','none','Color',color1,'Marker','*','MarkerSize',10,'MarkerFaceColor',color1);
% Plot fit power law
loglog(xApprox,yApprox,'Line','-','Color','k','Marker','none');

ylabel('Amount imported (tons)','FontSize',24);
xlabel('Number of import connections','FontSize',24);
legend({'fit' 'LDC' 'Annex1' },'Location','Northwest');
title(['Wheat, ' periodlabel{1}],'FontSize',28)
axis([10^-1 10^2.5 10^-1 10^8]);     
axis square
%set(gca,'XTick',[1.5 3.5 5.5 7.5])
set(gca,'Box','on')
%set(gca,'XTickLabel',{'NE';'S';'W';'FW'})
set(get(gcf,'CurrentAxes'),'FontSize',18)
set(gca,'XMinorTick','off')  % sets Minor X Ticks to display
set(gcf,'Renderer','painters') %only vector graphics that matlab has- painters
%set(gcf,'PaperOrientation','landscape')
set(gcf,'OuterPosition',[170 1200 500 500]) %left edge botton edge; width height
set(gcf,'PaperPositionMode','auto')
if save_figure>0
    print('-depsc','-painters','-r600',[figure_dir 'DegreeStrength_subset_' flow_type '_' commod_title '_' periodlabel{1}  '.eps' ])
end


figure
xvariable = nanmean(k_D_in_g(:,i_periodB),2); 
yvariable = nanmean(strengthIMPORT_g(i_periodB,:),1)';
[slope, intercept, MSE, R2, S] = logfit_v2(xvariable,yvariable,'loglog');
xApprox = [.01 .1 1 10 100 1000 10000 100000 1000000 10000000 100000000 10000000000];
yApprox = (10^intercept)*xApprox.^(slope);
loglog(xApprox,yApprox,'Line','-','Color','k','Marker','none');
text(10,50,['\it{\beta}=' num2str(slope,3)],'FontSize',24)

hold on
% %Subset 3
% xvariable = nanmean(kinimportsubset_g3(:,i_periodB),2); 
% yvariable = nanmean(strengthIMPORTsubset_g3(i_periodB,:),1)';
% loglog(xvariable,yvariable,'Line','none','Color',color1,'Marker','s','MarkerSize',6,'MarkerFaceColor',color1);
%Subset 1
xvariable = nanmean(kinimportsubset_g1(:,i_periodB),2); 
yvariable = nanmean(strengthIMPORTsubset_g1(i_periodB,:),1)';
loglog(xvariable,yvariable,'Line','none','Color',color1,'Marker','.','MarkerSize',20);
%Subset 2
xvariable = nanmean(kinimportsubset_g2(:,i_periodB),2); 
yvariable = nanmean(strengthIMPORTsubset_g2(i_periodB,:),1)';
loglog(xvariable,yvariable,'Line','none','Color',color1,'Marker','*','MarkerSize',10,'MarkerFaceColor',color1);

ylabel('Import Strength(tons)','FontSize',24);
xlabel('Degree, k_{in}','FontSize',24);
%legend({'Other' 'LIFD' 'G8' 'Plus 5' },'Location','Northwest');
title(['Wheat, ' periodlabel{2}],'FontSize',28)
axis([10^-1 10^2.5 10^-1 10^8]);     
axis square
%set(gca,'XTick',[1.5 3.5 5.5 7.5])
set(gca,'Box','on')
%set(gca,'XTickLabel',{'NE';'S';'W';'FW'})
set(get(gcf,'CurrentAxes'),'FontSize',18)
set(gca,'XMinorTick','off')  % sets Minor X Ticks to display
set(gcf,'Renderer','painters') %only vector graphics that matlab has- painters
%set(gcf,'PaperOrientation','landscape')
set(gcf,'OuterPosition',[170 1200 500 500]) %left edge botton edge; width height
set(gcf,'PaperPositionMode','auto')
if save_figure>0
    print('-depsc','-painters','-r600',[figure_dir 'DegreeStrength_subset_' flow_type '_' commod_title '_' periodlabel{2}  '.eps' ])
end

%% RICE
figure
xvariable = nanmean(k_D_in_b(:,i_periodA),2); 
yvariable = nanmean(strengthIMPORT_b(i_periodA,:),1)';
[slope, intercept, MSE, R2, S] = logfit_v2(xvariable,yvariable,'loglog');
xApprox = [.01 .1 1 10 100 1000 10000 100000 1000000 10000000 100000000 10000000000];
yApprox = (10^intercept)*xApprox.^(slope);
loglog(xApprox,yApprox,'Line','-','Color','k','Marker','none');
text(10,50,['\it{\beta}=' num2str(slope,3)],'FontSize',24);

hold on
% %Subset 3
% xvariable = nanmean(kinimportsubset_b3(:,i_periodA),2); 
% yvariable = nanmean(strengthIMPORTsubset_b3(i_periodA,:),1)';
% loglog(xvariable,yvariable,'Line','none','Color',color2,'Marker','s','MarkerSize',6,'MarkerFaceColor',color2);
%Subset 1
xvariable = nanmean(kinimportsubset_b1(:,i_periodA),2); 
yvariable = nanmean(strengthIMPORTsubset_b1(i_periodA,:),1)';
loglog(xvariable,yvariable,'Line','none','Color',color2,'Marker','.','MarkerSize',20);
%Subset 2
xvariable = nanmean(kinimportsubset_b2(:,i_periodA),2); 
yvariable = nanmean(strengthIMPORTsubset_b2(i_periodA,:),1)';
loglog(xvariable,yvariable,'Line','none','Color',color2,'Marker','*','MarkerSize',10,'MarkerFaceColor',color2);

ylabel('Import Strength (tons)','FontSize',24);
xlabel('Degree, k_{in}','FontSize',24);
legend({'LDC' 'Annex 1' },'Location','Northwest');
title(['Rice, ' periodlabel{1}],'FontSize',28)
axis([10^-1 10^2.5 10^-1 10^7]);     
axis square
%set(gca,'XTick',[1.5 3.5 5.5 7.5])
set(gca,'Box','on')
%set(gca,'XTickLabel',{'NE';'S';'W';'FW'})
set(get(gcf,'CurrentAxes'),'FontSize',18)
set(gca,'XMinorTick','off')  % sets Minor X Ticks to display
set(gcf,'Renderer','painters') %only vector graphics that matlab has- painters
%set(gcf,'PaperOrientation','landscape')
set(gcf,'OuterPosition',[170 1200 500 500]) %left edge botton edge; width height
set(gcf,'PaperPositionMode','auto')
if save_figure>0
    print('-depsc','-painters','-r600',[figure_dir 'DegreeStrength_subset_' flow_type '_' commod_title2 '_' periodlabel{1}  '.eps' ])
end


figure
xvariable = nanmean(k_D_in_b(:,i_periodB),2); 
yvariable = nanmean(strengthIMPORT_b(i_periodB,:),1)';
[slope, intercept, MSE, R2, S] = logfit_v2(xvariable,yvariable,'loglog');
xApprox = [.01 .1 1 10 100 1000 10000 100000 1000000 10000000 100000000 10000000000];
yApprox = (10^intercept)*xApprox.^(slope);
loglog(xApprox,yApprox,'Line','-','Color','k','Marker','none');
text(10,50,['\it{\beta}=' num2str(slope,3)],'FontSize',24);
hold on
% %Subset 3
% xvariable = nanmean(kinimportsubset_b3(:,i_periodB),2); 
% yvariable = nanmean(strengthIMPORTsubset_b3(i_periodB,:),1)';
% loglog(xvariable,yvariable,'Line','none','Color',color2,'Marker','s','MarkerSize',6,'MarkerFaceColor',color2);
%Subset 1
xvariable = nanmean(kinimportsubset_b1(:,i_periodB),2); 
yvariable = nanmean(strengthIMPORTsubset_b1(i_periodB,:),1)';
loglog(xvariable,yvariable,'Line','none','Color',color2,'Marker','.','MarkerSize',20);
%Subset 2
xvariable = nanmean(kinimportsubset_b2(:,i_periodB),2); 
yvariable = nanmean(strengthIMPORTsubset_b2(i_periodB,:),1)';
loglog(xvariable,yvariable,'Line','none','Color',color2,'Marker','*','MarkerSize',10,'MarkerFaceColor',color2);

ylabel('Import Strength (tons)','FontSize',24);
xlabel('Degree, k_{in}','FontSize',24);
%legend({'LDC' 'Annex 1'},'Location','Northwest');
title(['Rice, ' periodlabel{2}],'FontSize',28)
axis([10^-1 10^2.5 10^-1 10^7]);      
axis square
%set(gca,'XTick',[1.5 3.5 5.5 7.5])
set(gca,'Box','on')
%set(gca,'XTickLabel',{'NE';'S';'W';'FW'})
set(get(gcf,'CurrentAxes'),'FontSize',18)
set(gca,'XMinorTick','off')  % sets Minor X Ticks to display
set(gcf,'Renderer','painters') %only vector graphics that matlab has- painters
%set(gcf,'PaperOrientation','landscape')
set(gcf,'OuterPosition',[170 1200 500 500]) %left edge botton edge; width height
set(gcf,'PaperPositionMode','auto')
if save_figure>0
    print('-depsc','-painters','-r600',[figure_dir 'DegreeStrength_subset_' flow_type '_' commod_title2 '_' periodlabel{2}  '.eps' ])
end


return






%% Total flows in the network as relative change from base year (year 1)
figure
hold on
plot(year_start+offset_yr:year_end,100*(TotTradeGlobal_g(offset_yr+1:end)-TotTradeGlobal_g(offset_yr+1))./TotTradeGlobal_g(offset_yr+1),'LineStyle','-','LineWidth',1.5,'Marker','s','MarkerSize',8,...
    'MarkerFaceColor','r','Color','k')
plot(year_start+offset_yr:year_end,100*(TotTradeGlobal_b(offset_yr+1:end)-TotTradeGlobal_b(offset_yr+1))./TotTradeGlobal_b(offset_yr+1),'LineStyle','--','LineWidth',1.5,'Marker','o','MarkerSize',8,...
    'MarkerFaceColor','b','Color','b')
%plot(year_start:year_end,100*(TotTradeGlobal_3-TotTradeGlobal_3(1))./TotTradeGlobal_3(1),'LineStyle','--','LineWidth',1.5,'Marker','d','MarkerSize',10,...
%    'MarkerFaceColor','g','Color','k')
set(get(gcf,'CurrentAxes'),'FontSize',18)
xlabel('Year','FontSize',17);
ylabel('Relative Changes (%)','FontSize',18);
title(['Trade Flow (Base Yr 1992)'],'FontSize',20)
%legend(commod_print,commod_print2,commod_print3,'Location','Northwest')
xlim([1990 2010])
hold off
box on
axis square
set(gcf,'Renderer','painters')
set(gcf,'OuterPosition',[100   500   450 450])
set(gcf,'PaperPositionMode','auto')
if save_figure>0
    print('-depsc','-painters','-r600',[figure_dir 'relative_' flow_type '_' commod_title '_' commod_title2 '_' commod_title3 '_' num2str(year_start) '_' num2str(year_end) '.eps' ])
end 

%% Links in the network as relative change from base year (year 1)
figure
hold on
plot(year_start+offset_yr:year_end,100*(NumLinks_g(offset_yr+1:end)-NumLinks_g(offset_yr+1))./NumLinks_g(offset_yr+1),'LineStyle','-','LineWidth',1.5,'Marker','s','MarkerSize',8,...
    'MarkerFaceColor','r','Color','k')
plot(year_start+offset_yr:year_end,100*(NumLinks_b(offset_yr+1:end)-NumLinks_b(offset_yr+1))./NumLinks_b(offset_yr+1),'LineStyle','--','LineWidth',1.5,'Marker','o','MarkerSize',8,...
    'MarkerFaceColor','b','Color','b')
%plot(year_start:year_end,100*(TotTradeGlobal_3-TotTradeGlobal_3(1))./TotTradeGlobal_3(1),'LineStyle','--','LineWidth',1.5,'Marker','d','MarkerSize',10,...
%    'MarkerFaceColor','g','Color','k')
set(get(gcf,'CurrentAxes'),'FontSize',18)
xlabel('Year','FontSize',17);
ylabel('Relative Changes (%)','FontSize',18);
title(['Trade Links (Base Yr 1992)'],'FontSize',20)
%legend(commod_print,commod_print2,commod_print3,'Location','Northwest')
xlim([1990 2010])
hold off
box on
axis square
set(gcf,'Renderer','painters')
set(gcf,'OuterPosition',[100   500   450 450])
set(gcf,'PaperPositionMode','auto')
if save_figure>0
    print('-depsc','-painters','-r600',[figure_dir 'relative_numberconnections_' commod_title '_' commod_title2 '_' commod_title3 '_' num2str(year_start) '_' num2str(year_end) '.eps' ])
end 

% figure
% %xvariable(find(xvariable==0))=[];
% %yvariable(find(yvariable==0))=[];
% xvariable = nanmean(k_D_g(:,i_periodA),2); 
% yvariable = nanmean(strength_g(i_periodA,:),1)';
% loglog(xvariable,yvariable,'Line','none','Color',color1,'Marker','x');
% hold on
% xvariable = nanmean(kbothsubset_g1(:,i_periodA),2); 
% yvariable = nanmean(strengthsubset_g1(i_periodA,:),1)';
% loglog(xvariable,yvariable,'Line','none','Color',color1,'Marker','.','MarkerSize',18);
% xvariable = nanmean(kbothsubset_g2(:,i_periodA),2); 
% yvariable = nanmean(strengthsubset_g2(i_periodA,:),1)';
% loglog(xvariable,yvariable,'Line','none','Color',color1,'Marker','s','MarkerSize',6,'MarkerFaceColor',color1);
% xvariable = nanmean(kbothsubset_g3(:,i_periodA),2); 
% yvariable = nanmean(strengthsubset_g3(i_periodA,:),1)';
% loglog(xvariable,yvariable,'Line','none','Color',color1,'Marker','^','MarkerSize',6,'MarkerFaceColor',color1);
% ylabel('Trade Strength, s','FontSize',24);
% xlabel('Degree, \itk','FontSize',24);
% %legend({num2str(year_start) num2str(year_end)},'Location','Northeast');
% title(['Wheat, ' periodlabel{1}],'FontSize',28)
% %axis([0 9 0 2200]);     
% axis square
% %set(gca,'XTick',[1.5 3.5 5.5 7.5])
% set(gca,'Box','on')
% %set(gca,'XTickLabel',{'NE';'S';'W';'FW'})
% set(get(gcf,'CurrentAxes'),'FontSize',18)
% set(gca,'XMinorTick','on')  % sets Minor X Ticks to display
% set(gcf,'Renderer','painters') %only vector graphics that matlab has- painters
% %set(gcf,'PaperOrientation','landscape')
% set(gcf,'OuterPosition',[170 1200 500 500]) %left edge botton edge; width height
% set(gcf,'PaperPositionMode','auto')
% if save_figure>0
%     print('-depsc','-painters','-r600',[figure_dir 'DegreeStrength_' flow_type '_' commod_title '_' commod_title2 '_' num2str(year_start) '_' num2str(year_end)  '.eps' ])
% end
% 
% 
% 
% %% Strength for flows of subset of the network
% figure
% temp_plot = [nanmean(strengthEXPORTsubset_g(i_periodA,:),1);nanmean(strengthEXPORTsubset_g(i_periodB,:),1)]/mmt_convert;
% iqr_values = iqr(temp_plot);
% temp_plot(isnan(temp_plot))=NaN;
% hl=boxplot(temp_plot','colors','br','whisker',1.5);
% for ih=1:6
%     set(hl(ih,:),'LineWidth',2);
% end
% %xlabel('Site','FontSize',14)
% ylabel('Trade strength, s (mmt)','FontSize',24);
% set(get(gcf,'CurrentAxes'),'FontSize',24)
% %axis([0.5 2.5 0 40]);
% set(gca,'XTick',[1:2])
% set(gca,'XTickLabel',periodlabel,'FontSize',24)
% set(gca,'Box','on')
% set(gcf,'Renderer','painters') %only vector graphics that matlab has- painters
% %set(gcf,'PaperOrientation','landscape')
% title(['Exports, ' country_title],'FontSize',28)
% set(gcf,'OuterPosition',[170 1200 500 500]) %left edge botton edge; width height
% set(gcf,'PaperPositionMode','auto')
% if save_figure>0
%     print('-depsc','-painters','-r600',[figure_dir 'CVStrengthExport_' country_print flow_type '_' commod_title '_' commod_title2 '_' num2str(year_start) '_' num2str(year_end)  '.eps' ])
% end
% 
% %% Mean Export Flow
% % Cumulative distribution plot
% temp_plot = [nanmean(strengthEXPORTsubset_g(i_periodA,:),1)];%
% temp_plot(temp_plot==0)=[];
% figure
% %hold on
% eX = eprob(temp_plot');
% loglog(eX.sort,eX.eprob,'LineWidth',2,'Color',color1);
% hold on 
% temp_plot = [nanmean(strengthEXPORTsubset_g(i_periodB,:),1)];%
% temp_plot(temp_plot==0)=[];
% eX = eprob(temp_plot');
% loglog(eX.sort,eX.eprob,'LineWidth',2,'Color',color2,'LineStyle','--');
% set(get(gcf,'CurrentAxes'),'FontSize',24)
% ylabel('P(S>s)','FontSize',24);
% xlabel('Trade strength, s','FontSize',24);
% %legend({num2str(year_start) num2str(year_end)},'Location','Northeast');
% title(['Exports'],'FontSize',28)
% %axis([0 9 0 2200]);     axis square
% %set(gca,'XTick',[1.5 3.5 5.5 7.5])
% set(gca,'Box','on')
% %set(gca,'XTickLabel',{'NE';'S';'W';'FW'})
% set(gcf,'Renderer','painters') %only vector graphics that matlab has- painters
% %set(gcf,'PaperOrientation','landscape')
% set(get(gcf,'CurrentAxes'),'FontSize',14)
% set(gcf,'OuterPosition',[170 1200 400 400]) %left edge botton edge; width height
% set(gcf,'PaperPositionMode','auto')
% if save_figure>0
%     print('-depsc','-painters','-r600',[figure_dir 'CVStrengthExport_' country_print flow_type '_' commod_title '_' commod_title2 '_' num2str(year_start) '_' num2str(year_end)  '.eps' ])
% end
% 
% % figure
% % temp_plot = [nanmean(strengthEXPORT_g(offset_yr+1:end,:),1);nanmean(strengthEXPORT_b(offset_yr+1:end,:),1)];
% % iqr_values = iqr(temp_plot);
% % temp_plot(isnan(temp_plot))=NaN;
% % hl=boxplot(temp_plot','colors','br','whisker',1.5);
% % for ih=1:6
% %     set(hl(ih,:),'LineWidth',2);
% % end
% % %xlabel('Site','FontSize',14)
% % ylabel('std','FontSize',18);
% % title(['Exports'],'FontSize',20)
% % set(get(gcf,'CurrentAxes'),'FontSize',14)
% % %axis([0 9 0 2200]);     axis square
% % %set(gca,'XTick',[1.5 3.5 5.5 7.5])
% % set(gca,'Box','off')
% % %set(gca,'XTickLabel',{'NE';'S';'W';'FW'})
% % set(gcf,'Renderer','painters') %only vector graphics that matlab has- painters
% % %set(gcf,'PaperOrientation','landscape')
% % set(get(gcf,'CurrentAxes'),'FontSize',14)
% % set(gcf,'OuterPosition',[170 1200 400 400]) %left edge botton edge; width height
% % set(gcf,'PaperPositionMode','auto')
% % if save_figure>0
% %     print('-depsc','-painters','-r600',[figure_dir 'CVStrengthExport_' flow_type '_' commod_title '_' commod_title2 '_' num2str(year_start) '_' num2str(year_end)  '.eps' ])
% % end
% 
% figure
% hold on
% plot(year_start+offset_yr:year_end,nanstd(strengthEXPORT_g(offset_yr+1:end,:),0,2)...%./nanmean(strengthIMPORT_g(offset_yr+1:end,:),2)
%     ,'LineStyle','-','LineWidth',2.5,'Marker','s','MarkerSize',7 ...
%     ,'MarkerFaceColor',color1,'Color',color1)
% plot(year_start+offset_yr:year_end,nanstd(strengthEXPORT_b(offset_yr+1:end,:),0,2)...%./nanmean(strengthIMPORT_b(offset_yr+1:end,:),2)
%     ,'LineStyle','--','LineWidth',2.5,'Marker','.','MarkerSize',28 ...
%     ,'MarkerFaceColor',color2,'Color',color2)
% %plot(year_start:year_end,TotTradeGlobal_3/mmt_convert,'LineStyle','--','LineWidth',1.5,'Marker','d','MarkerSize',10,...
% %    'MarkerFaceColor','g','Color','k')
% set(get(gcf,'CurrentAxes'),'FontSize',18)
% %xlabel('Year','FontSize',17);
% ylabel('std','FontSize',18);
% title(['Exports'],'FontSize',20)
% %legend(commod_print,commod_print2,commod_print3,'Location','Northwest')
% xlim([1991 2010])
% %ylim([4 7])
% hold off
% box on
% %axis square
% set(gcf,'Renderer','painters')
% set(gcf,'OuterPosition',[100   500   500 450])
% set(gcf,'PaperPositionMode','auto')
% if save_figure>0
%     print('-depsc','-painters','-r600',[figure_dir 'CVStrengthExport_' flow_type '_' commod_title '_' commod_title2 '_' num2str(year_start) '_' num2str(year_end)  '.eps' ])
% end
% 
% 
% %% Mean Import Flow
% figure
% hold on
% plot(year_start+offset_yr:year_end,nanstd(strengthIMPORT_g(offset_yr+1:end,:),0,2)...%./nanmean(strengthIMPORT_g(offset_yr+1:end,:),2)
%     ,'LineStyle','-','LineWidth',2.5,'Marker','s','MarkerSize',7 ...
%     ,'MarkerFaceColor',color1,'Color',color1)
% plot(year_start+offset_yr:year_end,nanstd(strengthIMPORT_b(offset_yr+1:end,:),0,2)...%./nanmean(strengthIMPORT_b(offset_yr+1:end,:),2)
%     ,'LineStyle','--','LineWidth',2.5,'Marker','.','MarkerSize',28 ...
%     ,'MarkerFaceColor',color2,'Color',color2)
% %plot(year_start:year_end,TotTradeGlobal_3/mmt_convert,'LineStyle','--','LineWidth',1.5,'Marker','d','MarkerSize',10,...
% %    'MarkerFaceColor','g','Color','k')
% set(get(gcf,'CurrentAxes'),'FontSize',18)
% %xlabel('Year','FontSize',17);
% ylabel('std','FontSize',18);
% title(['Imports'],'FontSize',20)
% %legend(commod_print,commod_print2,commod_print3,'Location','Northwest')
% xlim([1991 2010])
% %ylim([1 4])
% hold off
% box on
% %axis square
% set(gcf,'Renderer','painters')
% set(gcf,'OuterPosition',[100   500   500 450])
% set(gcf,'PaperPositionMode','auto')
% if save_figure>0
%     print('-depsc','-painters','-r600',[figure_dir 'CVStrengthImport_' flow_type '_' commod_title '_' commod_title2 '_' num2str(year_start) '_' num2str(year_end)  '.eps' ])
% end
% 
% 
% 
% return;
% %% Concentration Index Time Series
% figure
% hold on
% plot(year_start+offset_yr:year_end,nanmean(Iconcentrate_g(:,offset_yr+1:end),1), 'LineStyle','-','LineWidth',1.5,'Marker','s','MarkerSize',8,...
%     'MarkerFaceColor','r','Color','k')
% plot(year_start+offset_yr:year_end,nanmean(Iconcentrate_b(:,offset_yr+1:end),1),'LineStyle','--','LineWidth',1.5,'Marker','o','MarkerSize',8,...
%      'MarkerFaceColor','b','Color','b')
% %plot(year_start:year_end,nanmean(Iconcentrate_3,1),'LineStyle','--','LineWidth',1.5,'Marker','d','MarkerSize',10,...
% %    'MarkerFaceColor','g','Color','k')
% set(get(gcf,'CurrentAxes'),'FontSize',18)
% ylabel('COncentration Index','FontSize',18);
% %xlabel('year','FontSize',18);
% title(['Concentration Index','FontSize',20])% 
% %title(['Mean Concentration Index, ' flow_title])% ' to ' num2str(year1_end)])
% %legend(commod_print,commod_print2,commod_print3,'Location','Northwest')
% box on
% axis square
% hold off
% set(gcf,'Renderer','painters')
% set(gcf,'OuterPosition',[100   500   500 500])
% set(gcf,'PaperPositionMode','auto')
% if save_figure>0
%     print('-depsc','-painters','-r600',[figure_dir 'concentration_' flow_type '_' commod_title '_' commod_title2 '_' commod_title3 '_' num2str(year_start) '_' num2str(year_end) '.eps' ])
% end
% %% Normalized histogram of distribution of the concentration index 
% delta = 1;
% variable_g_min = nanmin(Iconcentrate_g(:)); variable_g_max = nanmax(Iconcentrate_g(:)); 
% variable_g_bins = variable_g_min-delta:delta:variable_g_max+delta;
%  
% variable_b_min = nanmin(Iconcentrate_b(:)); variable_b_max = nanmax(Iconcentrate_b(:)); 
% variable_b_bins = variable_b_min-delta:delta:variable_b_max+delta;
%  
% figure
% [no_Icg ,xo_Icg]  = histnorm(nanmean(Iconcentrate_g(:,i_periodA),2), variable_g_bins);
% [no_Icg2 ,xo_Icg2]  = histnorm(nanmean(Iconcentrate_g(:,i_periodB),2), variable_g_bins);
% hold on
% h_cg =stairs(xo_Icg,no_Icg,'g');
% h_cg2=stairs(xo_Icg2,no_Icg2,'k');
% legend(periodlabel,'Location','Northeast');
% xlabel(['Concentration Index'],'FontSize',18);
% ylabel('# in bin/total #/bin width','FontSize',18)
% title(['Concentration Index, ' commod_print],'FontSize',20)% 
% set(get(gcf,'CurrentAxes'),'FontSize',18)
% set(h_cg,'LineWidth', 2, 'LineStyle','-');
% set(h_cg2,'LineWidth', 2, 'LineStyle','--');
% hold off
% box on
% axis square
% hold off
% set(gcf,'Renderer','painters')
% set(gcf,'OuterPosition',[100   500   400 400])
% set(gcf,'PaperPositionMode','auto')
% if save_figure>0
%     print('-depsc','-painters','-r600',[figure_dir 'Histconcentration_' flow_type '_' commod_title '.eps' ])
% end
%  
% figure
% [no_Icb ,xo_Icb]  = histnorm(nanmean(Iconcentrate_b(:,i_periodA),2), variable_b_bins);
% [no_Icb2 ,xo_Icb2]  = histnorm(nanmean(Iconcentrate_b(:,i_periodB),2), variable_b_bins);
% hold on
% h_cb =stairs(xo_Icb,no_Icb,'g');
% h_cb2=stairs(xo_Icb2,no_Icb2,'k');
% legend(periodlabel,'Location','Northeast');
% xlabel(['Concentration Index'],'FontSize',18);
% ylabel('# in bin/total #/bin width','FontSize',18)
% title(['Concentration Index, ' commod_print2],'FontSize',20)% 
% set(get(gcf,'CurrentAxes'),'FontSize',18)
% set(h_cb,'LineWidth', 2, 'LineStyle','-');
% set(h_cb2,'LineWidth', 2, 'LineStyle','--');
% hold off
% box on
% axis square
% hold off
% set(gcf,'Renderer','painters')
% set(gcf,'OuterPosition',[100   500   400 400])
% set(gcf,'PaperPositionMode','auto')
% if save_figure>0
%     print('-depsc','-painters','-r600',[figure_dir 'Histconcentration_' flow_type '_' commod_title2 '.eps' ])
% end
%  
%  
% return
%  
% %% Compute Spearman rank correlation
% rank_matrix_g(find(rank_matrix_g==0))=NaN;
% RHO_g = corr(rank_matrix_g,'type','Spearman','rows','pairwise');
% RHO_g_lag1=zeros(length(RHO_g)-1,1);
% for i = 1:length(RHO_g)-1
%     RHO_g_lag1(i,1)=RHO_g(i+1,i);
% end
%  
% rank_matrix_b(find(rank_matrix_b==0))=NaN;
% RHO_b = corr(rank_matrix_b,'type','Spearman','rows','pairwise');
% RHO_b_lag1=zeros(length(RHO_b)-1,1);
% for i = 1:length(RHO_b)-1
%     RHO_b_lag1(i,1)=RHO_b(i+1,i);
% end
%  
% rank_matrix_3(find(rank_matrix_3==0))=NaN;
% RHO_3 = corr(rank_matrix_3,'type','Spearman','rows','pairwise');
% RHO_3_lag1=zeros(length(RHO_3)-1,1);
% for i = 1:length(RHO_3)-1
%     RHO_3_lag1(i,1)=RHO_3(i+1,i);
% end
%  
%  
% %% Figure with number of exporting nodes
% figure
% hold on
% plot(year_start:year_end,number_exporters_g./NumNodes_g, 'LineStyle','-','LineWidth',1.5,'Marker','s','MarkerSize',8,...
%     'MarkerFaceColor','r','Color','k')
% plot(year_start:year_end,number_exporters_b./NumNodes_b,'LineStyle','--','LineWidth',1.5,'Marker','o','MarkerSize',8,...
%      'MarkerFaceColor','b','Color','b')
% plot(year_start:year_end,number_exporters_3./NumNodes_3,'LineStyle','--','LineWidth',1.5,'Marker','d','MarkerSize',10,...
%     'MarkerFaceColor','g','Color','k') 
% set(get(gcf,'CurrentAxes'),'FontSize',18)
% ylabel('# of Exporters/# Countries Active','FontSize',18);
% xlabel('year','FontSize',18);
% legend(commod_print,commod_print2,commod_print3,'Location','Northwest')
% title(['Fraction of Countries Exporting'])% ' to ' num2str(year1_end)])
% box on
% axis square
% hold off
% set(gcf,'Renderer','painters');
% set(gcf,'OuterPosition',[100   500   500 500]);
% set(gcf,'PaperPositionMode','auto');
% if save_figure>0
%     print('-depsc','-painters','-r600',[figure_dir 'frexport_' flow_type '_' commod_title '_' commod_title2 '_' commod_title3 '_' num2str(year_start) '_' num2str(year_end) '.eps' ])
% end
%  
% %% Figure with number of importer nodes
% figure
% hold on
% plot(year_start:year_end,number_importers_g./NumNodes_g, 'LineStyle','-','LineWidth',1.5,'Marker','s','MarkerSize',8,...
%     'MarkerFaceColor','r','Color','k')
% plot(year_start:year_end,number_importers_b./NumNodes_b,'LineStyle','--','LineWidth',1.5,'Marker','o','MarkerSize',8,...
%      'MarkerFaceColor','b','Color','b')
% plot(year_start:year_end,number_importers_3./NumNodes_3,'LineStyle','--','LineWidth',1.5,'Marker','d','MarkerSize',10,...
%     'MarkerFaceColor','g','Color','k') 
% set(get(gcf,'CurrentAxes'),'FontSize',18)
% ylabel('# of Importers/# Countries Active','FontSize',18);
% xlabel('year','FontSize',18);
% legend(commod_print,commod_print2,commod_print3,'Location','Southeast')
% title(['Fraction of Countries Importing'])% ' to ' num2str(year1_end)])
% box on
% axis square
% hold off
% set(gcf,'Renderer','painters');
% set(gcf,'OuterPosition',[100   500   500 500]);
% set(gcf,'PaperPositionMode','auto');
% if save_figure>0
%     print('-depsc','-painters','-r600',[figure_dir 'frimport_' flow_type '_' commod_title '_' commod_title2 '_' commod_title3 '_' num2str(year_start) '_' num2str(year_end) '.eps' ])
% end
%  
%  
%  
%  
%  
% %% Control Index Time Series
% figure
% hold on
% plot(year_start:year_end,hcontrol_g_mean, 'LineStyle','-','LineWidth',1.5,'Marker','s','MarkerSize',8,...
%     'MarkerFaceColor','r','Color','k')
% plot(year_start:year_end,hcontrol_b_mean,'LineStyle','--','LineWidth',1.5,'Marker','o','MarkerSize',8,...
%      'MarkerFaceColor','b','Color','b')
% plot(year_start:year_end,hcontrol_3_mean,'LineStyle','--','LineWidth',1.5,'Marker','d','MarkerSize',10,...
%     'MarkerFaceColor','g','Color','k')
% set(get(gcf,'CurrentAxes'),'FontSize',18)
% ylabel('<s_j>','FontSize',18);
% xlabel('year','FontSize',18);
% title(['Control Index'])% 
% %title(['Mean Control Index, ' flow_title])% ' to ' num2str(year1_end)])
% %legend(commod_print,commod_print2,commod_print3,'Location','Southeast')
% box on
% axis square
% hold off
% set(gcf,'Renderer','painters')
% set(gcf,'OuterPosition',[100   500   500 500])
% set(gcf,'PaperPositionMode','auto')
% if save_figure>0
%     print('-depsc','-painters','-r600',[figure_dir 'control_' flow_type '_' commod_title '_' commod_title2 '_' commod_title3 '_' num2str(year_start) '_' num2str(year_end)  '.eps' ])
% end
%  
% return
% 



% % %% Figure with Spearman's rank correlation
% % figure
% % hold on
% % plot((year_start:(year_end-1))+.5,RHO_g_lag1,'LineStyle','none','LineWidth',1.5,'Marker','s','MarkerSize',8,...
% %     'MarkerFaceColor','g','Color','g')
% % plot((year_start:(year_end-1))+.5,RHO_b_lag1,'LineStyle','none','LineWidth',1.5,'Marker','o','MarkerSize',8,...
% %     'MarkerFaceColor','b','Color','b')
% % set(get(gcf,'CurrentAxes'),'FontSize',16)
% % xlabel('Year','FontSize',16);
% % ylabel(['\rho (-)'],'FontSize',16);
% % title(['Spearman''s rank correlation, ' commod_print],'FontSize',18)
% % %legend('green','blue','Location','Northwest')
% % hold off
% % box on
% % if save_figure>0
% %     print('-depsc','-painters','-r600',[figure_dir 'Rho_' commod_title '_' num2str(year_start) '_' num2str(year_end) '.eps' ])
% % end
% 
% 
% return
% 
% 
% 
% %% Fit of exponential distribution to cumulative distribution of the node degrees 
% figure
% set(gcf,'Renderer','painters')
% set(gcf,'OuterPosition',[100   500   800 800])
% set(gcf,'PaperPositionMode','auto')
% 
% subplot(2,2,1)
% hold on
% eX = eprob(k_D_out_g(:,1));
% plot(eX.sort,eX.eprob,'LineWidth',2,'Color','g');
% eX = eprob(k_D_out_g(:,end));
% plot(eX.sort,eX.eprob,'LineWidth',2,'Color','k','LineStyle','--');
% set(get(gcf,'CurrentAxes'),'FontSize',18)
% ylabel('P(K>k)','FontSize',18);
% xlabel('k','FontSize',18);
% title(['Node Degree for Exports (Green), ' commod_print])% 
% legend({num2str(year_start) num2str(year_end)},'Location','Northeast');
% hold off
% 
% subplot(2,2,2)
% hold on
% eX = eprob(k_D_in_g(:,1));
% plot(eX.sort,eX.eprob,'LineWidth',2,'Color','g');
% eX = eprob(k_D_in_g(:,end));
% plot(eX.sort,eX.eprob,'LineWidth',2,'Color','k','LineStyle','--');
% set(get(gcf,'CurrentAxes'),'FontSize',18)
% ylabel('P(K>k)','FontSize',18);
% xlabel('k','FontSize',18);
% title(['Node Degree for Imports (Green), ' commod_print])% 
% legend({num2str(year_start) num2str(year_end)},'Location','Northeast');
% hold off
% 
% subplot(2,2,3)
% hold on
% eX = eprob(k_D_out_b(:,1));
% plot(eX.sort,eX.eprob,'LineWidth',2,'Color','b');
% eX = eprob(k_D_out_b(:,end));
% plot(eX.sort,eX.eprob,'LineWidth',2,'Color','k','LineStyle','--');
% set(get(gcf,'CurrentAxes'),'FontSize',18)
% ylabel('P(K>k)','FontSize',18);
% xlabel('k','FontSize',18);
% title(['Node Degree for Exports (Blue), ' commod_print])% 
% legend({num2str(year_start) num2str(year_end)},'Location','Northeast');
% hold off
% 
% subplot(2,2,4)
% hold on
% eX = eprob(k_D_in_b(:,1));
% plot(eX.sort,eX.eprob,'LineWidth',2,'Color','b');
% eX = eprob(k_D_in_b(:,end));
% plot(eX.sort,eX.eprob,'LineWidth',2,'Color','k','LineStyle','--');
% set(get(gcf,'CurrentAxes'),'FontSize',18)
% ylabel('P(K>k)','FontSize',18);
% xlabel('k','FontSize',18);
% title(['Node Degree for Imports (Blue), ' commod_print])% 
% legend({num2str(year_start) num2str(year_end)},'Location','Northeast');
% hold off
% 
% %% Fit of exponential distribution to cumulative distribution of the node strengths 
% figure
% set(gcf,'Renderer','painters')
% set(gcf,'OuterPosition',[100   500   800 800])
% set(gcf,'PaperPositionMode','auto')
% 
% subplot(2,2,1)
% hold on
% eX = eprob(kw_D_out_g(:,1));
% plot(eX.sort,eX.eprob,'LineWidth',2,'Color','g');
% eX = eprob(kw_D_out_g(:,end));
% plot(eX.sort,eX.eprob,'LineWidth',2,'Color','k','LineStyle','--');
% set(get(gcf,'CurrentAxes'),'FontSize',18)
% ylabel('ln(P(S>s))','FontSize',18);
% xlabel('s','FontSize',18);
% title(['Node Strength for Exports (Green), ' commod_print])% 
% legend({num2str(year_start) num2str(year_end)},'Location','Northeast');
% hold off
% 
% subplot(2,2,2)
% hold on
% eX = eprob(kw_D_in_g(:,1));
% plot(eX.sort,eX.eprob,'LineWidth',2,'Color','g');
% eX = eprob(kw_D_in_g(:,end));
% plot(eX.sort,eX.eprob,'LineWidth',2,'Color','k','LineStyle','--');
% set(get(gcf,'CurrentAxes'),'FontSize',18)
% ylabel('ln(P(S>s))','FontSize',18);
% xlabel('s','FontSize',18);
% title(['Node Strength for Imports (Green), ' commod_print])% 
% legend({num2str(year_start) num2str(year_end)},'Location','Northeast');
% hold off
% 
% subplot(2,2,3)
% hold on
% eX = eprob(kw_D_out_b(:,1));
% plot(eX.sort,eX.eprob,'LineWidth',2,'Color','b');
% eX = eprob(kw_D_out_b(:,end));
% plot(eX.sort,eX.eprob,'LineWidth',2,'Color','k','LineStyle','--');
% set(get(gcf,'CurrentAxes'),'FontSize',18)
% ylabel('ln(P(S>s))','FontSize',18);
% xlabel('s','FontSize',18);
% title(['Node Strength for Exports (Blue), ' commod_print])% 
% legend({num2str(year_start) num2str(year_end)},'Location','Northeast');
% hold off
% 
% subplot(2,2,4)
% hold on
% eX = eprob(kw_D_in_b(:,1));
% plot(eX.sort,eX.eprob,'LineWidth',2,'Color','b');
% eX = eprob(kw_D_in_b(:,end));
% plot(eX.sort,eX.eprob,'LineWidth',2,'Color','k','LineStyle','--');
% set(get(gcf,'CurrentAxes'),'FontSize',18)
% ylabel('ln(P(S>s))','FontSize',18);
% xlabel('s','FontSize',18);
% title(['Node Strength for Imports (Blue), ' commod_print])% 
% legend({num2str(year_start) num2str(year_end)},'Location','Northeast');
% hold off
% 
% % if save_figure>0
% %     print('-depsc','-painters','-r600',[basefolder 'Figures/'  'Exceed_Prob' num2str(year1_start) '_' num2str(year2_end) '.eps' ])
% % end
% 
% 
% return
% 
% % %% Remove degrees in network equal to zero
% % for i_yr = 1:num_years
% %     temp_out = k_D_out_g(:,i_yr);
% %     temp_in = k_D_in_g(:,i_yr);
% %     
% %     temp_out(temp_out==0)=[];
% %     temp_in(temp_in==0)=[];
% %     
% %     k_D_out_g_mean(i_yr)=mean(temp_out);
% %     k_D_in_g_mean(i_yr)=mean(temp_in);
% % 
% %     k_D_out_g_single(i_yr)=sum(temp_out==1);
% %     k_D_in_g_single(i_yr)=sum(temp_in==1);
% %     
% %     k_D_out_g_less5(i_yr)=sum(temp_out<=5);
% %     k_D_in_g_less5(i_yr)=sum(temp_in<=5);
% %     
% %     k_D_out_g_greater10(i_yr)=sum(temp_out>=10);
% %     k_D_in_g_greater10(i_yr)=sum(temp_in>=10);
% % 
% %     eX_out = eprob(temp_out);
% %     eX_in = eprob(temp_in);
% %     
% % end
% 
% % figure
% % hold on
% % plot(year_start:year_end,k_D_out_g_single, 'LineStyle','-','LineWidth',1.5,'Marker','s','MarkerSize',8,...
% %     'MarkerFaceColor','g','Color','k')
% % plot(year_start:year_end,k_D_in_g_single, 'LineStyle','-','LineWidth',1.5,'Marker','o','MarkerSize',8,...
% %     'MarkerFaceColor','b','Color','k')
% % set(get(gcf,'CurrentAxes'),'FontSize',18)
% % ylabel('Number','FontSize',18);
% % xlabel('year','FontSize',18);
% % title(['Number of countries with single trade partner, ' commod_print])% ' to ' num2str(year1_end)])
% % legend('export','import','Location','Northeast')
% % hold off
% % 
% % figure
% % hold on
% % plot(year_start:year_end,k_D_out_g_less5, 'LineStyle','-','LineWidth',1.5,'Marker','s','MarkerSize',8,...
% %     'MarkerFaceColor','g','Color','k')
% % plot(year_start:year_end,k_D_in_g_less5, 'LineStyle','-','LineWidth',1.5,'Marker','o','MarkerSize',8,...
% %     'MarkerFaceColor','b','Color','k')
% % set(get(gcf,'CurrentAxes'),'FontSize',18)
% % ylabel('Number','FontSize',18);
% % xlabel('year','FontSize',18);
% % title(['Number of countries with 5 or less trade partners, ' commod_print])% ' to ' num2str(year1_end)])
% % legend('export','import','Location','Northeast')
% % hold off
% % 
% % figure
% % hold on
% % plot(year_start:year_end,k_D_out_g_greater10, 'LineStyle','-','LineWidth',1.5,'Marker','s','MarkerSize',8,...
% %     'MarkerFaceColor','g','Color','k')
% % plot(year_start:year_end,k_D_in_g_greater10, 'LineStyle','-','LineWidth',1.5,'Marker','o','MarkerSize',8,...
% %     'MarkerFaceColor','b','Color','k')
% % set(get(gcf,'CurrentAxes'),'FontSize',18)
% % ylabel('Number','FontSize',18);
% % xlabel('year','FontSize',18);
% % title(['Number of countries with 10 or more trade partners, ' commod_print])% ' to ' num2str(year1_end)])
% % legend('export','import','Location','Northeast')
% % hold off
% % 
% % return
% % 
% % 
% % %% Plot total strength of the network (i.e., the sum of the strengths of 
% % % all nodes in the network))
% % figure
% % hold on
% % plot(year_start:year_end,s_D_g, 'Marker','s','LineStyle','-')
% % set(get(gcf,'CurrentAxes'),'FontSize',18)
% % ylabel('\Sigma s_{i} (m^3 virtual water)','FontSize',18);
% % xlabel('year','FontSize',18);
% % title(['Total Strength of Network'])% ' to ' num2str(year1_end)])
% % hold off
% % if save_figure>0
% %     print('-depsc','-painters','-r600',[basefolder 'Figures/'  'TotStrength' num2str(year1_start) '_' num2str(year2_end) '.eps' ])
% % end
% % 
% % 
% % 
% % 
% % 
% % 
% % %% Node Degree Distribution: 
% % % Exceedance Probability Plot
% % figure
% % set(gcf,'Renderer','painters')
% % set(gcf,'OuterPosition',[100   500   400 800])
% % set(gcf,'PaperPositionMode','auto')
% % 
% % subplot(3,1,1)
% % hold on
% % eX = eprob(k_1U);
% % plot(eX.sort,eX.eprob,'LineWidth',2);
% % eX = eprob(k_3U);
% % plot(eX.sort,eX.eprob,'LineWidth',2,'Color','g');
% % set(get(gcf,'CurrentAxes'),'FontSize',18)
% % ylabel('P(K>k)','FontSize',18);
% % xlabel('k','FontSize',18);
% % title(['Undirected Network'])% ' to ' num2str(year1_end)])
% % legend({num2str(year1_start),num2str(year3_start)},'Location','Northeast');
% % hold off
% % 
% % subplot(3,1,2)
% % hold on
% % eX = eprob(k_1D_out);
% % plot(eX.sort,eX.eprob,'LineWidth',2);
% % eX = eprob(k_3D_out);
% % plot(eX.sort,eX.eprob,'LineWidth',2,'Color','g');
% % set(get(gcf,'CurrentAxes'),'FontSize',18)
% % ylabel('P(K>k)','FontSize',18);
% % xlabel('k_{out}, Export','FontSize',18);
% % title(['Directed Network, Export'])% ' to ' num2str(year1_end)])
% % legend({num2str(year1_start),num2str(year3_start)},'Location','Northeast');
% % hold off
% % 
% % subplot(3,1,3)
% % hold on
% % eX = eprob(k_1D_in);
% % plot(eX.sort,eX.eprob,'LineWidth',2);
% % eX = eprob(k_3D_in);
% % plot(eX.sort,eX.eprob,'LineWidth',2,'Color','g');
% % set(get(gcf,'CurrentAxes'),'FontSize',18)
% % ylabel('P(K>k)','FontSize',18);
% % xlabel('k_{in}, Import','FontSize',18);
% % title(['Directed Network, Import'])% ' to ' num2str(year1_end)])
% % legend({num2str(year1_start),num2str(year3_start)},'Location','Northeast');
% % hold off
% % 
% % if save_figure>0
% %     print('-depsc','-painters','-r600',[basefolder 'Figures/'  'Exceed_Prob' num2str(year1_start) '_' num2str(year2_end) '.eps' ])
% % end
% % 
% % 
% % 
% % 
% % %% Nearest neighbor degree plot
% % figure; 
% % hold on
% % scatter(NewrestNeighborData1.Degree,NewrestNeighborData1.AverageNeiborsOutDegree,'b')
% % scatter(NewrestNeighborData2.Degree,NewrestNeighborData2.AverageNeiborsOutDegree,'g')
% % scatter(NewrestNeighborData3.Degree,NewrestNeighborData3.AverageNeiborsOutDegree,'k')
% % hold off
% % set(get(gcf,'CurrentAxes'),'FontSize',18)
% % ylabel('k_{nn}','FontSize',18);
% % xlabel('k','FontSize',18);
% % legend({'2006','2007','2008'},'Location','Northeast');
% 
% 
% 
% % act_rank_matrix_g=zeros(size(rank_matrix_g));
% % i_year = 0;
% % for year = year_start:year_end
% %     
% %     i_year = i_year + 1; % year index
% %     % get ranks of green water
% %     data_temp = rank_matrix_g(:,i_year);
% %     act_rank_matrix_g(:,i_year)=floor(tiedrank(-data_temp));
% %     % get ranks of blue water
% %     data_temp = rank_matrix_b(:,i_year);
% %     act_rank_matrix_b(:,i_year)=floor(tiedrank(-data_temp));    
% %     
% % end
% 
% 
% 
% 
% % %%  Identify top nodes
% % figure
% % hold on
% % top10 = zeros(10,num_years);
% % for i_rank = 1:10
% %     row_ind = find(act_rank_matrix_g(:,1)==i_rank);
% %     for i_yr = 1:num_years
% %         top10(i_rank,i_yr) = act_rank_matrix_g(row_ind,i_yr);
% %         top10_link(i_rank,i_yr) =  row_ind;
% %         top10names.exporter{i_rank,i_yr} = Graph_g.Index.Names(Graph_g.Data(row_ind,1));
% %         top10names.importer{i_rank,i_yr} = Graph_g.Index.Names(Graph_g.Data(row_ind,2));
% %     end
% %     top10(find(top10>400)) = NaN;
% %     plot(year_start:year_end,top10(i_rank,:), 'Marker','s','LineStyle','-')
% % end
% % set(get(gcf,'CurrentAxes'),'FontSize',18)
% % %ylabel('\Sigma s_{i} (m^3 virtual water)','FontSize',18);
% % xlabel('year','FontSize',18);
% % %title(['Total Strength of Network'])% ' to ' num2str(year1_end)])
% % hold off
% 
% 
% 
% 
% %     %% Sort ascending
% %     [B,index]  = sortrows(Graph_g.Data,3);
% %     index=flipud(index); B=flipud(B);
% %     Graph_g.Data(index(1),:)
% %     B_top5 = B(1:5,:);
% %     Graph_b.Index.Names(60)
% 
% 
% % %     kout_knnout_D_in_g = [1:length(k_D_in_g); knn_D_in_g]';
% % %     kout_knnout_D_in_g = flipud(sortrows(kout_knnout_D_in_g,2));
% % % 
% % %     % second dataset    
% % %     for i =1:size(k_D_in_b,1)
% % %         if sum(AU_b(i,:)==1)>0
% % %             j_indices = find(k_D_in_b(i,:)==1);
% % %             sum_kj = sum(k_D_in_b(j_indices));
% % %             knn_D_in_b(i) = sum_kj/k_D_in_b(i);
% % %         else
% % %             knn_D_in_b(i) = 0;
% % %         end
% % %     end
% % %     kout_knnout_D_in_b = [1:length(k_D_in_b); knn_D_in_b]';
% % %     kout_knnout_D_in_b = flipud(sortrows(kout_knnout_D_in_b,2));
% % %     
% % %     % third dataset   
% % %     for i =1:size(k_D_in_3,1)
% % %         if sum(AU_3(i,:)==1)>0
% % %             j_indices = find(k_D_in_3(i,:)==1);
% % %             sum_kj = sum(k_D_in_3(j_indices));
% % %             knn_D_in_3(i) = sum_kj/k_D_in_3(i);
% % %         else
% % %             knn_D_in_3(i) = 0;
% % %         end
% % %     end
% % %     
% % %     kout_knnout_D_in_3 = [1:length(k_D_in_3); knn_D_in_3]';
% % %     kout_knnout_D_in_3 = flipud(sortrows(kout_knnout_D_in_3,2));
% % 
% %     
% % %     
% % %     %% Compute other network properties   
% % %     k_U_g(:,i_year) = nansum(AU_g)';
% % %     k_D_in_g(:,i_year) =  nansum(AD_g,1)'; % sum each column for import
% % %     number_importers_g(:,i_year) = nnz(k_D_in_g(:,i_year)); % compute number of importers 
% % %     number_exporters_g(:,i_year) = nnz(k_D_out_g(:,i_year)); % compute number of exporter 
% % %     
% % %     k_U_b(:,i_year) = nansum(AU_b)';
% % %     k_D_in_b(:,i_year) =  nansum(AD_b,1)'; % sum each column for import
% % %     number_importers_b(:,i_year) = nnz(k_D_in_b(:,i_year)); % compute number of importers 
% % %     number_exporters_b(:,i_year) = nnz(k_D_out_b(:,i_year)); % compute number of exporter 
% % %     
% % %     k_U_3(:,i_year) = nansum(AU_3)';
% % %     k_D_in_3(:,i_year) =  nansum(AD_3,1)'; % sum each column for import
% % %     number_importers_3(:,i_year) = nnz(k_D_in_3(:,i_year)); % compute number of importers 
% % %     number_exporters_3(:,i_year) = nnz(k_D_out_3(:,i_year)); % compute number of exporter 
% % %     
% %     %% Compute weighted degrees in network
% %     kw_U_g(:,i_year) = nansum(WU_g)';
% %     kw_D_in_g(:,i_year) =  nansum(WD_g,1)'; % sum each column for import
% %     kw_D_out_g(:,i_year) = nansum(WD_g,2);  % sum each row for export
% % 
% %     kw_U_b(:,i_year) = nansum(WU_b)';
% %     kw_D_in_b(:,i_year) =  nansum(WD_b,1)'; % sum each column for import
% %     kw_D_out_b(:,i_year) = nansum(WD_b,2);  % sum each row for export
% %     
% %     kw_U_3(:,i_year) = nansum(WU_3)';
% %     kw_D_in_3(:,i_year) =  nansum(WD_3,1)'; % sum each column for import
% %     kw_D_out_3(:,i_year) = nansum(WD_3,2);  % sum each row for export
  
% %     %% Clustering Coefficient
% %     CCout_g = mexGraphClusteringCoefficient(Graph_g,[],'inverse');
% %     CCout_b = mexGraphClusteringCoefficient(Graph_b,[],'inverse');
% %     CCout_3 = mexGraphClusteringCoefficient(Graph_3,[],'inverse');