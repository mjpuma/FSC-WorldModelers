## Compute Network statistics for Food Shock Cascade (FSC) Model

## Compute network statistics on export matrix ----
#    https://www.r-bloggers.com/network-centrality-in-r-an-introduction/
#    https://kateto.net/networks-r-igraph
#    https://kateto.net/netscix2016.html

library(igraph)
library(ggraph)
library(tidyverse)
library(netrankr)
library(tnet) # for clustering analysis

# RStudio version: Specify arguments  ====
setwd("~/Coronavirus/GitHub_codes_data/")


source("/Users/puma/GitHub_mjpuma/FSC-WorldModelers/main/FSC_network_funcs.R")

runname <- c('Wheat_Avg20152017')
#runname <- c('Rice_Avg20152017')
#runname <- c('Maize_Avg20152017')
Ematrix<-readRDS(file = paste0("COVID-19_data/data_network/",runname,"ExportStatic.rds"))

unitname <- c('kcal')

#list actoday country
ACToday_country <- c("Bangladesh","Colombia","Guatemala","Senegal","Vietnam","Ethiopia")
ACToday_iso3 <- c("BGD","COL","GTM","SEN","VNM","ETH")
IASC_country <- c("Burundi","Djibouti","Eritrea","Ethiopia","Kenya","Rwanda","Somalia",
                  "South Sudan","Tanzania","Uganda","Haiti","Mozambique")
IASC_iso3 <- c("BDI","DJI","ERI","ETH","KEN","RWA","SOM","SSD","TZA","UGA","HTI","MOZ")

num_yrs <- 1#dim(Ematrix)[3]
num_countries <- dim(Ematrix)[2]

# Initialize  output vectors ====
Exports_ACToday <-  array(0, c(length(ACToday_country), num_countries))
Imports_ACToday <-  array(0, c(num_countries, length(ACToday_country)))
Exports_IASC    <-  array(0, c(length(IASC_country), num_countries))
Imports_IASC    <-  array(0, c(num_countries, length(IASC_country)))

#for (i in 1:num_yrs) {
#   Extract 2D arrays
i=1
G  <- Ematrix[, , i]
Gnet <- graph_from_adjacency_matrix(G, mode = "directed", weighted = TRUE)

# Node degrees
Gdeg_total <- degree(Gnet, mode = "all")
Gdeg_out <- degree(Gnet, mode = "out")
Gdeg_in <- degree(Gnet, mode = "in")

Gbtw <- betweenness(Gnet,directed = TRUE, weights = NULL)
Gbtw <-Gbtw/((num_countries-1)*(num_countries-2)/2)
Geigencentral <- eigen_centrality(Gnet)$vector
Gedgedensity<-edge_density(Gnet, loops=F)
hs <- hub_score(Gnet)$vector

Gstrength_total <- strength(Gnet)
Gstrength_out <- strength(Gnet, mode = "out")
Gstrength_in <- strength(Gnet, mode = "in")

# Subset: Columbia World Project: ACToday
ACToday_Exports <- G[ACToday_iso3, ]
ACToday_Imports <- G[, ACToday_iso3]
ACToday_deg_total <- Gdeg_total[ACToday_iso3]
ACToday_deg_out <- Gdeg_out[ACToday_iso3]
ACToday_deg_in <- Gdeg_in[ACToday_iso3]

# Subset: Inter-Agency Standing Committee
IASC_Exports <- G[IASC_iso3, ]
IASC_Imports <- G[, IASC_iso3]
IASC_deg_total <- Gdeg_total[IASC_iso3]
IASC_deg_out <- Gdeg_out[IASC_iso3]
IASC_deg_in <- Gdeg_in[IASC_iso3]  

## Add column names to output files
column_names<-runname #c('Yr 0', 'Yr 1', 'Yr 2', 'Yr 3', 'Yr 4')
#colnames(Gdeg_total)  <- column_names
#colnames(Gdeg_out)  <- column_names

## Save as CSV
# Subset countries
ACToday_Exports_df <- data.frame(ACToday_Exports)
ACToday_Exports_df <- tibble::rownames_to_column(ACToday_Exports_df, "iso3")
write.csv(ACToday_Exports_df,paste0("COVID-19_data/data_network/", runname[1],"Exports_ACToday_", unitname, ".csv"), row.names = FALSE)

ACToday_Imports_df <- data.frame(ACToday_Imports)
ACToday_Imports_df <- tibble::rownames_to_column(ACToday_Imports_df, "iso3")
write.csv(ACToday_Imports_df,paste0("COVID-19_data/data_network/", runname[1],"Imports_ACToday_", unitname, ".csv"), row.names = FALSE)

IASC_Exports_df <- data.frame(IASC_Exports)
IASC_Exports_df <- tibble::rownames_to_column(IASC_Exports_df, "iso3")
write.csv(IASC_Exports_df,paste0("COVID-19_data/data_network/", runname[1],"IASC_Exports_", unitname, ".csv"), row.names = FALSE)

IASC_Imports_df <- data.frame(IASC_Imports)
IASC_Imports_df <- tibble::rownames_to_column(IASC_Imports_df, "iso3")
write.csv(IASC_Imports_df,paste0("COVID-19_data/data_network/", runname[1],"IASC_Imports_", unitname, ".csv"), row.names = FALSE)

Gdeg_total_df <- data.frame(Gdeg_total)
Gdeg_total_df <- tibble::rownames_to_column(Gdeg_total_df, "iso3")
write.csv(Gdeg_total_df,paste0("COVID-19_data/data_network/", runname[1],"Gdeg_total_", unitname, ".csv"), row.names = FALSE)

Gdeg_out_df <- data.frame(Gdeg_out)
Gdeg_out_df <- tibble::rownames_to_column(Gdeg_out_df, "iso3")
write.csv(Gdeg_out_df,paste0("COVID-19_data/data_network/", runname[1],"Gdeg_out_", unitname, ".csv"), row.names = FALSE)

Gdeg_in_df <- data.frame(Gdeg_in)
Gdeg_in_df <- tibble::rownames_to_column(Gdeg_in_df, "iso3")
write.csv(Gdeg_in_df,paste0("COVID-19_data/data_network/", runname[1],"Gdeg_in_", unitname, ".csv"), row.names = FALSE)

Gstrength_total_df <- data.frame(Gstrength_total)
Gstrength_total_df <- tibble::rownames_to_column(Gstrength_total_df, "iso3")
write.csv(Gstrength_total_df,paste0("COVID-19_data/data_network/", runname[1],"Gstrength_total_", unitname, ".csv"), row.names = FALSE)

Gstrength_out_df <- data.frame(Gstrength_out)
Gstrength_out_df <- tibble::rownames_to_column(Gstrength_out_df, "iso3")
write.csv(Gstrength_out_df,paste0("COVID-19_data/data_network/", runname[1],"Gstrength_out_", unitname, ".csv"), row.names = FALSE)

Gstrength_in_df <- data.frame(Gstrength_in)
Gstrength_in_df <- tibble::rownames_to_column(Gstrength_in_df, "iso3")
write.csv(Gstrength_in_df,paste0("COVID-19_data/data_network/", runname[1],"Gstrength_in_", unitname, ".csv"), row.names = FALSE)

# Plots
hist(Gdeg_out, breaks=1:vcount(Gnet)-1, main="Histogram of out degree")
hist(Gdeg_in, breaks=1:vcount(Gnet)-1, main="Histogram of in degree")

Gdeg_out.dist <- degree_distribution(Gnet, cumulative=T, mode="out")
plot( x=0:max(Gdeg_out), y=1-Gdeg_out.dist, pch=19, cex=1.2, col="orange",xlab="Out Degree", ylab="Cumulative Frequencyof Out Degree")

Gdeg_in.dist <- degree_distribution(Gnet, cumulative=T, mode="in")
plot( x=0:max(Gdeg_in), y=1-Gdeg_in.dist, pch=19, cex=1.2, col="orange",xlab="In Degree", ylab="Cumulative Frequency of In Degree")

#trade openness;modeled, value density fraction