## Compute Network statistics for Food Shock Cascade (FSC) Model

## Compute network statistics on export matrix ----
#    https://www.r-bloggers.com/network-centrality-in-r-an-introduction/
#    https://kateto.net/networks-r-igraph
#    https://kateto.net/netscix2016.html

library(igraph)
library(ggraph)
library(tidyverse)
library(netrankr)

# RStudio version: Specify arguments  ====
setwd("~/GitHub_mjpuma/FSC-WorldModelers/")


source("main/FSC_network_funcs.R")

#runname <- c('Wheat_Avg20152017')
#runname <- c('Rice_Avg20152017')
runname <- c('Maize_Avg20152017')

Ematrix<-readRDS(file = paste0("COVID-19_data/data_network/",runname,"ExportStatic.rds"))
load(paste0("COVID-19_data/", runname, "E0.RData")) #Export Matrix ordered by FAOSTAT country code (increasing)
unitname <- c('kcal')


#for (i in 1:num_yrs) {
#   Extract 2D arrays
#i=1
#G  <- Ematrix[, , i]

G  <- E0_avg
num_countries <- dim(G)[2]
num_yrs <- 1#dim(G)[3]

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


## Add column names to output files
column_names<-runname #c('Yr 0', 'Yr 1', 'Yr 2', 'Yr 3', 'Yr 4')
#colnames(Gdeg_total)  <- column_names
#colnames(Gdeg_out)  <- column_names

## Save as CSV
Gdeg_total_df <- data.frame(Gdeg_total)
Gdeg_total_df <- tibble::rownames_to_column(Gdeg_total_df, "iso3")
write.csv(Gdeg_total_df,paste0("COVID-19_data/data_network/GlobalMetrics/", runname[1],"Gdeg_total_", unitname, ".csv"), row.names = FALSE)

Gdeg_out_df <- data.frame(Gdeg_out)
Gdeg_out_df <- tibble::rownames_to_column(Gdeg_out_df, "iso3")
write.csv(Gdeg_out_df,paste0("COVID-19_data/data_network/GlobalMetrics/", runname[1],"Gdeg_out_", unitname, ".csv"), row.names = FALSE)

Gdeg_in_df <- data.frame(Gdeg_in)
Gdeg_in_df <- tibble::rownames_to_column(Gdeg_in_df, "iso3")
write.csv(Gdeg_in_df,paste0("COVID-19_data/data_network/GlobalMetrics/", runname[1],"Gdeg_in_", unitname, ".csv"), row.names = FALSE)

Gstrength_total_df <- data.frame(Gstrength_total)
Gstrength_total_df <- tibble::rownames_to_column(Gstrength_total_df, "iso3")
write.csv(Gstrength_total_df,paste0("COVID-19_data/data_network/GlobalMetrics/", runname[1],"Gstrength_total_", unitname, ".csv"), row.names = FALSE)

Gstrength_out_df <- data.frame(Gstrength_out)
Gstrength_out_df <- tibble::rownames_to_column(Gstrength_out_df, "iso3")
write.csv(Gstrength_out_df,paste0("COVID-19_data/data_network/GlobalMetrics/", runname[1],"Gstrength_out_", unitname, ".csv"), row.names = FALSE)

Gstrength_in_df <- data.frame(Gstrength_in)
Gstrength_in_df <- tibble::rownames_to_column(Gstrength_in_df, "iso3")
write.csv(Gstrength_in_df,paste0("COVID-19_data/data_network/GlobalMetrics/", runname[1],"Gstrength_in_", unitname, ".csv"), row.names = FALSE)

Gbtw_df <- data.frame(Gbtw)
Gbtw_df <- tibble::rownames_to_column(Gbtw_df, "iso3")
write.csv(Gbtw_df,paste0("COVID-19_data/data_network/GlobalMetrics/", runname[1],"Gbtw_", unitname, ".csv"), row.names = FALSE)



# Plots
hist(Gdeg_out, breaks=1:vcount(Gnet)-1, main="Histogram of out degree")
hist(Gdeg_in, breaks=1:vcount(Gnet)-1, main="Histogram of in degree")

Gdeg_out.dist <- degree_distribution(Gnet, cumulative=T, mode="out")
plot( x=0:max(Gdeg_out), y=1-Gdeg_out.dist, pch=19, cex=1.2, col="orange",xlab="Out Degree", ylab="Cumulative Frequencyof Out Degree")

Gdeg_in.dist <- degree_distribution(Gnet, cumulative=T, mode="in")
plot( x=0:max(Gdeg_in), y=1-Gdeg_in.dist, pch=19, cex=1.2, col="orange",xlab="In Degree", ylab="Cumulative Frequency of In Degree")


