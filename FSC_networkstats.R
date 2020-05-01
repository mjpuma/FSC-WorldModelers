## Compute Network statistics for Food Shock Cascade (FSC) Model

## Compute network statistics on export matrix ----
#    https://www.r-bloggers.com/network-centrality-in-r-an-introduction/
#    https://kateto.net/networks-r-igraph
#    https://kateto.net/netscix2016.html


# RStudio version: Specify arguments  ====
setwd("~/GitHub_mjpuma/FSC-WorldModelers/")
#runname <- c('Wheat_Avg20162017')
#runname <- c('Maize_Avg20162017')
runname <- c('Rice_Avg20162017')
unitname <- c('kcal')

Ematrix<-readRDS(file = "outputs/ExportSeries.rds")

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
Exports_IASC    <-  array(0, c(length(ACToday_country), num_countries))
Imports_ACToday <-  array(0, c(num_countries, length(ACToday_country)))
Imports_IASC    <-  array(0, c(num_countries, length(ACToday_country)))

for (i in 1:num_yrs) {
  #   Extract 2D arrays
  G  <- Ematrix[, , i]
  Gnet <-graph_from_adjacency_matrix(G,mode = "directed",weighted=TRUE)
  
  # Node degrees
  Gdeg_total <- degree(Gnet, mode="all")
  Gdeg_out <- degree(Gnet, mode="out")
  Gdeg_in <- degree(Gnet, mode="in")
  
  Gstrength_total <- strength(Gnet)
  Gstrength_out <- strength(Gnet, mode="out")
  Gstrength_in <- strength(Gnet, mode="in")
  
  # Subset
  Exports_ACToday <- G[ACToday_iso3,] # Columbia World Project: ACToday
  Exports_IASC <- G[ACToday_iso3,]    # Inter-Agency Standing Committee
  
  Imports_ACToday <- G[,ACToday_iso3] # Columbia World Project: ACToday
  Imports_IASC <- G[,ACToday_iso3]    # Inter-Agency Standing Committee
  
}

## Add column names to output files
column_names<-runname #c('Yr 0', 'Yr 1', 'Yr 2', 'Yr 3', 'Yr 4')
#colnames(Gdeg_total)  <- column_names
#colnames(Gdeg_out)  <- column_names

## Save as CSV
write.csv(Exports_ACToday,paste0("outputs/", runname[1], "Exports_ACToday_", unitname, ".csv"), row.names = TRUE)
write.csv(Exports_IASC,paste0("outputs/", runname[1],"Exports_IASC_", unitname, ".csv"), row.names = TRUE)

write.csv(Imports_ACToday,paste0("outputs/", runname[1], "Imports_ACToday_", unitname, ".csv"), row.names = TRUE)
write.csv(Imports_IASC,paste0("outputs/", runname[1],"Imports_IASC_", unitname, ".csv"), row.names = TRUE)

write.csv(Gdeg_total,paste0("outputs/", runname[1],"Gdeg_total_", unitname, ".csv"), row.names = TRUE)
write.csv(Gdeg_out,paste0("outputs/", runname[1],"Gdeg_out_", unitname, ".csv"), row.names = TRUE)
write.csv(Gdeg_in,paste0("outputs/", runname[1],"Gdeg_in_", unitname, ".csv"), row.names = TRUE)

write.csv(Gstrength_total,paste0("outputs/", runname[1],"Gstrength_total_", unitname, ".csv"), row.names = TRUE)
write.csv(Gstrength_out,paste0("outputs/", runname[1],"Gstrength_out_", unitname, ".csv"), row.names = TRUE)
write.csv(Gstrength_in,paste0("outputs/", runname[1],"Gstrength_in_", unitname, ".csv"), row.names = TRUE)


# Plots
hist(Gdeg_out, breaks=1:vcount(Gnet)-1, main="Histogram of out degree")
hist(Gdeg_in, breaks=1:vcount(Gnet)-1, main="Histogram of in degree")

Gdeg_out.dist <- degree_distribution(Gnet, cumulative=T, mode="out")
plot( x=0:max(Gdeg_out), y=1-Gdeg_out.dist, pch=19, cex=1.2, col="orange",xlab="Out Degree", ylab="Cumulative Frequencyof Out Degree")

Gdeg_in.dist <- degree_distribution(Gnet, cumulative=T, mode="in")
plot( x=0:max(Gdeg_in), y=1-Gdeg_in.dist, pch=19, cex=1.2, col="orange",xlab="In Degree", ylab="Cumulative Frequency of In Degree")

#trade openness;modeled, value density fraction