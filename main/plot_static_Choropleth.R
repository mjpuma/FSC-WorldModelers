#install.packages("ggplot2")   #only do this once
#install.packages("tidyverse") #only do this once
#install.packages("viridis")
#install.packages("countrycode")
#install.packages("libwgeom")
#install.packages(c("cowplot", "sf"))
#https://r-spatial.org/r/2018/10/25/ggplot2-sf-3.html
#https://rpsychologist.com/working-with-shapefiles-projections-and-world-maps-in-ggplot

library(countrycode)
library(ggplot2)              #needs to be done each r session
library(tidyverse)            #needs to be done each r session
library(viridis)
library(sf)
library(cowplot)


#classic dark-on-light theme for ggplot2 (theme_bw), 
# which is more appropriate for maps:
# theme_set(theme_bw())
working_directory <- "/Users/puma/GitHub_mjpuma/FSC-WorldModelers/"

# Map Data
mapdata <- map_data("world") %>% 
  filter(region != "Antarctica")

# Add iso3 codes to map data
mapdata$iso3 <- countryname(mapdata$region, destination = "iso3c", warn = TRUE)

# Numeric Data by Country
mydata <- read.csv(paste0(working_directory,"outputs/","outputFSC.csv"))

# Merge the datasets by Region
mapdata1 <- left_join(mapdata, mydata,  by = c("iso3"))

# Separate before and after data
mapdata_initial <- mapdata1 %>% filter(mapdata1$Year == 2020)
mapdata_final <- mapdata1 %>% filter(mapdata1$Year == 2021)

# Production Before Disruption
gworld2 <- ggplot(data = mapdata_initial, aes( x = long, y = lat, group = group)) +
    borders("world", colour = "grey50", xlim = c(-180,180), ylim = c(-60,90), size = .2) +
    geom_polygon(aes(fill = Production)) +
    coord_equal(expand = FALSE) +
    geom_rect(xmin = 20, xmax = 50, ymin = 25, ymax = 55, fill = NA, colour = "black", size = 1) +
    scale_fill_viridis_c(option = "plasma", trans = "sqrt", na.value="grey") +
    xlab("Longitude") + ylab("Latitude") +
    ggtitle("Production Before Disruption (in kilocalories)") +
    theme(panel.background = element_rect(fill = "azure"),
     panel.border = element_rect(fill = NA))

gblacksea <- ggplot(data = mapdata_initial, aes( x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = Production)) +
  annotate(geom = "text", x = 34, y = 43, label = "Black Sea", 
     fontface = "italic", color = "grey22", size = 6) +
  coord_sf(xlim = c(20, 50), ylim = c(25, 55), expand = FALSE) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt",na.value="white") +
  theme(legend.position = "none", axis.title.x = element_blank(), 
     axis.title.y = element_blank(), panel.background = element_rect(fill = "azure"), 
     panel.border = element_rect(fill = NA))

plot_grid(gworld2, gblacksea, nrow = 1, rel_widths = c(2.505, 1))
ggsave(paste0(working_directory, "outputs/", "Production_initial.png"), width = 15, height =  5)


# Production After Disruption
gworld2 <- ggplot(data = mapdata_final, aes( x = long, y = lat, group = group)) +
    borders("world", colour = "grey50", xlim = c(-180,180), ylim = c(-60,90), size = .2) +
    geom_polygon(aes(fill = Production)) +
    coord_equal(expand = FALSE) +
    geom_rect(xmin = 20, xmax = 50, ymin = 25, ymax = 55, fill = NA, colour = "black", size = 1) +
    scale_fill_viridis_c(option = "plasma", trans = "sqrt", na.value="grey") +
    xlab("Longitude") + ylab("Latitude") +
    ggtitle("Production After Disruption (in kilocalories)") +
    theme(panel.background = element_rect(fill = "azure"),
     panel.border = element_rect(fill = NA))

gblacksea <- ggplot(data = mapdata_final, aes( x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = Production)) +
  annotate(geom = "text", x = 34, y = 43, label = "Black Sea", 
     fontface = "italic", color = "grey22", size = 6) +
  coord_sf(xlim = c(20, 50), ylim = c(25, 55), expand = FALSE) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt",na.value="white") +
  theme(legend.position = "none", axis.title.x = element_blank(), 
     axis.title.y = element_blank(), panel.background = element_rect(fill = "azure"), 
     panel.border = element_rect(fill = NA))

plot_grid(gworld2, gblacksea, nrow = 1, rel_widths = c(2.505, 1))
ggsave(paste0(working_directory, "outputs/", "Production_final.png"), width = 15, height =  5)

# Imports Before Disruption
gworld2 <- ggplot(data = mapdata_initial, aes( x = long, y = lat, group = group)) +
    borders("world", colour = "grey50", xlim = c(-180,180), ylim = c(-60,90), size = .2) +
    geom_polygon(aes(fill = Imports)) +
    coord_equal(expand = FALSE) +
    geom_rect(xmin = 20, xmax = 50, ymin = 25, ymax = 55, fill = NA, colour = "black", size = 1) +
    scale_fill_viridis_c(option = "plasma", trans = "sqrt", na.value="grey") +
    xlab("Longitude") + ylab("Latitude") +
    ggtitle("Imports Before Disruption (in kilocalories)") +
    theme(panel.background = element_rect(fill = "azure"),
     panel.border = element_rect(fill = NA))

gblacksea <- ggplot(data = mapdata_initial, aes( x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = Imports)) +
  annotate(geom = "text", x = 34, y = 43, label = "Black Sea", 
     fontface = "italic", color = "grey22", size = 6) +
  coord_sf(xlim = c(20, 50), ylim = c(25, 55), expand = FALSE) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt",na.value="white") +
  theme(legend.position = "none", axis.title.x = element_blank(), 
     axis.title.y = element_blank(), panel.background = element_rect(fill = "azure"), 
     panel.border = element_rect(fill = NA))

plot_grid(gworld2, gblacksea, nrow = 1, rel_widths = c(2.505, 1))
ggsave(paste0(working_directory, "outputs/", "Imports_initial.png"), width = 15, height =  5)


# Imports After Disruption
gworld2 <- ggplot(data = mapdata_final, aes( x = long, y = lat, group = group)) +
    borders("world", colour = "grey50", xlim = c(-180,180), ylim = c(-60,90), size = .2) +
    geom_polygon(aes(fill = Imports)) +
    coord_equal(expand = FALSE) +
    geom_rect(xmin = 20, xmax = 50, ymin = 25, ymax = 55, fill = NA, colour = "black", size = 1) +
    scale_fill_viridis_c(option = "plasma", trans = "sqrt", na.value="grey") +
    xlab("Longitude") + ylab("Latitude") +
    ggtitle("Imports After Disruption (in kilocalories)") +
    theme(panel.background = element_rect(fill = "azure"),
     panel.border = element_rect(fill = NA))

gblacksea <- ggplot(data = mapdata_final, aes( x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = Imports)) +
  annotate(geom = "text", x = 34, y = 43, label = "Black Sea", 
     fontface = "italic", color = "grey22", size = 6) +
  coord_sf(xlim = c(20, 50), ylim = c(25, 55), expand = FALSE) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt",na.value="white") +
  theme(legend.position = "none", axis.title.x = element_blank(), 
     axis.title.y = element_blank(), panel.background = element_rect(fill = "azure"), 
     panel.border = element_rect(fill = NA))

plot_grid(gworld2, gblacksea, nrow = 1, rel_widths = c(2.505, 1))
ggsave(paste0(working_directory, "outputs/", "Imports_final.png"), width = 15, height =  5)

# Exports Before Disruption
gworld2 <- ggplot(data = mapdata_initial, aes( x = long, y = lat, group = group)) +
    borders("world", colour = "grey50", xlim = c(-180,180), ylim = c(-60,90), size = .2) +
    geom_polygon(aes(fill = Exports)) +
    coord_equal(expand = FALSE) +
    geom_rect(xmin = 20, xmax = 50, ymin = 25, ymax = 55, fill = NA, colour = "black", size = 1) +
    scale_fill_viridis_c(option = "plasma", trans = "sqrt", na.value="grey") +
    xlab("Longitude") + ylab("Latitude") +
    ggtitle("Exports Before Disruption (in kilocalories)") +
    theme(panel.background = element_rect(fill = "azure"),
     panel.border = element_rect(fill = NA))

gblacksea <- ggplot(data = mapdata_initial, aes( x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = Exports)) +
  annotate(geom = "text", x = 34, y = 43, label = "Black Sea", 
     fontface = "italic", color = "grey22", size = 6) +
  coord_sf(xlim = c(20, 50), ylim = c(25, 55), expand = FALSE) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt",na.value="white") +
  theme(legend.position = "none", axis.title.x = element_blank(), 
     axis.title.y = element_blank(), panel.background = element_rect(fill = "azure"), 
     panel.border = element_rect(fill = NA))

plot_grid(gworld2, gblacksea, nrow = 1, rel_widths = c(2.505, 1))
ggsave(paste0(working_directory, "outputs/", "Exports_initial.png"), width = 15, height =  5)

# Exports After Disruption
gworld2 <- ggplot(data = mapdata_final, aes( x = long, y = lat, group = group)) +
    borders("world", colour = "grey50", xlim = c(-180,180), ylim = c(-60,90), size = .2) +
    geom_polygon(aes(fill = Exports)) +
    coord_equal(expand = FALSE) +
    geom_rect(xmin = 20, xmax = 50, ymin = 25, ymax = 55, fill = NA, colour = "black", size = 1) +
    scale_fill_viridis_c(option = "plasma", trans = "sqrt", na.value="grey") +
    xlab("Longitude") + ylab("Latitude") +
    ggtitle("Exports After Disruption (in kilocalories)") +
    theme(panel.background = element_rect(fill = "azure"),
     panel.border = element_rect(fill = NA))

gblacksea <- ggplot(data = mapdata_final, aes( x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = Exports)) +
  annotate(geom = "text", x = 34, y = 43, label = "Black Sea", 
     fontface = "italic", color = "grey22", size = 6) +
  coord_sf(xlim = c(20, 50), ylim = c(25, 55), expand = FALSE) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt",na.value="white") +
  theme(legend.position = "none", axis.title.x = element_blank(), 
     axis.title.y = element_blank(), panel.background = element_rect(fill = "azure"), 
     panel.border = element_rect(fill = NA))

plot_grid(gworld2, gblacksea, nrow = 1, rel_widths = c(2.505, 1))
ggsave(paste0(working_directory, "outputs/", "Exports_final.png"), width = 15, height =  5)

# Impaired Supply
gworld2 <- ggplot(data = mapdata_final, aes( x = long, y = lat, group = group)) +
    borders("world", colour = "grey50", xlim = c(-180,180), ylim = c(-60,90), size = .2) +
    geom_polygon(aes(fill = Impaired.Supply)) +
    coord_equal(expand = FALSE) +
    geom_rect(xmin = 20, xmax = 50, ymin = 25, ymax = 55, fill = NA, colour = "black", size = 1) +
    scale_fill_viridis_c(option = "plasma", trans = "sqrt", na.value="grey") +
    xlab("Longitude") + ylab("Latitude") +
    ggtitle("Impaired Supply (in kilocalories)") +
    theme(panel.background = element_rect(fill = "azure"),
     panel.border = element_rect(fill = NA))

gblacksea <- ggplot(data = mapdata_final, aes( x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = Impaired.Supply)) +
  annotate(geom = "text", x = 34, y = 43, label = "Black Sea", 
     fontface = "italic", color = "grey22", size = 6) +
  coord_sf(xlim = c(20, 50), ylim = c(25, 55), expand = FALSE) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt",na.value = "white") +
  theme(legend.position = "none", axis.title.x = element_blank(), 
     axis.title.y = element_blank(), panel.background = element_rect(fill = "azure"), 
     panel.border = element_rect(fill = NA))

plot_grid(gworld2, gblacksea, nrow = 1, rel_widths = c(2.505, 1))
ggsave(paste0(working_directory, "outputs/", "ImpairedSupply.png"), width = 15, height =  5)

# Ratio of Impaired Supply To Reserves
gworld2 <- ggplot(data = mapdata_final, aes( x = long, y = lat, group = group)) +
    borders("world", colour = "grey50", xlim = c(-180,180), ylim = c(-60,90), size = .2) +
    geom_polygon(aes(fill = Ratio.of.Impaired.Supply.To.Reserves)) +
    coord_equal(expand = FALSE) +
    geom_rect(xmin = 20, xmax = 50, ymin = 25, ymax = 55, fill = NA, colour = "black", size = 1) +
    scale_fill_viridis_c(option = "plasma", trans = "sqrt", na.value="grey",name="Impaired Supply\n/Reserves") +
    xlab("Longitude") + ylab("Latitude") +
    ggtitle("Ratio of Impaired Supply To Reserves (in kilocalories)") +
    theme(panel.background = element_rect(fill = "azure"),
     panel.border = element_rect(fill = NA))

gblacksea <- ggplot(data = mapdata_final, aes( x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = Ratio.of.Impaired.Supply.To.Reserves)) +
  annotate(geom = "text", x = 34, y = 43, label = "Black Sea", 
     fontface = "italic", color = "grey22", size = 6) +
  coord_sf(xlim = c(20, 50), ylim = c(25, 55), expand = FALSE) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt",na.value = "white") +
  theme(legend.position = "none", axis.title.x = element_blank(), 
     axis.title.y = element_blank(), panel.background = element_rect(fill = "azure"), 
     panel.border = element_rect(fill = NA))

plot_grid(gworld2, gblacksea, nrow = 1, rel_widths = c(2.505, 1))
ggsave(paste0(working_directory, "outputs/", "RatioofImpairedSupplyToReserves.png"), width = 15, height =  5)

# Number of Export Partners Before Disruption
gworld2 <- ggplot(data = mapdata_initial, aes( x = long, y = lat, group = group)) +
    borders("world", colour = "grey50", xlim = c(-180,180), ylim = c(-60,90), size = .2) +
    geom_polygon(aes(fill = Gdeg_out)) +
    coord_equal(expand = FALSE) +
    geom_rect(xmin = 20, xmax = 50, ymin = 25, ymax = 55, fill = NA, colour = "black", size = 1) +
    scale_fill_viridis_c(option = "plasma", trans = "sqrt", na.value="grey") +
    xlab("Longitude") + ylab("Latitude") +
    ggtitle("Number of Export Partners Before Disruption (in kilocalories)") +
    theme(panel.background = element_rect(fill = "azure"),
     panel.border = element_rect(fill = NA))

gblacksea <- ggplot(data = mapdata_initial, aes( x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = Gdeg_out)) +
  annotate(geom = "text", x = 34, y = 43, label = "Black Sea", 
     fontface = "italic", color = "grey22", size = 6) +
  coord_sf(xlim = c(20, 50), ylim = c(25, 55), expand = FALSE) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt",na.value="white") +
  theme(legend.position = "none", axis.title.x = element_blank(), 
     axis.title.y = element_blank(), panel.background = element_rect(fill = "azure"), 
     panel.border = element_rect(fill = NA))

plot_grid(gworld2, gblacksea, nrow = 1, rel_widths = c(2.505, 1))
ggsave(paste0(working_directory, "outputs/", "Gdeg_out_initial.png"), width = 15, height =  5)

# Exports After Disruption
gworld2 <- ggplot(data = mapdata_final, aes( x = long, y = lat, group = group)) +
    borders("world", colour = "grey50", xlim = c(-180,180), ylim = c(-60,90), size = .2) +
    geom_polygon(aes(fill = Gdeg_out)) +
    coord_equal(expand = FALSE) +
    geom_rect(xmin = 20, xmax = 50, ymin = 25, ymax = 55, fill = NA, colour = "black", size = 1) +
    scale_fill_viridis_c(option = "plasma", trans = "sqrt", na.value="grey") +
    xlab("Longitude") + ylab("Latitude") +
    ggtitle("Number of Export Partners After Disruption (in kilocalories)") +
    theme(panel.background = element_rect(fill = "azure"),
     panel.border = element_rect(fill = NA))

gblacksea <- ggplot(data = mapdata_final, aes( x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = Gdeg_out)) +
  annotate(geom = "text", x = 34, y = 43, label = "Black Sea", 
     fontface = "italic", color = "grey22", size = 6) +
  coord_sf(xlim = c(20, 50), ylim = c(25, 55), expand = FALSE) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt",na.value="white") +
  theme(legend.position = "none", axis.title.x = element_blank(), 
     axis.title.y = element_blank(), panel.background = element_rect(fill = "azure"), 
     panel.border = element_rect(fill = NA))

plot_grid(gworld2, gblacksea, nrow = 1, rel_widths = c(2.505, 1))
ggsave(paste0(working_directory, "outputs/", "Gdeg_out_final.png"), width = 15, height =  5)

# Number of Import Partners Before Disruption
gworld2 <- ggplot(data = mapdata_initial, aes( x = long, y = lat, group = group)) +
    borders("world", colour = "grey50", xlim = c(-180,180), ylim = c(-60,90), size = .2) +
    geom_polygon(aes(fill = Gdeg_in)) +
    coord_equal(expand = FALSE) +
    geom_rect(xmin = 20, xmax = 50, ymin = 25, ymax = 55, fill = NA, colour = "black", size = 1) +
    scale_fill_viridis_c(option = "plasma", trans = "sqrt", na.value="grey") +
    xlab("Longitude") + ylab("Latitude") +
    ggtitle("Number of Import Partners Before Disruption (in kilocalories)") +
    theme(panel.background = element_rect(fill = "azure"),
     panel.border = element_rect(fill = NA))

gblacksea <- ggplot(data = mapdata_initial, aes( x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = Gdeg_in)) +
  annotate(geom = "text", x = 34, y = 43, label = "Black Sea", 
     fontface = "italic", color = "grey22", size = 6) +
  coord_sf(xlim = c(20, 50), ylim = c(25, 55), expand = FALSE) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt",na.value="white") +
  theme(legend.position = "none", axis.title.x = element_blank(), 
     axis.title.y = element_blank(), panel.background = element_rect(fill = "azure"), 
     panel.border = element_rect(fill = NA))

plot_grid(gworld2, gblacksea, nrow = 1, rel_widths = c(2.505, 1))
ggsave(paste0(working_directory, "outputs/", "Gdegin_initial.png"), width = 15, height =  5)

# Exports After Disruption
gworld2 <- ggplot(data = mapdata_final, aes( x = long, y = lat, group = group)) +
    borders("world", colour = "grey50", xlim = c(-180,180), ylim = c(-60,90), size = .2) +
    geom_polygon(aes(fill = Gdeg_in)) +
    coord_equal(expand = FALSE) +
    geom_rect(xmin = 20, xmax = 50, ymin = 25, ymax = 55, fill = NA, colour = "black", size = 1) +
    scale_fill_viridis_c(option = "plasma", trans = "sqrt", na.value="grey") +
    xlab("Longitude") + ylab("Latitude") +
    ggtitle("Number of Import Partners After Disruption (in kilocalories)") +
    theme(panel.background = element_rect(fill = "azure"),
     panel.border = element_rect(fill = NA))

gblacksea <- ggplot(data = mapdata_final, aes( x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = Gdeg_in)) +
  annotate(geom = "text", x = 34, y = 43, label = "Black Sea", 
     fontface = "italic", color = "grey22", size = 6) +
  coord_sf(xlim = c(20, 50), ylim = c(25, 55), expand = FALSE) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt",na.value="white") +
  theme(legend.position = "none", axis.title.x = element_blank(), 
     axis.title.y = element_blank(), panel.background = element_rect(fill = "azure"), 
     panel.border = element_rect(fill = NA))

plot_grid(gworld2, gblacksea, nrow = 1, rel_widths = c(2.505, 1))
ggsave(paste0(working_directory, "outputs/", "Gdeg_in_final.png"), width = 15, height =  5)


# Number of Import Partners Before Disruption
gworld2 <- ggplot(data = mapdata_initial, aes( x = long, y = lat, group = group)) +
    borders("world", colour = "grey50", xlim = c(-180,180), ylim = c(-60,90), size = .2) +
    geom_polygon(aes(fill = Gdeg_in)) +
    coord_equal(expand = FALSE) +
    geom_rect(xmin = 20, xmax = 50, ymin = 25, ymax = 55, fill = NA, colour = "black", size = 1) +
    scale_fill_viridis_c(option = "plasma", trans = "sqrt", na.value="grey") +
    xlab("Longitude") + ylab("Latitude") +
    ggtitle("Number of Import Partners Before Disruption (in kilocalories)") +
    theme(panel.background = element_rect(fill = "azure"),
     panel.border = element_rect(fill = NA))

gblacksea <- ggplot(data = mapdata_initial, aes( x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = Gdeg_in)) +
  annotate(geom = "text", x = 34, y = 43, label = "Black Sea", 
     fontface = "italic", color = "grey22", size = 6) +
  coord_sf(xlim = c(20, 50), ylim = c(25, 55), expand = FALSE) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt",na.value="white") +
  theme(legend.position = "none", axis.title.x = element_blank(), 
     axis.title.y = element_blank(), panel.background = element_rect(fill = "azure"), 
     panel.border = element_rect(fill = NA))

plot_grid(gworld2, gblacksea, nrow = 1, rel_widths = c(2.505, 1))
ggsave(paste0(working_directory, "outputs/", "Gdegin_initial.png"), width = 15, height =  5)

# Number of Import Partners After Disruption
gworld2 <- ggplot(data = mapdata_final, aes( x = long, y = lat, group = group)) +
    borders("world", colour = "grey50", xlim = c(-180,180), ylim = c(-60,90), size = .2) +
    geom_polygon(aes(fill = Gdeg_in)) +
    coord_equal(expand = FALSE) +
    geom_rect(xmin = 20, xmax = 50, ymin = 25, ymax = 55, fill = NA, colour = "black", size = 1) +
    scale_fill_viridis_c(option = "plasma", trans = "sqrt", na.value="grey") +
    xlab("Longitude") + ylab("Latitude") +
    ggtitle("Number of Import Partners After Disruption (in kilocalories)") +
    theme(panel.background = element_rect(fill = "azure"),
     panel.border = element_rect(fill = NA))

gblacksea <- ggplot(data = mapdata_final, aes( x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = Gdeg_in)) +
  annotate(geom = "text", x = 34, y = 43, label = "Black Sea", 
     fontface = "italic", color = "grey22", size = 6) +
  coord_sf(xlim = c(20, 50), ylim = c(25, 55), expand = FALSE) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt",na.value="white") +
  theme(legend.position = "none", axis.title.x = element_blank(), 
     axis.title.y = element_blank(), panel.background = element_rect(fill = "azure"), 
     panel.border = element_rect(fill = NA))

plot_grid(gworld2, gblacksea, nrow = 1, rel_widths = c(2.505, 1))
ggsave(paste0(working_directory, "outputs/", "Gdeg_in_final.png"), width = 15, height =  5)

# Betweenness Centrality Before Disruption
gworld2 <- ggplot(data = mapdata_initial, aes( x = long, y = lat, group = group)) +
    borders("world", colour = "grey50", xlim = c(-180,180), ylim = c(-60,90), size = .2) +
    geom_polygon(aes(fill = Gbtw)) +
    coord_equal(expand = FALSE) +
    geom_rect(xmin = 20, xmax = 50, ymin = 25, ymax = 55, fill = NA, colour = "black", size = 1) +
    scale_fill_viridis_c(option = "plasma", trans = "sqrt", na.value="grey") +
    xlab("Longitude") + ylab("Latitude") +
    ggtitle("Betweenness Centrality Before Disruption (in kilocalories)") +
    theme(panel.background = element_rect(fill = "azure"),
     panel.border = element_rect(fill = NA))

gblacksea <- ggplot(data = mapdata_initial, aes( x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = Gbtw)) +
  annotate(geom = "text", x = 34, y = 43, label = "Black Sea", 
     fontface = "italic", color = "grey22", size = 6) +
  coord_sf(xlim = c(20, 50), ylim = c(25, 55), expand = FALSE) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt",na.value="white") +
  theme(legend.position = "none", axis.title.x = element_blank(), 
     axis.title.y = element_blank(), panel.background = element_rect(fill = "azure"), 
     panel.border = element_rect(fill = NA))

plot_grid(gworld2, gblacksea, nrow = 1, rel_widths = c(2.505, 1))
ggsave(paste0(working_directory, "outputs/", "Gbtw_initial.png"), width = 15, height =  5)

# Betweenness Centrality After Disruption
gworld2 <- ggplot(data = mapdata_final, aes( x = long, y = lat, group = group)) +
    borders("world", colour = "grey50", xlim = c(-180,180), ylim = c(-60,90), size = .2) +
    geom_polygon(aes(fill = Gbtw)) +
    coord_equal(expand = FALSE) +
    geom_rect(xmin = 20, xmax = 50, ymin = 25, ymax = 55, fill = NA, colour = "black", size = 1) +
    scale_fill_viridis_c(option = "plasma", trans = "sqrt", na.value="grey") +
    xlab("Longitude") + ylab("Latitude") +
    ggtitle("Betweenness Centrality After Disruption (in kilocalories)") +
    theme(panel.background = element_rect(fill = "azure"),
     panel.border = element_rect(fill = NA))

gblacksea <- ggplot(data = mapdata_final, aes( x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = Gbtw)) +
  annotate(geom = "text", x = 34, y = 43, label = "Black Sea", 
     fontface = "italic", color = "grey22", size = 6) +
  coord_sf(xlim = c(20, 50), ylim = c(25, 55), expand = FALSE) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt",na.value="white") +
  theme(legend.position = "none", axis.title.x = element_blank(), 
     axis.title.y = element_blank(), panel.background = element_rect(fill = "azure"), 
     panel.border = element_rect(fill = NA))

plot_grid(gworld2, gblacksea, nrow = 1, rel_widths = c(2.505, 1))
ggsave(paste0(working_directory, "outputs/", "Gbtw_final.png"), width = 15, height =  5)

# Eigenvector Centrality Before Disruption
gworld2 <- ggplot(data = mapdata_initial, aes( x = long, y = lat, group = group)) +
    borders("world", colour = "grey50", xlim = c(-180,180), ylim = c(-60,90), size = .2) +
    geom_polygon(aes(fill = Geigencentral)) +
    coord_equal(expand = FALSE) +
    geom_rect(xmin = 20, xmax = 50, ymin = 25, ymax = 55, fill = NA, colour = "black", size = 1) +
    scale_fill_viridis_c(option = "plasma", trans = "sqrt", na.value="grey") +
    xlab("Longitude") + ylab("Latitude") +
    ggtitle("Eigenvector Centrality Before Disruption (in kilocalories)") +
    theme(panel.background = element_rect(fill = "azure"),
     panel.border = element_rect(fill = NA))

gblacksea <- ggplot(data = mapdata_initial, aes( x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = Geigencentral)) +
  annotate(geom = "text", x = 34, y = 43, label = "Black Sea", 
     fontface = "italic", color = "grey22", size = 6) +
  coord_sf(xlim = c(20, 50), ylim = c(25, 55), expand = FALSE) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt",na.value="white") +
  theme(legend.position = "none", axis.title.x = element_blank(), 
     axis.title.y = element_blank(), panel.background = element_rect(fill = "azure"), 
     panel.border = element_rect(fill = NA))

plot_grid(gworld2, gblacksea, nrow = 1, rel_widths = c(2.505, 1))
ggsave(paste0(working_directory, "outputs/", "Geigencentral_initial.png"), width = 15, height =  5)

# Eigenvector Centrality After Disruption
gworld2 <- ggplot(data = mapdata_final, aes( x = long, y = lat, group = group)) +
    borders("world", colour = "grey50", xlim = c(-180,180), ylim = c(-60,90), size = .2) +
    geom_polygon(aes(fill = Geigencentral)) +
    coord_equal(expand = FALSE) +
    geom_rect(xmin = 20, xmax = 50, ymin = 25, ymax = 55, fill = NA, colour = "black", size = 1) +
    scale_fill_viridis_c(option = "plasma", trans = "sqrt", na.value="grey") +
    xlab("Longitude") + ylab("Latitude") +
    ggtitle("Eigenvector Centrality After Disruption (in kilocalories)") +
    theme(panel.background = element_rect(fill = "azure"),
     panel.border = element_rect(fill = NA))

gblacksea <- ggplot(data = mapdata_final, aes( x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = Geigencentral)) +
  annotate(geom = "text", x = 34, y = 43, label = "Black Sea", 
     fontface = "italic", color = "grey22", size = 6) +
  coord_sf(xlim = c(20, 50), ylim = c(25, 55), expand = FALSE) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt",na.value="white") +
  theme(legend.position = "none", axis.title.x = element_blank(), 
     axis.title.y = element_blank(), panel.background = element_rect(fill = "azure"), 
     panel.border = element_rect(fill = NA))

plot_grid(gworld2, gblacksea, nrow = 1, rel_widths = c(2.505, 1))
ggsave(paste0(working_directory, "outputs/", "Geigencentral_final.png"), width = 15, height =  5)