#loading libraries
library(sf)
library(dplyr)
library(ggplot2)
library(reshape2)
library(stringr)
library(tmap)
library(tmaptools)
library(leaflet)
library(ggplot2)
library(rgdal)
library(plyr)
library(foreign) # reading .dbf files

# Setting working directory
setwd("/FSC-WorldModelers/")

# Making shapefile dataframe----

shp <- readOGR(dsn = './CFS_dissolved_Great_Lakes',layer = 'CFSArea_DissoCounty_GreatLakes')

faf_zone_id <- read.csv('inputs/CFS_area_code_FAF4_zone_id.csv')
faf_zone_id <- faf_zone_id[,c('FAF','CFSAREANAM')]
shp.data <- data.frame(id=rownames(shp@data),
                       CFSAREANAM=shp@data$CFS12_NAME)
shp.data <- merge(faf_zone_id, shp.data)
shp.df1 <- read.dbf('inputs/US_Shapefile/US_Shapefile.dbf')
shp.df2 <- read.csv('inputs/CFS_area_code_FAF4_zone_id.csv')
colnames(shp.df2)[6]<- "CFS12_NAME"
df12<- merge(shp.df1, shp.df2,by = "CFS12_NAME")

# Conumption change wrt initial consumption calculation ----

data <- read.csv('./outputs/ConsumptiontoC0Series.csv')
data$Value <- (1 - data$Value)*100

input <- merge(shp.data, data, by.x='FAF', by.y = 'faf')

# Fortify data to extract spatial information
shp.fort <- fortify(shp)
map.df <- join(shp.fort, input, by='id')
#map.df$cons[map.df$cons<=0] <- NA #convert zeros to NA

# Calculate plot limits
limit <- max(abs(data$Value))

# Plot data
ggplot(map.df, aes(x=long,y=lat,group=group)) + 
  geom_polygon(aes(fill=Value)) + # sets what to display: VWS_m3_prod/yield, Storage_Bu, ...
  coord_map(xlim=c(-125, -66),ylim=c(24, 50)) + 
  scale_fill_distiller(name='',
                       palette='RdBu',
                       direction=1,
                       na.value='grey90') +
  #scale_color_gradient2(low='red',mid='grey',high='blue',midpoint=0,na.value='grei90') +
  #scale_color_gradientn(
  #			colours=c('red','grey','blue'),
  #			values=rescale(c((min(map.df$cons)-1),0,(max(map.df$cons)+1))),
  #			na.value='grey90') +
  # Define title label, etc
  #labs(title=sprintf('%s\n%s, %s', str_replace_all(identifier,'_',' '), simpleCap(tolower(commodity)), year),
  ##labs(title=sprintf('%s\n%s, 1996-2005', str_replace_all(identifier,'_',' '), simpleCap(tolower(commodity))),
  #    #subtitle=sprintf('Min: %s, Max: %s', formatC( min(map.df$plot_fill,na.rm=TRUE), format='e', digits=2 ), formatC( max(map.df$plot_fill,na.rm=TRUE), format='e', digits=2 ) ),
  labs(x='', # x axis label
       y='', 
       title = "Percentage change in consumption wrt to initial consumption") + # y axis label
  theme(#plot.title=element_text(size=24),
    #plot.subtitle=element_text(size=20),
    legend.title=element_text(size=24),
    legend.text=element_text(size=20),
    panel.grid.major=element_blank(), 
    panel.grid.minor=element_blank(), 
    panel.background=element_blank(), 
    axis.line=element_blank(), 
    axis.text=element_blank(), 
    axis.ticks=element_blank())

# Save plot
path <- 'maps/faf_ConsC0_sctg5_lbs_2017_map.pdf'

ggsave(path)#,
#width = 7,
#height = 4.25)#, dpi = 1200)
print(sprintf('Plot saved to %s',path))


#Export change wrt initial export calculation ----
#Changing directories

quant = 0
data <- read.csv('outputs/ExportSeries.csv')
df_1 <- data[,c("X",grep('^[a-zA-Z0-9]+\\.1$', names(data), value=TRUE))]
colnames(df_1)[1] <- "orig_faf"
df_1<- melt(df_1, id= c('orig_faf'), variable.name = "dest_faf", value.name = "export_1")
df_1$dest_faf <- gsub("^X([0-9]+)\\.1","\\1", df_1$dest_faf)
df_2 <- data[,c("X",grep('^[a-zA-Z0-9]+\\.2$', names(data), value=TRUE))]
colnames(df_2)[1] <- "orig_faf"
df_2<- melt(df_2, id= c('orig_faf'), variable.name = "dest_faf", value.name = "export_2")
df_2$dest_faf <- gsub("^X([0-9]+)\\.2","\\1", df_2$dest_faf)

df <- merge(df_1,df_2)
df$diff <- df$export_1 - df$export_2
df <- df[!df$diff == 0,]
df$diffprop <- (df$diff/df$export_1)*100
df <- df[!df$orig_faf == df$dest_faf,]

#Fortify to extract spatial information
map <- fortify(shp)

# merging by destination faf zones
df <- merge(df,df12,by.x='dest_faf',by.y='FAF')
df <- df[,c('orig_faf','dest_faf','diffprop','xcoord','ycoord')]

colnames(df) <- c('ori','des','val','des_lon','des_lat')

# merging by origin faf zones
df <- merge(df,df12,by.x='ori',by.y='FAF') # origin

df <- df[,c(1:5,9:10)]
colnames(df) <- c('ori','des','val','des_lon','des_lat','ori_lon','ori_lat')

# Organize smallest to largest for plotting
df <- df[order(df$val),]

# Limit which arcs to display
df <- df[df$val >= quantile(df$val,quant,na.rm=T),]
df <- df[!df$ori == df$des,]
head(df)

# Create plot ----

ggplot() + 
  geom_polygon(data= map, aes(long,lat, group=group), fill='gray30') +
  geom_curve(data=df, aes(x = ori_lon, y = ori_lat, xend = des_lon, yend = des_lat, color=val, alpha=val),curvature = -0.2, arrow = arrow(length = unit(0.01, 'npc'))) +
  scale_colour_gradient(low='#9ecae1',high='#3182bd') + # blues 
  coord_equal() + 
  coord_quickmap(xlim=c(-200, -50),ylim=c(20, 70)) + #boundary box that excludes Alask and hawaii
  ggtitle("SCTG 5 FAF Export % Difference") +
  labs(alpha = "Value in %") +
  labs(color = "Value in %") +
  labs(x = "Longitude", y = "Latitude")
ggsave('maps/faf_exportchange_sctg5_2017.pdf')
