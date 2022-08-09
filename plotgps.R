#PLOT GPS ---------------------------------------------------------------------
#
# Authors: Chloe Miller and Madelon Case
# Acknowledgements: Thanks to Lauren Hallett and the Hallett lab and also R Data Guy for template
# Email: chloeamiller@hotmail.com 
# 
# Date: 03 August 2022
#
# Script Name: Plot GPS
# 
# Script Description: Code used/sourced for making maps and scatter plots with Rangeland
#                     Analysis Platform and gps data, showing relationships/patterns
#                     (no actual maps/graphs, see plotgps_graphsmaps.R).
#                     
#
#


# SET WORKING DIRECTORY -----------------------------
cat("SETTING WORKING DIRECTORY...\n\n", sep = "")
wd <- "~/PlotGPSfolder"
setwd(wd)
cat("WORKING DIRECTORY HAS BEEN SET TO: ", wd, sep = "")


# INSTALL PACKAGES & LOAD LIBRARIES -----------------
cat("INSTALLING PACKAGES & LOADING LIBRARIES... \n\n", sep = "")
packages <- c("raster", "terra", "base", "plotKML", "sp")
n_packages <- length(packages) #count number of packages required

new.pkg <- packages[!(packages %in% installed.packages())] #determine which packages aren't installed

#install missing packages
if(length(new.pkg)){
  install.packages(new.pkg)
}

#load all required libraries
for(n in 1:n_packages){
  cat("Loading Library #", n, " of ", n_packages, "... Currently Loading: ", packages[n], "\n", sep = "")
  lib_load <- paste("library(\"",packages[n],"\")", sep = "") # create string of text for loading each library
  eval(parse(text = lib_load)) # evaluate the string to load the library
}

#### This section reads in our GPX data and assigns key variables.
#Reads GPX data into R (waypoints)
gpxdata <- readGPX(gpx.file = "gpsdata/GPS1_220630.GPX", waypoints = TRUE, bounds = FALSE, tracks = FALSE, routes = FALSE)

#Creates waypoints variable from gpxdata
waypoints <- gpxdata$waypoints

#Subsets out latitude, longitude, and name of waypoints into dataframe
lon <- data.frame(as.numeric(waypoints$lon))
lat <- data.frame(as.numeric(waypoints$lat))
plot_data <- data.frame(cbind(col1 = lon, col2 = lat, col3 = waypoints$name))
colnames(plot_data) <- c("lon", "lat", "name")

#### This next section creates subsests from gpxdata into specific groups (by name)
#Subsets out MapWellPt1
mapwellpt1 <- plot_data[plot_data$name == "MapWellPt1",]

#Makes MapwellPt1 into spatial points data frame and plot layer
RAPsaddle1986 <- raster("RAP_exports_Chloe_Saddle/RAP_VegCover_1986.tif") #Reads in .tiv SaddleBHwy RAP map from 1986, is used to make mapwellpt1_lyr crs
mapwellpt1_coords <- as.data.frame(cbind(mapwellpt1$lon, mapwellpt1$lat)) #creates dataframe with mapwellpt1 coordinates
mapwellpt1_spdf <- SpatialPointsDataFrame(coords = mapwellpt1_coords, data = mapwellpt1, proj4string = CRS("+proj=longlat")) #spdf
mapwellpt1_lyr <- spTransform(mapwellpt1_spdf, crs(RAPsaddle1986)) #plot layer

#Subsets all points for PowerLine
powerlinedata <- plot_data[grep("PowerlineWaterholePt",plot_data$name),]

#Make PowerLine data into spdf and then into plotable layer for RAP data
RAPsage1986 <- raster("RAP_exports_Chloe_Sagehen/RAP_VegCover_1986.tif") ##Reads in .tiv SageHen RAP map from 1986, is used to make all plot layers crs for remaining waterholes
powerline_coords <- as.data.frame(cbind(powerlinedata$lon, powerlinedata$lat))
powerline_spdf <- SpatialPointsDataFrame(coords=powerline_coords, data=powerlinedata, proj4string = CRS("+proj=longlat"))
powerline_lyr <- spTransform(powerline_spdf, crs(RAPsage1986))

#Subsets all points for BridgeWaterhole
bridgewaterholedata <- plot_data[grep("BridgeWaterholePt", plot_data$name),]

#Make BridgeWaterhole data into spdf and plot layer 
bridgewaterhole_coords <- as.data.frame(cbind(bridgewaterholedata$lon, bridgewaterholedata$lat))
bridgewaterhole_spdf <- SpatialPointsDataFrame(coords = bridgewaterhole_coords, data = bridgewaterholedata, proj4string = CRS("+proj=longlat"))
bridgewaterhole_lyr <- spTransform(bridgewaterhole_spdf, crs(RAPsage1986))

#Subsets all points for SageHenHillReservoir
sagehenhillreservoirdata <- plot_data[grep("SageHenHillResivor",plot_data$name),]

#Make SageHenHillReservoir data into spdf, plot layer 
sagehenhillreservoir_coords <- as.data.frame(cbind(sagehenhillreservoirdata$lon, sagehenhillreservoirdata$lat))
sagehenhillreservoir_spdf <- SpatialPointsDataFrame(coords = sagehenhillreservoir_coords, data = sagehenhillreservoirdata, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))
sagehenhillreservoir_lyr <- spTransform(sagehenhillreservoir_spdf, crs(RAPsage1986))

#Subset all points for PineSpring
pinespringdata <- plot_data[grep("PineSpringPt", plot_data$name),]

#Make PineSpring data into spdf, plot layer 
pinespring_coords <- as.data.frame(cbind(pinespringdata$lon, pinespringdata$lat))
pinespring_spdf <- SpatialPointsDataFrame(coords=pinespring_coords, data = pinespringdata, proj4string = CRS("+proj=longlat"))
pinespring_lyr <- spTransform(pinespring_spdf, crs(RAPsage1986))

#Subset all points for UpperFay
upperfaydata <- plot_data[grep("UpperFayPt", plot_data$name),]

#Make UpperFay data into spdf, plot layer
upperfay_coords <- as.data.frame(cbind(upperfaydata$lon, upperfaydata$lat))
upperfay_spdf <- SpatialPointsDataFrame(coords=upperfay_coords, data= upperfaydata, proj4string = CRS("+proj=longlat"))
upperfay_lyr <- spTransform(upperfay_spdf, crs(RAPsage1986))

#Subset all plot_data for just points plotted in Sagehen area (all ponds excluding Mapwell)
allfielddata <- plot_data[grep("Pt", plot_data$name),]
sagehenareadata <- subset(allfielddata, name!="MapWellPt1") 

#Make sagehenareadata into spdf, plot layer (all ponds excluding Mapwell)
sagehenarea_coords <- as.data.frame(cbind(sagehenareadata$lon, sagehenareadata$lat))
sagehenarea_spdf <- SpatialPointsDataFrame(coords = sagehenarea_coords, data = sagehenareadata, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))
sagehenarea_lyr <- spTransform(sagehenarea_spdf, crs(RAPsage1986))


#### This section creates plot polygons from points of waterholes (sagehen, all ponds excluding mapwell)

#Polygon for powerline
shpPts_powerline <- spsample(Spatial(bbox=bbox(powerline_lyr)), 20, type = "random") #Create SpatialPoints object
shpPolys_powerline <- SpatialPolygons(list(Polygons(list(Polygon(powerline_coords)), 1))) #Create SpatialPolygons object

#Polygon for upperfay
shpPts_upperfay <- spsample(Spatial(bbox=bbox(upperfay_lyr)), 20, type = "random")
shpPolys_upperfay <- SpatialPolygons(list(Polygons(list(Polygon(upperfay_coords)), 1)))

#Polygon for sagehenhillreservoir
shpPts_sagehenhillreservoir <-spsample(Spatial(bbox = bbox(sagehenhillreservoir_lyr)), 20, type = "random")
shpPolys_sagehenhillreservoir <- SpatialPolygons(list(Polygons(list(Polygon(sagehenhillreservoir_coords)), 1)))

#Polygon for bridgewaterhole
shpPts_bridgewaterhole <- spsample(Spatial(bbox = bbox(bridgewaterhole_lyr)), 20, type = "random")
shpPolys_bridgewaterhole <- SpatialPolygons(list(Polygons(list(Polygon(bridgewaterhole_coords)), 1)))

#Polygon for pinespring
shpPts_pinespring <- spsample(Spatial(bbox = bbox(pinespring_lyr)), 20, type = "random")
shpPolys_pinespring <- SpatialPolygons(list(Polygons(list(Polygon(pinespring_coords)), 1)))


#### This section reads in RAP data for Sagehen and Saddle butte areas

#Reads in .tiv SageHen RAP maps from all dates
readraster_sagehen <- list.files(path = "RAP_exports_Chloe_Sagehen", full.names = TRUE, pattern = ".tif$")
sagehen_stack <- stack(readraster_sagehen)

#Reads in .tiv SaddleButteHwy RAP maps from all dates (into stack)
readraster_saddle <- list.files(path = "RAP_exports_Chloe_Saddle", full.names = TRUE, pattern = ".tif$")
saddle_stack <- stack(readraster_saddle)


#### This section organizes stacks above to subset specific bands of raster
####Can call 5 specific raster bands: AFG, PFG, BGR, SHR, TRE

#Substring for saddle butte
bandnumbers_saddle <- substr(names(saddle_stack),19,19) # substring function pulls out elements from a particular location in character strings - here, the number denoting which band it's from is the 19th character in each layer name (e.g. in "RAP_VegCover_1986.1", the 19th character is the 1 at the end)

# Subset functional groups from full imported stacks (saddle)
saddle_stack_AFG <- saddle_stack[[which(bandnumbers_saddle==1)]]
saddle_stack_BGR <- saddle_stack[[which(bandnumbers_saddle==2)]]
saddle_stack_PFG <- saddle_stack[[which(bandnumbers_saddle==3)]]
saddle_stack_SHR <- saddle_stack[[which(bandnumbers_saddle==4)]]
saddle_stack_TRE <- saddle_stack[[which(bandnumbers_saddle==5)]]

#Substring for sagehen
bandnumbers_sagehen <- substr(names(sagehen_stack),19,19)

# Subset functional groups from full imported stacks (sagehen)
sagehen_stack_AFG <- sagehen_stack[[which(bandnumbers_sagehen==1)]]
sagehen_stack_BGR <- sagehen_stack[[which(bandnumbers_sagehen==2)]]
sagehen_stack_PFG <- sagehen_stack[[which(bandnumbers_sagehen==3)]]
sagehen_stack_SHR <- sagehen_stack[[which(bandnumbers_sagehen==4)]]
sagehen_stack_TRE <- sagehen_stack[[which(bandnumbers_sagehen==5)]]


#### This section subsets and averages last 5 years RAP data (AFG, PFG, BGR) into plotable layers (mapwell/saddle)

#Saddle data
saddle_stack_AFG5 <- mean(saddle_stack_AFG[[32:36]]) #AFG
saddle_stack_PFG5 <- mean(saddle_stack_PFG[[32:36]]) #PFG
saddle_stack_BGR5 <- mean(saddle_stack_BGR[[32:36]]) #BGR

#Sagehen data
sagehen_stack_AFG5 <- mean(sagehen_stack_AFG[[32:36]]) #AFG
sagehen_stack_PFG5 <- mean(sagehen_stack_PFG[[32:36]]) #PFG
sagehen_stack_BGR5 <- mean(sagehen_stack_BGR[[32:36]]) #BGR


####This section creates buffers of 100m-500m for each watering hole (including all middles/overlap)

##Pinespring
#pinespring 100m buffer (creates buffer around pinespring including pinespring)
buffer_100m_pinespring <- buffer(pinespring_lyr, width = 100)
#pinespring 200m buffer
buffer_200m_pinespring <- buffer(pinespring_lyr, width = 200)
#pinespring 300m buffer
buffer_300m_pinespring <- buffer(pinespring_lyr, width = 300)
#pinespring 400m buffer
buffer_400m_pinespring <- buffer(pinespring_lyr, width = 400)
#pinespring 500m buffer 
buffer_500m_pinespring <- buffer(pinespring_lyr, width = 500)
#600m buffer
buffer_600m_pinespring <- buffer(pinespring_lyr, width = 600)
#700m buffer
buffer_700m_pinespring <- buffer(pinespring_lyr, width = 700)
#800m buffer
buffer_800m_pinespring <- buffer(pinespring_lyr, width = 800)
#900m buffer
buffer_900m_pinespring <- buffer(pinespring_lyr, width = 900)
#pinespring 1000m buffer
buffer_1000m_pinespring <- buffer(pinespring_lyr, width = 1000)

##Upperfay
#100m buffer 
buffer_100m_upperfay <- buffer(upperfay_lyr, width = 100)
#200m buffer
buffer_200m_upperfay <- buffer(upperfay_lyr, width = 200)
#300m buffer
buffer_300m_upperfay <- buffer(upperfay_lyr, width = 300)
#400m buffer
buffer_400m_upperfay <- buffer(upperfay_lyr, width = 400)
#500m buffer 
buffer_500m_upperfay <- buffer(upperfay_lyr, width = 500)
#600m
buffer_600m_upperfay <- buffer(upperfay_lyr, width = 600)
#700m
buffer_700m_upperfay <- buffer(upperfay_lyr, width = 700)
#800m
buffer_800m_upperfay <- buffer(upperfay_lyr, width = 800)
#900m
buffer_900m_upperfay <- buffer(upperfay_lyr, width = 900)
#1000m
buffer_1000m_upperfay <- buffer(upperfay_lyr, width = 1000)


##Sagehenhill reservior
#100m buffer 
buffer_100m_sagehenhill <- buffer(sagehenhillreservoir_lyr, width = 100)
#200m buffer
buffer_200m_sagehenhill <- buffer(sagehenhillreservoir_lyr, width = 200)
#300m buffer
buffer_300m_sagehenhill <- buffer(sagehenhillreservoir_lyr, width = 300)
#400m buffer
buffer_400m_sagehenhill <- buffer(sagehenhillreservoir_lyr, width = 400)
#500m buffer 
buffer_500m_sagehenhill <- buffer(sagehenhillreservoir_lyr, width = 500)
#600m
buffer_600m_sagehenhill <- buffer(sagehenhillreservoir_lyr, width = 600)
#700m
buffer_700m_sagehenhill <- buffer(sagehenhillreservoir_lyr, width = 700)
#800m
buffer_800m_sagehenhill <- buffer(sagehenhillreservoir_lyr, width = 800)
#900m
buffer_900m_sagehenhill <- buffer(sagehenhillreservoir_lyr, width = 900)
#1000m
buffer_1000m_sagehenhill <- buffer(sagehenhillreservoir_lyr, width = 1000)

##Powerline
#100m buffer 
buffer_100m_powerline <- buffer(powerline_lyr, width = 100)
#200m buffer
buffer_200m_powerline <- buffer(powerline_lyr, width = 200)
#300m buffer
buffer_300m_powerline <- buffer(powerline_lyr, width = 300)
#400m buffer
buffer_400m_powerline <- buffer(powerline_lyr, width = 400)
#500m buffer 
buffer_500m_powerline <- buffer(powerline_lyr, width = 500)
#600m
buffer_600m_powerline <- buffer(powerline_lyr, width = 600)
#700m
buffer_700m_powerline <- buffer(powerline_lyr, width = 700)
#800m
buffer_800m_powerline <- buffer(powerline_lyr, width = 800)
#900m
buffer_900m_powerline <- buffer(powerline_lyr, width = 900)
#1000m
buffer_1000m_powerline <- buffer(powerline_lyr, width = 1000)


##Bridgewaterhole
#100m buffer 
buffer_100m_bridgewaterhole <- buffer(bridgewaterhole_lyr, width = 100)
#200m buffer
buffer_200m_bridgewaterhole <- buffer(bridgewaterhole_lyr, width = 200)
#300m buffer
buffer_300m_bridgewaterhole <- buffer(bridgewaterhole_lyr, width = 300)
#400m buffer
buffer_400m_bridgewaterhole <- buffer(bridgewaterhole_lyr, width = 400)
#500m buffer 
buffer_500m_bridgewaterhole <- buffer(bridgewaterhole_lyr, width = 500)
#600m
buffer_600m_bridgewaterhole <- buffer(bridgewaterhole_lyr, width = 600)
#700m
buffer_700m_bridgewaterhole <- buffer(bridgewaterhole_lyr, width = 700)
#800m
buffer_800m_bridgewaterhole <- buffer(bridgewaterhole_lyr, width = 800)
#900m
buffer_900m_bridgewaterhole <- buffer(bridgewaterhole_lyr, width = 900)
#1000m
buffer_1000m_bridgewaterhole <- buffer(bridgewaterhole_lyr, width = 1000)

##Mapwellpt1
#30m buffer (to exclude water source from data in replacement of polygon for other plots since singular point)
buffer_30m_mapwell <- buffer(mapwellpt1_lyr, width = 30)
#100m buffer 
buffer_100m_mapwell <- buffer(mapwellpt1_lyr, width = 100)
#200m buffer
buffer_200m_mapwell <- buffer(mapwellpt1_lyr, width = 200)
#300m buffer
buffer_300m_mapwell <- buffer(mapwellpt1_lyr, width = 300)
#400m buffer
buffer_400m_mapwell <- buffer(mapwellpt1_lyr, width = 400)
#500m buffer 
buffer_500m_mapwell <- buffer(mapwellpt1_lyr, width = 500)
#600m 
buffer_600m_mapwell <- buffer(mapwellpt1_lyr, width = 600)
#700m
buffer_700m_mapwell <- buffer(mapwellpt1_lyr, width = 700)
#800
buffer_800m_mapwell <- buffer(mapwellpt1_lyr, width = 800)
#900m
buffer_900m_mapwell <- buffer(mapwellpt1_lyr, width = 900)
#1000m
buffer_1000m_mapwell <- buffer(mapwellpt1_lyr, width =1000)


#### This section creates donuts (excluding middle/overlap) of 100m-500m for each watering hole, all cover types

##Pinespring
#Creates/extracts pinespring 100m donut buffer for AFG,PFG,and BGR (excluded middle/overlap)
nomiddle_buffer_100m_pinespring <- buffer_100m_pinespring - shpPolys_pinespring #used to create the first buffer excluding pond
donut100m_pinespring_AFG<- extract(sagehen_stack_AFG5, nomiddle_buffer_100m_pinespring) 
donut100m_pinespring_PFG <-extract(sagehen_stack_PFG5, nomiddle_buffer_100m_pinespring) 
donut100m_pinespring_BGR <-extract(sagehen_stack_BGR5, nomiddle_buffer_100m_pinespring) 
unlist_pinespring_100m_AFG <- as.numeric(unlist(donut100m_pinespring_AFG)) #unlists to make numeric value, AFG
unlist_pinespring_100m_PFG <- as.numeric(unlist(donut100m_pinespring_PFG)) 
unlist_pinespring_100m_BGR <- as.numeric(unlist(donut100m_pinespring_BGR)) 
donut100m_pinespring_coords_AFG <- data.frame(x=100, y=unlist_pinespring_100m_AFG) #makes into coordinates to plot
donut100m_pinespring_coords_PFG <- data.frame(x=100, y=unlist_pinespring_100m_PFG)
donut100m_pinespring_coords_BGR <- data.frame(x=100, y=unlist_pinespring_100m_BGR)

#pinespring donut 200m for all covers
nomiddle_buffer_200m_pinespring <- buffer_200m_pinespring - shpPolys_pinespring - buffer_100m_pinespring
donut200m_pinespring_AFG <- extract(sagehen_stack_AFG5, nomiddle_buffer_200m_pinespring)
donut200m_pinespring_PFG <-extract(sagehen_stack_PFG5, nomiddle_buffer_200m_pinespring)
donut200m_pinespring_BGR <-extract(sagehen_stack_BGR5, nomiddle_buffer_200m_pinespring)
unlist_pinespring_200m_AFG <- as.numeric(unlist(donut200m_pinespring_AFG))
unlist_pinespring_200m_PFG <- as.numeric(unlist(donut200m_pinespring_PFG))
unlist_pinespring_200m_BGR <- as.numeric(unlist(donut200m_pinespring_BGR))
donut200m_pinespring_coords_AFG <- data.frame(x=200, y=unlist_pinespring_200m_AFG)
donut200m_pinespring_coords_PFG <- data.frame(x=200, y=unlist_pinespring_200m_PFG)
donut200m_pinespring_coords_BGR <- data.frame(x=200, y=unlist_pinespring_200m_BGR)

#pinespring donut 300m for all covers
nomiddle_buffer_300m_pinespring <- buffer_300m_pinespring - shpPolys_pinespring - buffer_100m_pinespring - buffer_200m_pinespring
donut300m_pinespring_AFG <- extract(sagehen_stack_AFG5, nomiddle_buffer_300m_pinespring)
donut300m_pinespring_PFG <-extract(sagehen_stack_PFG5, nomiddle_buffer_300m_pinespring)
donut300m_pinespring_BGR <-extract(sagehen_stack_BGR5, nomiddle_buffer_300m_pinespring)
unlist_pinespring_300m_AFG <- as.numeric(unlist(donut300m_pinespring_AFG))
unlist_pinespring_300m_PFG <- as.numeric(unlist(donut300m_pinespring_PFG))
unlist_pinespring_300m_BGR <- as.numeric(unlist(donut300m_pinespring_BGR))
donut300m_pinespring_coords_AFG <- data.frame(x=300, y=unlist_pinespring_300m_AFG)
donut300m_pinespring_coords_PFG <- data.frame(x=300, y=unlist_pinespring_300m_PFG)
donut300m_pinespring_coords_BGR <- data.frame(x=300, y=unlist_pinespring_300m_BGR)

#pinespring donut 400m for all covers
nomiddle_buffer_400m_pinespring <- buffer_400m_pinespring - shpPolys_pinespring - buffer_100m_pinespring - buffer_200m_pinespring - buffer_300m_pinespring
donut400m_pinespring_AFG <- extract(sagehen_stack_AFG5, nomiddle_buffer_400m_pinespring)
donut400m_pinespring_PFG <-extract(sagehen_stack_PFG5, nomiddle_buffer_400m_pinespring)
donut400m_pinespring_BGR <-extract(sagehen_stack_BGR5, nomiddle_buffer_400m_pinespring)
unlist_pinespring_400m_AFG <- as.numeric(unlist(donut400m_pinespring_AFG))
unlist_pinespring_400m_PFG <- as.numeric(unlist(donut400m_pinespring_PFG))
unlist_pinespring_400m_BGR <- as.numeric(unlist(donut400m_pinespring_BGR))
donut400m_pinespring_coords_AFG <- data.frame(x=400, y=unlist_pinespring_400m_AFG)
donut400m_pinespring_coords_PFG <- data.frame(x=400, y=unlist_pinespring_400m_PFG)
donut400m_pinespring_coords_BGR <- data.frame(x=400, y=unlist_pinespring_400m_BGR)

#pinespring donut 500m for all covers
nomiddle_buffer_500m_pinespring <- buffer_500m_pinespring - shpPolys_pinespring - buffer_100m_pinespring - buffer_200m_pinespring - buffer_300m_pinespring - buffer_400m_pinespring
donut500m_pinespring_AFG <- extract(sagehen_stack_AFG5, nomiddle_buffer_500m_pinespring)
donut500m_pinespring_PFG <-extract(sagehen_stack_PFG5, nomiddle_buffer_500m_pinespring)
donut500m_pinespring_BGR <-extract(sagehen_stack_BGR5, nomiddle_buffer_500m_pinespring)
unlist_pinespring_500m_AFG <- as.numeric(unlist(donut500m_pinespring_AFG))
unlist_pinespring_500m_PFG <- as.numeric(unlist(donut500m_pinespring_PFG))
unlist_pinespring_500m_BGR <- as.numeric(unlist(donut500m_pinespring_BGR))
donut500m_pinespring_coords_AFG <- data.frame(x=500, y=unlist_pinespring_500m_AFG)
donut500m_pinespring_coords_PFG <- data.frame(x=500, y=unlist_pinespring_500m_PFG)
donut500m_pinespring_coords_BGR <- data.frame(x=500, y=unlist_pinespring_500m_BGR)

#600m
nomiddle_buffer_600m_pinespring <- buffer_600m_pinespring - shpPolys_pinespring - buffer_100m_pinespring - buffer_200m_pinespring - buffer_300m_pinespring - buffer_400m_pinespring - buffer_500m_pinespring
donut600m_pinespring_AFG <-extract(sagehen_stack_AFG5, nomiddle_buffer_600m_pinespring)
donut600m_pinespring_PFG <-extract(sagehen_stack_PFG5, nomiddle_buffer_600m_pinespring)
donut600m_pinespring_BGR <-extract(sagehen_stack_BGR5, nomiddle_buffer_600m_pinespring)
unlist_pinespring_600m_AFG <- as.numeric(unlist(donut600m_pinespring_AFG))
unlist_pinespring_600m_PFG <- as.numeric(unlist(donut600m_pinespring_PFG))
unlist_pinespring_600m_BGR <- as.numeric(unlist(donut600m_pinespring_BGR))
donut600m_pinespring_coords_AFG <- data.frame(x=600, y=unlist_pinespring_600m_AFG)
donut600m_pinespring_coords_PFG <- data.frame(x=600, y=unlist_pinespring_600m_PFG)
donut600m_pinespring_coords_BGR <- data.frame(x=600, y=unlist_pinespring_600m_BGR)

#700m
nomiddle_buffer_700m_pinespring <- buffer_700m_pinespring - shpPolys_pinespring - buffer_100m_pinespring - buffer_200m_pinespring - buffer_300m_pinespring - buffer_400m_pinespring - buffer_500m_pinespring - buffer_600m_pinespring
donut700m_pinespring_AFG <-extract(sagehen_stack_AFG5, nomiddle_buffer_700m_pinespring)
donut700m_pinespring_PFG <-extract(sagehen_stack_PFG5, nomiddle_buffer_700m_pinespring)
donut700m_pinespring_BGR <-extract(sagehen_stack_BGR5, nomiddle_buffer_700m_pinespring)
unlist_pinespring_700m_AFG <- as.numeric(unlist(donut700m_pinespring_AFG))
unlist_pinespring_700m_PFG <- as.numeric(unlist(donut700m_pinespring_PFG))
unlist_pinespring_700m_BGR <- as.numeric(unlist(donut700m_pinespring_BGR))
donut700m_pinespring_coords_AFG <- data.frame(x=700, y=unlist_pinespring_700m_AFG)
donut700m_pinespring_coords_PFG <- data.frame(x=700, y=unlist_pinespring_700m_PFG)
donut700m_pinespring_coords_BGR <- data.frame(x=700, y=unlist_pinespring_700m_BGR)

#800m
nomiddle_buffer_800m_pinespring <- buffer_800m_pinespring - shpPolys_pinespring - buffer_100m_pinespring - buffer_200m_pinespring - buffer_300m_pinespring - buffer_400m_pinespring - buffer_500m_pinespring - buffer_600m_pinespring - buffer_700m_pinespring
donut800m_pinespring_AFG <-extract(sagehen_stack_AFG5, nomiddle_buffer_800m_pinespring)
donut800m_pinespring_PFG <-extract(sagehen_stack_PFG5, nomiddle_buffer_800m_pinespring)
donut800m_pinespring_BGR <-extract(sagehen_stack_BGR5, nomiddle_buffer_800m_pinespring)
unlist_pinespring_800m_AFG <- as.numeric(unlist(donut800m_pinespring_AFG))
unlist_pinespring_800m_PFG <- as.numeric(unlist(donut800m_pinespring_PFG))
unlist_pinespring_800m_BGR <- as.numeric(unlist(donut800m_pinespring_BGR))
donut800m_pinespring_coords_AFG <- data.frame(x=800, y=unlist_pinespring_800m_AFG)
donut800m_pinespring_coords_PFG <- data.frame(x=800, y=unlist_pinespring_800m_PFG)
donut800m_pinespring_coords_BGR <- data.frame(x=800, y=unlist_pinespring_800m_BGR)

#900m
nomiddle_buffer_900m_pinespring <- buffer_900m_pinespring - shpPolys_pinespring - buffer_100m_pinespring - buffer_200m_pinespring - buffer_300m_pinespring - buffer_400m_pinespring - buffer_500m_pinespring - buffer_600m_pinespring - buffer_700m_pinespring - buffer_800m_pinespring
donut900m_pinespring_AFG <-extract(sagehen_stack_AFG5, nomiddle_buffer_900m_pinespring)
donut900m_pinespring_PFG <-extract(sagehen_stack_PFG5, nomiddle_buffer_900m_pinespring)
donut900m_pinespring_BGR <-extract(sagehen_stack_BGR5, nomiddle_buffer_900m_pinespring)
unlist_pinespring_900m_AFG <- as.numeric(unlist(donut900m_pinespring_AFG))
unlist_pinespring_900m_PFG <- as.numeric(unlist(donut900m_pinespring_PFG))
unlist_pinespring_900m_BGR <- as.numeric(unlist(donut900m_pinespring_BGR))
donut900m_pinespring_coords_AFG <- data.frame(x=900, y=unlist_pinespring_900m_AFG)
donut900m_pinespring_coords_PFG <- data.frame(x=900, y=unlist_pinespring_900m_PFG)
donut900m_pinespring_coords_BGR <- data.frame(x=900, y=unlist_pinespring_900m_BGR)

#1000m
nomiddle_buffer_1000m_pinespring <- buffer_1000m_pinespring - shpPolys_pinespring - buffer_100m_pinespring - buffer_200m_pinespring - buffer_300m_pinespring - buffer_400m_pinespring - buffer_500m_pinespring - buffer_600m_pinespring - buffer_700m_pinespring- buffer_800m_pinespring- buffer_900m_pinespring
donut1000m_pinespring_AFG <-extract(sagehen_stack_AFG5, nomiddle_buffer_1000m_pinespring)
donut1000m_pinespring_PFG <-extract(sagehen_stack_PFG5, nomiddle_buffer_1000m_pinespring)
donut1000m_pinespring_BGR <-extract(sagehen_stack_BGR5, nomiddle_buffer_1000m_pinespring)
unlist_pinespring_1000m_AFG <- as.numeric(unlist(donut1000m_pinespring_AFG))
unlist_pinespring_1000m_PFG <- as.numeric(unlist(donut1000m_pinespring_PFG))
unlist_pinespring_1000m_BGR <- as.numeric(unlist(donut1000m_pinespring_BGR))
donut1000m_pinespring_coords_AFG <- data.frame(x=1000, y=unlist_pinespring_500m_AFG)
donut1000m_pinespring_coords_PFG <- data.frame(x=1000, y=unlist_pinespring_500m_PFG)
donut1000m_pinespring_coords_BGR <- data.frame(x=1000, y=unlist_pinespring_500m_BGR)

##Upperfay
#100m donut (creates buffer excluding middle (subtraction), then uses extract to apply buffer to raster GPS data)
nomiddle_buffer_100m_upperfay <- buffer_100m_upperfay - shpPolys_upperfay
donut100m_upperfay_AFG <- extract(sagehen_stack_AFG5, nomiddle_buffer_100m_upperfay) 
donut100m_upperfay_PFG <- extract(sagehen_stack_PFG5, nomiddle_buffer_100m_upperfay) 
donut100m_upperfay_BGR <- extract(sagehen_stack_BGR5, nomiddle_buffer_100m_upperfay) 
unlist_upperfay_100m_AFG <- as.numeric(unlist(donut100m_upperfay_AFG))
unlist_upperfay_100m_PFG <- as.numeric(unlist(donut100m_upperfay_PFG))
unlist_upperfay_100m_BGR <- as.numeric(unlist(donut100m_upperfay_BGR))
donut100m_upperfay_coords_AFG <- data.frame(x=100, y=unlist_upperfay_100m_AFG)
donut100m_upperfay_coords_PFG <- data.frame(x=100, y=unlist_upperfay_100m_PFG)
donut100m_upperfay_coords_BGR <- data.frame(x=100, y=unlist_upperfay_100m_BGR)

#200m donut
nomiddle_buffer_200m_upperfay <- buffer_200m_upperfay - shpPolys_upperfay - buffer_100m_upperfay
donut200m_upperfay_AFG <- extract(sagehen_stack_AFG5, nomiddle_buffer_200m_upperfay) 
donut200m_upperfay_PFG <- extract(sagehen_stack_PFG5, nomiddle_buffer_200m_upperfay) 
donut200m_upperfay_BGR <- extract(sagehen_stack_BGR5, nomiddle_buffer_200m_upperfay) 
unlist_upperfay_200m_AFG <- as.numeric(unlist(donut200m_upperfay_AFG))
unlist_upperfay_200m_PFG <- as.numeric(unlist(donut200m_upperfay_PFG))
unlist_upperfay_200m_BGR <- as.numeric(unlist(donut200m_upperfay_BGR))
donut200m_upperfay_coords_AFG <- data.frame(x=200, y=unlist_upperfay_200m_AFG)
donut200m_upperfay_coords_PFG <- data.frame(x=200, y=unlist_upperfay_200m_PFG)
donut200m_upperfay_coords_BGR <- data.frame(x=200, y=unlist_upperfay_200m_BGR)

#300m donut
nomiddle_buffer_300m_upperfay <- buffer_300m_upperfay - shpPolys_upperfay - buffer_100m_upperfay - buffer_200m_upperfay
donut300m_upperfay_AFG <- extract(sagehen_stack_AFG5, nomiddle_buffer_300m_upperfay) 
donut300m_upperfay_PFG <- extract(sagehen_stack_PFG5, nomiddle_buffer_300m_upperfay) 
donut300m_upperfay_BGR <- extract(sagehen_stack_BGR5, nomiddle_buffer_300m_upperfay) 
unlist_upperfay_300m_AFG <- as.numeric(unlist(donut300m_upperfay_AFG))
unlist_upperfay_300m_PFG <- as.numeric(unlist(donut300m_upperfay_PFG))
unlist_upperfay_300m_BGR <- as.numeric(unlist(donut300m_upperfay_BGR))
donut300m_upperfay_coords_AFG <- data.frame(x=300, y=unlist_upperfay_300m_AFG)
donut300m_upperfay_coords_PFG <- data.frame(x=300, y=unlist_upperfay_300m_PFG)
donut300m_upperfay_coords_BGR <- data.frame(x=300, y=unlist_upperfay_300m_BGR)

#400m donut
nomiddle_buffer_400m_upperfay <- buffer_400m_upperfay - shpPolys_upperfay - buffer_100m_upperfay - buffer_200m_upperfay - buffer_300m_upperfay
donut400m_upperfay_AFG <- extract(sagehen_stack_AFG5, nomiddle_buffer_400m_upperfay) 
donut400m_upperfay_PFG <- extract(sagehen_stack_PFG5, nomiddle_buffer_400m_upperfay) 
donut400m_upperfay_BGR <- extract(sagehen_stack_BGR5, nomiddle_buffer_400m_upperfay) 
unlist_upperfay_400m_AFG <- as.numeric(unlist(donut400m_upperfay_AFG))
unlist_upperfay_400m_PFG <- as.numeric(unlist(donut400m_upperfay_PFG))
unlist_upperfay_400m_BGR <- as.numeric(unlist(donut400m_upperfay_BGR))
donut400m_upperfay_coords_AFG <- data.frame(x=400, y=unlist_upperfay_400m_AFG)
donut400m_upperfay_coords_PFG <- data.frame(x=400, y=unlist_upperfay_400m_PFG)
donut400m_upperfay_coords_BGR <- data.frame(x=400, y=unlist_upperfay_400m_BGR)

#500m donut
nomiddle_buffer_500m_upperfay <- buffer_500m_upperfay - shpPolys_upperfay - buffer_100m_upperfay - buffer_200m_upperfay - buffer_300m_upperfay - buffer_400m_upperfay
donut500m_upperfay_AFG <- extract(sagehen_stack_AFG5, nomiddle_buffer_500m_upperfay) 
donut500m_upperfay_PFG <- extract(sagehen_stack_PFG5, nomiddle_buffer_500m_upperfay) 
donut500m_upperfay_BGR <- extract(sagehen_stack_BGR5, nomiddle_buffer_500m_upperfay) 
unlist_upperfay_500m_AFG <- as.numeric(unlist(donut500m_upperfay_AFG))
unlist_upperfay_500m_PFG <- as.numeric(unlist(donut500m_upperfay_PFG))
unlist_upperfay_500m_BGR <- as.numeric(unlist(donut500m_upperfay_BGR))
donut500m_upperfay_coords_AFG <- data.frame(x=500, y=unlist_upperfay_500m_AFG)
donut500m_upperfay_coords_PFG <- data.frame(x=500, y=unlist_upperfay_500m_PFG)
donut500m_upperfay_coords_BGR <- data.frame(x=500, y=unlist_upperfay_500m_BGR)

#600m donut
nomiddle_buffer_600m_upperfay <- buffer_600m_upperfay - shpPolys_upperfay - buffer_100m_upperfay - buffer_200m_upperfay - buffer_300m_upperfay - buffer_400m_upperfay - buffer_500m_upperfay
donut600m_upperfay_AFG <- extract(sagehen_stack_AFG5, nomiddle_buffer_600m_upperfay) 
donut600m_upperfay_PFG <- extract(sagehen_stack_PFG5, nomiddle_buffer_600m_upperfay) 
donut600m_upperfay_BGR <- extract(sagehen_stack_BGR5, nomiddle_buffer_600m_upperfay) 
unlist_upperfay_600m_AFG <- as.numeric(unlist(donut600m_upperfay_AFG))
unlist_upperfay_600m_PFG <- as.numeric(unlist(donut600m_upperfay_PFG))
unlist_upperfay_600m_BGR <- as.numeric(unlist(donut600m_upperfay_BGR))
donut600m_upperfay_coords_AFG <- data.frame(x=600, y=unlist_upperfay_600m_AFG)
donut600m_upperfay_coords_PFG <- data.frame(x=600, y=unlist_upperfay_600m_PFG)
donut600m_upperfay_coords_BGR <- data.frame(x=600, y=unlist_upperfay_600m_BGR)

#700m donut
nomiddle_buffer_700m_upperfay <- buffer_700m_upperfay - shpPolys_upperfay - buffer_100m_upperfay - buffer_200m_upperfay - buffer_300m_upperfay - buffer_400m_upperfay - buffer_500m_upperfay - buffer_600m_upperfay
donut700m_upperfay_AFG <- extract(sagehen_stack_AFG5, nomiddle_buffer_700m_upperfay) 
donut700m_upperfay_PFG <- extract(sagehen_stack_PFG5, nomiddle_buffer_700m_upperfay) 
donut700m_upperfay_BGR <- extract(sagehen_stack_BGR5, nomiddle_buffer_700m_upperfay) 
unlist_upperfay_700m_AFG <- as.numeric(unlist(donut700m_upperfay_AFG))
unlist_upperfay_700m_PFG <- as.numeric(unlist(donut700m_upperfay_PFG))
unlist_upperfay_700m_BGR <- as.numeric(unlist(donut700m_upperfay_BGR))
donut700m_upperfay_coords_AFG <- data.frame(x=700, y=unlist_upperfay_700m_AFG)
donut700m_upperfay_coords_PFG <- data.frame(x=700, y=unlist_upperfay_700m_PFG)
donut700m_upperfay_coords_BGR <- data.frame(x=700, y=unlist_upperfay_700m_BGR)

#800m donut
nomiddle_buffer_800m_upperfay <- buffer_800m_upperfay - shpPolys_upperfay - buffer_100m_upperfay - buffer_200m_upperfay - buffer_300m_upperfay - buffer_400m_upperfay - buffer_500m_upperfay - buffer_600m_upperfay - buffer_700m_upperfay
donut800m_upperfay_AFG <- extract(sagehen_stack_AFG5, nomiddle_buffer_800m_upperfay) 
donut800m_upperfay_PFG <- extract(sagehen_stack_PFG5, nomiddle_buffer_800m_upperfay) 
donut800m_upperfay_BGR <- extract(sagehen_stack_BGR5, nomiddle_buffer_800m_upperfay) 
unlist_upperfay_800m_AFG <- as.numeric(unlist(donut800m_upperfay_AFG))
unlist_upperfay_800m_PFG <- as.numeric(unlist(donut800m_upperfay_PFG))
unlist_upperfay_800m_BGR <- as.numeric(unlist(donut800m_upperfay_BGR))
donut800m_upperfay_coords_AFG <- data.frame(x=800, y=unlist_upperfay_800m_AFG)
donut800m_upperfay_coords_PFG <- data.frame(x=800, y=unlist_upperfay_800m_PFG)
donut800m_upperfay_coords_BGR <- data.frame(x=800, y=unlist_upperfay_800m_BGR)

#900m donut
nomiddle_buffer_900m_upperfay <- buffer_900m_upperfay - shpPolys_upperfay - buffer_100m_upperfay - buffer_200m_upperfay - buffer_300m_upperfay - buffer_400m_upperfay - buffer_500m_upperfay - buffer_600m_upperfay - buffer_700m_upperfay - buffer_800m_upperfay
donut900m_upperfay_AFG <- extract(sagehen_stack_AFG5, nomiddle_buffer_900m_upperfay) 
donut900m_upperfay_PFG <- extract(sagehen_stack_PFG5, nomiddle_buffer_900m_upperfay) 
donut900m_upperfay_BGR <- extract(sagehen_stack_BGR5, nomiddle_buffer_900m_upperfay) 
unlist_upperfay_900m_AFG <- as.numeric(unlist(donut900m_upperfay_AFG))
unlist_upperfay_900m_PFG <- as.numeric(unlist(donut900m_upperfay_PFG))
unlist_upperfay_900m_BGR <- as.numeric(unlist(donut900m_upperfay_BGR))
donut900m_upperfay_coords_AFG <- data.frame(x=900, y=unlist_upperfay_900m_AFG)
donut900m_upperfay_coords_PFG <- data.frame(x=900, y=unlist_upperfay_900m_PFG)
donut900m_upperfay_coords_BGR <- data.frame(x=900, y=unlist_upperfay_900m_BGR)

#1000m donut
nomiddle_buffer_1000m_upperfay <- buffer_1000m_upperfay - shpPolys_upperfay - buffer_100m_upperfay - buffer_200m_upperfay - buffer_300m_upperfay - buffer_400m_upperfay - buffer_500m_upperfay - buffer_600m_upperfay - buffer_700m_upperfay - buffer_800m_upperfay - buffer_900m_upperfay
donut1000m_upperfay_AFG <- extract(sagehen_stack_AFG5, nomiddle_buffer_1000m_upperfay) 
donut1000m_upperfay_PFG <- extract(sagehen_stack_PFG5, nomiddle_buffer_1000m_upperfay) 
donut1000m_upperfay_BGR <- extract(sagehen_stack_BGR5, nomiddle_buffer_1000m_upperfay) 
unlist_upperfay_1000m_AFG <- as.numeric(unlist(donut1000m_upperfay_AFG))
unlist_upperfay_1000m_PFG <- as.numeric(unlist(donut1000m_upperfay_PFG))
unlist_upperfay_1000m_BGR <- as.numeric(unlist(donut1000m_upperfay_BGR))
donut1000m_upperfay_coords_AFG <- data.frame(x=1000, y=unlist_upperfay_1000m_AFG)
donut1000m_upperfay_coords_PFG <- data.frame(x=1000, y=unlist_upperfay_1000m_PFG)
donut1000m_upperfay_coords_BGR <- data.frame(x=1000, y=unlist_upperfay_1000m_BGR)

##Sagehenhill reservior
#100m donut (creates buffer excluding middle (subtraction), then uses extract to apply buffer to raster GPS data)
nomiddle_buffer_100m_sagehenhill <- buffer_100m_sagehenhill - shpPolys_sagehenhillreservoir
donut100m_sagehenhill_AFG <- extract(sagehen_stack_AFG5, nomiddle_buffer_100m_sagehenhill)
donut100m_sagehenhill_PFG <- extract(sagehen_stack_PFG5, nomiddle_buffer_100m_sagehenhill)
donut100m_sagehenhill_BGR <- extract(sagehen_stack_BGR5, nomiddle_buffer_100m_sagehenhill)
unlist_sagehenhill_100m_AFG <- as.numeric(unlist(donut100m_sagehenhill_AFG))
unlist_sagehenhill_100m_PFG <- as.numeric(unlist(donut100m_sagehenhill_PFG))
unlist_sagehenhill_100m_BGR <- as.numeric(unlist(donut100m_sagehenhill_BGR))
donut100m_sagehenhill_coords_AFG <- data.frame(x=100, y=unlist_sagehenhill_100m_AFG)
donut100m_sagehenhill_coords_PFG <- data.frame(x=100, y=unlist_sagehenhill_100m_PFG)
donut100m_sagehenhill_coords_BGR <- data.frame(x=100, y=unlist_sagehenhill_100m_BGR)

#200m donut
nomiddle_buffer_200m_sagehenhill <- buffer_200m_sagehenhill - shpPolys_sagehenhillreservoir - buffer_100m_sagehenhill
donut200m_sagehenhill_AFG <- extract(sagehen_stack_AFG5, nomiddle_buffer_200m_sagehenhill)
donut200m_sagehenhill_PFG <- extract(sagehen_stack_PFG5, nomiddle_buffer_200m_sagehenhill)
donut200m_sagehenhill_BGR <- extract(sagehen_stack_BGR5, nomiddle_buffer_200m_sagehenhill)
unlist_sagehenhill_200m_AFG <- as.numeric(unlist(donut200m_sagehenhill_AFG))
unlist_sagehenhill_200m_PFG <- as.numeric(unlist(donut200m_sagehenhill_PFG))
unlist_sagehenhill_200m_BGR <- as.numeric(unlist(donut200m_sagehenhill_BGR))
donut200m_sagehenhill_coords_AFG <- data.frame(x=200, y=unlist_sagehenhill_200m_AFG)
donut200m_sagehenhill_coords_PFG <- data.frame(x=200, y=unlist_sagehenhill_200m_PFG)
donut200m_sagehenhill_coords_BGR <- data.frame(x=200, y=unlist_sagehenhill_200m_BGR)

#300m donut
nomiddle_buffer_300m_sagehenhill <- buffer_300m_sagehenhill - shpPolys_sagehenhillreservoir - buffer_100m_sagehenhill - buffer_200m_sagehenhill
donut300m_sagehenhill_AFG <- extract(sagehen_stack_AFG5, nomiddle_buffer_300m_sagehenhill)
donut300m_sagehenhill_PFG <- extract(sagehen_stack_PFG5, nomiddle_buffer_300m_sagehenhill)
donut300m_sagehenhill_BGR <- extract(sagehen_stack_BGR5, nomiddle_buffer_300m_sagehenhill)
unlist_sagehenhill_300m_AFG <- as.numeric(unlist(donut300m_sagehenhill_AFG))
unlist_sagehenhill_300m_PFG <- as.numeric(unlist(donut300m_sagehenhill_PFG))
unlist_sagehenhill_300m_BGR <- as.numeric(unlist(donut300m_sagehenhill_BGR))
donut300m_sagehenhill_coords_AFG <- data.frame(x=300, y=unlist_sagehenhill_300m_AFG)
donut300m_sagehenhill_coords_PFG <- data.frame(x=300, y=unlist_sagehenhill_300m_PFG)
donut300m_sagehenhill_coords_BGR <- data.frame(x=300, y=unlist_sagehenhill_300m_BGR)

#400m donut
nomiddle_buffer_400m_sagehenhill <- buffer_400m_sagehenhill - shpPolys_sagehenhillreservoir - buffer_100m_sagehenhill - buffer_200m_sagehenhill - buffer_300m_sagehenhill
donut400m_sagehenhill_AFG <- extract(sagehen_stack_AFG5, nomiddle_buffer_400m_sagehenhill)
donut400m_sagehenhill_PFG <- extract(sagehen_stack_PFG5, nomiddle_buffer_400m_sagehenhill)
donut400m_sagehenhill_BGR <- extract(sagehen_stack_BGR5, nomiddle_buffer_400m_sagehenhill)
unlist_sagehenhill_400m_AFG <- as.numeric(unlist(donut400m_sagehenhill_AFG))
unlist_sagehenhill_400m_PFG <- as.numeric(unlist(donut400m_sagehenhill_PFG))
unlist_sagehenhill_400m_BGR <- as.numeric(unlist(donut400m_sagehenhill_BGR))
donut400m_sagehenhill_coords_AFG <- data.frame(x=400, y=unlist_sagehenhill_400m_AFG)
donut400m_sagehenhill_coords_PFG <- data.frame(x=400, y=unlist_sagehenhill_400m_PFG)
donut400m_sagehenhill_coords_BGR <- data.frame(x=400, y=unlist_sagehenhill_400m_BGR)

#500m donut
nomiddle_buffer_500m_sagehenhill <- buffer_500m_sagehenhill - shpPolys_sagehenhillreservoir - buffer_100m_sagehenhill - buffer_200m_sagehenhill - buffer_300m_sagehenhill- buffer_400m_sagehenhill
donut500m_sagehenhill_AFG <- extract(sagehen_stack_AFG5, nomiddle_buffer_500m_sagehenhill)
donut500m_sagehenhill_PFG <- extract(sagehen_stack_PFG5, nomiddle_buffer_500m_sagehenhill)
donut500m_sagehenhill_BGR <- extract(sagehen_stack_BGR5, nomiddle_buffer_500m_sagehenhill)
unlist_sagehenhill_500m_AFG <- as.numeric(unlist(donut500m_sagehenhill_AFG))
unlist_sagehenhill_500m_PFG <- as.numeric(unlist(donut500m_sagehenhill_PFG))
unlist_sagehenhill_500m_BGR <- as.numeric(unlist(donut500m_sagehenhill_BGR))
donut500m_sagehenhill_coords_AFG <- data.frame(x=500, y=unlist_sagehenhill_500m_AFG)
donut500m_sagehenhill_coords_PFG <- data.frame(x=500, y=unlist_sagehenhill_500m_PFG)
donut500m_sagehenhill_coords_BGR <- data.frame(x=500, y=unlist_sagehenhill_500m_BGR)

#600m donut
nomiddle_buffer_600m_sagehenhill <- buffer_600m_sagehenhill - shpPolys_sagehenhillreservoir - buffer_100m_sagehenhill - buffer_200m_sagehenhill - buffer_300m_sagehenhill- buffer_400m_sagehenhill - buffer_500m_sagehenhill
donut600m_sagehenhill_AFG <- extract(sagehen_stack_AFG5, nomiddle_buffer_600m_sagehenhill)
donut600m_sagehenhill_PFG <- extract(sagehen_stack_PFG5, nomiddle_buffer_600m_sagehenhill)
donut600m_sagehenhill_BGR <- extract(sagehen_stack_BGR5, nomiddle_buffer_600m_sagehenhill)
unlist_sagehenhill_600m_AFG <- as.numeric(unlist(donut600m_sagehenhill_AFG))
unlist_sagehenhill_600m_PFG <- as.numeric(unlist(donut600m_sagehenhill_PFG))
unlist_sagehenhill_600m_BGR <- as.numeric(unlist(donut600m_sagehenhill_BGR))
donut600m_sagehenhill_coords_AFG <- data.frame(x=600, y=unlist_sagehenhill_600m_AFG)
donut600m_sagehenhill_coords_PFG <- data.frame(x=600, y=unlist_sagehenhill_600m_PFG)
donut600m_sagehenhill_coords_BGR <- data.frame(x=600, y=unlist_sagehenhill_600m_BGR)

#700m donut
nomiddle_buffer_700m_sagehenhill <- buffer_700m_sagehenhill - shpPolys_sagehenhillreservoir - buffer_100m_sagehenhill - buffer_200m_sagehenhill - buffer_300m_sagehenhill- buffer_400m_sagehenhill - buffer_500m_sagehenhill - buffer_600m_sagehenhill
donut700m_sagehenhill_AFG <- extract(sagehen_stack_AFG5, nomiddle_buffer_700m_sagehenhill)
donut700m_sagehenhill_PFG <- extract(sagehen_stack_PFG5, nomiddle_buffer_700m_sagehenhill)
donut700m_sagehenhill_BGR <- extract(sagehen_stack_BGR5, nomiddle_buffer_700m_sagehenhill)
unlist_sagehenhill_700m_AFG <- as.numeric(unlist(donut700m_sagehenhill_AFG))
unlist_sagehenhill_700m_PFG <- as.numeric(unlist(donut700m_sagehenhill_PFG))
unlist_sagehenhill_700m_BGR <- as.numeric(unlist(donut700m_sagehenhill_BGR))
donut700m_sagehenhill_coords_AFG <- data.frame(x=700, y=unlist_sagehenhill_700m_AFG)
donut700m_sagehenhill_coords_PFG <- data.frame(x=700, y=unlist_sagehenhill_700m_PFG)
donut700m_sagehenhill_coords_BGR <- data.frame(x=700, y=unlist_sagehenhill_700m_BGR)

#800m donut
nomiddle_buffer_800m_sagehenhill <- buffer_800m_sagehenhill - shpPolys_sagehenhillreservoir - buffer_100m_sagehenhill - buffer_200m_sagehenhill - buffer_300m_sagehenhill- buffer_400m_sagehenhill- buffer_500m_sagehenhill - buffer_600m_sagehenhill - buffer_700m_sagehenhill
donut800m_sagehenhill_AFG <- extract(sagehen_stack_AFG5, nomiddle_buffer_800m_sagehenhill)
donut800m_sagehenhill_PFG <- extract(sagehen_stack_PFG5, nomiddle_buffer_800m_sagehenhill)
donut800m_sagehenhill_BGR <- extract(sagehen_stack_BGR5, nomiddle_buffer_800m_sagehenhill)
unlist_sagehenhill_800m_AFG <- as.numeric(unlist(donut800m_sagehenhill_AFG))
unlist_sagehenhill_800m_PFG <- as.numeric(unlist(donut800m_sagehenhill_PFG))
unlist_sagehenhill_800m_BGR <- as.numeric(unlist(donut800m_sagehenhill_BGR))
donut800m_sagehenhill_coords_AFG <- data.frame(x=800, y=unlist_sagehenhill_800m_AFG)
donut800m_sagehenhill_coords_PFG <- data.frame(x=800, y=unlist_sagehenhill_800m_PFG)
donut800m_sagehenhill_coords_BGR <- data.frame(x=800, y=unlist_sagehenhill_800m_BGR)

#900m donut
nomiddle_buffer_900m_sagehenhill <- buffer_900m_sagehenhill - shpPolys_sagehenhillreservoir - buffer_100m_sagehenhill - buffer_200m_sagehenhill - buffer_300m_sagehenhill- buffer_400m_sagehenhill- buffer_500m_sagehenhill - buffer_600m_sagehenhill- buffer_700m_sagehenhill - buffer_800m_sagehenhill
donut900m_sagehenhill_AFG <- extract(sagehen_stack_AFG5, nomiddle_buffer_900m_sagehenhill)
donut900m_sagehenhill_PFG <- extract(sagehen_stack_PFG5, nomiddle_buffer_900m_sagehenhill)
donut900m_sagehenhill_BGR <- extract(sagehen_stack_BGR5, nomiddle_buffer_900m_sagehenhill)
unlist_sagehenhill_900m_AFG <- as.numeric(unlist(donut900m_sagehenhill_AFG))
unlist_sagehenhill_900m_PFG <- as.numeric(unlist(donut900m_sagehenhill_PFG))
unlist_sagehenhill_900m_BGR <- as.numeric(unlist(donut900m_sagehenhill_BGR))
donut900m_sagehenhill_coords_AFG <- data.frame(x=900, y=unlist_sagehenhill_900m_AFG)
donut900m_sagehenhill_coords_PFG <- data.frame(x=900, y=unlist_sagehenhill_900m_PFG)
donut900m_sagehenhill_coords_BGR <- data.frame(x=900, y=unlist_sagehenhill_900m_BGR)

#1000m donut
nomiddle_buffer_1000m_sagehenhill <- buffer_1000m_sagehenhill - shpPolys_sagehenhillreservoir - buffer_100m_sagehenhill - buffer_200m_sagehenhill - buffer_300m_sagehenhill- buffer_400m_sagehenhill- buffer_500m_sagehenhill - buffer_600m_sagehenhill- buffer_700m_sagehenhill- buffer_800m_sagehenhill - buffer_900m_sagehenhill
donut1000m_sagehenhill_AFG <- extract(sagehen_stack_AFG5, nomiddle_buffer_1000m_sagehenhill)
donut1000m_sagehenhill_PFG <- extract(sagehen_stack_PFG5, nomiddle_buffer_1000m_sagehenhill)
donut1000m_sagehenhill_BGR <- extract(sagehen_stack_BGR5, nomiddle_buffer_1000m_sagehenhill)
unlist_sagehenhill_1000m_AFG <- as.numeric(unlist(donut1000m_sagehenhill_AFG))
unlist_sagehenhill_1000m_PFG <- as.numeric(unlist(donut1000m_sagehenhill_PFG))
unlist_sagehenhill_1000m_BGR <- as.numeric(unlist(donut1000m_sagehenhill_BGR))
donut1000m_sagehenhill_coords_AFG <- data.frame(x=1000, y=unlist_sagehenhill_1000m_AFG)
donut1000m_sagehenhill_coords_PFG <- data.frame(x=1000, y=unlist_sagehenhill_1000m_PFG)
donut1000m_sagehenhill_coords_BGR <- data.frame(x=1000, y=unlist_sagehenhill_1000m_BGR)

##Powerline
#100m donut (creates buffer excluding middle (subtraction), then uses extract to apply buffer to raster GPS data)
nomiddle_buffer_100m_powerline <- buffer_100m_powerline - shpPolys_powerline
donut100m_powerline_AFG <- extract(sagehen_stack_AFG5, nomiddle_buffer_100m_powerline)
donut100m_powerline_PFG <- extract(sagehen_stack_PFG5, nomiddle_buffer_100m_powerline) 
donut100m_powerline_BGR <- extract(sagehen_stack_BGR5, nomiddle_buffer_100m_powerline) 
unlist_powerline_100m_AFG <- as.numeric(unlist(donut100m_powerline_AFG))
unlist_powerline_100m_PFG <- as.numeric(unlist(donut100m_powerline_PFG))
unlist_powerline_100m_BGR <- as.numeric(unlist(donut100m_powerline_BGR))
donut100m_powerline_coords_AFG <- data.frame(x=100, y=unlist_powerline_100m_AFG)
donut100m_powerline_coords_PFG <- data.frame(x=100, y=unlist_powerline_100m_PFG)
donut100m_powerline_coords_BGR <- data.frame(x=100, y=unlist_powerline_100m_BGR)

#200m donut
nomiddle_buffer_200m_powerline <- buffer_200m_powerline - shpPolys_powerline - buffer_100m_powerline
donut200m_powerline_AFG <- extract(sagehen_stack_AFG5, nomiddle_buffer_200m_powerline)
donut200m_powerline_PFG <- extract(sagehen_stack_PFG5, nomiddle_buffer_200m_powerline) 
donut200m_powerline_BGR <- extract(sagehen_stack_BGR5, nomiddle_buffer_200m_powerline) 
unlist_powerline_200m_AFG <- as.numeric(unlist(donut200m_powerline_AFG))
unlist_powerline_200m_PFG <- as.numeric(unlist(donut200m_powerline_PFG))
unlist_powerline_200m_BGR <- as.numeric(unlist(donut200m_powerline_BGR))
donut200m_powerline_coords_AFG <- data.frame(x=200, y=unlist_powerline_200m_AFG)
donut200m_powerline_coords_PFG <- data.frame(x=200, y=unlist_powerline_200m_PFG)
donut200m_powerline_coords_BGR <- data.frame(x=200, y=unlist_powerline_200m_BGR)

#300m donut
nomiddle_buffer_300m_powerline <- buffer_300m_powerline - shpPolys_powerline - buffer_100m_powerline - buffer_200m_powerline
donut300m_powerline_AFG <- extract(sagehen_stack_AFG5, nomiddle_buffer_300m_powerline)
donut300m_powerline_PFG <- extract(sagehen_stack_PFG5, nomiddle_buffer_300m_powerline) 
donut300m_powerline_BGR <- extract(sagehen_stack_BGR5, nomiddle_buffer_300m_powerline) 
unlist_powerline_300m_AFG <- as.numeric(unlist(donut300m_powerline_AFG))
unlist_powerline_300m_PFG <- as.numeric(unlist(donut300m_powerline_PFG))
unlist_powerline_300m_BGR <- as.numeric(unlist(donut300m_powerline_BGR))
donut300m_powerline_coords_AFG <- data.frame(x=300, y=unlist_powerline_300m_AFG)
donut300m_powerline_coords_PFG <- data.frame(x=300, y=unlist_powerline_300m_PFG)
donut300m_powerline_coords_BGR <- data.frame(x=300, y=unlist_powerline_300m_BGR)

#400m donut
nomiddle_buffer_400m_powerline <- buffer_400m_powerline - shpPolys_powerline - buffer_100m_powerline - buffer_200m_powerline - buffer_300m_powerline
donut400m_powerline_AFG <- extract(sagehen_stack_AFG5, nomiddle_buffer_400m_powerline)
donut400m_powerline_PFG <- extract(sagehen_stack_PFG5, nomiddle_buffer_400m_powerline) 
donut400m_powerline_BGR <- extract(sagehen_stack_BGR5, nomiddle_buffer_400m_powerline) 
unlist_powerline_400m_AFG <- as.numeric(unlist(donut400m_powerline_AFG))
unlist_powerline_400m_PFG <- as.numeric(unlist(donut400m_powerline_PFG))
unlist_powerline_400m_BGR <- as.numeric(unlist(donut400m_powerline_BGR))
donut400m_powerline_coords_AFG <- data.frame(x=400, y=unlist_powerline_400m_AFG)
donut400m_powerline_coords_PFG <- data.frame(x=400, y=unlist_powerline_400m_PFG)
donut400m_powerline_coords_BGR <- data.frame(x=400, y=unlist_powerline_400m_BGR)

#500m donut
nomiddle_buffer_500m_powerline <- buffer_500m_powerline - shpPolys_powerline - buffer_100m_powerline - buffer_200m_powerline - buffer_300m_powerline - buffer_400m_powerline
donut500m_powerline_AFG <- extract(sagehen_stack_AFG5, nomiddle_buffer_500m_powerline)
donut500m_powerline_PFG <- extract(sagehen_stack_PFG5, nomiddle_buffer_500m_powerline) 
donut500m_powerline_BGR <- extract(sagehen_stack_BGR5, nomiddle_buffer_500m_powerline) 
unlist_powerline_500m_AFG <- as.numeric(unlist(donut500m_powerline_AFG))
unlist_powerline_500m_PFG <- as.numeric(unlist(donut500m_powerline_PFG))
unlist_powerline_500m_BGR <- as.numeric(unlist(donut500m_powerline_BGR))
donut500m_powerline_coords_AFG <- data.frame(x=500, y=unlist_powerline_500m_AFG)
donut500m_powerline_coords_PFG <- data.frame(x=500, y=unlist_powerline_500m_PFG)
donut500m_powerline_coords_BGR <- data.frame(x=500, y=unlist_powerline_500m_BGR)

#600m
nomiddle_buffer_600m_powerline <- buffer_600m_powerline - shpPolys_powerline - buffer_100m_powerline - buffer_200m_powerline - buffer_300m_powerline - buffer_400m_powerline - buffer_500m_powerline
donut600m_powerline_AFG <- extract(sagehen_stack_AFG5, nomiddle_buffer_600m_powerline)
donut600m_powerline_PFG <- extract(sagehen_stack_PFG5, nomiddle_buffer_600m_powerline) 
donut600m_powerline_BGR <- extract(sagehen_stack_BGR5, nomiddle_buffer_600m_powerline) 
unlist_powerline_600m_AFG <- as.numeric(unlist(donut600m_powerline_AFG))
unlist_powerline_600m_PFG <- as.numeric(unlist(donut600m_powerline_PFG))
unlist_powerline_600m_BGR <- as.numeric(unlist(donut600m_powerline_BGR))
donut600m_powerline_coords_AFG <- data.frame(x=600, y=unlist_powerline_600m_AFG)
donut600m_powerline_coords_PFG <- data.frame(x=600, y=unlist_powerline_600m_PFG)
donut600m_powerline_coords_BGR <- data.frame(x=600, y=unlist_powerline_600m_BGR)

#700m
nomiddle_buffer_700m_powerline <- buffer_700m_powerline - shpPolys_powerline - buffer_100m_powerline - buffer_200m_powerline - buffer_300m_powerline - buffer_400m_powerline - buffer_500m_powerline - buffer_600m_powerline
donut700m_powerline_AFG <- extract(sagehen_stack_AFG5, nomiddle_buffer_700m_powerline)
donut700m_powerline_PFG <- extract(sagehen_stack_PFG5, nomiddle_buffer_700m_powerline) 
donut700m_powerline_BGR <- extract(sagehen_stack_BGR5, nomiddle_buffer_700m_powerline) 
unlist_powerline_700m_AFG <- as.numeric(unlist(donut700m_powerline_AFG))
unlist_powerline_700m_PFG <- as.numeric(unlist(donut700m_powerline_PFG))
unlist_powerline_700m_BGR <- as.numeric(unlist(donut700m_powerline_BGR))
donut700m_powerline_coords_AFG <- data.frame(x=700, y=unlist_powerline_700m_AFG)
donut700m_powerline_coords_PFG <- data.frame(x=700, y=unlist_powerline_700m_PFG)
donut700m_powerline_coords_BGR <- data.frame(x=700, y=unlist_powerline_700m_BGR)

#800m
nomiddle_buffer_800m_powerline <- buffer_800m_powerline - shpPolys_powerline - buffer_100m_powerline - buffer_200m_powerline - buffer_300m_powerline - buffer_400m_powerline - buffer_500m_powerline - buffer_600m_powerline - buffer_700m_powerline
donut800m_powerline_AFG <- extract(sagehen_stack_AFG5, nomiddle_buffer_800m_powerline)
donut800m_powerline_PFG <- extract(sagehen_stack_PFG5, nomiddle_buffer_800m_powerline) 
donut800m_powerline_BGR <- extract(sagehen_stack_BGR5, nomiddle_buffer_800m_powerline) 
unlist_powerline_800m_AFG <- as.numeric(unlist(donut800m_powerline_AFG))
unlist_powerline_800m_PFG <- as.numeric(unlist(donut800m_powerline_PFG))
unlist_powerline_800m_BGR <- as.numeric(unlist(donut800m_powerline_BGR))
donut800m_powerline_coords_AFG <- data.frame(x=800, y=unlist_powerline_800m_AFG)
donut800m_powerline_coords_PFG <- data.frame(x=800, y=unlist_powerline_800m_PFG)
donut800m_powerline_coords_BGR <- data.frame(x=800, y=unlist_powerline_800m_BGR)

#900m
nomiddle_buffer_900m_powerline <- buffer_900m_powerline - shpPolys_powerline - buffer_100m_powerline - buffer_200m_powerline - buffer_300m_powerline - buffer_400m_powerline - buffer_500m_powerline - buffer_600m_powerline - buffer_700m_powerline - buffer_800m_powerline
donut900m_powerline_AFG <- extract(sagehen_stack_AFG5, nomiddle_buffer_900m_powerline)
donut900m_powerline_PFG <- extract(sagehen_stack_PFG5, nomiddle_buffer_900m_powerline) 
donut900m_powerline_BGR <- extract(sagehen_stack_BGR5, nomiddle_buffer_900m_powerline) 
unlist_powerline_900m_AFG <- as.numeric(unlist(donut900m_powerline_AFG))
unlist_powerline_900m_PFG <- as.numeric(unlist(donut900m_powerline_PFG))
unlist_powerline_900m_BGR <- as.numeric(unlist(donut900m_powerline_BGR))
donut900m_powerline_coords_AFG <- data.frame(x=900, y=unlist_powerline_900m_AFG)
donut900m_powerline_coords_PFG <- data.frame(x=900, y=unlist_powerline_900m_PFG)
donut900m_powerline_coords_BGR <- data.frame(x=900, y=unlist_powerline_900m_BGR)

#1000m
nomiddle_buffer_1000m_powerline <- buffer_1000m_powerline - shpPolys_powerline - buffer_100m_powerline - buffer_200m_powerline - buffer_300m_powerline - buffer_400m_powerline - buffer_500m_powerline - buffer_600m_powerline - buffer_700m_powerline - buffer_800m_powerline - buffer_900m_powerline
donut1000m_powerline_AFG <- extract(sagehen_stack_AFG5, nomiddle_buffer_1000m_powerline)
donut1000m_powerline_PFG <- extract(sagehen_stack_PFG5, nomiddle_buffer_1000m_powerline) 
donut1000m_powerline_BGR <- extract(sagehen_stack_BGR5, nomiddle_buffer_1000m_powerline) 
unlist_powerline_1000m_AFG <- as.numeric(unlist(donut1000m_powerline_AFG))
unlist_powerline_1000m_PFG <- as.numeric(unlist(donut1000m_powerline_PFG))
unlist_powerline_1000m_BGR <- as.numeric(unlist(donut1000m_powerline_BGR))
donut1000m_powerline_coords_AFG <- data.frame(x=1000, y=unlist_powerline_1000m_AFG)
donut1000m_powerline_coords_PFG <- data.frame(x=1000, y=unlist_powerline_1000m_PFG)
donut1000m_powerline_coords_BGR <- data.frame(x=1000, y=unlist_powerline_1000m_BGR)

##Bridgewaterhole
#100m donut (creates buffer excluding middle (subtraction), then uses extract to apply buffer to raster GPS data)
nomiddle_buffer_100m_bridgewaterhole <- buffer_100m_bridgewaterhole - shpPolys_bridgewaterhole
donut100m_bridgewaterhole_AFG <- extract(sagehen_stack_AFG5, nomiddle_buffer_100m_bridgewaterhole)
donut100m_bridgewaterhole_PFG <- extract(sagehen_stack_PFG5, nomiddle_buffer_100m_bridgewaterhole)
donut100m_bridgewaterhole_BGR <- extract(sagehen_stack_BGR5, nomiddle_buffer_100m_bridgewaterhole)
unlist_bridgewaterhole_100m_AFG <- as.numeric(unlist(donut100m_bridgewaterhole_AFG))
unlist_bridgewaterhole_100m_PFG <- as.numeric(unlist(donut100m_bridgewaterhole_PFG))
unlist_bridgewaterhole_100m_BGR <- as.numeric(unlist(donut100m_bridgewaterhole_BGR))
donut100m_bridgewaterhole_coords_AFG <- data.frame(x=100, y=unlist_bridgewaterhole_100m_AFG)
donut100m_bridgewaterhole_coords_PFG <- data.frame(x=100, y=unlist_bridgewaterhole_100m_PFG)
donut100m_bridgewaterhole_coords_BGR <- data.frame(x=100, y=unlist_bridgewaterhole_100m_BGR)

#200m donut
nomiddle_buffer_200m_bridgewaterhole <- buffer_200m_bridgewaterhole - shpPolys_bridgewaterhole - buffer_100m_bridgewaterhole
donut200m_bridgewaterhole_AFG <- extract(sagehen_stack_AFG5, nomiddle_buffer_200m_bridgewaterhole)
donut200m_bridgewaterhole_PFG <- extract(sagehen_stack_PFG5, nomiddle_buffer_200m_bridgewaterhole)
donut200m_bridgewaterhole_BGR <- extract(sagehen_stack_BGR5, nomiddle_buffer_200m_bridgewaterhole)
unlist_bridgewaterhole_200m_AFG <- as.numeric(unlist(donut200m_bridgewaterhole_AFG))
unlist_bridgewaterhole_200m_PFG <- as.numeric(unlist(donut200m_bridgewaterhole_PFG))
unlist_bridgewaterhole_200m_BGR <- as.numeric(unlist(donut200m_bridgewaterhole_BGR))
donut200m_bridgewaterhole_coords_AFG <- data.frame(x=200, y=unlist_bridgewaterhole_200m_AFG)
donut200m_bridgewaterhole_coords_PFG <- data.frame(x=200, y=unlist_bridgewaterhole_200m_PFG)
donut200m_bridgewaterhole_coords_BGR <- data.frame(x=200, y=unlist_bridgewaterhole_200m_BGR)

#300m donut
nomiddle_buffer_300m_bridgewaterhole <- buffer_300m_bridgewaterhole- shpPolys_bridgewaterhole - buffer_100m_bridgewaterhole - buffer_200m_bridgewaterhole
donut300m_bridgewaterhole_AFG <- extract(sagehen_stack_AFG5, nomiddle_buffer_300m_bridgewaterhole)
donut300m_bridgewaterhole_PFG <- extract(sagehen_stack_PFG5, nomiddle_buffer_300m_bridgewaterhole)
donut300m_bridgewaterhole_BGR <- extract(sagehen_stack_BGR5, nomiddle_buffer_300m_bridgewaterhole)
unlist_bridgewaterhole_300m_AFG <- as.numeric(unlist(donut300m_bridgewaterhole_AFG))
unlist_bridgewaterhole_300m_PFG <- as.numeric(unlist(donut300m_bridgewaterhole_PFG))
unlist_bridgewaterhole_300m_BGR <- as.numeric(unlist(donut300m_bridgewaterhole_BGR))
donut300m_bridgewaterhole_coords_AFG <- data.frame(x=300, y=unlist_bridgewaterhole_300m_AFG)
donut300m_bridgewaterhole_coords_PFG <- data.frame(x=300, y=unlist_bridgewaterhole_300m_PFG)
donut300m_bridgewaterhole_coords_BGR <- data.frame(x=300, y=unlist_bridgewaterhole_300m_BGR)

#400m donut
nomiddle_buffer_400m_bridgewaterhole <- buffer_400m_bridgewaterhole - shpPolys_bridgewaterhole - buffer_100m_bridgewaterhole - buffer_200m_bridgewaterhole - buffer_300m_bridgewaterhole
donut400m_bridgewaterhole_AFG <- extract(sagehen_stack_AFG5, nomiddle_buffer_400m_bridgewaterhole)
donut400m_bridgewaterhole_PFG <- extract(sagehen_stack_PFG5, nomiddle_buffer_400m_bridgewaterhole)
donut400m_bridgewaterhole_BGR <- extract(sagehen_stack_BGR5, nomiddle_buffer_400m_bridgewaterhole)
unlist_bridgewaterhole_400m_AFG <- as.numeric(unlist(donut400m_bridgewaterhole_AFG))
unlist_bridgewaterhole_400m_PFG <- as.numeric(unlist(donut400m_bridgewaterhole_PFG))
unlist_bridgewaterhole_400m_BGR <- as.numeric(unlist(donut400m_bridgewaterhole_BGR))
donut400m_bridgewaterhole_coords_AFG <- data.frame(x=400, y=unlist_bridgewaterhole_400m_AFG)
donut400m_bridgewaterhole_coords_PFG <- data.frame(x=400, y=unlist_bridgewaterhole_400m_PFG)
donut400m_bridgewaterhole_coords_BGR <- data.frame(x=400, y=unlist_bridgewaterhole_400m_BGR)

#500m donut
nomiddle_buffer_500m_bridgewaterhole <- buffer_500m_bridgewaterhole - shpPolys_bridgewaterhole - buffer_100m_bridgewaterhole - buffer_200m_bridgewaterhole - buffer_300m_bridgewaterhole - buffer_400m_bridgewaterhole
donut500m_bridgewaterhole_AFG <- extract(sagehen_stack_AFG5, nomiddle_buffer_500m_bridgewaterhole)
donut500m_bridgewaterhole_PFG <- extract(sagehen_stack_PFG5, nomiddle_buffer_500m_bridgewaterhole)
donut500m_bridgewaterhole_BGR <- extract(sagehen_stack_BGR5, nomiddle_buffer_500m_bridgewaterhole)
unlist_bridgewaterhole_500m_AFG <- as.numeric(unlist(donut500m_bridgewaterhole_AFG))
unlist_bridgewaterhole_500m_PFG <- as.numeric(unlist(donut500m_bridgewaterhole_PFG))
unlist_bridgewaterhole_500m_BGR <- as.numeric(unlist(donut500m_bridgewaterhole_BGR))
donut500m_bridgewaterhole_coords_AFG <- data.frame(x=500, y=unlist_bridgewaterhole_500m_AFG)
donut500m_bridgewaterhole_coords_PFG <- data.frame(x=500, y=unlist_bridgewaterhole_500m_PFG)
donut500m_bridgewaterhole_coords_BGR <- data.frame(x=500, y=unlist_bridgewaterhole_500m_BGR)

#600m
nomiddle_buffer_600m_bridgewaterhole <- buffer_600m_bridgewaterhole - shpPolys_bridgewaterhole - buffer_100m_bridgewaterhole - buffer_200m_bridgewaterhole - buffer_300m_bridgewaterhole - buffer_400m_bridgewaterhole - buffer_500m_bridgewaterhole
donut600m_bridgewaterhole_AFG <- extract(sagehen_stack_AFG5, nomiddle_buffer_600m_bridgewaterhole)
donut600m_bridgewaterhole_PFG <- extract(sagehen_stack_PFG5, nomiddle_buffer_600m_bridgewaterhole)
donut600m_bridgewaterhole_BGR <- extract(sagehen_stack_BGR5, nomiddle_buffer_600m_bridgewaterhole)
unlist_bridgewaterhole_600m_AFG <- as.numeric(unlist(donut600m_bridgewaterhole_AFG))
unlist_bridgewaterhole_600m_PFG <- as.numeric(unlist(donut600m_bridgewaterhole_PFG))
unlist_bridgewaterhole_600m_BGR <- as.numeric(unlist(donut600m_bridgewaterhole_BGR))
donut600m_bridgewaterhole_coords_AFG <- data.frame(x=600, y=unlist_bridgewaterhole_600m_AFG)
donut600m_bridgewaterhole_coords_PFG <- data.frame(x=600, y=unlist_bridgewaterhole_600m_PFG)
donut600m_bridgewaterhole_coords_BGR <- data.frame(x=600, y=unlist_bridgewaterhole_600m_BGR)

#700m
nomiddle_buffer_700m_bridgewaterhole <- buffer_700m_bridgewaterhole - shpPolys_bridgewaterhole - buffer_100m_bridgewaterhole - buffer_200m_bridgewaterhole - buffer_300m_bridgewaterhole - buffer_400m_bridgewaterhole - buffer_500m_bridgewaterhole - buffer_600m_bridgewaterhole
donut700m_bridgewaterhole_AFG <- extract(sagehen_stack_AFG5, nomiddle_buffer_700m_bridgewaterhole)
donut700m_bridgewaterhole_PFG <- extract(sagehen_stack_PFG5, nomiddle_buffer_700m_bridgewaterhole)
donut700m_bridgewaterhole_BGR <- extract(sagehen_stack_BGR5, nomiddle_buffer_700m_bridgewaterhole)
unlist_bridgewaterhole_700m_AFG <- as.numeric(unlist(donut700m_bridgewaterhole_AFG))
unlist_bridgewaterhole_700m_PFG <- as.numeric(unlist(donut700m_bridgewaterhole_PFG))
unlist_bridgewaterhole_700m_BGR <- as.numeric(unlist(donut700m_bridgewaterhole_BGR))
donut700m_bridgewaterhole_coords_AFG <- data.frame(x=700, y=unlist_bridgewaterhole_700m_AFG)
donut700m_bridgewaterhole_coords_PFG <- data.frame(x=700, y=unlist_bridgewaterhole_700m_PFG)
donut700m_bridgewaterhole_coords_BGR <- data.frame(x=700, y=unlist_bridgewaterhole_700m_BGR)

#800m
nomiddle_buffer_800m_bridgewaterhole <- buffer_800m_bridgewaterhole - shpPolys_bridgewaterhole - buffer_100m_bridgewaterhole - buffer_200m_bridgewaterhole - buffer_300m_bridgewaterhole - buffer_400m_bridgewaterhole - buffer_500m_bridgewaterhole - buffer_600m_bridgewaterhole - buffer_700m_bridgewaterhole
donut800m_bridgewaterhole_AFG <- extract(sagehen_stack_AFG5, nomiddle_buffer_800m_bridgewaterhole)
donut800m_bridgewaterhole_PFG <- extract(sagehen_stack_PFG5, nomiddle_buffer_800m_bridgewaterhole)
donut800m_bridgewaterhole_BGR <- extract(sagehen_stack_BGR5, nomiddle_buffer_800m_bridgewaterhole)
unlist_bridgewaterhole_800m_AFG <- as.numeric(unlist(donut800m_bridgewaterhole_AFG))
unlist_bridgewaterhole_800m_PFG <- as.numeric(unlist(donut800m_bridgewaterhole_PFG))
unlist_bridgewaterhole_800m_BGR <- as.numeric(unlist(donut800m_bridgewaterhole_BGR))
donut800m_bridgewaterhole_coords_AFG <- data.frame(x=800, y=unlist_bridgewaterhole_800m_AFG)
donut800m_bridgewaterhole_coords_PFG <- data.frame(x=800, y=unlist_bridgewaterhole_800m_PFG)
donut800m_bridgewaterhole_coords_BGR <- data.frame(x=800, y=unlist_bridgewaterhole_800m_BGR)

#900m
nomiddle_buffer_900m_bridgewaterhole <- buffer_900m_bridgewaterhole - shpPolys_bridgewaterhole - buffer_100m_bridgewaterhole - buffer_200m_bridgewaterhole - buffer_300m_bridgewaterhole - buffer_400m_bridgewaterhole - buffer_500m_bridgewaterhole - buffer_600m_bridgewaterhole - buffer_700m_bridgewaterhole - buffer_800m_bridgewaterhole
donut900m_bridgewaterhole_AFG <- extract(sagehen_stack_AFG5, nomiddle_buffer_900m_bridgewaterhole)
donut900m_bridgewaterhole_PFG <- extract(sagehen_stack_PFG5, nomiddle_buffer_900m_bridgewaterhole)
donut900m_bridgewaterhole_BGR <- extract(sagehen_stack_BGR5, nomiddle_buffer_900m_bridgewaterhole)
unlist_bridgewaterhole_900m_AFG <- as.numeric(unlist(donut900m_bridgewaterhole_AFG))
unlist_bridgewaterhole_900m_PFG <- as.numeric(unlist(donut900m_bridgewaterhole_PFG))
unlist_bridgewaterhole_900m_BGR <- as.numeric(unlist(donut900m_bridgewaterhole_BGR))
donut900m_bridgewaterhole_coords_AFG <- data.frame(x=900, y=unlist_bridgewaterhole_900m_AFG)
donut900m_bridgewaterhole_coords_PFG <- data.frame(x=900, y=unlist_bridgewaterhole_900m_PFG)
donut900m_bridgewaterhole_coords_BGR <- data.frame(x=900, y=unlist_bridgewaterhole_900m_BGR)

#1000m
nomiddle_buffer_1000m_bridgewaterhole <- buffer_1000m_bridgewaterhole - shpPolys_bridgewaterhole - buffer_100m_bridgewaterhole - buffer_200m_bridgewaterhole - buffer_300m_bridgewaterhole - buffer_400m_bridgewaterhole - buffer_500m_bridgewaterhole - buffer_600m_bridgewaterhole - buffer_700m_bridgewaterhole - buffer_800m_bridgewaterhole - buffer_900m_bridgewaterhole
donut1000m_bridgewaterhole_AFG <- extract(sagehen_stack_AFG5, nomiddle_buffer_1000m_bridgewaterhole)
donut1000m_bridgewaterhole_PFG <- extract(sagehen_stack_PFG5, nomiddle_buffer_1000m_bridgewaterhole)
donut1000m_bridgewaterhole_BGR <- extract(sagehen_stack_BGR5, nomiddle_buffer_1000m_bridgewaterhole)
unlist_bridgewaterhole_1000m_AFG <- as.numeric(unlist(donut1000m_bridgewaterhole_AFG))
unlist_bridgewaterhole_1000m_PFG <- as.numeric(unlist(donut1000m_bridgewaterhole_PFG))
unlist_bridgewaterhole_1000m_BGR <- as.numeric(unlist(donut1000m_bridgewaterhole_BGR))
donut1000m_bridgewaterhole_coords_AFG <- data.frame(x=1000, y=unlist_bridgewaterhole_1000m_AFG)
donut1000m_bridgewaterhole_coords_PFG <- data.frame(x=1000, y=unlist_bridgewaterhole_1000m_PFG)
donut1000m_bridgewaterhole_coords_BGR <- data.frame(x=1000, y=unlist_bridgewaterhole_1000m_BGR)

##MapwellPt1
#100m donut (creates buffer excluding middle (subtraction), then uses extract to apply buffer to raster GPS data)
nomiddle_buffer_100m_mapwell <- buffer_100m_mapwell - buffer_30m_mapwell
donut100m_mapwell_AFG <- extract(saddle_stack_AFG5, nomiddle_buffer_100m_mapwell) 
donut100m_mapwell_PFG <- extract(saddle_stack_PFG5, nomiddle_buffer_100m_mapwell) 
donut100m_mapwell_BGR <- extract(saddle_stack_BGR5, nomiddle_buffer_100m_mapwell) 
unlist_mapwell_100m_AFG <- as.numeric(unlist(donut100m_mapwell_AFG))
unlist_mapwell_100m_PFG <- as.numeric(unlist(donut100m_mapwell_PFG))
unlist_mapwell_100m_BGR <- as.numeric(unlist(donut100m_mapwell_BGR))
donut100m_mapwell_coords_AFG <- data.frame(x=100, y=unlist_mapwell_100m_AFG)
donut100m_mapwell_coords_PFG <- data.frame(x=100, y=unlist_mapwell_100m_PFG)
donut100m_mapwell_coords_BGR <- data.frame(x=100, y=unlist_mapwell_100m_BGR)

#200m donut 
nomiddle_buffer_200m_mapwell <- buffer_200m_mapwell - buffer_100m_mapwell - buffer_30m_mapwell
donut200m_mapwell_AFG <- extract(saddle_stack_AFG5, nomiddle_buffer_200m_mapwell) 
donut200m_mapwell_PFG <- extract(saddle_stack_PFG5, nomiddle_buffer_200m_mapwell) 
donut200m_mapwell_BGR <- extract(saddle_stack_BGR5, nomiddle_buffer_200m_mapwell) 
unlist_mapwell_200m_AFG <- as.numeric(unlist(donut200m_mapwell_AFG))
unlist_mapwell_200m_PFG <- as.numeric(unlist(donut200m_mapwell_PFG))
unlist_mapwell_200m_BGR <- as.numeric(unlist(donut200m_mapwell_BGR))
donut200m_mapwell_coords_AFG <- data.frame(x=200, y=unlist_mapwell_200m_AFG)
donut200m_mapwell_coords_PFG <- data.frame(x=200, y=unlist_mapwell_200m_PFG)
donut200m_mapwell_coords_BGR <- data.frame(x=200, y=unlist_mapwell_200m_BGR)

#300m donut
nomiddle_buffer_300m_mapwell <- buffer_300m_mapwell - buffer_100m_mapwell - buffer_200m_mapwell - buffer_30m_mapwell
donut300m_mapwell_AFG <- extract(saddle_stack_AFG5, nomiddle_buffer_300m_mapwell) 
donut300m_mapwell_PFG <- extract(saddle_stack_PFG5, nomiddle_buffer_300m_mapwell) 
donut300m_mapwell_BGR <- extract(saddle_stack_BGR5, nomiddle_buffer_300m_mapwell) 
unlist_mapwell_300m_AFG <- as.numeric(unlist(donut300m_mapwell_AFG))
unlist_mapwell_300m_PFG <- as.numeric(unlist(donut300m_mapwell_PFG))
unlist_mapwell_300m_BGR <- as.numeric(unlist(donut300m_mapwell_BGR))
donut300m_mapwell_coords_AFG <- data.frame(x=300, y=unlist_mapwell_300m_AFG)
donut300m_mapwell_coords_PFG <- data.frame(x=300, y=unlist_mapwell_300m_PFG)
donut300m_mapwell_coords_BGR <- data.frame(x=300, y=unlist_mapwell_300m_BGR)

#400m donut
nomiddle_buffer_400m_mapwell <- buffer_400m_mapwell- buffer_100m_mapwell - buffer_200m_mapwell - buffer_300m_mapwell- buffer_30m_mapwell
donut400m_mapwell_AFG <- extract(saddle_stack_AFG5, nomiddle_buffer_400m_mapwell) 
donut400m_mapwell_PFG <- extract(saddle_stack_PFG5, nomiddle_buffer_400m_mapwell) 
donut400m_mapwell_BGR <- extract(saddle_stack_BGR5, nomiddle_buffer_400m_mapwell) 
unlist_mapwell_400m_AFG <- as.numeric(unlist(donut400m_mapwell_AFG))
unlist_mapwell_400m_PFG <- as.numeric(unlist(donut400m_mapwell_PFG))
unlist_mapwell_400m_BGR <- as.numeric(unlist(donut400m_mapwell_BGR))
donut400m_mapwell_coords_AFG <- data.frame(x=400, y=unlist_mapwell_400m_AFG)
donut400m_mapwell_coords_PFG <- data.frame(x=400, y=unlist_mapwell_400m_PFG)
donut400m_mapwell_coords_BGR <- data.frame(x=400, y=unlist_mapwell_400m_BGR)

#500m donut
nomiddle_buffer_500m_mapwell <- buffer_500m_mapwell - buffer_100m_mapwell - buffer_200m_mapwell - buffer_300m_mapwell - buffer_400m_mapwell - buffer_30m_mapwell
donut500m_mapwell_AFG <- extract(saddle_stack_AFG5, nomiddle_buffer_500m_mapwell) 
donut500m_mapwell_PFG <- extract(saddle_stack_PFG5, nomiddle_buffer_500m_mapwell) 
donut500m_mapwell_BGR <- extract(saddle_stack_BGR5, nomiddle_buffer_500m_mapwell) 
unlist_mapwell_500m_AFG <- as.numeric(unlist(donut500m_mapwell_AFG))
unlist_mapwell_500m_PFG <- as.numeric(unlist(donut500m_mapwell_PFG))
unlist_mapwell_500m_BGR <- as.numeric(unlist(donut500m_mapwell_BGR))
donut500m_mapwell_coords_AFG <- data.frame(x=500, y=unlist_mapwell_500m_AFG)
donut500m_mapwell_coords_PFG <- data.frame(x=500, y=unlist_mapwell_500m_PFG)
donut500m_mapwell_coords_BGR <- data.frame(x=500, y=unlist_mapwell_500m_BGR)

#600m 
nomiddle_buffer_600m_mapwell <- buffer_600m_mapwell - buffer_100m_mapwell - buffer_200m_mapwell - buffer_300m_mapwell - buffer_400m_mapwell - buffer_500m_mapwell
donut600m_mapwell_AFG <- extract(saddle_stack_AFG5, nomiddle_buffer_600m_mapwell) 
donut600m_mapwell_PFG <- extract(saddle_stack_PFG5, nomiddle_buffer_600m_mapwell) 
donut600m_mapwell_BGR <- extract(saddle_stack_BGR5, nomiddle_buffer_600m_mapwell) 
unlist_mapwell_600m_AFG <- as.numeric(unlist(donut600m_mapwell_AFG))
unlist_mapwell_600m_PFG <- as.numeric(unlist(donut600m_mapwell_PFG))
unlist_mapwell_600m_BGR <- as.numeric(unlist(donut600m_mapwell_BGR))
donut600m_mapwell_coords_AFG <- data.frame(x=600, y=unlist_mapwell_600m_AFG)
donut600m_mapwell_coords_PFG <- data.frame(x=600, y=unlist_mapwell_600m_PFG)
donut600m_mapwell_coords_BGR <- data.frame(x=600, y=unlist_mapwell_600m_BGR)

#700m
nomiddle_buffer_700m_mapwell <- buffer_700m_mapwell - buffer_100m_mapwell - buffer_200m_mapwell - buffer_300m_mapwell - buffer_400m_mapwell - buffer_500m_mapwell - buffer_600m_mapwell
donut700m_mapwell_AFG <- extract(saddle_stack_AFG5, nomiddle_buffer_700m_mapwell) 
donut700m_mapwell_PFG <- extract(saddle_stack_PFG5, nomiddle_buffer_700m_mapwell) 
donut700m_mapwell_BGR <- extract(saddle_stack_BGR5, nomiddle_buffer_700m_mapwell) 
unlist_mapwell_700m_AFG <- as.numeric(unlist(donut700m_mapwell_AFG))
unlist_mapwell_700m_PFG <- as.numeric(unlist(donut700m_mapwell_PFG))
unlist_mapwell_700m_BGR <- as.numeric(unlist(donut700m_mapwell_BGR))
donut700m_mapwell_coords_AFG <- data.frame(x=700, y=unlist_mapwell_700m_AFG)
donut700m_mapwell_coords_PFG <- data.frame(x=700, y=unlist_mapwell_700m_PFG)
donut700m_mapwell_coords_BGR <- data.frame(x=700, y=unlist_mapwell_700m_BGR)

#800m
nomiddle_buffer_800m_mapwell <- buffer_800m_mapwell - buffer_100m_mapwell - buffer_200m_mapwell - buffer_300m_mapwell - buffer_400m_mapwell - buffer_500m_mapwell - buffer_600m_mapwell - buffer_700m_mapwell
donut800m_mapwell_AFG <- extract(saddle_stack_AFG5, nomiddle_buffer_800m_mapwell) 
donut800m_mapwell_PFG <- extract(saddle_stack_PFG5, nomiddle_buffer_800m_mapwell) 
donut800m_mapwell_BGR <- extract(saddle_stack_BGR5, nomiddle_buffer_800m_mapwell) 
unlist_mapwell_800m_AFG <- as.numeric(unlist(donut800m_mapwell_AFG))
unlist_mapwell_800m_PFG <- as.numeric(unlist(donut800m_mapwell_PFG))
unlist_mapwell_800m_BGR <- as.numeric(unlist(donut800m_mapwell_BGR))
donut800m_mapwell_coords_AFG <- data.frame(x=800, y=unlist_mapwell_800m_AFG)
donut800m_mapwell_coords_PFG <- data.frame(x=800, y=unlist_mapwell_800m_PFG)
donut800m_mapwell_coords_BGR <- data.frame(x=800, y=unlist_mapwell_800m_BGR)

#900m
nomiddle_buffer_900m_mapwell <- buffer_900m_mapwell - buffer_100m_mapwell - buffer_200m_mapwell - buffer_300m_mapwell - buffer_400m_mapwell - buffer_500m_mapwell - buffer_600m_mapwell - buffer_700m_mapwell - buffer_800m_mapwell
donut900m_mapwell_AFG <- extract(saddle_stack_AFG5, nomiddle_buffer_900m_mapwell) 
donut900m_mapwell_PFG <- extract(saddle_stack_PFG5, nomiddle_buffer_900m_mapwell) 
donut900m_mapwell_BGR <- extract(saddle_stack_BGR5, nomiddle_buffer_900m_mapwell) 
unlist_mapwell_900m_AFG <- as.numeric(unlist(donut900m_mapwell_AFG))
unlist_mapwell_900m_PFG <- as.numeric(unlist(donut900m_mapwell_PFG))
unlist_mapwell_900m_BGR <- as.numeric(unlist(donut900m_mapwell_BGR))
donut900m_mapwell_coords_AFG <- data.frame(x=900, y=unlist_mapwell_900m_AFG)
donut900m_mapwell_coords_PFG <- data.frame(x=900, y=unlist_mapwell_900m_PFG)
donut900m_mapwell_coords_BGR <- data.frame(x=900, y=unlist_mapwell_900m_BGR)

#1000m
nomiddle_buffer_1000m_mapwell <- buffer_1000m_mapwell - buffer_100m_mapwell - buffer_200m_mapwell - buffer_300m_mapwell - buffer_400m_mapwell - buffer_500m_mapwell - buffer_600m_mapwell - buffer_700m_mapwell - buffer_800m_mapwell - buffer_900m_mapwell
donut1000m_mapwell_AFG <- extract(saddle_stack_AFG5, nomiddle_buffer_1000m_mapwell) 
donut1000m_mapwell_PFG <- extract(saddle_stack_PFG5, nomiddle_buffer_1000m_mapwell) 
donut1000m_mapwell_BGR <- extract(saddle_stack_BGR5, nomiddle_buffer_1000m_mapwell) 
unlist_mapwell_1000m_AFG <- as.numeric(unlist(donut1000m_mapwell_AFG))
unlist_mapwell_1000m_PFG <- as.numeric(unlist(donut1000m_mapwell_PFG))
unlist_mapwell_1000m_BGR <- as.numeric(unlist(donut1000m_mapwell_BGR))
donut1000m_mapwell_coords_AFG <- data.frame(x=1000, y=unlist_mapwell_1000m_AFG)
donut1000m_mapwell_coords_PFG <- data.frame(x=1000, y=unlist_mapwell_1000m_PFG)
donut1000m_mapwell_coords_BGR <- data.frame(x=1000, y=unlist_mapwell_1000m_BGR)


#### This section finds means of AFG, PFG, BGR from donuts (100m - 500m) for all watering holes

##Pinespring
#100m donut mean 
donut100m_pinespring_mean_AFG <- extract(sagehen_stack_AFG5, nomiddle_buffer_100m_pinespring, fun = mean) 
donut100m_pinespring_mean_PFG <- extract(sagehen_stack_PFG5, nomiddle_buffer_100m_pinespring, fun = mean)
donut100m_pinespring_mean_BGR <- extract(sagehen_stack_BGR5, nomiddle_buffer_100m_pinespring, fun = mean) 

#200m donut mean
donut200m_pinespring_mean_AFG <- extract(sagehen_stack_AFG5, nomiddle_buffer_200m_pinespring, fun = mean) 
donut200m_pinespring_mean_PFG <- extract(sagehen_stack_PFG5, nomiddle_buffer_200m_pinespring, fun = mean)
donut200m_pinespring_mean_BGR <- extract(sagehen_stack_BGR5, nomiddle_buffer_200m_pinespring, fun = mean) 

#300m donut mean
donut300m_pinespring_mean_AFG <- extract(sagehen_stack_AFG5, nomiddle_buffer_300m_pinespring, fun = mean) 
donut300m_pinespring_mean_PFG <- extract(sagehen_stack_PFG5, nomiddle_buffer_300m_pinespring, fun = mean)
donut300m_pinespring_mean_BGR <- extract(sagehen_stack_BGR5, nomiddle_buffer_300m_pinespring, fun = mean) 

#400m donut mean
donut400m_pinespring_mean_AFG <- extract(sagehen_stack_AFG5, nomiddle_buffer_400m_pinespring, fun = mean) 
donut400m_pinespring_mean_PFG <- extract(sagehen_stack_PFG5, nomiddle_buffer_400m_pinespring, fun = mean)
donut400m_pinespring_mean_BGR <- extract(sagehen_stack_BGR5, nomiddle_buffer_400m_pinespring, fun = mean) 

#500m donut mean
donut500m_pinespring_mean_AFG <- extract(sagehen_stack_AFG5, nomiddle_buffer_500m_pinespring, fun = mean) 
donut500m_pinespring_mean_PFG <- extract(sagehen_stack_PFG5, nomiddle_buffer_500m_pinespring, fun = mean)
donut500m_pinespring_mean_BGR <- extract(sagehen_stack_BGR5, nomiddle_buffer_500m_pinespring, fun = mean) 

##Upperfay
#100m donut mean
donut100m_upperfay_mean_AFG <- extract(sagehen_stack_AFG5, nomiddle_buffer_100m_upperfay, fun = mean)
donut100m_upperfay_mean_PFG <- extract(sagehen_stack_PFG5, nomiddle_buffer_100m_upperfay, fun = mean)
donut100m_upperfay_mean_BGR <- extract(sagehen_stack_BGR5, nomiddle_buffer_100m_upperfay, fun = mean)

#200m donut mean
donut200m_upperfay_mean_AFG <- extract(sagehen_stack_AFG5, nomiddle_buffer_200m_upperfay, fun = mean)
donut200m_upperfay_mean_PFG <- extract(sagehen_stack_PFG5, nomiddle_buffer_200m_upperfay, fun = mean)
donut200m_upperfay_mean_BGR <- extract(sagehen_stack_BGR5, nomiddle_buffer_200m_upperfay, fun = mean)

#300m donut mean
donut300m_upperfay_mean_AFG <- extract(sagehen_stack_AFG5, nomiddle_buffer_300m_upperfay, fun = mean)
donut300m_upperfay_mean_PFG <- extract(sagehen_stack_PFG5, nomiddle_buffer_300m_upperfay, fun = mean)
donut300m_upperfay_mean_BGR <- extract(sagehen_stack_BGR5, nomiddle_buffer_300m_upperfay, fun = mean)

#400m donut mean
donut400m_upperfay_mean_AFG <- extract(sagehen_stack_AFG5, nomiddle_buffer_400m_upperfay, fun = mean)
donut400m_upperfay_mean_PFG <- extract(sagehen_stack_PFG5, nomiddle_buffer_400m_upperfay, fun = mean)
donut400m_upperfay_mean_BGR <- extract(sagehen_stack_BGR5, nomiddle_buffer_400m_upperfay, fun = mean)

#500m donut mean
donut500m_upperfay_mean_AFG <- extract(sagehen_stack_AFG5, nomiddle_buffer_500m_upperfay, fun = mean)
donut500m_upperfay_mean_PFG <- extract(sagehen_stack_PFG5, nomiddle_buffer_500m_upperfay, fun = mean)
donut500m_upperfay_mean_BGR <- extract(sagehen_stack_BGR5, nomiddle_buffer_500m_upperfay, fun = mean)

##Sagehenhill
#100m donut mean
donut100m_sagehenhill_mean_AFG <- extract(sagehen_stack_AFG5, nomiddle_buffer_100m_sagehenhill, fun = mean)
donut100m_sagehenhill_mean_PFG <- extract(sagehen_stack_PFG5, nomiddle_buffer_100m_sagehenhill, fun = mean)
donut100m_sagehenhill_mean_BGR <- extract(sagehen_stack_BGR5, nomiddle_buffer_100m_sagehenhill, fun = mean)

#200m donut mean
donut200m_sagehenhill_mean_AFG <- extract(sagehen_stack_AFG5, nomiddle_buffer_200m_sagehenhill, fun = mean)
donut200m_sagehenhill_mean_PFG <- extract(sagehen_stack_PFG5, nomiddle_buffer_200m_sagehenhill, fun = mean)
donut200m_sagehenhill_mean_BGR <- extract(sagehen_stack_BGR5, nomiddle_buffer_200m_sagehenhill, fun = mean)

#300m donut mean
donut300m_sagehenhill_mean_AFG <- extract(sagehen_stack_AFG5, nomiddle_buffer_300m_sagehenhill, fun = mean)
donut300m_sagehenhill_mean_PFG <- extract(sagehen_stack_PFG5, nomiddle_buffer_300m_sagehenhill, fun = mean)
donut300m_sagehenhill_mean_BGR <- extract(sagehen_stack_BGR5, nomiddle_buffer_300m_sagehenhill, fun = mean)

#400m donut mean
donut400m_sagehenhill_mean_AFG <- extract(sagehen_stack_AFG5, nomiddle_buffer_400m_sagehenhill, fun = mean)
donut400m_sagehenhill_mean_PFG <- extract(sagehen_stack_PFG5, nomiddle_buffer_400m_sagehenhill, fun = mean)
donut400m_sagehenhill_mean_BGR <- extract(sagehen_stack_BGR5, nomiddle_buffer_400m_sagehenhill, fun = mean)

#500m donut mean
donut500m_sagehenhill_mean_AFG <- extract(sagehen_stack_AFG5, nomiddle_buffer_500m_sagehenhill, fun = mean)
donut500m_sagehenhill_mean_PFG <- extract(sagehen_stack_PFG5, nomiddle_buffer_500m_sagehenhill, fun = mean)
donut500m_sagehenhill_mean_BGR <- extract(sagehen_stack_BGR5, nomiddle_buffer_500m_sagehenhill, fun = mean)

##Powerline
#100m donut mean
donut100m_powerline_mean_AFG <- extract(sagehen_stack_AFG5, nomiddle_buffer_100m_powerline, fun = mean)
donut100m_powerline_mean_PFG <- extract(sagehen_stack_PFG5, nomiddle_buffer_100m_powerline, fun = mean)
donut100m_powerline_mean_BGR <- extract(sagehen_stack_BGR5, nomiddle_buffer_100m_powerline, fun = mean)

#200m donut mean
donut200m_powerline_mean_AFG <- extract(sagehen_stack_AFG5, nomiddle_buffer_200m_powerline, fun = mean)
donut200m_powerline_mean_PFG <- extract(sagehen_stack_PFG5, nomiddle_buffer_200m_powerline, fun = mean)
donut200m_powerline_mean_BGR <- extract(sagehen_stack_BGR5, nomiddle_buffer_200m_powerline, fun = mean)

#300m donut mean
donut300m_powerline_mean_AFG <- extract(sagehen_stack_AFG5, nomiddle_buffer_300m_powerline, fun = mean)
donut300m_powerline_mean_PFG <- extract(sagehen_stack_PFG5, nomiddle_buffer_300m_powerline, fun = mean)
donut300m_powerline_mean_BGR <- extract(sagehen_stack_BGR5, nomiddle_buffer_300m_powerline, fun = mean)

#400m donut mean
donut400m_powerline_mean_AFG <- extract(sagehen_stack_AFG5, nomiddle_buffer_400m_powerline, fun = mean)
donut400m_powerline_mean_PFG <- extract(sagehen_stack_PFG5, nomiddle_buffer_400m_powerline, fun = mean)
donut400m_powerline_mean_BGR <- extract(sagehen_stack_BGR5, nomiddle_buffer_400m_powerline, fun = mean)

#500m donut mean
donut500m_powerline_mean_AFG <- extract(sagehen_stack_AFG5, nomiddle_buffer_500m_powerline, fun = mean)
donut500m_powerline_mean_PFG <- extract(sagehen_stack_PFG5, nomiddle_buffer_500m_powerline, fun = mean)
donut500m_powerline_mean_BGR <- extract(sagehen_stack_BGR5, nomiddle_buffer_500m_powerline, fun = mean)

##Bridgewaterhole
#100m donut mean
donut100m_bridgewaterhole_mean_AFG <- extract(sagehen_stack_AFG5, nomiddle_buffer_100m_bridgewaterhole, fun = mean)
donut100m_bridgewaterhole_mean_PFG <- extract(sagehen_stack_PFG5, nomiddle_buffer_100m_bridgewaterhole, fun = mean)
donut100m_bridgewaterhole_mean_BGR <- extract(sagehen_stack_BGR5, nomiddle_buffer_100m_bridgewaterhole, fun = mean)

#200m donut mean
donut200m_bridgewaterhole_mean_AFG <- extract(sagehen_stack_AFG5, nomiddle_buffer_200m_bridgewaterhole, fun = mean)
donut200m_bridgewaterhole_mean_PFG <- extract(sagehen_stack_PFG5, nomiddle_buffer_200m_bridgewaterhole, fun = mean)
donut200m_bridgewaterhole_mean_BGR <- extract(sagehen_stack_BGR5, nomiddle_buffer_200m_bridgewaterhole, fun = mean)

#300m donut mean
donut300m_bridgewaterhole_mean_AFG <- extract(sagehen_stack_AFG5, nomiddle_buffer_300m_bridgewaterhole, fun = mean)
donut300m_bridgewaterhole_mean_PFG <- extract(sagehen_stack_PFG5, nomiddle_buffer_300m_bridgewaterhole, fun = mean)
donut300m_bridgewaterhole_mean_BGR <- extract(sagehen_stack_BGR5, nomiddle_buffer_300m_bridgewaterhole, fun = mean)

#400m donut mean
donut400m_bridgewaterhole_mean_AFG <- extract(sagehen_stack_AFG5, nomiddle_buffer_400m_bridgewaterhole, fun = mean)
donut400m_bridgewaterhole_mean_PFG <- extract(sagehen_stack_PFG5, nomiddle_buffer_400m_bridgewaterhole, fun = mean)
donut400m_bridgewaterhole_mean_BGR <- extract(sagehen_stack_BGR5, nomiddle_buffer_400m_bridgewaterhole, fun = mean)

#500m donut mean
donut500m_bridgewaterhole_mean_AFG <- extract(sagehen_stack_AFG5, nomiddle_buffer_500m_bridgewaterhole, fun = mean)
donut500m_bridgewaterhole_mean_PFG <- extract(sagehen_stack_PFG5, nomiddle_buffer_500m_bridgewaterhole, fun = mean)
donut500m_bridgewaterhole_mean_BGR <- extract(sagehen_stack_BGR5, nomiddle_buffer_500m_bridgewaterhole, fun = mean)

##Mapwell
#100m donut mean
donut100m_mapwell_mean_AFG <- extract(saddle_stack_AFG5, nomiddle_buffer_100m_mapwell, fun = mean)
donut100m_mapwell_mean_PFG <- extract(saddle_stack_PFG5, nomiddle_buffer_100m_mapwell, fun = mean)
donut100m_mapwell_mean_BGR <- extract(saddle_stack_BGR5, nomiddle_buffer_100m_mapwell, fun = mean)

#200m donut mean
donut200m_mapwell_mean_AFG <- extract(saddle_stack_AFG5, nomiddle_buffer_200m_mapwell, fun = mean)
donut200m_mapwell_mean_PFG <- extract(saddle_stack_PFG5, nomiddle_buffer_200m_mapwell, fun = mean)
donut200m_mapwell_mean_BGR <- extract(saddle_stack_BGR5, nomiddle_buffer_200m_mapwell, fun = mean)

#300m donut mean
donut300m_mapwell_mean_AFG <- extract(saddle_stack_AFG5, nomiddle_buffer_300m_mapwell, fun = mean)
donut300m_mapwell_mean_PFG <- extract(saddle_stack_PFG5, nomiddle_buffer_300m_mapwell, fun = mean)
donut300m_mapwell_mean_BGR <- extract(saddle_stack_BGR5, nomiddle_buffer_300m_mapwell, fun = mean)

#400m donut mean
donut400m_mapwell_mean_AFG <- extract(saddle_stack_AFG5, nomiddle_buffer_400m_mapwell, fun = mean)
donut400m_mapwell_mean_PFG <- extract(saddle_stack_PFG5, nomiddle_buffer_400m_mapwell, fun = mean)
donut400m_mapwell_mean_BGR <- extract(saddle_stack_BGR5, nomiddle_buffer_400m_mapwell, fun = mean)

#500m donut mean
donut500m_mapwell_mean_AFG <- extract(saddle_stack_AFG5, nomiddle_buffer_500m_mapwell, fun = mean)
donut500m_mapwell_mean_PFG <- extract(saddle_stack_PFG5, nomiddle_buffer_500m_mapwell, fun = mean)
donut500m_mapwell_mean_BGR <- extract(saddle_stack_BGR5, nomiddle_buffer_500m_mapwell, fun = mean)

