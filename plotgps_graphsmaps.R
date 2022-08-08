#PLOT GPS GRAPHS AND MAPS --------------------------------------------
#
# Authors: Chloe Miller and Madelon Case
# Acknowledgements: Thanks to Lauren Hallett and the Hallett lab and also R Data Guy for template
# Email: chloeamiller@hotmail.com 
# 
# Date: 03 August 2022
#
# Script Name: Plot GPS Graphs and Maps
# 
# Script Description: Includes a master data frame (with all watering holes, all
#                     cover types, from 100m-1000m), graphs (scatterplots), and
#                     maps made from Plot GPS code and master data frame.
#
#


# SET WORKING DIRECTORY -----------------------------
cat("SETTING WORKING DIRECTORY...\n\n", sep = "")
wd <- "~/PlotGPS"
setwd(wd)
cat("WORKING DIRECTORY HAS BEEN SET TO: ", wd, sep = "")


# INSTALL PACKAGES & LOAD LIBRARIES -----------------
cat("INSTALLING PACKAGES & LOADING LIBRARIES... \n\n", sep = "")
packages <- c("raster", "terra", "base", "plotKML", "sp", "ggplot2", "RColorBrewer")
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

#Source all info from plotgpsfinal (allowing us to work with code for plotting)
source("plotgps.R")

###This section creates the master dataframe (all watering holes, all covertypes, 100m-1000m)
##Dataframe for Pinespring 
#AFG dataframe
pinespring_df_AFG <- rbind(donut100m_pinespring_coords_AFG, donut200m_pinespring_coords_AFG,donut300m_pinespring_coords_AFG, donut400m_pinespring_coords_AFG, donut500m_pinespring_coords_AFG, donut600m_pinespring_coords_AFG, donut700m_pinespring_coords_AFG, donut800m_pinespring_coords_AFG, donut900m_pinespring_coords_AFG, donut1000m_pinespring_coords_AFG)
pinespring_df_AFG <- cbind(pinespring_df_AFG, covertype = "AFG")
#PFG dataframe
pinespring_df_PFG <- rbind(donut100m_pinespring_coords_PFG, donut200m_pinespring_coords_PFG, donut300m_pinespring_coords_PFG, donut400m_pinespring_coords_PFG, donut500m_pinespring_coords_PFG, donut600m_pinespring_coords_PFG, donut700m_pinespring_coords_PFG, donut800m_pinespring_coords_PFG, donut900m_pinespring_coords_PFG, donut1000m_pinespring_coords_PFG)
pinespring_df_PFG <- cbind(pinespring_df_PFG, covertype = "PFG")
#BGR dataframe
pinespring_df_BGR <- rbind(donut100m_pinespring_coords_BGR,donut200m_pinespring_coords_BGR, donut300m_pinespring_coords_BGR, donut400m_pinespring_coords_BGR, donut500m_pinespring_coords_BGR, donut600m_pinespring_coords_BGR, donut700m_pinespring_coords_BGR, donut800m_pinespring_coords_BGR, donut900m_pinespring_coords_BGR, donut1000m_pinespring_coords_BGR)
pinespring_df_BGR <- cbind(pinespring_df_BGR, covertype = "BGR")
#all covertypes included
pinespring_df <- rbind(pinespring_df_AFG, pinespring_df_PFG, pinespring_df_BGR)
pinespring_df <- cbind(pinespring_df, location = "Pinespring")


##Dataframe for Powerline 
#AFG dataframe
powerline_df_AFG <- rbind(donut100m_powerline_coords_AFG, donut200m_powerline_coords_AFG,donut300m_powerline_coords_AFG, donut400m_powerline_coords_AFG, donut500m_powerline_coords_AFG, donut600m_powerline_coords_AFG, donut700m_powerline_coords_AFG, donut800m_powerline_coords_AFG, donut900m_powerline_coords_AFG, donut1000m_powerline_coords_AFG)
powerline_df_AFG <- cbind(powerline_df_AFG, covertype = "AFG")
#PFG dataframe
powerline_df_PFG <- rbind(donut100m_powerline_coords_PFG, donut200m_powerline_coords_PFG,donut300m_powerline_coords_PFG, donut400m_powerline_coords_PFG, donut500m_powerline_coords_PFG, donut600m_powerline_coords_PFG, donut700m_powerline_coords_PFG, donut800m_powerline_coords_PFG, donut900m_powerline_coords_PFG, donut1000m_powerline_coords_PFG)
powerline_df_PFG <- cbind(powerline_df_PFG, covertype = "PFG")
#BGR dataframe 
powerline_df_BGR <- rbind(donut100m_powerline_coords_BGR, donut200m_powerline_coords_BGR,donut300m_powerline_coords_BGR, donut400m_powerline_coords_BGR, donut500m_powerline_coords_BGR, donut600m_powerline_coords_BGR, donut700m_powerline_coords_BGR, donut800m_powerline_coords_BGR, donut900m_powerline_coords_BGR, donut1000m_powerline_coords_BGR)
powerline_df_BGR <- cbind(powerline_df_BGR, covertype = "BGR")
#All covertypes
powerline_df <- rbind(powerline_df_AFG, powerline_df_PFG, powerline_df_BGR)
powerline_df <- cbind(powerline_df, location = "Powerline")


##Dataframe for Upperfay 
#AFG
upperfay_df_AFG <- rbind(donut100m_upperfay_coords_AFG, donut200m_upperfay_coords_AFG,donut300m_upperfay_coords_AFG, donut400m_upperfay_coords_AFG, donut500m_upperfay_coords_AFG, donut600m_upperfay_coords_AFG, donut700m_upperfay_coords_AFG, donut800m_upperfay_coords_AFG, donut900m_upperfay_coords_AFG, donut1000m_upperfay_coords_AFG)
upperfay_df_AFG <- cbind(upperfay_df_AFG, covertype = "AFG")
#PFG
upperfay_df_PFG <- rbind(donut100m_upperfay_coords_PFG, donut200m_upperfay_coords_PFG,donut300m_upperfay_coords_PFG, donut400m_upperfay_coords_PFG, donut500m_upperfay_coords_PFG, donut600m_upperfay_coords_PFG, donut700m_upperfay_coords_PFG, donut800m_upperfay_coords_PFG, donut900m_upperfay_coords_PFG, donut1000m_upperfay_coords_PFG)
upperfay_df_PFG <- cbind(upperfay_df_PFG, covertype = "PFG")
#BGR
upperfay_df_BGR <- rbind(donut100m_upperfay_coords_BGR, donut200m_upperfay_coords_BGR,donut300m_upperfay_coords_BGR, donut400m_upperfay_coords_BGR, donut500m_upperfay_coords_BGR, donut600m_upperfay_coords_BGR, donut700m_upperfay_coords_BGR, donut800m_upperfay_coords_BGR, donut900m_upperfay_coords_BGR, donut1000m_upperfay_coords_BGR)
upperfay_df_BGR <- cbind(upperfay_df_BGR, covertype = "BGR")
#All covertypes
upperfay_df <- rbind(upperfay_df_AFG, upperfay_df_PFG, upperfay_df_BGR)
upperfay_df <- cbind(upperfay_df, location = "Upperfay")


##Dataframe for Bridgewaterhole 
#AFG
bridgewaterhole_df_AFG <- rbind(donut100m_bridgewaterhole_coords_AFG, donut200m_bridgewaterhole_coords_AFG,donut300m_bridgewaterhole_coords_AFG, donut400m_bridgewaterhole_coords_AFG, donut500m_bridgewaterhole_coords_AFG, donut600m_bridgewaterhole_coords_AFG, donut700m_bridgewaterhole_coords_AFG, donut800m_bridgewaterhole_coords_AFG, donut900m_bridgewaterhole_coords_AFG, donut1000m_bridgewaterhole_coords_AFG)
bridgewaterhole_df_AFG <- cbind(bridgewaterhole_df_AFG, covertype = "AFG")
#PFG
bridgewaterhole_df_PFG <- rbind(donut100m_bridgewaterhole_coords_PFG, donut200m_bridgewaterhole_coords_PFG,donut300m_bridgewaterhole_coords_PFG, donut400m_bridgewaterhole_coords_PFG, donut500m_bridgewaterhole_coords_PFG, donut600m_bridgewaterhole_coords_PFG, donut700m_bridgewaterhole_coords_PFG, donut800m_bridgewaterhole_coords_PFG, donut900m_bridgewaterhole_coords_PFG, donut1000m_bridgewaterhole_coords_PFG)
bridgewaterhole_df_PFG <- cbind(bridgewaterhole_df_PFG, covertype = "PFG")
#BGR
bridgewaterhole_df_BGR <- rbind(donut100m_bridgewaterhole_coords_BGR, donut200m_bridgewaterhole_coords_BGR,donut300m_bridgewaterhole_coords_BGR, donut400m_bridgewaterhole_coords_BGR, donut500m_bridgewaterhole_coords_BGR, donut600m_bridgewaterhole_coords_BGR, donut700m_bridgewaterhole_coords_BGR, donut800m_bridgewaterhole_coords_BGR, donut900m_bridgewaterhole_coords_BGR, donut1000m_bridgewaterhole_coords_BGR)
bridgewaterhole_df_BGR <- cbind(bridgewaterhole_df_BGR, covertype = "BGR")
#all cover types
bridgewaterhole_df <- rbind(bridgewaterhole_df_AFG, bridgewaterhole_df_PFG, bridgewaterhole_df_BGR)
bridgewaterhole_df <- cbind(bridgewaterhole_df, location = "Bridgewaterhole")


##Dataframe for Sagehenhill
#AFG
sagehenhill_df_AFG <- rbind(donut100m_sagehenhill_coords_AFG, donut200m_sagehenhill_coords_AFG, donut300m_sagehenhill_coords_AFG, donut400m_sagehenhill_coords_AFG, donut500m_sagehenhill_coords_AFG, donut600m_sagehenhill_coords_AFG, donut700m_sagehenhill_coords_AFG, donut800m_sagehenhill_coords_AFG, donut900m_sagehenhill_coords_AFG, donut1000m_sagehenhill_coords_AFG)
sagehenhill_df_AFG <- cbind(sagehenhill_df_AFG, covertype = "AFG")
#PFG
sagehenhill_df_PFG <- rbind(donut100m_sagehenhill_coords_PFG, donut200m_sagehenhill_coords_PFG, donut300m_sagehenhill_coords_PFG, donut400m_sagehenhill_coords_PFG, donut500m_sagehenhill_coords_PFG, donut600m_sagehenhill_coords_PFG, donut700m_sagehenhill_coords_PFG, donut800m_sagehenhill_coords_PFG, donut900m_sagehenhill_coords_PFG, donut1000m_sagehenhill_coords_PFG)
sagehenhill_df_PFG <- cbind(sagehenhill_df_PFG, covertype = "PFG")
#BGR
sagehenhill_df_BGR <- rbind(donut100m_sagehenhill_coords_BGR, donut200m_sagehenhill_coords_BGR, donut300m_sagehenhill_coords_BGR, donut400m_sagehenhill_coords_BGR, donut500m_sagehenhill_coords_BGR, donut600m_sagehenhill_coords_BGR, donut700m_sagehenhill_coords_BGR, donut800m_sagehenhill_coords_BGR, donut900m_sagehenhill_coords_BGR, donut1000m_sagehenhill_coords_BGR)
sagehenhill_df_BGR <- cbind(sagehenhill_df_BGR, covertype = "BGR")
#all cover types
sagehenhill_df <- rbind(sagehenhill_df_AFG, sagehenhill_df_PFG, sagehenhill_df_BGR)
sagehenhill_df <- cbind(sagehenhill_df, location = "SageHen Hill Reservior")


##Dataframe for Mapwell
#AFG
mapwell_df_AFG <- rbind(donut100m_mapwell_coords_AFG, donut200m_mapwell_coords_AFG, donut300m_mapwell_coords_AFG, donut400m_mapwell_coords_AFG, donut500m_mapwell_coords_AFG, donut600m_mapwell_coords_AFG, donut700m_mapwell_coords_AFG, donut800m_mapwell_coords_AFG, donut900m_mapwell_coords_AFG, donut1000m_mapwell_coords_AFG)
mapwell_df_AFG <- cbind(mapwell_df_AFG, covertype = "AFG")
#PFG
mapwell_df_PFG <- rbind(donut100m_mapwell_coords_PFG, donut200m_mapwell_coords_PFG, donut300m_mapwell_coords_PFG, donut400m_mapwell_coords_PFG, donut500m_mapwell_coords_PFG, donut600m_mapwell_coords_PFG, donut700m_mapwell_coords_PFG, donut800m_mapwell_coords_PFG, donut900m_mapwell_coords_PFG, donut1000m_mapwell_coords_PFG)
mapwell_df_PFG <- cbind(mapwell_df_PFG, covertype = "PFG")
#BGR
mapwell_df_BGR <- rbind(donut100m_mapwell_coords_BGR, donut200m_mapwell_coords_BGR, donut300m_mapwell_coords_BGR, donut400m_mapwell_coords_BGR, donut500m_mapwell_coords_BGR, donut600m_mapwell_coords_BGR, donut700m_mapwell_coords_BGR, donut800m_mapwell_coords_BGR, donut900m_mapwell_coords_BGR, donut1000m_mapwell_coords_BGR)
mapwell_df_BGR <- cbind(mapwell_df_BGR, covertype = "BGR")
#all cover types
mapwell_df <- rbind(mapwell_df_AFG, mapwell_df_PFG, mapwell_df_BGR)
mapwell_df <- cbind(mapwell_df, location = "Mapwell")

#Master dataframe
master_df <- rbind(pinespring_df, powerline_df, bridgewaterhole_df, upperfay_df, sagehenhill_df, mapwell_df)
colnames(master_df) <- c("distance", "percent_cover", "covertype", "location")

### This section makes scatter plots with master dataframe and plotgps 
#plots distance v. percent cover, colored by location, organized by covertype 
ggplot(data = master_df, aes(x=distance, y=percent_cover, col =location))+
  geom_jitter()+ #spaces out individual points on scatter plot
  facet_wrap(~covertype, scales = "free")+ #organizes by cover type, allows scales to adjust depending on data
  stat_summary(geom = "line", fun = mean, lwd = 1.5, col = "black")+ #plots averages for each ring (100m-1000m)
  labs(title ="Distance Vs. Percent Cover", subtitle = "Sorted by covertype", ) #titles for graph


#Plot AFG data for all waterholes, colored by location, organized by location
ggplot(data = subset(master_df,master_df$covertype %in% "AFG"), aes(x=distance, y = percent_cover, col = location))+
  scale_x_continuous(n.breaks = 10)+
  geom_jitter()+
  facet_wrap(~location, scales = "free")+
  stat_summary(geom = "line", fun = mean, col = "black", lwd = 1.5)+
  ggtitle("Distance Vs. Percent Cover for AFG")

#Plot PFG data for all waterholes, colored by location, organized by location
ggplot(data = subset(master_df,master_df$covertype %in% "PFG"), aes(x=distance, y = percent_cover, col = location))+
  scale_x_continuous(n.breaks = 10)+
  geom_jitter()+
  facet_wrap(~location, scales = "free")+
  stat_summary(geom = "line", fun = mean, col = "black", lwd = 1.5)+
  ggtitle("Distance Vs. Percent Cover for PFG")

#Plot BGR data for all waterholes, colored by location, organized by location
ggplot(data = subset(master_df,master_df$covertype %in% "BGR"), aes(x=distance, y = percent_cover, col = location))+
  scale_x_continuous(n.breaks = 10)+
  geom_jitter()+
  facet_wrap(~location, scales = "free")+
  stat_summary(geom = "line", fun = mean, col = "black", lwd = 1.5)+
  ggtitle("Distance Vs. Percent Cover for BGR")

#Plot AFG data for all waterholes, colored by location
ggplot(data = subset(master_df,master_df$covertype %in% "AFG"), aes(x=distance, y = percent_cover, col = location))+
  scale_x_continuous(n.breaks = 10)+
  geom_jitter()+
  stat_summary(geom = "line", fun = mean, col = "black", lwd = 1.5)+
  ggtitle("Distance Vs. Percent Cover for AFG")

#Plot PFG data for all waterholes, colored by location
ggplot(data = subset(master_df,master_df$covertype %in% "PFG"), aes(x=distance, y = percent_cover, col = location))+
  scale_x_continuous(n.breaks = 10)+
  geom_jitter()+
  stat_summary(geom = "line", fun = mean, col = "black", lwd = 1.5)+
  ggtitle("Distance Vs. Percent Cover for PFG")

#Plot BGR data for all waterhole, colored by location
ggplot(data = subset(master_df, master_df$covertype %in% "BGR"), aes(x=distance, y = percent_cover, col = location))+
  scale_x_continuous(n.breaks = 10)+
  geom_jitter()+
  stat_summary(geom = "line", fun = mean, col = "black", lwd = 1.5)+
  ggtitle("Distance Vs. Percent Cover for BGR")

#Color settings for graphs
cbbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#Plots all powerline data
ggplot(data = subset(master_df, master_df$location %in% "Powerline"), aes(x = distance, y = percent_cover, col = covertype))+
  scale_x_continuous(n.breaks = 10)+
  geom_jitter()+
  facet_wrap(~covertype, scales = "free")+
  stat_summary(geom = "line", fun = mean, col = "black" , lwd = 1.5)+
  ggtitle("Distance Vs. Percent Cover for Powerline")+
  xlab("Distance (in meters)")+ 
  ylab("Percent of Cover")+
  labs(color = "Cover Type")+
  scale_colour_manual(values=cbbPalette)

#Plot pinespring data
ggplot(data = subset(master_df, master_df$location %in% "Pinespring"), aes(x = distance, y = percent_cover, col = covertype), xlim = c(100,1000))+
  scale_x_continuous(n.breaks = 10)+
  geom_jitter()+
  facet_wrap(~covertype, scales = "free")+
  stat_summary(geom = "line", fun = mean, col = "black" , lwd = 1.5)+
  ggtitle("Distance Vs. Percent Cover for Pinespring")+
  xlab("Distance (in meters)")+ 
  ylab("Percent of Cover")+
  labs(color = "Cover Type")+
  scale_colour_manual(values=cbbPalette)

#Plot sagehenhill data
ggplot(data = subset(master_df, master_df$location %in% "SageHen Hill Reservior"), aes(x = distance, y = percent_cover, col = covertype), xlim = c(100,1000))+
  scale_x_continuous(n.breaks = 10)+
  geom_jitter()+
  facet_wrap(~covertype, scales = "free")+
  stat_summary(geom = "line", fun = mean, col = "black" , lwd = 1.5)+
  ggtitle("Distance Vs. Percent Cover for SageHen Hill Reservior")+
  xlab("Distance (in meters)")+ 
  ylab("Percent of Cover")+
  labs(color = "Cover Type")+
  scale_colour_manual(values=cbbPalette)

#Plot bridewaterhole data
ggplot(data = subset(master_df, master_df$location %in% "Bridgewaterhole"), aes(x = distance, y = percent_cover, col = covertype), xlim = c(100,1000))+
  scale_x_continuous(n.breaks = 10)+
  geom_jitter()+
  facet_wrap(~covertype, scales = "free", ncol = 1)+
  stat_summary(geom = "line", fun = mean, col = "black" , lwd = 1.5)+
  ggtitle("Distance Vs. Percent Cover for Bridgewaterhole")+
  xlab("Distance (in meters)")+ 
  ylab("Percent of Cover")+
  labs(color = "Cover Type")+
  scale_colour_manual(values=cbbPalette)

#Plot upperfay data
ggplot(data = subset(master_df, master_df$location %in% "Upperfay"), aes(x = distance, y = percent_cover, col = covertype))+
  scale_x_continuous(n.breaks = 10)+
  geom_jitter()+
  facet_wrap(~covertype, scales = "free")+
  stat_summary(geom = "line", fun = mean, col = "black" , lwd = 1.5)+
  ggtitle("Distance Vs. Percent Cover for Upperfay")+
  xlab("Distance (in meters)")+ 
  ylab("Percent of Cover")+
  labs(color = "Cover Type")+
  scale_colour_manual(values=cbbPalette)

#Plot mapwell data
ggplot(data = subset(master_df, master_df$location %in% "Mapwell"), aes(x = distance, y = percent_cover, col = covertype))+
  scale_x_continuous(n.breaks = 10)+
  geom_jitter()+
  facet_wrap(~covertype, scales = "free")+
  stat_summary(geom = "line", fun = mean, col = "black" , lwd = 1.5)+
  ggtitle("Distance Vs. Percent Cover for Mapwell")+
  xlab("Distance (in meters)")+ 
  ylab("Percent of Cover")+
  labs(color = "Cover Type")+
  scale_colour_manual(values=cbbPalette)

#### This section includes maps with plotgps code (used for poster)
#color settings
AFGcolorpalette <- brewer.pal(8, "OrRd")
PFGcolorpalette <- brewer.pal(8, "YlGn")
BGRcolorpalette <- brewer.pal(8, "PuBu")

#Bridgewaterhole with 100m-500m donuts (AFG)
plot(sagehen_stack_AFG5, xlim =c(-119.349,-119.332), ylim = c(43.537,43.55),
     main = "Percent Cover of AFG (Bridgewaterhole)", xlab = "Longitude", ylab = "Latitude", col = AFGcolorpalette)
plot(nomiddle_buffer_100m_bridgewaterhole, add = TRUE) #plots donut of 100m around bridgewaterhole
plot(nomiddle_buffer_200m_bridgewaterhole, add = TRUE) #plots donut of 200m
plot(nomiddle_buffer_300m_bridgewaterhole, add = TRUE)
plot(nomiddle_buffer_400m_bridgewaterhole, add = TRUE)
plot(nomiddle_buffer_500m_bridgewaterhole, add = TRUE)

#Bridewaterhole with 100m,200m,and 1000m donuts (AFG)
plot(sagehen_stack_AFG5, xlim =c(-119.360,-119.321), ylim = c(43.533,43.5521), col = AFGcolorpalette,
     main = "Percent Cover of AFG (Bridgewaterhole)", xlab = "Longitude", ylab = "Latitude")
plot(nomiddle_buffer_100m_bridgewaterhole, add = TRUE) #plots donut of 100m
plot(nomiddle_buffer_200m_bridgewaterhole, add = TRUE) #plots donut of 200m
plot(nomiddle_buffer_1000m_bridgewaterhole, add = TRUE)#plots donut of 1000m

#Bridewaterhole PFG (100-500m donuts)
plot(sagehen_stack_PFG5, xlim =c(-119.349,-119.332), ylim = c(43.537,43.55), col =PFGcolorpalette,
     main = "Percent Cover of PFG (Bridgewaterhole)", xlab = "Longitude", ylab = "Latitude")
plot(nomiddle_buffer_100m_bridgewaterhole, add = TRUE)
plot(nomiddle_buffer_200m_bridgewaterhole, add = TRUE)
plot(nomiddle_buffer_300m_bridgewaterhole, add = TRUE)
plot(nomiddle_buffer_400m_bridgewaterhole, add = TRUE)
plot(nomiddle_buffer_500m_bridgewaterhole, add = TRUE)

#Bridgewaterhole PFG cover 100-200m donut, 1000m donut
plot(sagehen_stack_PFG5, xlim =c(-119.360,-119.321), ylim = c(43.533,43.5521), col = PFGcolorpalette,
     main = "Percent Cover of PFG (Bridgewaterhole)", xlab = "Longitude", ylab = "Latitude")
plot(nomiddle_buffer_100m_bridgewaterhole, add = TRUE)
plot(nomiddle_buffer_200m_bridgewaterhole, add = TRUE)
plot(nomiddle_buffer_1000m_bridgewaterhole, add = TRUE)

#Bridgewaterhole BGR (100m-500m donuts)
plot(sagehen_stack_BGR5, xlim =c(-119.349,-119.332), ylim = c(43.537,43.55), col = BGRcolorpalette,
     main = "Percent Cover of BGR (Bridgewaterhole)", xlab = "Longitude", ylab = "Latitude")
plot(nomiddle_buffer_100m_bridgewaterhole, add = TRUE)
plot(nomiddle_buffer_200m_bridgewaterhole, add = TRUE)
plot(nomiddle_buffer_300m_bridgewaterhole, add = TRUE)
plot(nomiddle_buffer_400m_bridgewaterhole, add = TRUE)
plot(nomiddle_buffer_500m_bridgewaterhole, add = TRUE)

#Bridgewaterhole BGR, 100m-200m donut, 1000m donut
plot(sagehen_stack_BGR5, xlim =c(-119.360,-119.321), ylim = c(43.533,43.5521), col = BGRcolorpalette,
     main = "Percent Cover of BGR (Bridgewaterhole)", xlab = "Longitude", ylab = "Latitude")
plot(nomiddle_buffer_100m_bridgewaterhole, add = TRUE)
plot(nomiddle_buffer_200m_bridgewaterhole, add = TRUE)
plot(nomiddle_buffer_1000m_bridgewaterhole, add = TRUE)

