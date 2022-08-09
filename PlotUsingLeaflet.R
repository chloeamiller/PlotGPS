#Plot Using Leaflet --------------------------------------------
#
# Authors: Chloe Miller and Madelon Case
# Acknowledgements: Thanks to Lauren Hallett and the Hallett lab and also R Data Guy for template
# Email: chloeamiller@hotmail.com
# 
# Date: 03 August 2022
#
# Script Name: Plot Using Leaflet
# 
# Script Description: Plots GPS plot points on a contextual map using leaflet.
#
#
# Notes: This is separate from plotgps_graphsmaps because leaflet causes errors when using base plot or ggplot.


# SET WORKING DIRECTORY -----------------------------
cat("SETTING WORKING DIRECTORY...\n\n", sep = "")
wd <- "~/PlotGPS"
setwd(wd)
cat("WORKING DIRECTORY HAS BEEN SET TO: ", wd, sep = "")


# INSTALL PACKAGES & LOAD LIBRARIES -----------------
cat("INSTALLING PACKAGES & LOADING LIBRARIES... \n\n", sep = "")
packages <- c("raster", "terra", "base", "plotKML", "sp", "leaflet")
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
#Plot gps leaflet - plots data in leaflet

#Source needed code
source("plotgpsfinal.R")

#Plots waypoints using field data (will allow to zoom in or out)
leaflet(allfielddata) %>% addCircles(col = "red", radius = 500) %>% addTiles()
