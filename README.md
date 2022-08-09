# Metadata: PlotGPS
# Authors: Chloe Miller- chloeamiller@hotmail.com, Madelon Case
# Overview: Cattle watering hole plot data from Rangeland Analysis Platform (RAP) and GPS data collected from around Northern Great Basin Experimental Range near Burns, Oregon. Used for a study of cattle watering holes and effects on annual and perennial forbs and grasses and bare ground. GPS data collected by Chloe Miller and Madelon Case in June 2022. RAP data collected by Madelon Case in June 2022.

# Methods: Collected GPS data using Garmin GPS, walking around watering holes and marking 4 corners around border of watering hole. For Raster RAP data… TO BE ADDED BY MADDY

# Contents of this folder:
# gpsdata folder – Contains GPX file uploaded from Garmin GPS
# 	GPS1_220630.GPX, GPX file (can be loaded on GPS device,        contains locations of all plots as well as additional          information from previous projects)
# RAP_exports_Chloe_Saddle folder – Contains Raster data for all vegetation cover types, located around Saddle Butte Highway (used for the watering hole Map Well). Each file corresponds to one year (1986-2021).
# RAP_exports_Chloe_Sagehen folder – Contains Raster data for all vegetation cover types, located around Sagehen area (used for all watering holes, excluding Map Well). Each file corresponds to one year (1986-2021).
# Note for all RAP Raster data: There are 5 layers of the data. Layer 1 is AFG (annual forbs and grasses). Layer 2 is BGR (bare ground). Layer 3 is PFG (perennial forbs and grasses). Layer 4 is SHR (shrubs). Layer 5 is TRE (trees). 
# plotgps.R – Code used/sourced for making maps and scatterplots with RAP data and GPS data, showing relationships/patterns (no actual maps/graphs, see plotgps_graphsmaps.R)
# plotgps_graphsmaps.R – Includes a master data frame (with all watering holes, all cover types, from 100m-1000m radii, graphs (scatterplots), and maps made from plotgps.R and master data frame. 
# PlotUsingLeaflet.R – Plots GPS plot points on a contextual map using Leaflet. This is separate from plotgps_graphsmaps.R because leaflet causes errors when using base plot or ggplot.
