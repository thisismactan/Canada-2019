library(dplyr)
library(leaflet)
library(rgdal)
library(rmapshaper)
library(sf)
library(sp)

## Read shapefile
canada_districts <- readOGR(dsn = "Data/Shapefiles", layer = "FED_CA_2_2_ENG") %>%
  spTransform(CRS("+proj=lcc +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs "))
