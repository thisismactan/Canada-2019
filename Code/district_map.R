source("Code/library.R")

## Create Lambert conformal conic CRS for leaflet (see http://spatialreference.org/ref/esri/canada-lambert-conformal-conic/ )
crs_proj <- "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=63.390675 +lon_0=-91.86666666666666 +x_0=6200000 +y_0=3000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs "
crs_lcc <- leafletCRS(code = "ESRI:102002", proj4def = crs_proj)

## Read shapefile
canada_districts <- readOGR(dsn = "Data/Shapefiles", layer = "FED_CA_2_2_ENG") %>%
  ms_simplify()

## Transform to lat-long
canada_districts_latlong <- spTransform(canada_districts, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")) %>%
  st_as_sf() %>%
  merge(district_key_2013, by.x = "FED_NUM", by.y = "district_code", all = FALSE)

## Leaflet map
leaflet(canada_districts_latlong) %>%
  addPolygons(weight = 1, color = "#666666", opacity = 1, label = ~name_english)
