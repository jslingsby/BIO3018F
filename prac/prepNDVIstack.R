library(terra)
library(sf)
library(tidyverse)

# Get functions
source("prac/03_negexp_model.R")

# Get image dates
dates <- read.csv("prac/MODIS/dates.csv", header = F)
dates <- dates[seq(2,nrow(dates),2),]
dates <- substr(dates, 25, 34)

# Get raster data
ndvi <- rast("prac/MODIS/20220130_MODIS_v1g_POT_MOD13Q1_006_NDVI.tif")
names(ndvi) <- dates
ndvi <- ndvi*.0001

# Get veg and fire and map
veg <- st_read("/home/jasper/Documents/Datasets/SANBI/RLE_2021_Remnants/FynRemnantsClean.gpkg")
ndvi_g <- project(ndvi, st_crs(veg)$Wkt)
veg <- st_crop(veg, ndvi_g)

fire <- st_read("/home/jasper/Documents/Datasets/Fire/All_Fires_20_21_gw/All_Fires_20_21_gw.shp")
fire <- st_crop(fire, ndvi_g)
mapview::mapview(list(veg,fire), layer.name = c("name",))
mapview::mapview(veg, zcol = "name")

# Get sites and reproject
sites <- data.frame(site_name = c("grassy field", "invasion", "renosterveld", "sandstone_low", "sandstone_high", "limestone"),
                    lat = c(-34.375052, -34.386014, -34.374259, -34.3961, -34.372797, -34.424473),
                    lon = c(20.531749, 20.534986, 20.504233, 20.5494, 20.548398, 20.580608))

sites <- vect(sites, geom = c("lon", "lat"), crs = st_crs(4326))
sites <- project(sites, ndvi)

# Plot sites
plot(ndvi[[504]])
plot(sites, add = T)

# Extract timeseries
dat <- t(terra::extract(ndvi, sites))
rownames(dat)

caldate <- as.Date(dates, format = "%Y_%m_%d")
age <- (as.numeric(caldate) - min(as.numeric(caldate), na.rm = T))/365.25

###

# Get sites and reproject
pt <- data.frame(site_name = c("grassy field", "invasion", "renosterveld", "sandstone_low", "sandstone_high", "limestone"),
                    lat = c(-34.375052, -34.386014, -34.374259, -34.3961, -34.372797, -34.424473),
                    lon = c(20.531749, 20.534986, 20.504233, 20.5494, 20.548398, 20.580608))

pt <- st_as_sf(pt, coords = c("lon", "lat"), crs = st_crs(4326))


library(leaflet)
library(leaflet.extras)
library(htmltools)
leaflet() %>%
  enableTileCaching() %>%
  #addTiles(options = tileOptions(useCache = TRUE, crossOrigin = TRUE))
  # Add default OpenStreetMap map tiles
  addProviderTiles("Esri.WorldImagery", options = tileOptions(useCache = TRUE, crossOrigin = TRUE)) %>%  
  # Add our points
  addCircleMarkers(data = pt,
                   radius = 3, 
                   color = "blue") 
