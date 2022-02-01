library(terra)
library(sf)
library(tidyverse)

# Get functions
source("prac/02_plot.NDVI.R")
source("prac/03_negexp_model.R")

# Get image dates
dates <- read.csv("prac/MODIS/dates.csv", header = F)
dates <- dates[seq(2,nrow(dates),2),]
dates <- substr(dates, 25, 34)

# Get raster data
ndvi <- rast("prac/MODIS/20220130_MODIS_v1g_POT_MOD13Q1_006_NDVI.tif")
names(ndvi) <- dates
ndvi <- ndvi*.0001

# # Get veg map and fire and plot
# veg <- st_read("/home/jasper/Documents/Datasets/SANBI/RLE_2021_Remnants/FynRemnantsClean.gpkg")
# ndvi_g <- project(ndvi, st_crs(veg)$Wkt)
# veg <- st_crop(veg, ndvi_g)
# fire <- st_read("/home/jasper/Documents/Datasets/Fire/All_Fires_20_21_gw/CNCFires2021Clean.gpkg")
# fire <- st_crop(fire, ndvi_g)
# mapview::mapview(veg, zcol = "name") +
# mapview::mapview(fire, zcol = "year")

## Get sites and reproject
# sites <- data.frame(site_name = c("grassy field", "invasion", "renosterveld", "sandstone_low", "sandstone_high", "limestone"),
#                     lat = c(-34.375052, -34.386014, -34.374259, -34.3961, -34.372797, -34.424473),
#                     lon = c(20.531749, 20.534986, 20.504233, 20.5494, 20.548398, 20.580608))
# 
# sites <- vect(sites, geom = c("lon", "lat"), crs = st_crs(4326))

sites <- vect(st_read("/home/jasper/GIT/BIO3018F/prac/Potberg_prac_sites.kml"))
sites <- project(sites, ndvi)
#ndvi <- project(ndvi, crs(sites))


##############################################
###Prep and plot sites

# Plot sites
plot(ndvi[[504]])
plot(sites, add = T)

# Get sample locations
adat <- terra::extract(ndvi, sites, xy=T, cells = T)
#hmm[,506:508]
#geom(sites)
centroids <- xyFromCell(ndvi, cell = adat$cell)
NW <- centroids + data.frame(x = rep(-50,6), y = rep(50, 6))
SW <- centroids + data.frame(x = rep(-50,6), y = rep(-50, 6))
NE <- centroids + data.frame(x = rep(50,6), y = rep(50, 6))
SE <- centroids + data.frame(x = rep(50,6), y = rep(-50, 6))

sampsites <- bind_rows(NW, SW, NE, SE)
Team <- c(rep("NW", 6), rep("SW", 6), rep("NE", 6), rep("SE", 6))
Site <- rep(sites$Name,4)

sampsites$name <- paste0(Team, "_", Site)

pt <- st_as_sf(sampsites, coords = c("x", "y"), crs = crs(ndvi))
pt <- st_transform(pt, crs = st_crs(4326))

st_write(pt, "prac/sites.gpx", driver = "GPX")


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

#################################

# Extract and prep timeseries
adat <- as.data.frame(t(adat[,-c(1,506:508)]))
#adat <- as.data.frame(adat[-1,])
colnames(adat) <- sites$Name
adat$calendar_date <- as.Date(dates, format = "%Y_%m_%d")
adat <- adat %>% pivot_longer(cols = sites$Name, names_to = "site")
adat$age <- (as.numeric(adat$calendar_date) - min(as.numeric(adat$calendar_date), na.rm = T))/365.25 + 2922 #add 8 years...
adat$scale <- 1

###Plot all timeseries
adat %>%
  ggplot(aes(x = calendar_date, y = value*scale)) + 
  geom_line() +
  #  geom_point() +
  facet_wrap(.~ site) +
  ylab("NDVI") +
  ylim(0.2, 0.9)


###Run through sites fitting the model
# Get site names
sitnms <- unique(adat$site)

# Set initial parameters
par <- c(alpha = 0.2, gamma = 0.4, lambda = 0.5, A = 0.6, phi = 0)

# Make output table
out <- data.frame(initial = par)

# Set plotting window
par(mfrow=c(2,3)) 

# Loop through sites
for(i in 1:length(sitnms)) {
  
  # filter for focal site
  dat <- adat %>% filter(site == sitnms[i])
  
  # calculate age from date
  dat$age <- (as.numeric(dat$calendar_date) - min(as.numeric(dat$calendar_date), na.rm = T))/365.25
  
  # scale NDVI
  dat$NDVI <- dat$value*dat$scale
  
  # fit models
  fit_negexpMLES <- fit.negexpS.MLE(dat, par)
  
  # plot raw data
  plot.NDVI(dat, ylim = c(0.1, 0.9), main = sitnms[i])

  # add curve with MLE parameters
  lines(dat$age, pred.negexpS(fit_negexpMLES$par,dat$age), col = 'skyblue', lwd = 3)

  # bind to output
  out <- cbind(out, fit_negexpMLES$par)
}

names(out) <- c("initial", sitnms)
t(out)

# ####
# 
# # Get sites and reproject
# pt <- data.frame(site_name = c("grassy field", "invasion", "renosterveld", "sandstone_low", "sandstone_high", "limestone"),
#                     lat = c(-34.375052, -34.386014, -34.374259, -34.3961, -34.372797, -34.424473),
#                     lon = c(20.531749, 20.534986, 20.504233, 20.5494, 20.548398, 20.580608))
# 
# pt <- st_as_sf(pt, coords = c("lon", "lat"), crs = st_crs(4326))
# 
# 
# library(leaflet)
# library(leaflet.extras)
# library(htmltools)
# leaflet() %>%
#   enableTileCaching() %>%
#   #addTiles(options = tileOptions(useCache = TRUE, crossOrigin = TRUE))
#   # Add default OpenStreetMap map tiles
#   addProviderTiles("Esri.WorldImagery", options = tileOptions(useCache = TRUE, crossOrigin = TRUE)) %>%  
#   # Add our points
#   addCircleMarkers(data = pt,
#                    radius = 3, 
#                    color = "blue") 
