###Download NDVI time series for prac
library(MODISTools)
library(tidyverse)
library(readr)
library(sf)

# sites <- data.frame(site_name = c("grassy field", "invasion", "renosterveld", "sandstone_low", "sandstone_high", "limestone"),
#                     lat = c(-34.375052, -34.386014, -34.374259, -34.3961, -34.3748, -34.4309),
#                     lon = c(20.531749, 20.534986, 20.504233, 20.5494, 20.5428, 20.5666))

sites <- st_read("/home/jasper/GIT/BIO3018F/prac/Potberg_prac_sites.kml")

sites <- data.frame(site_name = sites$Name, lat = st_coordinates(sites)[,2], lon = st_coordinates(sites)[,1])

dat <- mt_batch_subset(df = sites,
                        product = "MOD13Q1",
                        band = "250m_16_days_NDVI",
                        internal = TRUE,
                        start = "2000-01-01",
                        end = "2022-01-30")

write_csv(dat, "MODISdat_batch_30Jan.csv")

#############  

# grass <- mt_subset(product = "MOD13Q1",
#                           lat = -34.375052,
#                           lon = 20.531749,
#                           band = "250m_16_days_NDVI",
#                           start = "2000-01-01",
#                           end = "2021-12-31",
#                           progress = FALSE,
#                           site_name = "grassy field")
# 
# invasion <- mt_subset(product = "MOD13Q1",
#                         lat = -34.386014,
#                         lon = 20.534986,
#                         band = "250m_16_days_NDVI",
#                         start = "2000-01-01",
#                         end = "2021-12-31",
#                         progress = FALSE,
#                         site_name = "invasion")
# 
# renosterveld <- mt_subset(product = "MOD13Q1",
#                        lat = -34.374259,
#                        lon = 20.504233,
#                        band = "250m_16_days_NDVI",
#                        start = "2000-01-01",
#                        end = "2021-12-31",
#                        progress = FALSE,
#                        site_name = "renosterveld")
# 
# sandstone_low <- mt_subset(product = "MOD13Q1",
#                        lat = -34.3983, 
#                        lon = 20.5552,
#                        band = "250m_16_days_NDVI",
#                        start = "2000-01-01",
#                        end = "2021-12-31",
#                        progress = FALSE,
#                        site_name = "sandstone_low")
# 
# sandstone_high <- mt_subset(product = "MOD13Q1",
#                            lat = -34.372797,
#                            lon = 20.548398,
#                            band = "250m_16_days_NDVI",
#                            start = "2000-01-01",
#                            end = "2021-12-31",
#                            progress = FALSE,
#                            site_name = "sandstone_high")
# 
# limestone <- mt_subset(product = "MOD13Q1",
#                       lat = -34.424473,
#                       lon = 20.580608,
#                       band = "250m_16_days_NDVI",
#                       start = "2000-01-01",
#                       end = "2021-12-31",
#                       progress = FALSE,
#                       site_name = "limestone")
# 
# dat <- bind_rows(grass, invasion, limestone, renosterveld, sandstone_high, sandstone_low)
# 
# write_csv(dat, "MODISdat.csv")