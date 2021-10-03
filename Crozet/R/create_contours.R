library(tidyverse)
library(raster)
library(terra)
library(sf)

# read DEM and crop
croz <- raster(file.path(data.dir,"dem1.tif"))
croz <- crop(croz,extent(c(-78.758,-78.721,38.073, 38.108)))

# convert to contour
croz_con <- rasterToContour(croz,
                            nlevels = 100)

croz_df <- 
  rasterToPoints(croz) %>% 
  as.data.frame() %>% 
  dplyr::rename(Longitude = x,
                Latitude = y,
                elev = dem1) %>% 
  mutate(grp = ifelse(Longitude < -78.75, 1,
                      ifelse(Longitude > -78.75 & Longitude < -78.74,2,
                             ifelse(Longitude > -78.74 & Longitude < -78.73,3,
                                    ifelse(Longitude > -78.73 & Longitude < -78.72,4,NA)))))

# convert to sf
croz_sf <- as(croz_con, "sf") %>% 
  mutate(elev = as.numeric(level)) %>% 
  dplyr::select(-level)

# save
save(croz_sf, croz_df,
     file = file.path(data.dir,"crozet_contour.rdata"))
