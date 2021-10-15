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
  mutate(grp = ifelse(Longitude > -78.746 & Longitude < -78.74038, 1,
                             ifelse(Longitude > -78.74038 & Longitude < -78.73475,2,
                                    ifelse(Longitude > -78.73475 & Longitude < -78.72912,3,
                                           ifelse(Longitude > -78.72912,4,NA)))))

# convert to sf
croz_sf <- as(croz_con, "sf") %>% 
  mutate(elev = as.numeric(level)) %>% 
  dplyr::select(-level)

# save
save(croz_sf, croz_df,
     file = file.path(data.dir,"crozet_contour.rdata"))
