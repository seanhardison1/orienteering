library(tidyverse)
library(osmdata)
library(raster)
library(sf)

#  bounding box----
bb<- st_bbox(c(xmin = -78.758, xmax = -78.721,
          ymin = 38.073, ymax = 38.108),
        crs = st_crs("+proj=longlat +datum=NAD83 +no_defs"))

# highways----
hw <- opq(bbox = bb) %>% 
  add_osm_feature(key  = "highway") %>% 
  osmdata_sf() %>% 
  .$osm_lines
osm_paths <- hw %>% 
  filter(highway %in% c("path","footway","track"))
tertiary <- hw %>% 
  filter(highway %in% c("tertiary"))
service <- hw %>% 
  filter(highway %in% c("service"))
unc <- hw %>% 
  filter(highway %in% c("unclassified"))
res <- hw %>% 
  filter(highway %in% c("residential"))

# natural features
(nf <- opq(bbox = bb) %>% 
    add_osm_feature(key = "natural") %>% 
    osmdata_sf())

lakes <- 
  nf$osm_polygons %>% 
  dplyr::select(name) %>%
  slice(1:3)

woods <- nf$osm_multipolygons

# water features
(wf <- opq(bbox = bb) %>% 
    add_osm_feature(key = "water") %>% 
    osmdata_sf())

# park boundary
(pb <- opq(bbox = bb) %>% 
    add_osm_feature(key = "boundary") %>% 
    osmdata_sf())

ms <- pb$osm_polygons %>% 
  slice(2)

# power line
(power <- opq(bbox = bb) %>% 
    add_osm_feature(key = "power") %>% 
    osmdata_sf())

pl <- power$osm_lines 

# save data----
save(osm_paths,
     ms,
     power,
     tertiary,
     service,
     pl,
     lakes,
     unc, res, file = here::here("Crozet/data/features.rdata"))
