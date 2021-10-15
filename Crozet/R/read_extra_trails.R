library(raster)
library(sf)
library(tidyverse)

extra_trails <- NULL
for (i in c("ir1.kml","ir2.kml","ir3.kml","ir4.kml")){
  ir <- st_read(paste0(here::here("Crozet/data",i))) %>% 
    summarise(do_union = F) %>% 
    st_cast("LINESTRING")
  
  assign('extra_trails', bind_rows(extra_trails, ir))
}

# plot(extra_trails)


extras <- NULL
fl <- list.files(here::here("Crozet/data/extra"))
for (i in fl){
  ir <- st_read(paste0(here::here("Crozet/data/extra",i))) %>% 
    st_zm() %>% 
    st_as_sf() %>% 
    mutate(name = str_remove(str_remove(i, "\\.kml"), "_\\d{1,2}"))
  
  assign('extras', bind_rows(extras, ir))
}
save(extra_trails,extras, file = here::here("extras.rdata"))

