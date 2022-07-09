get_waypoints <- function(filename){
  
  SPP <- sf::st_read(here::here("Falmouth/gis/waypoints2.kml")) %>% 
    sf::st_zm() %>% 
    sf::st_as_sf()
  
  waypoints <- 
    dream::sfc_as_cols(SPP, names = c("lon","lat")) %>% 
    st_set_geometry(NULL) %>% 
    dplyr::select(lon, lat)
  
  coordinates(waypoints) <- ~lon + lat
  stashes <- waypoints %>% 
    as("sf") %>% 
    mutate(color = c("dwarf","elf","dwarf","dwarf","human",
              "human","dwarf","human","elf","human",
              "rivendell","dwarf","human","human",
              "human","dwarf","elf","dwarf","human","human")) %>% 
    filter(color != "rivendell")
  st_crs(stashes) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

  stashout <- stashes %>% 
    cbind(st_coordinates(.)) %>%
    arrange(X, Y) %>%
    select(-X, -Y) %>% 
    mutate(labels = 1:19)

  
  return(
    stashout
  )
}


