get_waypoints <- function(filename){
  
  SPP <- maptools::getKMLcoordinates(paste("gis/",filename,".kml",sep = ""))
  
  lat <- NULL
  lon <- NULL
  for (i in 1:length(SPP)){
    lon[i] <- SPP[1:length(SPP)][[i]][,1]
    lat[i] <- SPP[1:length(SPP)][[i]][,2]
  }
  waypoints <- data.frame(lon = lon,
                          lat = lat)
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
get_waypoints(filename= "waypoints2")



