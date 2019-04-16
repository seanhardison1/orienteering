get_lines <- function(filename, cast = "LINESTRING") {
  
  if (cast == 'LINESTRING'){
    SPP <- maptools::getKMLcoordinates(textConnection(system(paste("unzip -p gis/",filename,".kmz",sep = ""), intern = TRUE)))
  } else {
    SPP <- maptools::getKMLcoordinates(paste("gis/",filename,".kml",sep = ""))
  }
    SPP <- data.frame(lat = SPP [[1]][,1],
                    lon = SPP [[1]][,2],
                    id = 1)
  coordinates(SPP) <- ~lat + lon
  
  out <- SPP %>% as("sf") %>% 
      dplyr::group_by(id) %>%
    dplyr::summarise(do_union=FALSE) %>%
    sf::st_cast(cast)
  
  st_crs(out) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  
  return(out)
} 


