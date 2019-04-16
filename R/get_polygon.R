#Returns a polygon

rot = function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)

get_polygon <- function(save = F,filename){
  #get region for clipping
  SPP <- maptools::getKMLcoordinates(textConnection(system(paste("unzip -p gis/",filename,".kmz",sep = ""), intern = TRUE)))
  
  SPP <- data.frame(lat =SPP[[1]][,1],
                     lon = SPP[[1]][,2])
  
  p = Polygon(SPP)
  ps = Polygons(list(p),1)
  SPP = SpatialPolygons(list(ps))
  
  SPP <- as(SPP, "sf") 
  
  #rotate 180 degrees
  SPP_geometry <- st_geometry(SPP)
  cntrd = st_centroid(SPP_geometry)
  SPP = (SPP_geometry - cntrd) * rot(pi*2) + cntrd
  SPP <- SPP %>% st_cast("POLYGON")
  st_crs(SPP) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

  
  if (save){
    save(SPP, file = "gis/SPP_poly.rdata")
  }
  return(SPP)
}
