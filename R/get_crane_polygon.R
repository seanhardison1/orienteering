#Returns a polygon of the FCWA 

rot = function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)

get_crane_polygon <- function(save = F){
  #get region for clipping
  FCWA <- maptools::getKMLcoordinates(textConnection(system("unzip -p gis/FCWA.kmz", intern = TRUE)))
  FCWA <- data.frame(lat =FCWA[[1]][,1],
                     lon = FCWA[[1]][,2])
  
  p = Polygon(FCWA)
  ps = Polygons(list(p),1)
  FCWA = SpatialPolygons(list(ps))
  
  FCWA <- as(FCWA, "sf") 
  
  #rotate 180 degrees
  fcwa_geometry <- st_geometry(FCWA)
  cntrd = st_centroid(fcwa_geometry)
  FCWA = (fcwa_geometry - cntrd) * rot(pi*2) + cntrd
  FCWA <- FCWA %>% st_cast("POLYGON")
  st_crs(FCWA) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

  
  if (save){
    save(FCWA, file = "gis/FCWA_poly.rdata")
  }
  return(FCWA)
}