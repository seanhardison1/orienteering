query_osm <- function(key, value = NULL, crop = "fcwa", geo){
  
  if (is.null(value)){
    group_shapes <- opq(bbox = st_bbox(FCWA)) %>%
      add_osm_feature(key = key, value = NULL) %>%
      osmdata_sf()
  } else {
    group_shapes <- opq(bbox = st_bbox(FCWA)) %>%
      add_osm_feature(key = key, value = value) %>%
      osmdata_sf()
  }
  
  if (crop == "test"){
    group_lines <- group_shapes[[geo]] %>% 
      st_crop( c( xmin = -70.61, ymin = 41.64,ymax = 41.655, xmax = -70.59))
  } else if (crop == "fcwa"){
    group_lines <- group_shapes[[geo]] %>% 
      st_intersection(FCWA)
  }   
  
  return(group_lines)
  
}