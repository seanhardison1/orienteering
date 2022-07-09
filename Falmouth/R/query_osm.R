query_osm <- function(key, value = NULL, bb, select = NULL){
  
  group_shapes <- opq(bbox = st_bbox(get(bb))) %>%
    add_osm_feature(key = key, value = value) %>%
    osmdata_sf()
  
  if (!is.null(group_shapes)) message("Query successful")
  
  if (!is.null(select)){
    out <- group_shapes[select]
  } else {
    out <- group_shapes
  }
  
  return(out)
}
