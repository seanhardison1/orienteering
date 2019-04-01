
#bodies of water---------------------------------------------------------------------------------
osm_lakes_sf <- 
  opq(bbox = st_bbox(FCWA)) %>% 
  add_osm_feature(key = "water", value) %>% 
  osmdata_sf() 
osm_lakes_sf <- osm_lakes_sf$osm_polygons

#roads
osm_roads_primary.sf <- 
  opq(bbox = st_bbox(FCWA)) %>%
  add_osm_feature(key = 'highway', value = 'motorway') %>%
  osmdata_sf() 
osm_roads_primary <- 
  osm_roads_primary.sf$osm_lines %>% 
  st_intersection(FCWA)


osm_roads_secondary.sf <- 
  opq(bbox = st_bbox(FCWA)) %>%
  add_osm_feature(key = 'highway', value = 'secondary') %>%
  osmdata_sf() 
osm_roads_secondary <- 
  osm_roads_secondary.sf$osm_lines %>% 
  st_intersection(FCWA)


osm_roads_tertiary.sf <- 
  opq(bbox = st_bbox(FCWA)) %>%
  add_osm_feature(key = 'highway', value = 'tertiary') %>%
  osmdata_sf()
osm_roads_tertiary <- 
  osm_roads_tertiary.sf$osm_lines %>% 
  st_intersection(FCWA)

osm_roads_unclassified.sf <- 
  opq(bbox = st_bbox(FCWA)) %>%
  add_osm_feature(key = 'highway', value = 'unclassified') %>%
  osmdata_sf() 
osm_roads_unclassified <- 
  osm_roads_unclassified.sf$osm_lines %>% 
  st_intersection(FCWA)


osm_roads_residential.sf <- 
  opq(bbox = st_bbox(FCWA)) %>%
  add_osm_feature(key = 'highway', value = 'residential') %>%
  osmdata_sf() 

osm_roads_residential <- 
  osm_roads_residential.sf$osm_lines %>% 
  st_intersection(FCWA)

osm_roads_offramp.sf <- 
  opq(bbox = st_bbox(FCWA)) %>%
  add_osm_feature(key = 'highway', value = 'motorway_link') %>%
  osmdata_sf() 

osm_roads_offramp <- 
  osm_roads_offramp.sf$osm_lines %>% 
  st_intersection(FCWA)

osm_roads_service.sf <- 
  opq(bbox = st_bbox(FCWA)) %>%
  add_osm_feature(key = 'highway') %>%
  osmdata_sf() 

osm_roads_service <- 
  osm_roads_service.sf$osm_lines %>% 
  st_intersection(FCWA)

osm_paths <- 
  opq(bbox = st_bbox(FCWA)) %>%
  add_osm_feature(key = 'highway', value = "path") %>%
  osmdata_sf() 
  
osm_paths <- osm_paths$osm_lines %>% 
  st_intersection(FCWA)

#---------------------------------------------------------------------------------

query_osm <- function(key, value = NULL, crop = "test", geo){
  
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
    group_lines <- group_shapes[osm_lines] %>% 
      st_crop( c( xmin = -70.61, ymin = 41.64,ymax = 41.655, xmax = -70.59))
  } else if (crop == "fcwa"){
    load("gis/FCWA_poly.rdata")
    group_lines <- group_shapes$osm_lines %>% 
      st_intersection(FCWA)
  }   
  
  return(group_lines)
  
}

osm_all_roads <- query_osm_lines(key = "highway", crop = "test")
osm_paths <- osm_all_roads %>% 
  filter(highway %in% c("path","footway","track"))


ggplot() +
  geom_sf(data = test, color = "grey", size = 0.05) +
  geom_sf(data = osm_all_roads, size = 0.1, color = "black") +
  geom_sf(data = osm_paths, size = 0.1, color = "orange")
  # geom_sf(data = FCWA_sf) +  
  # geom_sf(data = osm_lakes_sf, fill = '#9ecae1', colour = NA) +
  # geom_sf(data = osm_roads_primary, colour = '#636363', size = 1) +
  # geom_sf(data = osm_roads_secondary, colour = '#636363', size = 1) +
  # geom_sf(data = osm_roads_tertiary, colour = '#636363', size = 1) +
  # geom_sf(data = osm_roads_unclassified, colour = '#636363', size = 1) +
  # geom_sf(data = osm_roads_residential, color = '#636363', size = 1) +
  # geom_sf(data = osm_roads_offramp, color = "#636363", size = 1) +
  # geom_sf(data = osm_roads_service, color = "#636363", size = 1) +
  # geom_sf(data = osm_roads_path, color = "orange", size = 0.25) +
  # # geom_sf(data = osm_roads_track, color = "#636363", size = 1) +
  # geom_sf(data = osm_rail_path, color = "brown", size = 1) +
  # scale_x_continuous(expand = c(0.001,0.001)) +
  # scale_y_continuous(expand = c(0.001,0.001)) +
  # theme(axis.text.x = element_text(angle = -rotate, vjust = 1.1, hjust = ifelse(rotate > 0,0,1)),
  #        axis.text.y = element_text(angle = -rotate)) + 
  coord_sf(xlim = c(xmin,xmax),ylim = c(ymin,ymax)) +
  theme_map() +
  theme(rect = element_rect(fill = "transparent") )

  
