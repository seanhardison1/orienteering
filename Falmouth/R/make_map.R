library(raster)
library(rgdal)
library(sf)
library(dplyr)
library(ggplot2)
library(ecodata)
library(plyr)
library(osmdata)
library(ggsn)
library(extrafont)

#set up------------------------------------------------------------------------
gis.dir <- here::here("Falmouth/gis")
r.dir <- here::here("Falmouth/R")
pdf.dir <- here::here("Falmouth/pdf")

#if process raw contour data
process_raw <- F

#get polygons
source(file.path(r.dir,"get_polygon.R"))

FCWA <- sf::st_read(paste0(here::here("Falmouth/gis/"),filename,".kml"))

mf1 <- sf::st_read(paste0(here::here("Falmouth/gis/mixedforest1.kml"))) %>% st_intersection(FCWA) 
mf2 <- sf::st_read(paste0(here::here("Falmouth/gis/mixedforest2.kml"))) %>% st_intersection(FCWA) 
mf3 <- sf::st_read(paste0(here::here("Falmouth/gis/mixedforest3.kml"))) %>% st_intersection(FCWA) 
mf4 <- sf::st_read(paste0(here::here("Falmouth/gis/mixedforest4.kml")))
mf5 <- sf::st_read(paste0(here::here("Falmouth/gis/mixedforest5.kml"))) %>% st_intersection(FCWA) 

p1 <- sf::st_read(paste0(here::here("Falmouth/gis/private1.kml"))) %>% st_intersection(FCWA) 
p2 <- sf::st_read(paste0(here::here("Falmouth/gis/private2.kml"))) %>% st_intersection(FCWA) 
p3 <- sf::st_read(paste0(here::here("Falmouth/gis/private3.kml"))) %>% st_intersection(FCWA) 

scrub1 <- sf::st_read(paste0(here::here("Falmouth/gis/scrubland.kml"))) %>% st_intersection(FCWA) 
# scrub2 <- get_polygon(filename = "scrubland2") %>% st_intersection(FCWA) 

elev_filt <- sf::st_read(paste0(here::here("Falmouth/gis/elevation_filter.kml"))) %>% st_intersection(FCWA) 
elev_filt2 <- sf::st_read(paste0(here::here("Falmouth/gis/elevation_filter2.kml"))) %>% st_intersection(FCWA) 

roadcut1 <- sf::st_read(paste0(here::here("Falmouth/gis/roadcut1.kml")))
roadcut2 <- sf::st_read(paste0(here::here("Falmouth/gis/roadcut2.kml")))

#get lines
fcwa_roads1 <- sf::st_read(paste0(here::here("Falmouth/gis/fcwa-roads1.kml")))
fcwa_roads2 <- sf::st_read(paste0(here::here("Falmouth/gis/fcwa-roads2.kml")))
fcwa_roads3 <- sf::st_read(paste0(here::here("Falmouth/gis/fcwa-roads3.kml")))

fcwa_unlisted1 <- sf::st_read(paste0(here::here("Falmouth/gis/fcwa_unlisted1.kml")))
fcwa_unlisted2 <- sf::st_read(paste0(here::here("Falmouth/gis/fcwa_unlisted2.kml")))
fcwa_unlisted3 <- sf::st_read(paste0(here::here("Falmouth/gis/fcwa_unlisted3.kml")))

#get waypoints
source(file.path(r.dir,"get_waypoints.R"))
waypoints <- get_waypoints()
rivendell <- get_waypoints()

#Process base map--------------------------------------------------------------
if (process_raw){
  
  #Full topography
  top <- readOGR(file.path(gis.dir))
  
  #resolution of contours
  contour_res <- 4
  chosen_contours <- data.frame(ELEVATION = seq(min(top$ELEVATION),
                                                max(top$ELEVATION),contour_res))
  
  top_sf <- top %>%
    as("sf") %>%
    st_transform(4326) %>% 
    semi_join(.,chosen_contours, by = "ELEVATION") %>%
    dplyr::select(geometry, ELEVATION)
  
  #set CRS
  st_crs(top_sf) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  
  #Mask with FCWA polygon
  top_sf_crop = top_sf %>% st_intersection(FCWA) 
  
  save(top_sf_crop, file = file.path(gis.dir, "topo_4ft.Rdata"))
  
} else {
  load(file = file.path(gis.dir,"topo_5ft.Rdata"))

}

#Source OSM data--------------------------------------------------------------
source(file.path(r.dir,"query_osm.R"))

#Query all roads
osm_all_roads <- query_osm(key = "highway", crop = "fcwa", geo = "osm_lines")

osm_paths <- osm_all_roads %>% 
  filter(highway %in% c("path","footway","track"))
tertiary <- osm_all_roads %>% 
  filter(highway %in% c("tertiary"))
secondary <- osm_all_roads %>% 
  filter(highway %in% c("secondary"))
motorway <- osm_all_roads %>% 
  filter(highway %in% c("motorway","motorway_link"))
service <- osm_all_roads %>% 
  filter(highway %in% c("service"))

#Query railroad
railway <- query_osm(key = "railway", crop = "fcwa", geo = "osm_lines")

#Natural features
natural_features <- query_osm(key = "natural", geo = "osm_polygons")

#Power lines
power_lines <- query_osm(key = "power", geo = "osm_lines")

#Bridges
bridges <- query_osm(key = "bridge", geo = "osm_lines")

#Accessibility
access <- query_osm(key = "access", geo = "osm_polygons")


#Plotting---------------------------------------------------------------------
#bounding box
xmin <- -70.61
xmax <- -70.56
ymin <- 41.63
ymax <- 41.658
box <- c(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)


(map <- ggplot() +
    
  # Landcover
  geom_sf(data = mf1, fill = "#77BF7266", color = "transparent") +
  geom_sf(data = mf2, fill = "#77BF7266", color = "transparent") +
  geom_sf(data = mf3, fill = "#77BF7266", color = "transparent") +
  geom_sf(data = mf4, fill = "#77BF7266", color = "transparent") +
  geom_sf(data = mf5, fill = "#77BF7266", color = "transparent") +
    
  geom_sf(data = p1, fill = "#bcafb466", color = "transparent") +
  geom_sf(data = p2, fill = "#bcafb466", color = "transparent") +
  geom_sf(data = p3, fill = "#bcafb466", color = "transparent") +
    
  geom_sf(data = scrub1, fill = "#a9bf7266", color = "transparent") +
  # geom_sf(data = scrub2, fill = "#bfa77266", color = "transparent") +
  
  #Contour lines--------------------------------------------------------------
  geom_sf(data = top_sf_crop, aes(color = ELEVATION),size = 0.3) +
  guides(color = F) +
  scale_color_gradientn(colors = c("#00000000",
  "#000000FF")) +
  
  #OSM queries----------------------------------------------------------------
  #Trails
  geom_sf(data = osm_paths, color = "blue") +
    
  #unlisted trails
  geom_sf(data = fcwa_unlisted1, color = "blue") +
  geom_sf(data = fcwa_unlisted2, color = "blue") +
  geom_sf(data = fcwa_unlisted3, color = "blue") +
    
  #Rail
  geom_sf(data = railway, color = "#65432159", size = 1.15) +
  geom_sf(data = railway, color = "#654321BF", size = 1.15, linetype = "41") +
  
  #Roads
  
  #unlisted roads
  geom_sf(data = fcwa_roads1, color = "black", size = 1) +
  geom_sf(data = fcwa_roads2, color = "black", size = 1) +
  geom_sf(data = fcwa_roads3, color = "black", size = 1) +
  
  #offramp
  geom_sf(data = motorway %>% 
            filter(highway == "motorway_link"), color = "#000000FF", size = 1.25) +
  
  #151
  geom_sf(data = secondary, color = "#000000FF", size = 1.25) +
  geom_sf(data = secondary, color = "#eeee0080", size = 0.2, linetype = "21") +
  
  #28
  geom_sf(data = motorway %>% 
            filter(highway != "motorway_link"), color = "#000000FF", size = 1.25) +
  geom_sf(data = motorway %>% 
            filter(highway != "motorway_link"), color = "#faebd780", size = 0.2,
          linetype = "21") +

  #Service roads
  geom_sf(data = service, color = "#000000FF", size = 1) +
    
  #Natural features
  geom_sf(data = natural_features, fill = "lightblue", color = "lightblue")+
    
  #Power lines
  geom_sf(data = power_lines, color = "indianred") +
    
  ggsn::scalebar(data = top_sf_crop,
                 dist = 0.25,
                 dist_unit = "mi",
                 transform = T)+
    
  geom_point(data = waypoints %>% filter(color == "red"), 
             aes(geometry = geometry),
             stat = "sf_coordinates", shape = 1, color = "red",
             size = 2, stroke = 1.5) +
    
  geom_point(data = waypoints %>% filter(color == "yellow"), 
             aes(geometry = geometry),
            stat = "sf_coordinates", shape = 1, color = "#eb42f4",
            size = 2, stroke = 1.5) +
    
  geom_point(data = waypoints %>% filter(color == "blue"), 
             aes(geometry = geometry),
             stat = "sf_coordinates", shape = 1, color = "blue",
             size = 2, stroke = 1.5) +  
    
  #Label contours-------------------------------------------------------------
  geom_text(
    data = top_sf_crop %>% 
      filter(ELEVATION %in% seq(40,220,20)),
    aes(label = ELEVATION, geometry = geometry),
    stat = "sf_coordinates",
    color = "black",
    size = 1.5,
    family = "KeltNormal") +
    
    geom_text(
      data = top_sf_crop %>% st_intersection(elev_filt) %>%
        filter(ELEVATION %in% seq(40,210,8)),
      aes(label = ELEVATION, geometry = geometry),
      stat = "sf_coordinates",
      color = "black",
      size = 1.5,
      family = "KeltNormal") +
  
  #Label waypoints------------------------------------------------------------
  ggrepel::geom_text_repel(
    data = waypoints,
    aes(label = labels, geometry = geometry),
    stat = "sf_coordinates",
    color = "red",
    size = 4,
    family = "Ringbearer"
  ) +

  
  #Theme----------------------------------------------------------------------
  # coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
  scale_x_continuous(expand = c(0.001,0.001)) +
  scale_y_continuous(expand = c(0.001,0.001)) +
  theme(rect = element_rect(fill = "transparent"),
        text = element_text(family = "KeltNormal"),
        panel.grid.major = element_line(color = "grey50", linetype = 1),
        panel.background = element_rect(fill = "white", color = NA),
        panel.ontop = F)
  )

  
ggsave(map,
       device = "tiff",
       filename = file.path(pdf.dir,"FCWA.tiff"),
       dpi = 400,
       bg = "transparent")

ggplot() +
  
  geom_point(data = waypoints %>% filter(!color %in% c("secret",
                                                       "rivendell")),
             aes(geometry = geometry),
             stat = "sf_coordinates",
             shape = 1) 
