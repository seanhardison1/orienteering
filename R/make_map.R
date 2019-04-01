library(raster)
library(rgdal)
library(sf)
library(dplyr)
library(ggplot2)
library(ecodata)
library(ggmap)
library(plyr)
library(osmdata)
library(maptools)
library(rgeos)

#set up------------------------------------------------------------------------
gis.dir <- here::here("gis")
r.dir <- here::here("R")
pdf.dir <- here::here("pdf")

#if process raw contour data
process_raw <- F

#get FCWA polygon
source(file.path(r.dir,"get_crane_polygon.R"))
FCWA <- get_crane_polygon() 


#Process base map--------------------------------------------------------------
if (process_raw){
  
  #Full topography
  top <- readOGR(file.path(gis.dir))
  
  #resolution of contours
  contour_res <- 6
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
  
} else {
  load(file = file.path(gis.dir,"topo_6ft.Rdata"))

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


#Plotting---------------------------------------------------------------------
#bounding box
xmin <- -70.61
xmax <- -70.575
ymin <- 41.635
ymax <- 41.658
box <- c(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)


map <- ggplot() +
  
  #Contour lines--------------------------------------------------------------
  geom_sf(data = top_sf_crop, aes(color = ELEVATION),size = 0.2) +
  scale_color_gradientn(colors = c("#00000000",
                                   "#000000FF")) +
  #OSM queries----------------------------------------------------------------
  #Trails
  geom_sf(data = osm_paths, color = "blue") +
  
  #Rail
  geom_sf(data = railway, color = "#65432159", size = 1.15) +
  geom_sf(data = railway, color = "#654321BF", size = 1.15, linetype = "41") +
  
  #Roads
  
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
  
  #Label contours-------------------------------------------------------------
  geom_text(
    data = top_sf_crop %>% filter(ELEVATION %in% seq(36,216,18)),
    aes(label = ELEVATION, geometry = geometry),
    stat = "sf_coordinates",
    colour = "black",
    size = 1.5) +
  
  #Theme----------------------------------------------------------------------
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
  scale_x_continuous(expand = c(0.001,0.001)) +
  scale_y_continuous(expand = c(0.001,0.001)) +
  theme_map() +
  theme(rect = element_rect(fill = "transparent") )
map


ggsave(map,
       device = "pdf",
       filename = file.path(pdf.dir,"FCWA.pdf"),
       dpi = 400,
       bg = "transparent")


#testing ------------------------------------------------------------------------------


xmin <- -70.59
xmax <- -70.58
ymin <- 41.64
ymax <- 41.65

atest <- top_sf_crop %>%
  st_crop(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)

ggplot() +
  geom_sf(data = atest, aes(color = ELEVATION),size = 0.05) +
  geom_sf(data = atest %>% filter(ELEVATION %% 10 == 0),
          aes(color = ELEVATION),size = 1) +
  scale_color_gradientn(colors = c("#00000000",
                                 "#00000080",
                                 "#000000FF")) +
  theme(panel.background = element_rect(fill = "#ebd5b3")) + 
  geom_sf_text(aes(label = ELEVATION), colour = "white")

df <- data.frame(x = 1:20,
                 y = rnorm(20))

t <- osm_all_roads %>% filter(highway == "service")

ggplot() +
  geom_sf(data = t, color = "#000000FF", size = 1.25)


