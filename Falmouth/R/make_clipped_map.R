library(tidyverse)
library(osmdata)
library(smoothr)
library(sfnetworks)
library(elevatr)
library(ggplot2)
library(raster)
library(rgdal)
library(rnaturalearth)
library(sp)
library(smoothr)
library(sf)
library(geomtextpath)
library(ggnewscale)
library(raster)
library(extrafont)

rot = function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)

#set up------------------------------------------------------------------------
gis.dir <- here::here("Falmouth/gis")
r.dir <- here::here("Falmouth/R")
pdf.dir <- here::here("Falmouth/pdf")

#if process raw contour data
process_raw <- F

#get polygons
fcwa_outer <- sf::st_read(here::here("falmouth/gis/fcwa_outer.kml"))

FCWA <- sf::st_read(paste0(here::here("Falmouth/gis/"),filename,".kml")) %>% st_intersection(fcwa_outer)

mf1 <- sf::st_read(paste0(here::here("Falmouth/gis/mixedforest1.kml"))) %>% st_intersection(fcwa_outer)
mf2 <- sf::st_read(paste0(here::here("Falmouth/gis/mixedforest2.kml"))) %>% st_intersection(fcwa_outer)
mf3 <- sf::st_read(paste0(here::here("Falmouth/gis/mixedforest3.kml"))) %>% st_intersection(fcwa_outer)
mf4 <- sf::st_read(paste0(here::here("Falmouth/gis/mixedforest4.kml")))%>% st_intersection(fcwa_outer)
mf5 <- sf::st_read(paste0(here::here("Falmouth/gis/mixedforest5.kml"))) %>% st_intersection(fcwa_outer)

p1 <- sf::st_read(paste0(here::here("Falmouth/gis/private1.kml"))) %>% st_intersection(fcwa_outer)
p2 <- sf::st_read(paste0(here::here("Falmouth/gis/private2.kml"))) %>% st_intersection(fcwa_outer)
p3 <- sf::st_read(paste0(here::here("Falmouth/gis/private3.kml"))) %>% st_intersection(fcwa_outer)

scrub1 <- sf::st_read(paste0(here::here("Falmouth/gis/scrubland.kml"))) %>% st_intersection(fcwa_outer)
# scrub2 <- get_polygon(filename = "scrubland2") 

elev_filt <- sf::st_read(paste0(here::here("Falmouth/gis/elevation_filter.kml"))) %>% st_intersection(fcwa_outer)
elev_filt2 <- sf::st_read(paste0(here::here("Falmouth/gis/elevation_filter2.kml"))) %>% st_intersection(fcwa_outer)

roadcut1 <- sf::st_read(paste0(here::here("Falmouth/gis/roadcut1.kml")))%>% st_intersection(fcwa_outer)
roadcut2 <- sf::st_read(paste0(here::here("Falmouth/gis/roadcut2.kml")))%>% st_intersection(fcwa_outer)

#get lines
fcwa_roads1 <- sf::st_read(paste0(here::here("Falmouth/gis/fcwa-roads1.kml")))%>% st_intersection(fcwa_outer)
fcwa_roads2 <- sf::st_read(paste0(here::here("Falmouth/gis/fcwa-roads2.kml")))%>% st_intersection(fcwa_outer)
fcwa_roads3 <- sf::st_read(paste0(here::here("Falmouth/gis/fcwa-roads3.kml")))%>% st_intersection(fcwa_outer)

fcwa_unlisted1 <- sf::st_read(paste0(here::here("Falmouth/gis/fcwa_unlisted1.kml")))%>% st_intersection(fcwa_outer)
fcwa_unlisted2 <- sf::st_read(paste0(here::here("Falmouth/gis/fcwa_unlisted2.kml")))%>% st_intersection(fcwa_outer)
fcwa_unlisted3 <- sf::st_read(paste0(here::here("Falmouth/gis/fcwa_unlisted3.kml")))%>% st_intersection(fcwa_outer)



#get waypoints
source(file.path(r.dir,"get_waypoints.R"))
waypoints <- get_waypoints()

map_op <- fcwa_outer %>% 
  sf::st_zm() %>% 
  as_Spatial()

map_extent <- extent(map_op)

dem <- raster::raster(here::here("falmouth/gis/USGS_13_n42w071_20191216.tif")) %>% 
  crop(.,extent(map_op)) %>% 
  mask(.,map_op) %>% 
  raster::disaggregate(.,2)

# create slope and hillshade

#Source OSM data--------------------------------------------------------------

source(file.path(r.dir,"query_osm.R"))

#Query all roads
osm_all_roads <- query_osm(key = "highway", bb = "map_extent")

osm_paths <- osm_all_roads$osm_lines %>% 
  filter(highway %in% c("path","footway","track")) %>%
  st_intersection(fcwa_outer)
tertiary <- osm_all_roads$osm_lines %>% 
  filter(highway %in% c("tertiary")) %>%
  st_intersection(fcwa_outer)
secondary <- osm_all_roads$osm_lines %>% 
  filter(highway %in% c("secondary")) %>%
  st_intersection(fcwa_outer)
motorway <- osm_all_roads$osm_lines %>% 
  filter(highway %in% c("motorway","motorway_link")) %>%
  st_intersection(fcwa_outer)
service <- osm_all_roads$osm_lines %>% 
  filter(highway %in% c("service")) %>% 
  mutate(length = as.numeric(st_length(.))) %>% 
  dplyr::select(length) %>% 
  filter(length > 200) %>%
  st_intersection(fcwa_outer)


#Query railroad
railway <- query_osm(key = "railway", bb = "map_extent")$osm_lines %>%
  st_intersection(fcwa_outer)

#Natural features
natural_features <- query_osm(key = "natural", bb = "map_extent")$osm_polygons %>%
  st_intersection(fcwa_outer)

#Power lines
power_lines <- query_osm(key = "power", bb = "map_extent")$osm_lines %>%
  st_intersection(fcwa_outer)

#Bridges
bridges <- query_osm(key = "bridge", bb = "map_extent")$osm_lines %>%
  st_intersection(fcwa_outer)

#Accessibility
access <- query_osm(key = "access", bb = "map_extent")$osm_polygons %>%
  st_intersection(fcwa_outer)

landuse <- query_osm(key = "landuse", bb = "map_extent")$osm_polygons

ccourse <- query_osm(key = "leisure", bb = "map_extent")$osm_polygons

course <- 
  landuse %>% 
  filter(landuse == "grass")%>%
  bind_rows(ccourse %>% 
              filter(leisure == "golf_course")) %>% 
  st_intersection(fcwa_outer)

greens <- 
  landuse %>% 
  filter(golf == "fairway")


#Plotting---------------------------------------------------------------------
dt_inset_color <- "black"
trail_color <- "purple"
hs_alpha <- 0.9
mp <- 31.5

slope = terrain(dem, opt='slope')
aspect = terrain(dem, opt='aspect')
hill = hillShade(slope, aspect, 10, 270)
hill_df <- hill %>% 
  dream::rst_to_tib(var_name = "slope")
full_map <- st_sf(geom = st_sfc(st_point(c(map_extent[1],map_extent[3])),
                                st_point(c(map_extent[2], map_extent[4]))))

# contours
cont <- rasterToContour(dem, nlevels = 20)
cont_sf <- as(cont, "sf") %>% 
  st_transform(4326) %>% 
  smoothr::smooth(., method = "ksmooth", smoothness = 10) %>%
  st_cast("LINESTRING") %>% 
  mutate(level = as.numeric(level),
         length = as.numeric(st_length(.)),
         id = 1:nrow(.)) %>% 
  filter(length > 100) %>%
  st_crop(full_map)
 
plot(cont_sf)

t <- 
  sf::st_coordinates(cont_sf) %>% 
  as_tibble() %>% 
  left_join(.,cont_sf %>% 
              mutate(id = 1:nrow(.)) %>% 
              st_set_geometry(NULL),
            by = c("L1" = "id")) %>% 
  # filter(length > 100) %>% 
  distinct() %>% 
  mutate(cont_lab = ifelse(level %in% seq(15, 65, 10),
                           "show","no show"))



  # dream::rst_to_tib()
ggplot() +
  # hill shade
  # geom_tile(data = hill_df, aes(x = longitude, y = latitude,
  #                               fill = slope), show.legend = F) +
  # scale_fill_gradient(low = "black", high = "white") +
  # new_scale_fill()+
  # dem
  # geom_raster(data = dem_df, aes(x = longitude, y = latitude,
  #                                fill = fill_var),
  #             alpha = hs_alpha, show.legend = F) +
  # rcartocolor::scale_fill_carto_c(type = "diverging", 
  #                                 palette = "Earth", direction = -1) +
  # scico::scale_fill_scico(palette = "tofino")
  # scale_fill_gradient2(low = "white",
  #                      high = "#5a8c54",
  #                      midpoint = mp) +
  # # Landcover
  # geom_sf(data = mf1, fill = "#77BF7266", color = "transparent") +
  # geom_sf(data = mf2, fill = "#77BF7266", color = "transparent") +
  # geom_sf(data = mf3, fill = "#77BF7266", color = "transparent") +
  # # geom_sf(data = mf4, fill = "#77BF7266", color = "transparent") +
  # geom_sf(data = mf5, fill = "#77BF7266", color = "transparent") +
  # geom_sf(data = roadcut1, fill = "#77BF7266", color = "transparent") +
  # geom_sf(data = roadcut2, fill = "#77BF7266", color = "transparent") +
  # 
  # geom_sf(data = p1, fill = "#bfa77266", color = "transparent") +
  # geom_sf(data = p2, fill = "#bfa77266", color = "transparent") +
  # geom_sf(data = p3, fill = "#bfa77266", color = "transparent") +
  # 
  # geom_sf(data = scrub1, fill = "#cfbb7c", color = "transparent",
  #         alpha = 0.5) +
  
  # contours
  # geom_path(data = t %>% filter(cont_lab == "no show"),
  #           aes(x = X, y = Y, group = L1),
  #           size = 0.2, color = "black") +
  #   
  geom_textpath(data = t ,
                  aes(x = X, y = Y, group = L1, label = level),
                  size = 1, padding  = unit(0, "pt"),
                linewidth = 0.2, color = "black") +
  # geom_sf(data = cont_sf, aes(alpha = level),
  #         size = 0.2, color = "black", show.legend = F) +
  # geom_sf(data = cont_sf, aes(color = level),size = 0.3,
  #         show.legend = F) +
  # scale_color_gradientn(colors = c("#00000000",
  #                                  "#000000FF")) +
  geom_sf(data = osm_paths, color = "blue", size = 0.4) +
  
  #unlisted trails
  # geom_sf(data = fcwa_unlisted1, color = "blue") +
  # geom_sf(data = fcwa_unlisted2, color = "blue") +
  # geom_sf(data = fcwa_unlisted3, color = "blue") +
  
  #Rail
  geom_sf(data = railway, color = "#65432159", size = 1.15) +
  geom_sf(data = railway, color = "#654321BF", size = 1.15, linetype = "41") +
  
  geom_sf(data = course, fill = "green", color = "transparent",alpha = 0.125) +
  # geom_sf(data = golf, fill = "green", color = "transparent", alpha = 0.5) +
  
  #Roads
  
  #unlisted roads
  geom_sf(data = fcwa_roads1, color = "black", size = 1) +
  geom_sf(data = fcwa_roads2, color = "black", size = 1) +
  geom_sf(data = fcwa_roads3, color = "black", size = 1) +
  
  #offramp
  geom_sf(data = motorway %>% 
            filter(highway == "motorway_link"), 
          color = "#000000FF", size = 1.25) +
  
  #151
  geom_sf(data = secondary, color = "#000000FF", size = 1.25) +
  geom_sf(data = secondary, color = "#eeee0080", size = 0.2, linetype = "21") +
  
  #28
  geom_sf(data = motorway %>% 
            filter(highway != "motorway_link"), 
          color = "#000000FF", size = 1.25) +
  geom_sf(data = motorway %>% 
            filter(highway != "motorway_link"), 
          color = "#faebd780", size = 0.2,
          linetype = "21") +
  
  #Service roads
  # geom_sf(data = service, color = "#000000FF", size = 0.25, alpha = 0.5) +
  
  #Natural features
  geom_sf(data = natural_features %>% 
            filter(natural %in% c("water",
                                  "wetland")), 
          fill = "lightblue", color = "lightblue") +
  
  #Power lines
  geom_sf(data = power_lines, color = "indianred") +
  
  geom_point(data = waypoints %>% filter(color == "dwarf"), 
             aes(geometry = geometry),
             stat = "sf_coordinates", shape = 1, color = "black",
             size = 2.3, stroke = 1.5) +
  geom_point(data = waypoints %>% filter(color == "dwarf"), 
             aes(geometry = geometry),
             stat = "sf_coordinates", shape = 1, color = "red",
             size = 2, stroke = 1.5) +
  
  geom_point(data = waypoints %>% filter(color == "elf"),
             aes(geometry = geometry),
             stat = "sf_coordinates", shape = 1, color = "black",
             size = 2.3, stroke = 1.5) +
  geom_point(data = waypoints %>% filter(color == "elf"), 
             aes(geometry = geometry),
             stat = "sf_coordinates", shape = 1, color = "#ffeb7c",
             size = 2, stroke = 1.5) +
  
  geom_point(data = waypoints %>% filter(color == "human"), 
             aes(geometry = geometry),
             stat = "sf_coordinates", shape = 1, color = "black",
             size = 2.3, stroke = 1.5) +  
  geom_point(data = waypoints %>% filter(color == "human"), 
             aes(geometry = geometry),
             stat = "sf_coordinates", shape = 1, color = "purple",
             size = 2, stroke = 1.5) +  
  
  #Label waypoints------------------------------------------------------------
  ggrepel::geom_text_repel(
    data = waypoints,
    aes(label = labels, geometry = geometry),
    stat = "sf_coordinates",
    color = "red",
    size = 4,
    box.padding = 0,
    family = "Ringbearer",
    seed = 1,
    stroke = 4.1,
    stroke.color = "white"
  ) +

  #Theme----------------------------------------------------------------------
  coord_sf(xlim = c(map_extent[1], map_extent[2]), 
           ylim = c(map_extent[3], map_extent[4])) +
  scale_x_continuous(expand = c(0.001,0.001)) +
  scale_y_continuous(expand = c(0.001,0.001)) +
  guides(alpha = "none",
         color= "none",
         fill = "none") +
  theme(rect = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank())


ggsave(device = "tiff",
       filename = file.path(pdf.dir,"FCWA_clipped2.tiff"),
       dpi = 400,
       bg = "transparent")
