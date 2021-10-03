library(tidyverse)
library(raster)
library(sf)
library(metR)
library(ggpattern)
library(magrittr)
library(plotly)

# data file path----
data.dir <- here::here("Crozet/data")

# get contour----
load(file.path(data.dir,"crozet_contour.rdata"))

# load features
load(file.path(data.dir,"features.rdata"))

# extra trails
load(file.path(data.dir,"extras.rdata"))

# trails to exclude
exclude <- c(19764793, 19768573, 19762579, 19763347,
             139041718, 827673372, 827673371, 19771223, 19757270,
             19766041, 19771291, 19769606, 19757247,
             19771495, 19756120, 19779033, 19779031)

# trails that are gravel roads
gravel <- c(19764793, 19768573, 19771223, 19757270,
            19766041, 19771291, 19769606, 19757247)


unforested_1 <- extras %>% filter(Name == "diff_1") %>% 
  st_difference(extras %>% filter(name == "lower_forest"))
unforested_2 <- 
  extras %>% filter(Name == "diff_2") %>% 
    st_difference(
      extras %>% 
        filter(name %in% c("upper_forest",
                                    "n_boundary_forest")) %>% 
        st_union()
  )
unforested_3 <- 
  extras %>% filter(Name == "diff_3") %>% 
  st_difference(
    extras %>% 
      filter(name %in% c("upper_forest",
                         "lower_forest")) %>% 
      st_union()
  )
unforested_4 <- 
  extras %>% filter(Name == "diff_4") %>% 
  st_difference(
    extras %>% 
      filter(name %in% c("upper_forest"))
  )

upper_forest <- 
  extras %>% 
  filter(name == "upper_forest") %>% 
  st_difference(.,extras %>% filter(str_detect(Name, "neighb|light")) %>% 
                  st_union())

# pattern
# source(here::here("Crozet/R/pattern_simple.R"))
# options(ggpattern_array_funcs = list(simple = create_pattern_simple))
# exposed_sf <- 
#   extras %>% 
#   filter(name == "exposed") %>% 
#   st_transform(crs = st_crs(croz_sf)) %>% 
#   st_intersection(croz_sf)

# colors----
# neighb_col <- "#f5a9a4"
neighb_col <- "#960f0fA3"
# forest_col <- "#77BF7266"
forest_col <- "#4E4F615e"
# forest_col <- "#5E5E5E5e"
# open_col <- "#a9bf7266"
open_col <- "#493D2A40"
res_col <- "grey20"
grav_col <- "grey60"
grav_col <- "#645145"
# water_col <- "lightblue"
water_col <- "#7A0C0D"
# bound_col <- "#5B202A"
bound_col <- "#ff5900"
# bound_col <- "#8F6352"
tert_col <- "#000000FF"
stripe_col <- "#faebd780"
# trail_col <- "blue"
# trail_col <- "#E65100"
trail_col <- "#8d21ff"
# pl_col <- "indianred"
pl_col <- "#503435"

# make map----
map <- ggplot() +
  
  #contours----
  geom_contour(data = croz_df,
               aes(x = Longitude, y = Latitude, z = elev, colour = ..level..),
               bins = 75,
               size = 0.4) +
  geom_text_contour(data = croz_df,
                    aes(x = Longitude, y = Latitude, 
                        z = elev, group = grp),size = 2,skip = 0,
                    label.placement = label_placement_flattest()) +
  scale_color_gradientn(colors = c("#00000080",
                                   "#000000FF")) +
  guides(color = "none") +

  # Restricted areas----
  geom_sf(data = extras %>% slice(1),
          fill = neighb_col, colour = 'transparent') +
  
  geom_sf(data = extras %>% filter(name == "neighborhood"),
          fill = neighb_col, colour = 'transparent') +
  
  geom_sf(data = extras %>% filter(name  %in% c("lower_forest",
                                                "n_boundary_forest")),
          fill = forest_col, color = "transparent") +
  
  geom_sf(data = upper_forest,
          fill = forest_col, color = "transparent") +
  
  geom_sf(data = unforested_1, fill = open_col, color = "transparent") +
  geom_sf(data = unforested_2, fill = open_col, color = "transparent") +
  geom_sf(data = unforested_3, fill = open_col, color = "transparent") +
  geom_sf(data = unforested_4, fill = open_col, color = "transparent") +
  geom_sf(data = extras %>% 
            filter(name == "light_cover"), fill = open_col, color = "transparent") +

  
  #Service and residential roads----
  geom_sf(data = res, color = res_col, size = 0.5) +
  geom_sf(data = osm_paths %>% filter(osm_id %in% gravel), 
          color = grav_col,
          size = 0.75) +
  geom_sf(data = extras %>% filter(Name == "missing_road_1"), 
          color = grav_col,
          size = 0.75) +
 
  #Jarman's Gap Rd.
  geom_sf(data = unc, color = grav_col, size = 1) +

  #creeks
  geom_sf(data = extras %>% filter(name == "Creeks"), 
          color = water_col, size = 0.75) +
  
  # Park boundary
  geom_sf(data = ms, fill = "transparent",
          linetype = 4,
          size = 1,
          color = bound_col) +
  
  #lakes
  geom_sf(data = lakes, fill = water_col,
          color = "transparent") +
  geom_sf(data = extras %>% filter(name == "reservoir"), 
          fill = water_col,
          color = "transparent") +
  
  #Tertiary roads
  geom_sf(data = tertiary, color = tert_col, size = 1.25) +
  geom_sf(data = tertiary, color = stripe_col, size = 0.2,
          linetype = "21") +
  
  #Trails----
  geom_sf(data = osm_paths %>% 
            filter(!osm_id %in% exclude), 
          color = trail_col, size = 0.6) +
  geom_sf(data = extra_trails, color = trail_col, size = 0.6) +
  
  #Power line
  geom_sf(data = pl, color = pl_col)  +
  #Map boundary
  coord_sf(xlim = c(-78.755, -78.725),
       ylim = c(38.075, 38.1065)) +

  #Theme
  scale_x_continuous(expand = c(0.001,0.001)) +
  scale_y_continuous(expand = c(0.001,0.001)) +
  theme(rect = element_rect(fill = "transparent"),
        text = element_text(family = "NeonVampire",
                            color = "grey50",
                            size =6),
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid.major = element_line(color = "grey30", linetype = 1,
                                        size = 0.15),
        panel.background = element_rect(fill = "white", color = NA),
        panel.ontop = F)  +
ggsn::scalebar(dist = 0.5,
               dist_unit = "km",
               transform = T,
               location = "topleft",
               height = 0.01,
               x.min = -78.755,
               x.max =  -78.725,
               y.min = 38.075,
               y.max = 38.1065,
               border.size = 0.1) 
# map
# t <- osm_paths
# ggplotly(
# ggplot()  +
#  geom_sf(data = t, aes(group = osm_id), color = "blue", size = 0.4)
# )

ggsave(map,
       device = "pdf",
       height = 17,
       width = 11,
       filename = here::here("crozet/map/crozet.pdf"),
       dpi = 300,
       bg = "transparent",
       units = "in")

