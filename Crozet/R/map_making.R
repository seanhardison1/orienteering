library(tidyverse)
library(raster)
library(sf)
library(metR)

# data file path----
data.dir <- here::here("Crozet/data")

# get contour----
load(file.path(data.dir,"crozet_contour.rdata"))

# load features
load(file.path(data.dir,"features.rdata"))

# extra trails
load(here::here("extras.rdata"))

# make map----
ggplot() +
  
  geom_contour(data = croz_df,
               aes(x = Longitude, y = Latitude, z = elev, colour = ..level..),
               bins = 50,
               size = 0.3) +
  geom_text_contour(data = croz_df,
                    aes(x = Longitude, y = Latitude, z = elev),size = 2,skip = 0,
                    label.placement = label_placement_flattest()) +
  scale_color_gradientn(colors = c("#00000080",
                                   "#000000FF")) +
  guides(color = "none") +

  
  # geom_sf(data =  woods, fill = "#BEC88C",color = "transparent",
  #         alpha = 0.2) +
  
# Park boundary
  geom_sf(data = ms, fill = "transparent",
          linetype = 4,
          color = "#5B202A",
          alpha = 0.25) +
  
  #Service and residential roads
  # geom_sf(data = service, color = "#000000FF", size = 0.5, alpha = 0.5) +
  geom_sf(data = res, color = "grey20", size = 0.5) +

  #Tertiary roads
  geom_sf(data = tertiary, color = "#000000FF", size = 1.25) +
  geom_sf(data = tertiary, color = "#faebd780", size = 0.2,
          linetype = "21") +
  
 
  #Jarman's Gap Rd.
  geom_sf(data = unc, color = "grey10", size = 1) +

  #lakes
  geom_sf(data = lakes, fill = "lightblue",
          color = "transparent") +
  
  #Trails
  # geom_sf(data = osm_paths, color = "#441116") +
  
  #Power line
  geom_sf(data = pl, color = "indianred")  +
  #Map boundary
  coord_sf(xlim = c(-78.755, -78.72125),
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
        panel.ontop = F)

