#---------------
library(fields) 
library(sp)
library(sf)
library(dplyr)
library(rgdal)
library(metR)
library(ggplot2)

#get raw contour
top <- readOGR(file.path(gis.dir))

sf_to_raster <- function(x, field, ncol, nrow, fun){
  x <- sf::as_Spatial(x)
  r <- raster::raster(ncol = ncol, nrow = nrow)
  raster::extent(r) <- raster::extent(x)
  r <- raster::rasterize(x, r, field = field, fun = fun)
  return(r)
}

#resolution of contours
contour_res <- 4

#bounding box
xmin <- -70.56
xmax <- -70.55
ymin <- 41.63
ymax <- 41.64
box <- c(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)

#choose contours
chosen_contours <- data.frame(ELEVATION = seq(min(top$ELEVATION),max(top$ELEVATION),contour_res))

#Get base map
top_sf <- top %>%
  as("sf") %>%
  st_transform(4326) %>% 
  semi_join(.,chosen_contours, by = "ELEVATION") %>%
  dplyr::select(geometry, ELEVATION) 

st_crs(top_sf) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

top_sf_crop = st_intersection(FCWA, top_sf)

test <- top_sf_crop %>% 
  st_crop(box) 

clipper <- st_intersection(test, FCWA)

#Move from: SF -> raster -> points
OUT <- sf_to_raster(test, ncol = 300, nrow = 300, field = "ELEVATION", fun = mean)
# outpoints <- rasterToPoints(OUT)

testdf <- rasterToPoints(OUT)
testdf <- data.frame(lon = testdf[,1],
                     lat = testdf[,2],
                     z = testdf[,3])




#empty raster for clipping
r <- raster(ncol = ncol(OUT), nrow = nrow(OUT))
extent(r) <- extent(OUT)
crs(r) <- crs(OUT)

#get points from lines, turn to SPDF
df <- data.frame(lon = outpoints[,1],
                 lat = outpoints[,2],
                 z = outpoints[,3])
sp <- SpatialPoints(df[,c(1,2)], proj4string = crs(OUT))
spdf <- SpatialPointsDataFrame(sp, data = df)



#interpolate points
# gs <- gstat::gstat(formula=z~1, locations = spdf, nmax=2, set=list(idp = 0))
# gs <- gstat::gstat(formula=z~1, locations = spdf)
# p <- interpolate(r, gs)

#crop to polygon
pp <- mask(p, FCWA)

#Convert to data frame
pp_df <- pp %>%
  as("SpatialPixelsDataFrame") %>%
  as("data.frame") %>% 
  dplyr::rename(ELEVATION = var1.pred,
                lon = x,
                lat = y)

ggplot(data = pp_df, aes(x = lon, y = lat, z = ELEVATION)) +
  geom_contour2(color = "black") +
  geom_text_contour(stroke = 0.2, skip = 5, size = 3)
  # geom_sf(data = test, inherit.aes = F, aes(color = "ELEVATION"))
  # geom_sf(data = test, inherit.aes = F, aes(color = "ELEVATION"))

plot(test)

plot(pp)
plot(test, add = T)
plot(OUT)
contour(pp)

as("")
