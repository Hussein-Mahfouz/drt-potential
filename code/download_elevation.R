### The purpose of this script is to download elevation data that can be used in a routing engine ###

library(tidyverse)
library(sf)
library(elevatr)
library(tmap)
# ----------------------------------------- Download elevation data (tif file) ----------------------------------------- #

# --- load in study area geometry
study_area <- st_read("data/interim/study_area_boundary.geojson") %>%
  st_transform(4326)

# --- buffer study area
study_area_buffered <- study_area %>%
  # turn into one polygon
  st_union() %>%
  # transform to metric -> buffer -> transform back
  st_transform(3587) %>% st_buffer(10000) %>%
  st_transform(4326)

# --- download elevation
study_area_elev <- elevatr::get_elev_raster(study_area_buffered,
                                            z= 9,  # higher value is more granular # https://github.com/tilezen/joerd/blob/master/docs/data-sources.md#what-is-the-ground-resolution
                                            clip = "locations")


# --- check that it covers study area extents
tm_shape(study_area_elev)+
  tm_raster(n = 50)+
  tm_layout(legend.show = FALSE) +
tm_shape(study_area %>% st_union()) +
  tm_borders()

# --- save
raster::writeRaster(study_area_elev, "data/interim/elevation.tif", overwrite = TRUE)








