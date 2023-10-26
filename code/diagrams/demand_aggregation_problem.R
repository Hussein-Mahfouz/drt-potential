library(tidyverse)
library(sf)
library(gtfstools)
library(tmap)
library(od)

source("R/study_area_geographies.R")
# --------------- Load in the layers for the diagram

# --- administrative boundaries
study_area <- st_read("data/interim/study_area_boundary.geojson")

# convert to desired resolution
res = "MSOA"
boundaries = study_area_geographies(study_area = study_area,
                                    geography = res)
# identify crs
crs <- st_crs(boundaries)

# --- gtfs feed
gtfs <- gtfstools::read_gtfs("data/interim/study_area_gtfs_bus.zip")
gtfs_shapes_sf <- convert_shapes_to_sf(gtfs)
gtfs_stops_sf <- convert_stops_to_sf(gtfs)



# --------------- Identify which stops are associated with which gtfs shapes


gtfs_shapes_sf_buffered <- gtfs_shapes_sf %>%
  st_transform(3857) %>%
  st_buffer(50) %>%
  st_transform(crs)

gtfs_shapes_sf_joined <- gtfs_stops_sf %>%
  st_join(gtfs_shapes_sf_buffered)

# --------------- Select a specific route and identify all zones with a stop from that route

# select a trip
shape_example <- gtfs_shapes_sf[1,]

# get all stops associated with the trip
shape_example_stops <- gtfs_shapes_sf_joined %>%
  filter(shape_id %in% shape_example$shape_id)

# get all zones associated with the trip
shape_example_zones <- boundaries %>%
  st_filter(shape_example_stops)


# --------------- create desire lines from identified zones

# convert point layer to od matrix
shape_example_zones_od <- shape_example_zones %>% st_centroid() %>%
  od::points_to_od(interzone_only = TRUE)

# create desire lines from points
shape_examples_zones_desire <- shape_example_zones_od %>%
  od::od_to_sf(z = shape_example_zones)



# --------------- plot the results

tm_shape(shape_example_zones) +
  tm_polygons(col = "red", alpha = 0.3) +
tm_shape(boundaries) +
  tm_borders() +
tm_shape(shape_examples_zones_desire) +
  tm_lines(col = "grey55", alpha = 0.3) +
tm_shape(shape_examples_zones_desire[20,]) +
  tm_lines(col = "blue", alpha = 0.8, lwd = 7) +
tm_shape(shape_example) +
  tm_lines(col = "green", lwd = 3)+
tm_shape(shape_example_stops) +
  tm_dots(col = "darkgreen") +
tm_scale_bar(color.dark = "gray20") -> diagram

diagram
tmap_save(tm = diagram, filename = paste0("data/processed/plots/diagrams/demand_aggregation_", res, ".png"))



# facet map

tm_shape(shape_examples_zones_desire) +
  tm_lines(col = "grey55", alpha = 0.3) +
  tm_facets(by = "O") +
tm_shape(shape_example_zones) +
  tm_polygons(col = "red", alpha = 0.3) +
tm_shape(boundaries) +
  tm_borders() +
tm_shape(shape_examples_zones_desire[20,]) +
  tm_lines(col = "blue", alpha = 0.8, lwd = 7) +
tm_shape(shape_example) +
  tm_lines(col = "green", lwd = 3)+
tm_shape(shape_example_stops) +
  tm_dots(col = "darkgreen") +
tm_scale_bar(color.dark = "gray20")








