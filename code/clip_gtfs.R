###################################################################################################
###    The purpose of this script is to clip gtfs feeds based on a defined polygon geometry.    ###
###################################################################################################

library(tidyverse)
library(gtfstools)
library(sf)

# ----- load in necessary functions (by sourcing all scripts in the R/ directory)
file.sources <- dir("R/", ".R$", full.names = TRUE)
sapply(file.sources, source)


# ----- read in the gtfs feed
#gtfs_feed <- read_gtfs_feeds(feed_dir = "data/raw/gtfs/bus/")
gtfs_feed <- read_gtfs_feeds(feed_dir = "data/raw/gtfs/rail/")


# ----- filter the feed to a specified study area

# load in the study area boundary
study_area <- st_read("data/interim/study_area_boundary.geojson")

# filter uk gtfs to the boundary
gtfs_clipped <- gtfs_feed[[1]] %>%
  gtfstools::filter_by_sf(geom = study_area, keep = TRUE)

# ----- check the clipping worked

# convert to shapes and plot
if("shapes" %in% names(gtfs_clipped)){
  gtfs_clipped_sf <- gtfs_clipped %>%
    gtfstools::convert_shapes_to_sf()
} else{
  gtfs_clipped_sf <- gtfs_clipped %>%
    gtfstools::convert_stops_to_sf()
}

plot(st_geometry(study_area))
plot(st_geometry(gtfs_clipped_sf), add = TRUE, col = "red")
#plot(st_geometry(gtfs_clipped_sf %>% filter(str_detect(stop_name, "Burley Park"))), add = TRUE, col = "green")


# ----- save the gtfs feed
#gtfstools::write_gtfs(gtfs_clipped, "data/interim/study_area_gtfs_bus.zip")
gtfstools::write_gtfs(gtfs_clipped, "data/interim/study_area_gtfs_rail.zip")




