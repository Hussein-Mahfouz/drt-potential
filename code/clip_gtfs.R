###################################################################################################
###    The purpose of this script is to clip gtfs feeds based on a defined polygon geometry.    ###
###################################################################################################

library(tidyverse)
library(gtfstools)
library(sf)

# ----- load in necessary functions (by sourcing all scripts in the R/ directory)
file.sources <- dir("R/", ".R$", full.names = TRUE)
sapply(file.sources, source)

# ----- Download the GTFS feed
gh_release_download(tag = "initial-inputs",
                    pattern = "gtfs_england_06_23.zip",
                    dest = "data/raw")


# ----- read in the gtfs feed
gtfs_feed <- read_gtfs_feeds(feed_dir = "data/raw/")


# ----- filter the feed to a specified study area

# load in the study area boundary
study_area <- st_read("data/interim/study_area_boundary.geojson")

# filter uk gtfs to the boundary
gtfs_clipped <- gtfs_feed[[1]] %>%
  gtfstools::filter_by_sf(geom = study_area)

# ----- check the clipping worked

# convert to shapes and plot
shapes_gtfs_clipped <- gtfs_clipped %>%
  gtfstools::convert_shapes_to_sf()

plot(st_geometry(study_area))
plot(st_geometry(shapes_gtfs_clipped), add = TRUE, col = "red")



# ----- save the gtfs feed
gtfstools::write_gtfs(gtfs_clipped, "data/interim/study_area_gtfs.zip")



