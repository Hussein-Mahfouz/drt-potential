###################################################################################################
###    The purpose of this script is to explore the rail data from ATOC. I discovered that ATOC ###
###    data does not have LNER routes, and I will document this here                            ###
###################################################################################################


library(tidyverse)
library(sf)
library(UK2GTFS)

# ----- load in necessary functions (by sourcing all scripts in the R/ directory)
file.sources <- dir("R/", ".R$", full.names = TRUE)
sapply(file.sources, source)

# ------------------------ explore feed converted using atoc2gtfs() --------------------------- #
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

# --- get agency table
agency_all <- gtfs_feed[[1]]$agency
agency_clipped <- gtfs_clipped$agency

# --- try to find LNER in unfiltered gtfs

# agency_id of LNER
agency_all %>% filter(agency_name == "London North Eastern Railway")

# LNER in agency table?
agency_all %>% filter(agency_id == "HB") # YES

# LNER in routes table?
routes_all <- gtfs_feed[[1]]$routes
routes_all %>% filter(agency_id == "HB") # NO




# ------------------------ revisit atoc2gtfs --------------------------- #

# the first step in atoc2gtfs is the importMCA() function. The MCA file has the timetable data (according to this: https://itsleeds.github.io/UK2GTFS/articles/ATOC.html)
# lets load it in to see if it has LNER

# --- unzip

# --- run
# gtfs_rail2 <- UK2GTFS::importMCA(file = paste0("data/ttisf823.mca"),
#                                 #ncores = parallel::detectCores() - 2,
#                                 ncores = 4,
#                                 silent = FALSE)
