###################################################################################################
###    The purpose of this script is to prepare the data for building the routing graph. This   ###
###    is done by moving the osm and pbf files to the directory where the graph will be built   ###
###################################################################################################

source("R/utils.R")
# --------------------------------- PREPARE INPUT DATA --------------------------------- #

# ----- 1. define directory where graph will be built
graph_path <- paste0("data/interim/routing_graph/")

# ----- 2. remove any files created during a previous graph build
# list existing files
previous_files <- list.files("data/interim/routing_graph/")
# delete existing files (unlink does not delete the parent directory if it's empty, but file.remove does)
unlink(paste0(graph_path, previous_files))

# ----- 3. add the osm road network and gtfs feed(s) to the path where the routing graph will be built

# gtfs files
copy_files(source = "data/interim/", dest = "data/interim/routing_graph/", pattern = ".zip")
# elevation file
copy_files(source = "data/interim/", dest = "data/interim/routing_graph/", pattern = ".tif")
# osm road network file
copy_files(source = "data/external/", dest = "data/interim/routing_graph/", pattern = ".osm.pbf")


# clear environment
rm(list = ls())



