###################################################################################################
###    The purpose of this script is to prepare the data for building the routing graph. This   ###
###    is done by moving the osm and pbf files to the directory where the graph will be built   ###
###################################################################################################

# --------------------------------- PREPARE INPUT DATA --------------------------------- #

# ----- 1. define directory where graph will be built
graph_path <- paste0("data/interim/routing_graph/")

# ----- 2. remove any files created during a previous graph build
# list existing files
previous_files <- list.files("data/interim/routing_graph/")
# delete existing files (unlink does not delete the parent directory if it's empty, but file.remove does)
unlink(paste0(graph_path, previous_files))

# ----- 3. add the osm road network and gtfs feed(s) to the path where the routing graph will be built

# --- osm road network
osm_file_name <- "west-yorkshire-latest.osm.pbf"
osm_source <- paste0("data/external/", osm_file_name)
osm_destination <- paste0(graph_path, osm_file_name)
# copy the file
file.copy(osm_source, osm_destination)

# --- gtfs feed

# list the gtfs files
gtfs_files <- list.files("data/interim", pattern = ".zip")
# add the directory to the file names
gtfs_source <- paste0("data/interim/", gtfs_files)
# define the destination directory
gtfs_destination <- paste0(graph_path, gtfs_files)
# copy the file(s)
file.copy(gtfs_source, gtfs_destination)

# clear environment
rm(list = ls())


