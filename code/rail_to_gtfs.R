###################################################################################################
###    The purpose of this script is to convert UK rail data (from ATOC) from CIF to GTFS using ###
###    UK2GTFS                                                                                  ###
###################################################################################################

library(tidyverse)
library(UK2GTFS)


# ----- Load in the data and convert it

file_name <- "ttis823"
data_path <- paste0("data/external/", file_name, ".zip")

gtfs_rail <- UK2GTFS::atoc2gtfs(path_in = data_path,
                                #ncores = parallel::detectCores() - 2,
                                ncores = 4,
                                silent = FALSE)


# ----- save the gtfs feed

gtfs_write(gtfs_rail,
           folder = "data/raw/gtfs/rail/",
           name = paste0("gtfs_rail_", file_name))





