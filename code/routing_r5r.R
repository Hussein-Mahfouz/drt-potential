###################################################################################################
###    The purpose of this script is to calculate a travel time matrix for each different mode  ###
###    combination. r5r is used for the calculations                                            ###
###################################################################################################

library(tidyverse)
library(sf)
library(r5r)

# increase the memory available to Java. Needs to be done at the beginning of the script
options(java.parameters = "-Xmx30G")  # 3 gegabytes

# define path where graph will be built (path with osm and pbf data)
graph_path <- paste0("data/interim/routing_graph/")

# ------------------------------------- PREPARE (BASE) OD MATRIX LAYER ------------------------------------- #

study_area <- st_read("data/interim/study_area_boundary.geojson")
# r5r requires a POINT sf object with WGS84 CRS
study_area <- study_area %>%
  st_transform(4326) %>%
  st_centroid()



# Function takes a base layer, gets the geometry centroid, and renames the id column that we pass to it into a standard name
prep_base_layer = function(layer, id_col){
  # rename existing ID column with "id"
  id_col = sym(id_col)
  layer = layer %>%
    rename(id = !! id_col)
  # transform crs
  layer = layer %>% st_transform(4326)
  # get centroid
  layer = layer %>% #select(id) %>%
    st_centroid()
}

# apply the function
study_area_r5 <- prep_base_layer(layer = study_area, id_col = "OBJECTID")


# ------------------------------------- DEFINE ROUTING PARAMETERS ------------------------------------- #

max_walk_time <- 10   # meters
max_trip_duration <- 75 # minutes
max_lts = 2
# travel modes
mode = c("WALK", "TRANSIT")
mode_egress = "WALK"

# This needs to be within the start_date and end_date of the calendar.txt
departure_datetime <- as.POSIXct("09-08-2023 07:30:00",
                                 format = "%d-%m-%Y %H:%M:%S")

time_window <- 60 # in minutes (adding this value to departure datetime, gives you the end of the time window)
percentiles <- 75

# use elevation or not
elevation = "NONE"    # "TOBLER"

# ------------------------------------- CALCULATE TRAVEL TIME MATRIX ------------------------------------- #

r5r_core <- setup_r5(data_path = graph_path,
                     verbose = TRUE,
                     elevation = elevation,   # we need to download a tiff file and then turn this to TRUE
                     overwrite = TRUE) # turn to true once we have elevation

ttm <- r5r::travel_time_matrix(r5r_core = r5r_core,
                               origins = study_area,
                               destinations = study_area,
                               time_window = time_window,
                               percentiles = percentiles,
                               mode = mode,
                               mode_egress = mode_egress,
                               departure_datetime = departure_datetime,
                               max_walk_time = max_walk_time,
                               max_trip_duration = max_trip_duration,
                               max_lts = max_lts,
                               # number of threads
                               #n_threads = 2,
                               # to suppress r5 output. Change to true if debugging
                               verbose = FALSE,
                               # slow down function by ~20%
                               progress = TRUE)
