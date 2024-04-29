###################################################################################################
###    The purpose of this script is to calculate a travel time matrix for each different mode  ###
###    combination. r5r is used for the calculations                                            ###
###################################################################################################

source("code/routing_prep.R")
source("R/study_area_geographies.R")
source("R/r5r_routing_wrappers.R")

library(tidyverse)
library(sf)
library(r5r)

# increase the memory available to Java. Needs to be done at the beginning of the script
options(java.parameters = "-Xmx40G")  # 3 gegabytes

# define path where graph will be built (path with osm and pbf data)
graph_path <- paste0("data/interim/routing_graph/")
# define path where routing results will be saved
travel_time_path <- paste0("data/processed/travel_times/")

geography <- "OA"

# what number do we want to give to OD pairs that cannot be reached within our travel time threshold
na_time_replace <- 150  # 2.5 hours

# create a directory to store the results
dir.create(paste0(travel_time_path, geography))

# ------------------------------------- PREPARE (BASE) OD MATRIX LAYER ------------------------------------- #

study_area <- st_read("data/interim/study_area_boundary.geojson")

# edit the study area to match the chosen resolution
study_area <- study_area_geographies(study_area = study_area,
                                     geography = geography)


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

scenarios <- tribble(
  ~scenario, ~mode,  ~departure_datetime,
  # public transport at different times of day / week
  "pt_wkday_morning", c("WALK", "TRANSIT"),  "14-08-2023 07:30:00",
  "pt_wkday_afternoon", c("WALK", "TRANSIT"),  "14-08-2023 12:30:00",
  "pt_wkday_evening", c("WALK", "TRANSIT"), "14-08-2023 18:30:00",
  "pt_wkday_night", c("WALK", "TRANSIT"),  "14-08-2023 23:30:00",
  "pt_wkend_morning", c("WALK", "TRANSIT"), "13-08-2023 07:30:00",
  "pt_wkend_evening", c("WALK", "TRANSIT"), "13-08-2023 18:30:00",
  # car (travel time is the same regardless of day / time) - unless we add congestion
  "car", c("CAR"), "14-08-2023 07:30:00",
  #walking
  "walk", c("WALK"), "14-08-2023 07:30:00",
  # cycle
  "cycle", c("BICYCLE"),  "14-08-2023 07:30:00"
)


# ------------------------------------- BUILD ROUTING GRAPH ------------------------------------- #

# stop any running r5 instances
# r5r::stop_r5()
# java garbage collector to free up memory
#rJava::.jgc(R.gc = TRUE)

# setup r5
print("Setting up r5...")
r5r_core <- setup_r5(data_path = graph_path,
                     verbose = TRUE,
                     overwrite = TRUE) # turn to true once we have elevation

print("Graph built...")
# ------------------------------------- CALCULATE TRAVEL TIME MATRIX ------------------------------------- #


# ---------- 1. route using r5r::travel_time_matrix. This only gives you total travel time

# apply the routing function
tt_results <- tt_matrix(#scenarios = scenarios,
                        scenarios = scenarios, #[scenarios$scenario != "car", ],
                        zone_layer = study_area_r5,
                        time_window = 20,
                        percentiles = c(25, 50, 75))

# replace na results with very high number
tt_results <- tt_results %>%
  mutate(across(.cols = contains("travel_time"), ~ifelse(is.na(.), na_time_replace, .)))


# save the results
#arrow::write_parquet(tt_results, paste0(travel_time_path, geography, "/travel_time_matrix.parquet"))
arrow::write_parquet(tt_results, paste0(travel_time_path, geography, "/travel_time_matrix_acbm.parquet"))

#tt_results <- arrow::read_parquet(paste0(travel_time_path, geography, "/travel_time_matrix.parquet"))

# ---------- 2. route using r5r::expanded_travel_time_matrix. This gives you travel time broken down by journey components

# --- apply the routing function

# Option 1: story in memory

# tt_expanded_results <- tt_matrix_expanded(#scenarios = scenarios,
#                                           scenarios = scenarios[scenarios$scenario != "car", ],
#                                           zone_layer = study_area_r5[1:10,],
#                                           time_window = 5,
#                                           storage_option = "memory")

# Option 2: save to disk

tt_matrix_expanded(scenarios = scenarios,
  #scenarios = scenarios[scenarios$scenario != "car", ],
  zone_layer = study_area_r5,
  time_window = 10,
  storage_option = "save",
  save_format = "parquet",
  folder_path = paste0(travel_time_path, geography, "/travel_time_matrix_expanded_acbm"))


# --- read in the parquet files
# read in all the files using purrr::map
files <- dir(paste0(travel_time_path, geography, "/travel_time_matrix_expanded_acbm"), full.names = TRUE)
tt_results_expanded <- map(files, arrow::read_parquet)
tt_results_expanded <- bind_rows(tt_results_expanded)

# summarise the results to get one row per group. Currently we have one row for each minute in a time_window
tt_results_expanded_s <- summarise_ttm_expanded(ttm_expanded_results = tt_results_expanded)

# save the results
arrow::write_parquet(tt_results_expanded_s, paste0(travel_time_path, geography, "/travel_time_matrix_expanded.parquet"))
#tt_results_expanded_s <- arrow::read_parquet(paste0(travel_time_path, geography, "/travel_time_matrix_expanded.parquet"))






# -------------- CALCULATE DETAILED ITINERARIES TO GET CAR SHORTEST PATH GEOMETRIES

#TODO: create function to spin up an r5 routable graph -> move function to R/ and source it here -> move this part to pt_or_drt.R and source the function there as well

shortest_path_car <- r5r::detailed_itineraries(r5r_core = r5r_core,
                                               origins = study_area_r5,
                                               destinations = study_area_r5,
                                               mode = "CAR",
                                               departure_datetime = as.POSIXct(scenarios$departure_datetime[1][[1]],
                                                                               format = "%d-%m-%Y %H:%M:%S"),
                                               time_window = 1,
                                               max_walk_time = 10,
                                               max_trip_duration = 120,
                                               shortest_path = TRUE,
                                               all_to_all = TRUE,
                                               drop_geometry = FALSE,
                                               # to suppress r5 output. Change to true if debugging
                                               verbose = FALSE,
                                               # slow down function by ~20%
                                               progress = TRUE)

# keep necessary columns only
shortest_path_car <- shortest_path_car %>%
  select(from_id, to_id, total_duration, total_distance)

# TODO: save as geoparquet file when geoarrow r package matures https://geoarrow.github.io/geoarrow-r/index.html

#st_write(shortest_path_car, paste0(travel_time_path, geography, "/shortest_path_car.geojson"), delete_dsn = TRUE)
saveRDS(shortest_path_car, paste0(travel_time_path, geography, "/shortest_path_car.Rds"))




# stop r5
r5r::stop_r5(r5r_core)
# java garbage collector to free up memory
rJava::.jgc(R.gc = TRUE)

