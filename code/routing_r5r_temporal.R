###################################################################################################
###    The purpose of this script is to calculate a travel time matrix for each different mode  ###
###    combination. r5r is used for the calculations                                            ###
###################################################################################################

source("R/study_area_geographies.R")
source("R/r5r_routing_wrappers.R")
# source("code/routing_prep.R")

library(tidyverse)
library(sf)
library(r5r)

# increase the memory available to Java. Needs to be done at the beginning of the script
options(java.parameters = "-Xmx35G")  # 3 gegabytes

# define path where graph will be built (path with osm and pbf data)
graph_path <- paste0("data/interim/routing_graph/")
# define path where routing results will be saved
travel_time_path <- paste0("data/processed/travel_times/")

geography <- "MSOA"

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
  # weekday
  "pt_wkday_06_30", c("WALK", "TRANSIT"),  "14-08-2023 06:30:00",
  "pt_wkday_09_30", c("WALK", "TRANSIT"),  "14-08-2023 09:30:00",
  "pt_wkday_12_30", c("WALK", "TRANSIT"), "14-08-2023 12:30:00",
  "pt_wkday_15_30", c("WALK", "TRANSIT"), "14-08-2023 15:30:00",
  "pt_wkday_18_30", c("WALK", "TRANSIT"),  "14-08-2023 18:30:00",
  # weekend
  "pt_wkend_06_30", c("WALK", "TRANSIT"),  "13-08-2023 06:30:00",
  "pt_wkend_09_30", c("WALK", "TRANSIT"),  "13-08-2023 09:30:00",
  "pt_wkend_12_30", c("WALK", "TRANSIT"), "13-08-2023 12:30:00",
  "pt_wkend_15_30", c("WALK", "TRANSIT"), "13-08-2023 15:30:00",
  "pt_wkend_18_30", c("WALK", "TRANSIT"),  "13-08-2023 18:30:00",
  # car (travel time is the same regardless of day / time) - unless we add congestion
  #"car", c("CAR"), "14-08-2023 07:30:00",
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


# ---------- 2. route using r5r::expanded_travel_time_matrix. This gives you travel time broken down by journey components

# --- apply the routing function

# Option 1: story in memory

# tt_expanded_results <- tt_matrix_expanded(#scenarios = scenarios,
#                                           scenarios = scenarios[scenarios$scenario != "car", ],
#                                           zone_layer = study_area_r5[1:10,],
#                                           time_window = 5,
#                                           storage_option = "memory")

# Option 2: save to disk

tt_matrix_expanded(scenarios = scenarios[scenarios$scenario != "car", ],
                   zone_layer = study_area_r5,
                   time_window = 10,
                   storage_option = "save",
                   save_format = "parquet",
                   folder_path = paste0(travel_time_path, geography, "/travel_time_matrix_expanded/temporal"))


# --- read in the parquet files
# read in all the files using purrr::map
files <- dir(paste0(travel_time_path, geography, "/travel_time_matrix_expanded/temporal"), full.names = TRUE)
tt_results_expanded <- map(files, arrow::read_parquet)
tt_results_expanded <- bind_rows(tt_results_expanded)

# summarise the results to get one row per group. Currently we have one row for each minute in a time_window
tt_results_expanded_s <- summarise_ttm_expanded(ttm_expanded_results = tt_results_expanded)

# save the results
arrow::write_parquet(tt_results_expanded_s, paste0(travel_time_path, geography, "/travel_time_matrix_expanded_temporal.parquet"))
#tt_results_expanded_s <- arrow::read_parquet(paste0(travel_time_path, geography, "/travel_time_matrix_expanded.parquet"))




# stop r5
r5r::stop_r5(r5r_core)
# java garbage collector to free up memory
rJava::.jgc(R.gc = TRUE)

