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

od_demand_jittered = st_read(paste0("data/interim/travel_demand/", geography, "/od_demand_jittered_temporal_for_routing.geojson")) %>%
  st_transform(4326)

# prepare origins and destinations for routing
od_demand_origins = od_demand_jittered %>%
  mutate(geometry = lwgeom::st_startpoint(geometry),
         id = paste0(Origin, "-", Destination, "-", combination, "-", row_number()))

od_demand_destinations = od_demand_jittered %>%
  mutate(geometry = lwgeom::st_endpoint(geometry),
        id = paste0(Origin, "-", Destination, "-", combination, "-", row_number()))



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

# ---------- 1. route using r5r::expanded_travel_time_matrix. This gives you travel time broken down by journey components


# empty list to store results for each combination
results <- vector(mode = "list", length = nrow(od_demand_jittered))

for(i in 1:nrow(od_demand_origins)){
  if (i %% 100 == 0) {
    print(paste0("Processed ", i, " out of of ", nrow(od_demand_jittered), " OD pairs"))
  }
  departure_time = scenarios$departure_datetime[scenarios$scenario == od_demand_origins$combination[i]]
  ttm <- r5r::expanded_travel_time_matrix(r5r_core = r5r_core,
                                          origins = od_demand_origins[i,],
                                          destinations = od_demand_destinations[i,],
                                          time_window = 10,
                                          mode = c("WALK", "TRANSIT"),
                                          departure_datetime = as.POSIXct(departure_time,
                                                                          format = "%d-%m-%Y %H:%M:%S"),
                                          max_walk_time = 10, #max_walk_dist,
                                          max_trip_duration = 120,
                                          # number of threads (all - 1)
                                          #n_threads = parallel::detectCores() - 1,
                                          # draws will all be the same as this is not a frequency based feed. see r5r documentation
                                          draws_per_minute = 1,
                                          breakdown = TRUE,
                                          # to suppress r5 output. Change to true if debugging
                                          verbose = FALSE,
                                          # slow down function by ~20%
                                          progress = FALSE)

  ttm$combination = od_demand_origins$combination[i]

  results[[i]] = ttm

}

# stop r5
r5r::stop_r5(r5r_core)
# java garbage collector to free up memory
rJava::.jgc(R.gc = TRUE)


# combine results into one df
tt_results_expanded = bind_rows(results)


# ---------- 2. summarise the results to get one row per group. Currently we have one row for each minute in a time_window
tt_results_expanded_s <- summarise_ttm_expanded(ttm_expanded_results = tt_results_expanded)

# save the results
arrow::write_parquet(tt_results_expanded_s, paste0(travel_time_path, geography, "/travel_time_matrix_expanded_temporal_jitter.parquet"))
#tt_results_expanded_s <- arrow::read_parquet(paste0(travel_time_path, geography, "/travel_time_matrix_expanded.parquet"))



# ---------- 3. Add the travel times onto the demand data

# Create column for joining (since we routed on the row level), the row number is our id)
od_demand_jittered = od_demand_jittered %>%
  mutate(row = row_number())

tt_results_expanded_s = tt_results_expanded_s %>%
  mutate(row = as.numeric(str_extract(from_id, "\\d+$"))) %>%
  select(contains("time"), n_rides, row)

# join
od_demand_with_tt = od_demand_jittered %>%
  left_join(tt_results_expanded_s, by = "row") %>%
  select(-row)


# save the results
st_write(od_demand_with_tt, paste0(travel_time_path, geography, "/travel_time_matrix_expanded_temporal_jitter_demand.geojson"), delete_dsn = TRUE)
#tt_results_expanded_s <- arrow::read_parquet(paste0(travel_time_path, geography, "/travel_time_matrix_expanded.parquet"))


