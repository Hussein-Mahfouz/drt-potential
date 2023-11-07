
library(tidyverse)
library(sf)
library(gtfstools)

source("R/study_area_geographies.R")

########## ----------------------- Read in the data ----------------------- ##########

# ----------- 1. Study area

# --- administrative boundaries
study_area <- st_read("data/interim/study_area_boundary.geojson")

# convert to desired resolution
res = "OA"
study_area = study_area_geographies(study_area = study_area,
                                    geography = res)

# ----------- 2. GTFS feeds

# --- Identify GTFS file paths

# parent directory where feeds are saved
gtfs_dir <- "data/interim/gtfs_freq/"
# get name of gtfs feeds
gtfs_names = list.files(gtfs_dir, pattern = ".zip")
# get path for each feed
gtfs_paths = paste0(gtfs_dir, gtfs_names)


# --- read in the feeds
gtfs_bus <- gtfstools::read_gtfs(gtfs_paths[grepl("bus", gtfs_paths)])
#gtfs_rail <- gtfstools::read_gtfs(gtfs_paths[grepl("rail", gtfs_paths)])

# --- filter the feeds to a specific point in time
gtfs_trip_ids <- gtfs_bus$frequencies %>%
  filter(start_time == "09:00:00")

gtfs_bus <- gtfs_bus %>%
  gtfstools::filter_by_trip_id(gtfs_trip_ids$trip_id)

# ----------- 3.  Census OD data

# TODO: change to demand + travel time (i.e. demand + supply) - do this in demand_on_buses.R

od_demand <- read_csv("data/raw/travel_demand/od_census_2021/demand_study_area_oa.csv")


########## ----------------------- Identify which OD demand pairs are served by each bus ----------------------- ##########


# ----------- 1. Identify which zone each stop falls in

# get stop geometry
gtfs_stops <- gtfstools::convert_stops_to_sf(gtfs_bus, crs = st_crs(study_area))

# --- identify which zone each stop is in
gtfs_stops_int <- gtfs_stops %>% st_join(study_area,
                                         join = st_intersects,
                                         left = FALSE)

# ----------- 2. Identify which zones each trip passes by

# ------ stop_times has all the stops visited by each trip. Join the stop geometry onto that data
gtfs_trips_int <- gtfs_bus$stop_times %>%
  left_join(gtfs_stops_int, by = "stop_id")

# ----- get all OD pairs that are served by trip i

# for each trip
   # demand matrix |> filter(O %in% trip$O, D %in% trip$D)
   # do this using a map function to store a df for each trip


gtfs_trips_int %>% group_by(trip_id) %>%
  group_split() -> x
#
gtfs_trip_i <- gtfs_bus$trips[1,]

gtfs_st_i <- gtfs_bus$stop_times %>% filter(trip_id == gtfs_trip_i$trip_id) %>% select(trip_id, stop_id)
gtfs_stops_i <- gtfs_bus$stops %>% filter(stop_id %in% gtfs_st_i$stop_id)

# identify which zones each bus route intersects


test_trip_id <- gtfs_trip_ids[1,]


get_demand = function(trips, demand_matrix){
  results = vector()
}



x1 <- gtfs_trips_int %>% filter(trip_id == "VJ1b9f9c9f76ce2c7f21b4346d8cac9ba50ccaf984")

elements = x1$OA21CD


# Function to generate valid pairs respecting original order
generate_ordered_pairs <- function(elements) {
  valid_pairs <- list()
  for (i in 1:(length(elements) - 1)) {
    for (j in (i + 1):length(elements)) {
      valid_pairs[[length(valid_pairs) + 1]] <- data.frame(Origin = elements[i], Destination = elements[j])
    }
  }
  # turn into a df
  valid_pairs <- do.call(rbind, valid_pairs)
  # remove duplicate pairs
  valid_pairs <- distinct(valid_pairs, Origin, Destination)
  return(valid_pairs)
}

x <- generate_ordered_pairs(elements)

# Character list
# elements <- c("element1", "element2", "element3", "element4")

# Generate valid pairs respecting original order
pair_df <- generate_ordered_pairs(elements)

# Print the result
print(pair_df)










# Load the necessary libraries
library(dplyr)
library(purrr)

# Assuming you have a dataframe named 'df' and a custom function 'your_custom_function'


# Group the dataframe by the values in the "grouping_column"
grouped_df <-  gtfs_trips_int %>%
  group_by(trip_id)

# Apply your custom function to each group, which returns a list of dataframes
result_df <- grouped_df %>%
  nest() %>%
  mutate(result = map(data, ~ generate_ordered_pairs(.x$OA21CD))) %>%
  unnest(result) %>%
  select(-data)

# Ungroup the dataframe
result_df <- result_df %>%
  ungroup()



# ---------- Function 1: Identify which zones are served by each trip

# each trip passes through a number of zones. We want a list of these zones. We do this by 

  # a) identifying the stops associated with each trip
  # b) doing a spatial join between stops and zones and using that to match trips to zones

# This gives us a spatial df that has, for each trip, the unique stops that serve
# it and the zone that the stop falls in

# ---------- Function 2: Identify which OD pairs are served by each trip

# We have data on the zones served by each trip. We also know the order in which
# the zones are served (through the stop sequence). We use this information to get
# the OD pairs served by each trip. To obtain OD pairs, we need to respect the order
# in which zones are served. If a trip follows the itinerary: "zone1", "zone4", "zone8",
# then it serves

  # zone1 : zone4
  # zone1 : zone8
  # zone4 : zone8

# It DOES NOT serve the return journeys (i.e. zone8 : zone4)

