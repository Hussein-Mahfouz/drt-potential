
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
gtfs_trip_ids <- gtfs_bus$frequencies %>% filter(start_time == "09:00:00")

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
