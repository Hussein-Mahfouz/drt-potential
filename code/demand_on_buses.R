
library(tidyverse)
library(sf)
library(gtfstools)

source("R/study_area_geographies.R")
source("R/trips_to_zone_pairs.R")


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


# ----------- 1. Identify which od pairs are served directly by each trip

x2 <- gtfs_trips_od_coverage(gtfs = gtfs_bus, zones = study_area, zone_column = "OA21CD")



