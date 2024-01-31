###################################################################################################
###    The purpose of this script is to calculate the travel demand on bus routes by projecting ###
###    demand from each OD pair onto buses that directly serve that pair                        ###
###    We do this in two ways. Take an OD pair with demand = 20 that is served by 3 routes.     ###
###    Route A (freq = 6 buses/hour), Route B (freq = 3/hr, Route C (freq = 3/hr):              ###

###       (1) All-to-all: If an OD  pair has demand = 20 and is served by two direct routes,    ###
###          each of these routes is assigned the 20                                            ###
###       (2) Frequency-based: Demand (Route A) = Total Demand * freq A / sum(freq A, freq B, freq C)
###                                             = (20 * 6) / 12 = 10
###################################################################################################

library(tidyverse)
library(sf)
library(gtfstools)
library(tmap)

source("R/study_area_geographies.R")
source("R/trips_to_zone_pairs.R")
source("R/filter_od_matrix.R")


########## ----------------------- Read in the data ----------------------- ##########


# ----------- 1. Study area

# --- administrative boundaries
study_area <- st_read("data/interim/study_area_boundary.geojson")

# convert to desired resolution
geography = "MSOA"
study_area = study_area_geographies(study_area = study_area,
                                    geography = geography)

study_area <- study_area %>%
  st_cast("MULTIPOLYGON")

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
  filter(start_time == "07:30:00")

gtfs_bus <- gtfs_bus %>%
  gtfstools::filter_by_trip_id(gtfs_trip_ids$trip_id)

# ----------- 3.  Census OD data

# Demand (census) + supply (travel time) data

od_demand <- arrow::read_parquet(paste0("data/raw/travel_demand/od_census_2021/demand_study_area_", tolower(geography), ".parquet"))

# columns to reference (they differ based on geography)
from_id_col = paste0(geography, "21CD_home")
to_id_col = paste0(geography, "21CD_work")

# remove intrazone trips
# TODO: do we need to do this?
od_demand <- od_demand %>%
  filter(.data[[from_id_col]] != .data[[to_id_col]])

########## ----------------------- Identify which OD demand pairs are served by each bus ----------------------- ##########


# ----------- 1. Identify which od pairs are served directly by each trip

od_supply <- gtfs_trips_od_coverage(gtfs = gtfs_bus, zones = study_area, zone_column = paste0(toupper(geography), "21CD"))

# trip_id refers to each unique bus that goes from A to B - we want one row for each trip that goees from A -> B (think directional route_id)

# add other columns that can help identify a unique trip
od_supply <- od_supply %>%
  inner_join(gtfs_bus$trips %>%
               select(trip_id, route_id, trip_headsign, shape_id),
             by = "trip_id")

# identify unique trips using distinct
od_supply <- od_supply %>%
  distinct(Origin, Destination, start_time, route_id, trip_headsign, .keep_all = TRUE)


# save the output
arrow::write_parquet(od_supply, paste0("data/interim/travel_demand/", toupper(geography), "/od_pairs_bus_coverage.parquet"))
#od_supply <- arrow::read_parquet( paste0("data/interim/travel_demand/", toupper(geography), "/od_pairs_bus_coverage.parquet"))

# --- remove od pairs with very short distance

# value for dist_threshold based on NTS0308 "Trip length distribution"
# https://www.gov.uk/government/statistics/annual-bus-statistics-year-ending-march-2022/annual-bus-statistics-year-ending-march-2022

od_supply_filtered = filter_matrix_by_distance(zones = study_area,
                                               od_matrix = od_supply,
                                               dist_threshold = 1000)



# ----------- 2. Join OD demand onto the supply data ########## ----- (sd = supply_demand) ----- ##########

# od_sd <- od_supply_filtered %>%
#   st_drop_geometry() %>%
#   #left_join(od_demand,
#   inner_join(od_demand,
#              by = c("Origin" = from_id_col, "Destination" = to_id_col))

od_sd <- od_supply_filtered %>%
  st_drop_geometry() %>%
  #left_join(od_demand,
  inner_join(od_demand,
             by = c("Origin" = from_id_col, "Destination" = to_id_col, "start_time" = "departure_time"))

# # save output
# arrow::write_parquet(od_sd, paste0("data/interim/travel_demand/", toupper(geography), "/od_pairs_demand_and_supply.parquet"))


# ----------- 3. Get the total potential ridership on each unique trip (sd = supply_demand)

# Method 1: all_to_all
trips_sd_1 <- od_sd %>%
  group_by(trip_id, start_time, combination) %>%
  summarise(potential_demand_all_to_all = sum(commute_all, na.rm = TRUE)) %>%
  ungroup()

# Method 2: frequency-based
trips_sd_2 <- od_sd %>%
  group_by(start_time, combination, Origin, Destination) %>%
  # get number of passengers on each route for each OD pair
  mutate(group_id = cur_group_id(),
         frequency_min = 3600/headway_secs,
         commute_route = round((commute_all * frequency_min) / sum(frequency_min))) %>%
  ungroup() %>%
  # sum over the route
  group_by(trip_id, start_time, combination) %>%
  summarise(potential_demand_freq_based = sum(commute_route, na.rm = TRUE)) %>%
  ungroup()


# add all to one df
trips_sd <- trips_sd_1 %>%
  left_join(trips_sd_2, by = c("trip_id", "start_time", "combination"))


# ----------- 4. Add geometry to plot results

# get route by converting shapes.txt from point to linestring geometry
gtfs_shapes <- gtfstools::convert_shapes_to_sf(gtfs_bus)

# add the trip id to the shape geometry
gtfs_shapes <- gtfs_bus$trips %>%
  select(route_id, trip_id, trip_headsign, shape_id) %>%
  left_join(gtfs_shapes, by = "shape_id") %>%
  st_as_sf()

# add the geometry to the supply <> demand df
trips_sd_sf <- trips_sd %>%
  left_join(gtfs_shapes, by = "trip_id") %>%
  st_as_sf()


# ----------- 5. Save the output
st_write(trips_sd_sf, paste0("data/processed/travel_demand/trips_potential_demand_census_", geography, ".geojson"), delete_dsn = TRUE)



# If we want to get the total demand per day let's try and group by shape_id (same trip at different time has a different trip_id)
trips_sd_sf_shape_sum <- trips_sd_sf %>%
  group_by(shape_id) %>%
  summarise(across(contains("potential_demand"), ~ sum(.x, na.rm = TRUE))) %>%
  ungroup()

# ###########  ---------------------------  5. Plots  --------------------------- ##########
#
#
# # ----- a)  potential ridership on all different trips
#
# # to minimise overlaps, we keep only one time of day
# tmap_mode("plot")
#
# tm_shape(study_area) +
#   tm_borders(alpha = 0.1) +
# tm_shape(trips_sd_sf %>%
#            filter(start_time == "07:30:00")) +
#   tm_lines(col = "potential_demand_freq_based",
#            lwd = "potential_demand_freq_based",
#            scale = 3,
#            legend.is.portrait = FALSE,
#            alpha = 0.4) +
#   tm_layout(fontfamily = 'Georgia',
#             main.title = "Potential Ridership on Different Trips",
#             main.title.size = 1.3,
#             main.title.color = "azure4",
#             main.title.position = "left",
#             #legend.outside = TRUE,
#             #legend.outside.position = "bottom",
#             frame = FALSE)
#
#
# # ----- b)  potential ridership on all different trips - facet by method
#
# trips_sd_sf_long <- trips_sd_sf %>%
#   pivot_longer(cols = contains("potential_demand"),
#                names_to = "method",
#                values_to = "potential_demand")
#
#
# tm_shape(study_area) +
#   tm_borders(alpha = 0.1) +
# tm_shape(trips_sd_sf_long) +
#   tm_lines(col = "potential_demand",
#            alpha = 0.8,
#            #palette = "Blues",
#            style = "quantile",
#            legend.col.is.portrait = FALSE) +
#   tm_facets(by="method",
#             ncol = 2,
#             free.coords=FALSE)+
#   tm_layout(fontfamily = 'Georgia',
#             main.title = "Potential ridership on trips using different methods", # this works if you need it
#             main.title.size = 1.3,
#             main.title.color = "azure4",
#             main.title.position = "left",
#             legend.text.size = 1,
#             legend.outside = TRUE,
#             legend.outside.position = "bottom",
#             frame = FALSE)
#
#
# # ----- c)  potential ridership on all different trips - facet by time of day
#
# # NOTE: this is just a placeholder. The data needs to come from an activity based model.
# # Currently the census data does not represent a specific time of day
#
# tm_shape(study_area) +
#   tm_borders(alpha = 0.1) +
# tm_shape(trips_sd_sf) +
#   tm_lines(col = "potential_demand_freq_based",
#            alpha = 0.6,
#            legend.col.is.portrait = FALSE) +
#   tm_facets(by="start_time",
#             nrow = 2,
#             free.coords=FALSE)+
#   tm_layout(fontfamily = 'Georgia',
#             main.title = "Potential Ridership on Different Trips", # this works if you need it
#             main.title.size = 1.3,
#             main.title.color = "azure4",
#             main.title.position = "left",
#             legend.text.size = 1,
#             legend.outside = TRUE,
#             legend.outside.position = "bottom",
#             frame = FALSE)
#
#
# # layer 1 (fill): destinations reachable | accessibility | population
#
# # layer 2 (lines): od matrix
#
# # layer 3 (lines): potential ridership on buses
#
#
#
#
# # ----------- Redundancy: How many routes serve each OD pair
#
#
# # Get number of routes that directly serve each OD pair
# od_no_of_routes <- od_sd %>%
#   group_by(Origin, Destination, start_time, combination) %>%
#   summarise(route_options = n(),
#             #route_options_fct = cut_width(route_options, width = 3, boundary = 0),
#             route_options_fct = cut(route_options, breaks = seq(0, 50, by = 3)),
#             headway_minimum = min(headway_secs),
#             headway_minimum_fct = cut(headway_minimum, breaks = seq(0, 12000, by = 600)),
#             headway_med = median(headway_secs),
#             headway_med_fct = cut(headway_med, breaks = seq(0, 12000, by = 600)))
#
# # add geometry
# od_no_of_routes <- od_no_of_routes %>%
#   inner_join(od_supply_filtered %>%
#                select(Origin, Destination),
#              by = c("Origin", "Destination")) %>%
#   st_as_sf()
#
#
# # Plot OD pairs by the number of routes serving them
# tm_shape(study_area) +
#   tm_borders(alpha = 0.2) +
#   tm_shape(od_no_of_routes %>%
#              filter(route_options <= 10)) +
#   tm_lines(col = "headway_med",
#            title.col = "Median headway of routes serving OD pair",
#            alpha = 0.6,
#            legend.col.is.portrait = FALSE) +
#   tm_facets(by="route_options_fct",
#             nrow = 2,
#             free.coords=FALSE)+
#   tm_layout(fontfamily = 'Georgia',
#             main.title = "No. of routes Serving each OD pair", # this works if you need it
#             main.title.size = 1.3,
#             main.title.color = "azure4",
#             main.title.position = "left",
#             # legend title
#             legend.text.size = 1,
#             legend.outside = TRUE,
#             legend.outside.position = "bottom",
#             frame = FALSE)
#
#
# # CONTINUE HERE!! (Edit the plots)
#
#
# # Get OD pairs that are only served by one route - and add the route geometry
# od_one_route <- od_no_of_routes %>%
#   filter(route_options == 1)
#
#
# # Plot
#
# # --- Plot OD pairs by the number of routes serving them
# tm_shape(study_area) +
#   tm_borders(alpha = 0.1) +
#   tm_shape(od_one_route) +
#   tm_lines(col = "headway_med",
#            title.col = "Median headway of routes serving OD pair",
#            alpha = 0.6,
#            legend.col.is.portrait = FALSE) +
#   tm_layout(fontfamily = 'Georgia',
#             main.title = "OD pairs served by only one route", # this works if you need it
#             main.title.size = 1.3,
#             main.title.color = "azure4",
#             main.title.position = "left",
#             # legend title
#             legend.text.size = 1,
#             legend.outside = TRUE,
#             legend.outside.position = "bottom",
#             frame = FALSE)
#
# # facet by route headway
# tm_shape(study_area) +
#   tm_borders(alpha = 0.1) +
#   tm_shape(od_one_route) +
#   tm_lines(col = "headway_med",
#            title.col = "Median headway of routes serving OD pair",
#            alpha = 0.6,
#            legend.col.is.portrait = FALSE) +
#   tm_facets(by="headway_med_fct",
#             nrow = 2,
#             free.coords=FALSE)+
#   tm_layout(fontfamily = 'Georgia',
#             main.title = "OD pairs served by only one route", # this works if you need it
#             main.title.size = 1.3,
#             main.title.color = "azure4",
#             main.title.position = "left",
#             # legend title
#             legend.text.size = 1,
#             legend.outside = TRUE,
#             legend.outside.position = "bottom",
#             frame = FALSE)
#
# # Group by route and get sum of demand (for OD pairs served by one route only)
#
#
