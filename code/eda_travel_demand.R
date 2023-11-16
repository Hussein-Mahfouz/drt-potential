# This code visualises the output from code/demand_on_buses.R . The datasets we want to explore together are:
    # demographic data
    # travel time data
    # number of destinations reachable
    # accessibility to different services
    # travel demand data


source("R/study_area_geographies.R")
source("R/filter_od_matrix.R")

library(tidyverse)
library(sf)
library(tmap)

# where do we want to save the plots?
plots_path <- "data/processed/plots/eda/"


# -------------------- read in the outputs

# --- decide on geographic resolution

geography <- "MSOA"

# read in geography
study_area <- st_read("data/interim/study_area_boundary.geojson")
# convert to necessary resolution
study_area <- study_area_geographies(study_area = study_area,
                                     geography = geography)


########### ------------------------ Travel time and travel demand data ------------------------ ###########

# ---------------------- Loading in travel time and travel demand data


# --- travel time and demand (ttd) matrix

ttd_matrix <- arrow::read_parquet(paste0("data/raw/travel_demand/od_census_2021/demand_study_area_", tolower(geography), ".parquet"))

from_id_col = paste0(geography, "21CD_home")
to_id_col = paste0(geography, "21CD_work")

# -------------------- Analysis


# Get median values per origin
ttd_matrix_o <- ttd_matrix %>%
  group_by(across(from_id_col), combination, departure_time) %>%
  summarise(across(c(contains("_time"), n_rides), median, na.rm = TRUE),
            # number of destinations that can be reached
            reachable_destinations = n(),
            # TODO: number of destinations that can be reached directly
            # reachable_destinations_direct = count(n_rides == 1),
            commute_all = sum(commute_all)) %>%
  ungroup() %>%
  mutate(n_rides = round(n_rides))


# Get median values per destination
ttd_matrix_d <- ttd_matrix %>%
  group_by(across(to_id_col), combination, departure_time) %>%
  summarise(across(c(contains("_time"), n_rides), median, na.rm = TRUE),
            # number of origins that can reach the destination
            reachable_origins = n()) %>%
  ungroup() %>%
  mutate(n_rides = round(n_rides))

# # Get median values per destination (edited to account for r warnings about depreciated fns)
# ttd_matrix_d <- ttd_matrix %>%
#   group_by(across(all_of(to_id_col)), combination, departure_time) %>%
#   summarise(across(c(contains("_time"), n_rides), \(x) median(x, na.rm = TRUE)),
#             # number of origins that can reach the destination
#             reachable_origins = n()) %>%
#   ungroup() %>%
#   mutate(n_rides = round(n_rides))

########### ------------------------ Bus coverage of travel demand ------------------------ ###########


# --- data on which OD pairs are covered by direct buses
od_supply <- arrow::read_parquet(paste0("data/interim/travel_demand/", toupper(geography), "/od_pairs_bus_coverage.parquet"))


# get number of unique trips (directional routes) that serve each OD pair, and their headway
od_supply_avg <- od_supply %>%
  group_by(Origin, Destination, start_time) %>%
  summarise(number_of_trips = n(),
            headway_sec_avg = round(mean(headway_secs)),
            headway_sec_min = min(headway_secs),
            bus_per_hr_avg = round(3600/headway_sec_avg),
            bus_per_hr_min = round(3600/headway_sec_min)) %>%
  ungroup()

# filter od pairs by euclidian distance
od_supply_filtered = filter_matrix_by_distance(zones = study_area,
                                               od_matrix = od_supply_avg,
                                               dist_threshold = 1000)




########## --------------- Combine ttd matrices with bus service provision matrix --------------- ##########

# combine ttd matrix with matrix showing bus provision for ODs
od_sd <- od_supply_filtered %>%
  #TODO: check if we need an inner join (we only have 1 start time atm)
  left_join(ttd_matrix,
            by = c("Origin" = from_id_col,
                   "Destination" = to_id_col,
                   "start_time" = "departure_time"))


########## --------------- Plots --------------- ##########

# --------------- Preprocessing

# ---------- get desire lines

od_sd_sf <- od::od_to_sf(x = od_sd %>% st_drop_geometry(),
                         z = study_area %>% st_cast("MULTIPOLYGON"),
                         crs = 4326)

# ---------- create factor column for demand (for facet plots)

od_sd_sf <- od_sd_sf %>%
  mutate(commute_all_fct = cut_interval(commute_all, n = 5),
         bus_per_hr_min_fct = cut_interval(bus_per_hr_min, n = 5))

# ---------- get data for specific scenario

ttd_matrix_od = od::od_to_sf(x = ttd_matrix,
                             z = study_area %>% st_cast("MULTIPOLYGON"))

# pt weekday morning
ttd_matrix_od_1 <- ttd_matrix_od %>%
  filter(combination == "pt_wkday_morning") %>%
  filter(.data[[from_id_col]] != .data[[to_id_col]])


ttd_matrix_od_2 = filter_matrix_by_distance(zones = study_area,
                                            od_matrix = st_drop_geometry(ttd_matrix_od),
                                            dist_threshold = 1000)

# Which OD pairs are served by a direct bus?
#
#   map 1:
#
#   background: All OD pairs
#
# foreground: od pairs served by direct bus
#
# color = travel demand on OD pair
#

tm_shape(ttd_matrix_od) +
  tm_lines(col = "grey80",
           alpha = 0.05) +
tm_shape(ttd_matrix_od %>%
           filter(commute_all > quantile(commute_all, probs = 0.90, na.rm = TRUE))) +
  tm_lines(col = "commute_all",
           lwd = "commute_all",
           scale = 10)


ttd_matrix_od %>%
  filter(commute_all > quantile(commute_all, probs = 0.95)) -> x

ttd_matrix_od %>%
  mutate(factor_column = cut_interval(commute_all, n = 4)) -> x


tm_shape(x) +
  tm_lines(col = "grey80",
           alpha = 0.02) +
tm_shape(x) +
  tm_lines(col = "factor_column",
           lwd = "commute_all",
           palette = "YlOrRd",
           scale = 7) +
  tm_facets(by = "factor_column",
            free.coords = FALSE,
            nrow = 2)


# map 2:
#
#   background: all OD pairs
#
# foreground: OD pairs with high demand
#
# color: served by direct bus (YES / NO)
#
# color: served by direct bus (color gradient = bus frequency)

tm_shape(od_sd_sf) +
  tm_lines(col = "grey90",
           alpha = 0.5) +
tm_shape(od_sd_sf) +
  tm_lines(col = "number_of_trips",
           lwd = "bus_per_hr_min",
           scale = 5)

tm_shape(od_sd_sf) +
  tm_lines(col = "grey90",
           alpha = 0.5) +
tm_shape(od_sd_sf) +
  tm_lines(col = "number_of_trips",
           lwd = "bus_per_hr_min",
           scale = 5) +
tm_facets(by = "commute_all_fct",
          free.coords = FALSE,
            nrow = 2)

tm_shape(od_sd_sf) +
  tm_lines(col = "grey90",
           alpha = 0.5) +
  tm_shape(od_sd_sf) +
  tm_lines(col = "commute_all",
           lwd = "number_of_trips",
           scale = 5) +
  tm_facets(by = "bus_per_hr_min_fct",
            free.coords = FALSE,
            nrow = 2)

tm_shape(study_area) +
  tm_borders(col = "grey80",
           alpha = 0.5) +
tm_shape(od_sd_sf) +
  tm_lines(col = "grey90",
           alpha = 0.5) +
  tm_shape(od_sd_sf) +
  tm_lines(col = "commute_all",
           lwd = "bus_per_hr_min",
           scale = 5) +
  tm_facets(by = "bus_per_hr_min_fct",
            free.coords = FALSE,
            nrow = 2)


