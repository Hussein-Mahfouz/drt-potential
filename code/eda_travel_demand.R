# This code visualises the output from code/demand_on_buses.R . The datasets we want to explore together are:
    # demographic data
    # travel time data
    # number of destinations reachable
    # accessibility to different services
    # travel demand data


source("R/study_area_geographies.R")


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


# ----------------------- Plots

# --------------- Preprocessing

# ---------- get desire lines

ttd_matrix_od <- od::od_to_sf(x = ttd_matrix,
                              z = study_area %>% st_cast("MULTIPOLYGON"))

# ---------- get data for specific scenario

# pt weekday morning
ttd_matrix_od_1 <- ttd_matrix_od %>%
  filter(combination == "pt_wkday_morning") %>%
  filter(.data[[from_id_col]] != .data[[to_id_col]])

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

tm_shape(ttd_matrix_od_1) +
  tm_lines(col = "grey80",
           alpha = 0.05) +
tm_shape(ttd_matrix_od_1 %>%
           filter(commute_all > quantile(commute_all, probs = 0.90))) +
  tm_lines(col = "commute_all",
           lwd = "commute_all",
           scale = 10)


ttd_matrix_od_1 %>%
  filter(commute_all > quantile(commute_all, probs = 0.95)) -> x

ttd_matrix_od_1 %>%
  mutate(factor_column = cut_interval(commute_all, n = 4)) -> x

tt
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



tm_shape(study_area) +
  tm_polygons() +
tm_shape(x %>% filter(combination == "pt_wkday_morning",
                      commute_all >= 100)) +
  tm_lines(col = "commute_all")



x = od::od_data_df
z = od::od_data_zones
desire_lines = od::od_to_sf(x, z)
