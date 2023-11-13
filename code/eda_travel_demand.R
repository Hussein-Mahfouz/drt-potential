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


# ----------------------- Plots

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
# map 2:
#
#   background: all OD pairs
#
# foreground: OD pairs with high demand
#
# color: served by direct bus (YES / NO)
#
# color: served by direct bus (color gradient = bus frequency)

x <- od::od_to_sf(x = ttd_matrix[100:2000,],
                  z = study_area %>% st_cast("MULTIPOLYGON"))


tm_shape(study_area) +
  tm_polygons() +
tm_shape(x %>% filter(combination == "pt_wkday_morning",
                      commute_all >= 100)) +
  tm_lines(col = "commute_all")



x = od::od_data_df
z = od::od_data_zones
desire_lines = od::od_to_sf(x, z)
