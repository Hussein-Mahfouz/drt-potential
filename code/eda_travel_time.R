###################################################################################################
###    The purpose of this script is to explore outputs from the routing. I look at             ###
###       - Car vs pt travel time                                                               ###
###       - Access, egress, and waiting times                                                   ###
###       - Transfers                                                                           ###
###       - Which OD pairs have no PT route at all?                                             ###
###################################################################################################
source("R/study_area_geographies.R")


library(tidyverse)
library(sf)

# -------------------- read in the outputs

# --- decide on geographic resolution

geography <- "MSOA"

# read in geography
study_area <- st_read("data/interim/study_area_boundary.geojson")
# convert to necessary resolution
study_area <- study_area_geographies(study_area = study_area,
                                     geography = geography)

# --- routing results
tt_matrix <- arrow::read_parquet(paste0("data/processed/travel_times/", geography, "/travel_time_matrix_expanded.parquet"))


# -------------------- Analysis

# Get median values per origin
tt_matrix_o <- tt_matrix %>%
  group_by(from_id, combination, departure_time) %>%
  summarise(across(contains("_time"), median, na.rm = TRUE))

# Get median values per destination
tt_matrix_d <- tt_matrix %>%
  group_by(to_id, combination, departure_time) %>%
  summarise(across(contains("_time"), median, na.rm = TRUE))



# PLOT: BAR - wait time distribution per combination
tt_matrix_o %>%
  filter(grepl("pt_", combination)) %>%
  filter(wait_time > 0) %>%
  ggplot(aes(x = wait_time, fill = combination)) +
  geom_histogram(binwidth = 5, position = "identity", alpha = 0.7) +
  facet_wrap(~combination, scales = "fixed") +
  labs(title = "Wait time distribution at different times of day", subtitle = "Average per origin", x = "Time (min)", y = "No. of origins") +
  theme_minimal() +
  theme(legend.position = "top")


# PLOT: BAR - average time associated with each leg of the trip (access, egress, ride, transfer, wait) - bar chart showing difference between combinations
tt_matrix_o %>% group_by(combination) %>%
  select(-c(departure_time, total_time)) %>%
  filter(combination != "car") %>%
  summarise(across(contains("_time"), median, na.rm = TRUE)) %>%
  pivot_longer(cols = contains("time"), names_to = "leg", values_to = "time") %>%
  ggplot(aes(x = combination, y = time, fill = leg)) +
  geom_bar(position = "stack", stat = "identity") +
  theme_minimal() +
  labs(title = "What makes up the total travel time?")




# bivariate plot with transfer time and no. of transfers  https://r-tmap.github.io/tmap/articles/tmap_sneak_peek.html#multivariate-scales

# number of reachable destinations
   # expanded dataset - any row with total_time == NA means you can't reach
   # group_by origin, combination, departure_time -> count number of rows where total_time !- NA
