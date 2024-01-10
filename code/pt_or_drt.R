# This script proposes exploratory methods for answering the "PT vs DRT" question on a route-level #

library(tidyverse)
library(sf)
library(dodgr)


source("R/study_area_geographies.R")
source("R/filter_od_matrix.R")


# ----------- 1. Load in the data


# ----- Study area

# --- administrative boundaries
study_area <- st_read("data/interim/study_area_boundary.geojson")

# convert to desired resolution
geography = "MSOA"
study_area = study_area_geographies(study_area = study_area,
                                    geography = geography)

study_area <- study_area %>%
  st_cast("MULTIPOLYGON")




# ----- Travel time and demand matrix

od_demand <- arrow::read_parquet(paste0("data/raw/travel_demand/od_census_2021/demand_study_area_", tolower(geography), ".parquet"))

# ---------- 2. Prep the loaded data

# ----- rename Origin and Destination columns so that they are agnostic to geographic resolution
from_id_col = paste0(geography, "21CD_home")
to_id_col = paste0(geography, "21CD_work")

# rename for OD matrix
od_demand <- od_demand %>%
  rename(Origin = all_of(from_id_col),
         Destination = all_of(to_id_col))

# rename for study area
study_area <- study_area %>%
  rename(geoID = all_of(paste0(geography, "21CD")))
# prep study area centroids

study_area_centroid <- study_area %>%
  select(geoID) %>%
  st_centroid() %>%
  # transform to lon lat
  st_transform(4326)

# ---------- 2. METHOD A: Overline

# --- a. Filter OD pairs to those needing an improved service

# OD pairs without direct route
od_demand_no_direct = od_demand %>%
  filter(n_rides > 1)

#test
od_demand_2 <- od_demand %>% filter(combination == "pt_wkday_morning")
# --- b. get shortest path geometry for each OD pair
od_demand_no_direct_t <- od_demand_no_direct %>%
  filter(combination == "pt_wkday_morning")


# get origin and destination dfs for routing
from = od_demand_2 %>%
  select(Origin) %>%
  inner_join(study_area_centroid, by = c("Origin" = "geoID")) %>%
  st_as_sf() %>%
  st_coordinates(.) %>%
  as.matrix()

to = od_demand_2 %>%
  select(Destination) %>%
  inner_join(study_area_centroid, by = c("Destination" = "geoID")) %>%
  st_as_sf() %>%
  st_coordinates(.) %>%
  as.matrix()

od_demand_2 %>%
  select(Origin, Destination, commute_all) %>%
  pivot_wider(names_from = Destination, values_from = commute_all) %>%
  tibble::column_to_rownames(var = "Origin") %>%
  mutate(across(everything(), .fns = ~replace_na(.,0))) -> x

# --- extract street network for routing
streetnet_sc = dodgr::dodgr_streetnet_sc(pts = st_coordinates(study_area_centroid$geometry), )
streetnet_sf = dodgr::dodgr_streetnet(pts = st_coordinates(study_area_centroid$geometry))


# --- weight the street network

# # write the weighting profile locally so you can edit it
# dodgr::write_dodgr_wt_profile(file = "data/external/dodgr/weighting_profile_template")
#
# weight_profile = jsonlite::read_json("data/external/dodgr/weighting_profile_template.json")

graph <- weight_streetnet(streetnet_sc, wt_profile = "motorcar")


# --- routing
graph_flow <- dodgr_flows_aggregate(graph = graph,
                                    from = from,
                                    to = to,
                                    flows = x,
                                    contract = TRUE)

# --- c. Use overline to get total demand on each road segment

# not just indirect, but low frequency also
od_demand_no_direct_2 <- od_demand %>%
  filter(n_rides > 1 | wait_time > 30)





graph1 <- weight_streetnet (hampi, wt_profile = "foot")
set.seed (1)
from1 <- sample (graph$from_id, size = 10)
to1 <- sample (graph$to_id, size = 10)
flows1 <- matrix (10 * runif (length (from1) * length (to1)),
                 nrow = length (from1)
)

