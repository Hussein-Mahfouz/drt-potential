# the purpose of this script is to prepare the flow data for spatial clustering #
# This is done by jittering the flows so that it is not concentrated at zone centroids

library(tidyverse)
library(sf)
#library(lwgeom)
# jittering
library(odjitter)


source("R/study_area_geographies.R")
source("R/filter_od_matrix.R")


########## ----------------------- Read in the data ----------------------- ##########

# is the data disaggregated by mode?
mode <- TRUE
#mode <- FALSE


# ----------- 1. Study area

# --- administrative boundaries
study_area <- st_read("data/interim/study_area_boundary.geojson")

# convert to desired resolution
geography = "MSOA"
study_area = study_area_geographies(study_area = study_area,
                                    geography = geography)

study_area <- study_area %>%
  st_cast("MULTIPOLYGON")

# move the geographic ID to the first column. od::points_to_od() only keeps the first column as ID

geoid_col = paste0(geography, "21CD")

study_area <- study_area %>%
  relocate(all_of(geoid_col), .before = everything())

# ----------- 2.  Census OD data

# Demand (census) + supply (travel time) data

if(mode == FALSE){
  # data with "commute_all" only
  #od_demand <- arrow::read_parquet(paste0("data/raw/travel_demand/od_census_2021/demand_study_area_", tolower(geography), ".parquet"))
  od_demand <- arrow::read_parquet(paste0("data/raw/travel_demand/od_census_2021/demand_study_area_", tolower(geography), "_with_speed_and_pd.parquet"))
} else{
  # data with modes
  #od_demand <- arrow::read_parquet(paste0("data/raw/travel_demand/od_census_2021/demand_study_area_", tolower(geography), "_mode.parquet"))
  od_demand <- arrow::read_parquet(paste0("data/raw/travel_demand/od_census_2021/demand_study_area_", tolower(geography), "_mode_with_speed_and_pd.parquet"))
}

# filter to specific combination
# TODO: get seperate flows for car and pt, and keep two combinations
od_demand <- od_demand %>%
  filter(combination == "pt_wkday_morning")

od_demand <- od_demand %>%
  select(-distance_m)

# # rename columns as most functions are applied on generic column names
# from_id_col = paste0(geography, "21CD_home")
# to_id_col = paste0(geography, "21CD_work")
#
# od_demand = od_demand %>%
#   rename("Origin" = all_of(from_id_col),
#          "Destination" = all_of(to_id_col))

########## ----------------------- Convert df to sf desire lines ----------------------- ##########


# --- create desire lines and remove od pairs with very short distance

# TODO: edit this to avoid clusters of very short flows
# "Density-based clustering for bivariate-flow data" (section 5.2): preprocessing step to avoid
# clusters of very short flows. this involves splitting the data into 3 chunks
# based on length (
od_demand_filtered = filter_matrix_by_distance(zones = study_area,
                                               od_matrix = od_demand,
                                               dist_threshold = 1000)

# add unique id for each row
od_demand_filtered <- od_demand_filtered %>%
  mutate(od_id = paste0(Origin, "-", Destination, "-", combination))



########## ----------------------- Jitter the points ----------------------- ##########

#####  ----- STEP 1: Layer to use as subpoints

# Population Density Grid - Source: WorldPop https://hub.worldpop.org/

# load in the layer
sub_zones = terra::rast("data/external/population_density_grid_uk/gbr_pd_2020_1km_UNadj.tif")

# convert from raster to vector
sub_zones = sub_zones %>%
  terra::as.points() %>%
  sf::st_as_sf()

# filter to geographic extent
sub_zones <- sub_zones %>%
  st_transform(st_crs(study_area)) %>%
  st_filter(study_area)

# rename population density column
sub_zones <- sub_zones %>%
  rename(population = gbr_pd_2020_1km_UNadj)




##### ----- STEP 2: Jittering

# # --- clear temp directory:
unlink(paste0(normalizePath(tempdir()), "/", dir(tempdir())), recursive = TRUE)
# confirm it's empty
dir(tempdir())

od_demand_for_jittering <- od_demand_for_jittering %>%
  select(Origin, Destination, starts_with("commute_"))

# arguments are here: https://github.com/dabreegster/odjitter?tab=readme-ov-file#details

od_demand_jittered = odjitter::jitter(

  # ----- arguments for FLOW DATA ----- #

  od = od_demand_filtered,
  # column in "od" that specifies where trips originate
  origin_key = "Origin",
  destination_key = "Destination",
  # column with the flows (to be disaggregated)
  disaggregation_key = "commute_all",
  # What's the maximum number of trips per output OD row that's allowed?
  disaggregation_threshold = 95,

  # ----- arguments for ZONES ----- #

  zones = study_area,
  zone_name_key = paste0(geography, "21CD"),

  # ----- Arguments for SUBPOINTS ----- #

  # subpoints to jitter origins and destinations to
  subpoints = sub_zones,
  # # alternatively, define different points for origins and destinations
  # subpoints_origins = points_home,
  # subpoints_destinations = points_work,

  # If specified, this column will be used to more frequently choose subpoints in `subpoints_origins_path` with a higher weight value.
  # Otherwise all subpoints will be equally likely to be chosen
  weight_key_origins = population,
  weight_key_destinations = population,

  # ----- arguments OTHER ----- #

  # Guarantee that jittered origin and destination points are at least this distance apart
  min_distance_meters = 500,
  deduplicate_pairs = TRUE
)

# jittered returns fractions. Round them
od_demand_jittered <- od_demand_jittered %>%
  mutate(across(starts_with("commute_"), round))




########## ----------------------- Decide on the SCENARIOS we want to analyse ----------------------- ##########

# Scenario 1: All OD pairs
# Scenario 2: All OD pairs with poor PT supply
# Scenario 3: All OD pairs with poor PT supply and low potential demand

# ----- Option 1: All OD pairs

od_demand_1 <- od_demand_filtered

# ----- Option 2: OD pairs with poor PT supply (many transfers or low travel speed)

od_demand_2 <- od_demand_filtered %>%
  # transfers - NA transfers means there is no option to go by bus
  filter(n_rides > 1 | is.na(n_rides) |
           speed_percentile < 0.5 | is.na(speed_percentile))


# ----- Option 3: OD pairs with poor PT supply and low potential demand

# get percentiles
od_demand_3 <- od_demand_filtered %>%
  mutate(demand_route_percentile = percent_rank(potential_demand_equal_split),
         demand_route_percentile_fct = cut(demand_route_percentile,
                                           breaks = seq(0, 1, by = 0.25),
                                           include.lowest = TRUE))

# od_filtered: keeps od pairs in od_demand_poor_Supply that have low pd on routes
od_demand_3 <- od_demand_3 %>%
  filter(od_id %in% od_demand_2$od_id & demand_route_percentile < 0.5)



### -----  Add a column to identify which scenarios each od pair belongs to

# od_demand_jittered <- st_read(paste0("data/interim/travel_demand/", geography, "/od_demand_jittered_for_clustering_mode.geojson"))

# add a column to identify which scenarios each od pair belongs to
od_demand_jittered_scenarios <- od_demand_jittered %>%
  mutate(od_id = paste0(Origin, "-", Destination, "-", combination)) %>%
  mutate(scenario_1 = case_when(od_id %in% od_demand_1$od_id ~ 1,
                                TRUE ~ 0),
         scenario_2 = case_when(od_id %in% od_demand_2$od_id ~ 1,
                                TRUE ~ 0),
         scenario_3 = case_when(od_id %in% od_demand_3$od_id ~ 1,
                                TRUE ~ 0)
  )


### Save the sfs for each scenario


if(mode == FALSE){
  # data with "commute_all" only
  st_write(od_demand_jittered_scenarios, paste0("data/interim/travel_demand/", geography, "/od_demand_jittered_for_clustering_scenarios.geojson"), delete_dsn = TRUE)

} else{
  # data with modes
  st_write(od_demand_jittered_scenarios, paste0("data/interim/travel_demand/", geography, "/od_demand_jittered_for_clustering_scenarios_mode.geojson"), delete_dsn = TRUE)
  }

