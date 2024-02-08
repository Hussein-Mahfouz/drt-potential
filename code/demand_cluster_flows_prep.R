# the purpose of this script is to prepare the flow data for spatial clustering #
# This is done by jittering the flows so that it is not concentrated at zone centroids

library(tidyverse)
library(sf)
library(lwgeom)
# jittering
library(odjitter)


source("R/study_area_geographies.R")
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

# move the geographic ID to the first column. od::points_to_od() only keeps the first column as ID

geoid_col = paste0(geography, "21CD")

study_area <- study_area %>%
  relocate(all_of(geoid_col), .before = everything())

# ----------- 2.  Census OD data

# Demand (census) + supply (travel time) data

od_demand <- arrow::read_parquet(paste0("data/raw/travel_demand/od_census_2021/demand_study_area_", tolower(geography), ".parquet"))

# filter to specific combination
# TODO: get seperate flows for car and pt, and keep two combinations
od_demand <- od_demand %>%
  filter(combination == "pt_wkday_morning")

# rename columns as most functions are applied on generic column names
from_id_col = paste0(geography, "21CD_home")
to_id_col = paste0(geography, "21CD_work")

od_demand = od_demand %>%
  rename("Origin" = all_of(from_id_col),
         "Destination" = all_of(to_id_col))

########## ----------------------- Convert df to sf desire lines ----------------------- ##########


# --- create desire lines and remove od pairs with very short distance

# TODO: edit this to avoid clusters of very short flows
# "Density-based clustering for bivariate-flow data" (section 5.2): preprocessing step to avoid
# clusters of very short flows. this involves splitting the data into 3 chunks
# based on length (
od_demand_filtered = filter_matrix_by_distance(zones = study_area,
                                               od_matrix = od_demand,
                                               dist_threshold = 500)



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
  mutate(commute_all = round(commute_all))


# ---------- save output

st_write(od_demand_jittered, paste0("data/interim/travel_demand/", geography, "/od_demand_jittered_for_clustering.geojson"), delete_dsn = TRUE)




