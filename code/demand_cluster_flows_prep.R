# the purpose of this script is to prepare the flow data for spatial clustering #
# This is done by jittering the flows so that it is not concentrated at zone centroids
#
library(tidyverse)
library(sf)
#library(lwgeom)
# jittering
library(odjitter)
# plots at the bottom
library(ggridges)



source("R/study_area_geographies.R")
source("R/filter_od_matrix.R")


########## ----------------------- Read in the data ----------------------- ##########

# is the data disaggregated by mode?
mode <- FALSE


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

od_demand <- arrow::read_parquet(paste0("data/raw/travel_demand/cpc_matrices_2019/demand_study_area_", tolower(geography), "_with_speed_and_pd.parquet"))

# # filter to specific combination
# # TODO: get seperate flows for car and pt, and keep two combinations
# od_demand <- od_demand %>%
#   filter(combination == "pt_wkday_morning")

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


# --- Create more points (if we don't have enough points, then odjitter may not find a feasible solution)


#' Split Points into Multiple Offset Points with CRS Transformation
#'
#' This function takes an `sf` object containing spatial points and splits each
#' point into multiple new points that are offset from the original points.
#' Each new point represents an equal share of a specified numeric column value,
#' and is randomly distributed within a given distance range from the original
#' point. The function transforms the CRS to a specified metric CRS for
#' processing and then converts it back to the original CRS.
#'
#' @param sf_points An `sf` object representing multiple spatial points with
#'   geometry and a numeric column to be split.
#' @param col_to_split A string indicating the name of the numeric column to
#'   be split across the new points.
#' @param splits An integer indicating the number of new points to generate
#'   from each original point.
#' @param offset_dist_min A numeric value specifying the minimum distance
#'   (in meters) for the offsets of the new points from the original points.
#' @param offset_dist_max A numeric value specifying the maximum distance
#'   (in meters) for the offsets of the new points from the original points.
#' @param target_crs A numeric EPSG code or a PROJ string for the target
#'   metric CRS to be used during processing.
#'
#' @return An `sf` object containing the new points, with the specified
#'   numeric column value equally distributed among the generated points.
#'
#' @examples
#' # Example usage
#' library(sf)
#' sf_points <- st_as_sf(data.frame(id = 1,
#'                                   population = 100,
#'                                   geometry = st_sfc(st_point(c(1, 1)),
#'                                                     crs = 4326)))
#' result <- split_points(sf_points, col_to_split = "population",
#'                        splits = 5, offset_dist_min = 10,
#'                        offset_dist_max = 20, target_crs = 32630)
#' plot(st_geometry(result))
split_points <- function(sf_points, col_to_split, splits, offset_dist_min,
                         offset_dist_max, target_crs) {

  # Ensure that the specified column exists in the sf object
  if (!col_to_split %in% names(sf_points)) {
    stop(paste("Column", col_to_split, "does not exist in the sf object."))
  }

  # Store the original CRS
  original_crs <- st_crs(sf_points)

  # Transform to the target CRS
  sf_points_metric <- st_transform(sf_points, target_crs)

  # Create an empty list to store new points
  new_points_list <- vector("list", nrow(sf_points_metric))

  # Loop through each point in the sf object
  for (i in 1:nrow(sf_points_metric)) {
    point <- sf_points_metric[i, ]
    population <- point[[col_to_split]]  # Get the value of the specified column

    # Calculate new population per point
    new_col_to_split <- population / splits

    # Generate new points with offsets
    offsets <- lapply(1:splits, function(index) {
      angle <- runif(1, 0, 2 * pi)  # Random angle in radians
      offset_dist <- runif(1, min = offset_dist_min, max = offset_dist_max)
      offset_x <- offset_dist * cos(angle)
      offset_y <- offset_dist * sin(angle)
      st_point(c(st_coordinates(point)[1] + offset_x,
                 st_coordinates(point)[2] + offset_y))
    })

    # Create sf object for new points
    new_points_sf <- st_sf(
      # TODO: take column name from funciton argument instead of hardcoding
      population = rep(new_col_to_split, splits),  # Dynamically assign the column name
      geometry = st_sfc(offsets, crs = st_crs(point))
    )

    # Append the new points sf object to the list
    new_points_list[[i]] <- new_points_sf
  }

  # Combine all new points into a single sf object
  result_sf <- do.call(rbind, new_points_list)

  # Transform back to the original CRS
  result_sf <- st_transform(result_sf, original_crs)

  return(result_sf)
}

# apply function
sub_zones_2 <- split_points(sub_zones,
                            col_to_split = "population",
                            splits = 5,
                            offset_dist_min = 100,
                            offset_dist_max = 500,
                            target_crs = 3857)



#######

##### ----- STEP 2: Jittering

# # --- clear temp directory:
unlink(paste0(normalizePath(tempdir()), "/", dir(tempdir())), recursive = TRUE)
# confirm it's empty
dir(tempdir())

# selected_combination = "pt_wkday_evening"

od_demand_for_jittering <- od_demand_filtered %>%
  #filter(combination == selected_combination) %>%
  select(Origin, Destination, total_flow, combination)

# arguments are here: https://github.com/dabreegster/odjitter?tab=readme-ov-file#details

od_demand_jittered = odjitter::jitter(

  # ----- arguments for FLOW DATA ----- #

  od = od_demand_for_jittering,
  # column in "od" that specifies where trips originate
  origin_key = "Origin",
  destination_key = "Destination",
  # column with the flows (to be disaggregated)
  disaggregation_key = "total_flow",
  # What's the maximum number of trips per output OD row that's allowed?
  disaggregation_threshold = 30,

  # ----- arguments for ZONES ----- #

  zones = study_area,
  zone_name_key = paste0(geography, "21CD"),

  # ----- Arguments for SUBPOINTS ----- #

  # subpoints to jitter origins and destinations to
  subpoints = sub_zones_2,
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

# TODO: remove when jittering is fixed
# od_demand_jittered <- od_demand_filtered

# jittered returns fractions. Round them
od_demand_jittered <- od_demand_jittered %>%
  mutate(across(starts_with("total_flow"), round))

# # ---------- check if new ODs are in the same zone paairs as the old ODs ----------

od_demand_jittered_test <- od_demand_jittered %>%
  mutate(Origin_new = lwgeom::st_startpoint(geometry),
         Destination_new = lwgeom::st_endpoint(geometry)) %>%
  st_set_geometry("Origin_new") %>%
  st_join(study_area %>% select(MSOA21CD) %>%
            rename(MSOA_study_area_O = MSOA21CD)) %>%
  st_set_geometry("Destination_new") %>%
  st_join(study_area %>% select(MSOA21CD) %>%
            rename(MSOA_study_area_D = MSOA21CD)) %>%
  mutate(Origin_match = MSOA_study_area_O == Origin,
         Destination_match = MSOA_study_area_D == Destination)


# plot to check
plot(st_geometry(study_area))
plot(st_geometry(od_demand_jittered %>%
                   filter(Origin == "E02002331", Destination == "E02002335")),
     add = TRUE, col = "red")

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
  #filter(od_id %in% od_demand_2$od_id & potential_demand_equal_split < 500)
  filter(od_id %in% od_demand_2$od_id & demand_route_percentile < 0.75)



### -----  Add a column to identify which scenarios each od pair belongs to

# IMPORTANT: Read this as jittering has stopped working
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

st_write(od_demand_jittered_scenarios, paste0("data/interim/travel_demand/", geography, "/od_demand_jittered_for_clustering_scenarios_temporal.geojson"), delete_dsn = TRUE)






#
#
#
# # ---------------------- plot distributions
#
# plots_path <- "data/processed/plots/eda/speed_demand_cutoffs/"
#
# # ---------- SPEED
#
# # histogram
#
# od_demand_filtered %>%
#   #mutate(speed_kph = replace_na(speed_kph, 0)) %>%
#   ggplot(aes(x = speed_kph)) +
#   geom_histogram(binwidth = 1, alpha = 0.8) +
#   labs(title = "Average speeds between ODs using PT",
#        subtitle = "All reachable OD pairs",
#        x = "Speed (kph)",
#        y = "No. of OD pairs")
#
# ggsave(filename = paste0(plots_path, "plot_hist_speeds_reachable_od.png"))
#
#
#
# # histogram: Keep speed_kph = NA (replace with 0)
#
# od_demand_filtered %>%
#   mutate(speed_kph = replace_na(speed_kph, 0)) %>%
#   ggplot(aes(x = speed_kph)) +
#   geom_histogram(binwidth = 1, alpha = 0.8) +
#   labs(title = "Average speeds between ODs using PT",
#        subtitle = "All OD pairs",
#        x = "Speed (kph)",
#        y = "No. of OD pairs")
#
# ggsave(filename = paste0(plots_path, "plot_hist_speeds_all_od.png"))
#
# # density plot: facet by demand percentile
#
# od_demand_filtered %>%
#   ggplot(aes(x=speed_kph, y=demand_percentile_fct, fill = factor(stat(quantile)))) +
#   stat_density_ridges(
#     geom = "density_ridges_gradient", calc_ecdf = TRUE,
#     quantiles = 4, quantile_lines = TRUE
#   ) +
#   scale_fill_brewer(name = "Quartiles") +
#   labs(title = "Average speeds between ODs using PT",
#        subtitle = "All reachable OD pairs",
#        x = "Speed (kph)",
#        y = "Travel demand on busiest route\nserving OD pair (percentiles)")
#
# ggsave(filename = paste0(plots_path, "plot_dens_speeds_facet_demand_all_od.png"))
#
# # density plot: facet by demand percentile: Keep speed_kph = NA (replace with 0)
#
# od_demand_filtered %>%
#   mutate(speed_kph = replace_na(speed_kph, 0)) %>%
#   ggplot(aes(x=speed_kph, y=demand_percentile_fct, fill = factor(stat(quantile)))) +
#   stat_density_ridges(
#     geom = "density_ridges_gradient", calc_ecdf = TRUE,
#     quantiles = 4, quantile_lines = TRUE
#   ) +
#   scale_fill_brewer(name = "Quartiles") +
#   labs(title = "Average speeds between ODs using PT",
#        subtitle = "All OD pairs",
#        x = "Speed (kph)",
#        y = "Potential demand on busiest route\nserving OD pair (percentiles)")
#
# ggsave(filename = paste0(plots_path, "plot_dens_speeds_facet_demand_reachable_od.png"))
#
#
# # ---------- DEMAND (potential_demand_equal_split)
#
# # histogram
#
# od_demand_filtered %>%
#   #mutate(potential_demand_equal_split = replace_na(potential_demand_equal_split, 0)) %>%
#   ggplot(aes(x = potential_demand_equal_split)) +
#   geom_histogram(bins = 25, alpha = 0.8) +
#   labs(title = "Potential demand on busiest\nPT route serving OD pair",
#        subtitle = "All reachable OD pairs",
#        x = "Potential demand (no. of passengers)",
#        y = "No. of OD pairs")
#
# ggsave(filename = paste0(plots_path, "plot_hist_demand_reachable_od.png"))
#
#
# # histogram: facet by demand percentile
# od_demand_filtered %>%
#   #mutate(potential_demand_equal_split = replace_na(potential_demand_equal_split, 0)) %>%
#   filter(!is.na(speed_percentile_fct)) %>%
#   ggplot(aes(x = potential_demand_equal_split)) +
#   geom_histogram(binwidth = 1000, alpha = 0.8) +
#   labs(title = "Potential demand on busiest\nPT route serving OD pair",
#        subtitle= "Facet = speed percentiles",
#        x = "Potential demand (no. of passengers)",
#        y = "No. of OD pairs") +
#   facet_wrap(vars(speed_percentile_fct), nrow = 2)
#
# ggsave(filename = paste0(plots_path, "plot_hist_demand_facet_speed_reachable_od.png"))
#
# # density plot: facet by demand percentile
#
# od_demand_filtered %>%
#   mutate(potential_demand_equal_split = replace_na(potential_demand_equal_split, 0)) %>%
#   filter(!is.na(speed_percentile_fct)) %>%
#          ggplot(aes(x=potential_demand_equal_split, y=speed_percentile_fct, fill = factor(stat(quantile)))) +
#   stat_density_ridges(
#     geom = "density_ridges_gradient", calc_ecdf = TRUE,
#     quantiles = 4, quantile_lines = TRUE
#   ) +
#   scale_fill_brewer(name = "Quartiles") +
#   labs(title = "Potential demand on busiest\nPT route serving OD pair",
#        x = "Potential demand (no. of passengers)",
#        y = "Speed percentiles")
#
# ggsave(filename = paste0(plots_path, "plot_dens_demand_facet_speeds_all_od.png"))
#
#
# # density plot: facet by demand percentile: Keep potential_demand_equal_split = NA (replace with 0)
#
