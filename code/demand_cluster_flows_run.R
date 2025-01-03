library(tidyverse)
library(sf)
library(tmap)

# -------------------- Clustering


sensitivity = FALSE
day_time = "pt_wkday_06_30"
scenario = 2
source("code/demand_cluster_flows.R")
print(paste0("finished scenario: ", day_time))
# clear environment
rm(list = ls())
gc()

sensitivity = FALSE
day_time = "pt_wkday_09_30"
scenario = 2
source("code/demand_cluster_flows.R")
print(paste0("finished scenario: ", day_time))
# clear environment
rm(list = ls())
gc()

sensitivity = FALSE
day_time = "pt_wkday_12_30"
scenario = 2
source("code/demand_cluster_flows.R")
print(paste0("finished scenario: ", day_time))
# clear environment
rm(list = ls())
gc()

sensitivity = FALSE
day_time = "pt_wkday_15_30"
scenario = 2
source("code/demand_cluster_flows.R")
print(paste0("finished scenario: ", day_time))
# clear environment
rm(list = ls())
gc()

sensitivity = FALSE
day_time = "pt_wkday_18_30"
scenario = 2
source("code/demand_cluster_flows.R")
print(paste0("finished scenario: ", day_time))
# clear environment
rm(list = ls())
gc()

geography = "MSOA"

# --- Compare clusters at different points in the day
dbscan_sensitivity_res_wkday_06_30 = arrow::read_parquet(paste0("data/interim/travel_demand/", geography, "/od_demand_clustering_sensitivity_pt_wkday_morning.parquet")) %>%
  mutate(scenario = "pt_wkday_06_30")
dbscan_sensitivity_res_wkday_09_30 = arrow::read_parquet(paste0("data/interim/travel_demand/", geography, "/od_demand_clustering_sensitivity_pt_wkday_morning.parquet")) %>%
  mutate(scenario = "pt_wkday_09_30")
dbscan_sensitivity_res_wkday_12_30 = arrow::read_parquet(paste0("data/interim/travel_demand/", geography, "/od_demand_clustering_sensitivity_pt_wkday_morning.parquet")) %>%
  mutate(scenario = "pt_wkday_12_30")
dbscan_sensitivity_res_wkday_15_30 = arrow::read_parquet(paste0("data/interim/travel_demand/", geography, "/od_demand_clustering_sensitivity_pt_wkday_morning.parquet")) %>%
  mutate(scenario = "pt_wkday_15_30")
dbscan_sensitivity_res_wkday_18_30 = arrow::read_parquet(paste0("data/interim/travel_demand/", geography, "/od_demand_clustering_sensitivity_pt_wkday_morning.parquet")) %>%
  mutate(scenario = "pt_wkday_18_30")


dbscan_sensitivity_res_compare = bind_rows(dbscan_sensitivity_res_wkday_06_30,
                                           dbscan_sensitivity_res_wkday_09_30,
                                           dbscan_sensitivity_res_wkday_12_30,
                                           dbscan_sensitivity_res_wkday_15_30,
                                           dbscan_sensitivity_res_wkday_18_30
                                           )


dbscan_sensitivity_res_compare %>%
  filter(cluster != 0) %>%
  group_by(id) %>%
  #mutate(clusters = n()) %>%
  # How many clusters have more than 5 od pairs in them?
  mutate(clusters = sum(size > 30)) %>%
  ungroup() %>%
  filter(clusters > 25) %>%
  ggplot(aes(x = cluster, y = size, fill = commuters_sum)) +
  geom_col() +
  scale_y_continuous(trans='log10') +
  facet_grid(id ~ scenario, scales = "fixed") +
  labs(title = "Sensitivity analysis for clustering - Varying {eps} and {minPts}",
       subtitle = "Parameter combinations with > 25 clusters having at least 30 od pairs each",
       x = "Cluster no.",
       y = "No. of od pairs in cluster",
       fill= "No. of commuters") +
  theme_bw()


ggsave(paste0("data/processed/plots/eda/od_clustering/temporal/sensitivity_analysis_eps_minpts_filtered_compare.png"), width = 14, height = 10)

rm(list = ls())


# # minimum number of commuters in a cluster for it to be part of our analysis
# commuters_sum_minimum = 50
#
# # -------------- Plots
# day_time = "pt_wkday_06_30"
# source("code/demand_cluster_flows_maps_temporal.R")
# print(paste0("finished scenario: ", day_time))
# # clear environment
# rm(list = setdiff(ls(), "commuters_sum_minimum"))
# gc()
#
#
# day_time = "pt_wkday_09_30"
# source("code/demand_cluster_flows_maps_temporal.R")
# print(paste0("finished scenario: ", day_time))
# # clear environment
# rm(list = setdiff(ls(), "commuters_sum_minimum"))
# gc()
#
#
# day_time = "pt_wkday_12_30"
# source("code/demand_cluster_flows_maps_temporal.R")
# print(paste0("finished scenario: ", day_time))
# # clear environment
# rm(list = setdiff(ls(), "commuters_sum_minimum"))
# gc()
#
#
# day_time = "pt_wkday_15_30"
# source("code/demand_cluster_flows_maps_temporal.R")
# print(paste0("finished scenario: ", day_time))
# # clear environment
# rm(list = setdiff(ls(), "commuters_sum_minimum"))
# gc()
#
#
# day_time = "pt_wkday_18_30"
# source("code/demand_cluster_flows_maps_temporal.R")
# print(paste0("finished scenario: ", day_time))
# # clear environment
# rm(list = setdiff(ls(), "commuters_sum_minimum"))
# gc()

# Predefined list of values for commuters_sum_minimum
commuters_sum_minimum_list <- c(50, 100, 150, 200)
# commuters_sum_minimum_list <- c(200)


# Predefined list of day_time scenarios
day_time_list <- c("pt_wkday_06_30", "pt_wkday_09_30", "pt_wkday_12_30",
                   "pt_wkday_15_30", "pt_wkday_18_30")

# Define scenario values
scenario_list <- c(3, 2)
# scenario_list <- c(2)


# Loop through each scenario
for (scenario in scenario_list) {
  print(paste0("Running for scenario: ", scenario))

  # Loop through each value of commuters_sum_minimum
  for (commuters_sum_minimum in commuters_sum_minimum_list) {
    print(paste0("Running for commuters_sum_minimum: ", commuters_sum_minimum))

    # Loop through each day_time scenario
    for (day_time in day_time_list) {
      # These variables are available for the sourced script
      source("code/demand_cluster_flows_maps_temporal.R")
      print(paste0("Finished scenario: ", day_time,
                   " with commuters_sum_minimum: ", commuters_sum_minimum,
                   " in scenario: ", scenario))

      # Clear environment except the required variables
      rm(list = setdiff(ls(), c("commuters_sum_minimum",
                                "commuters_sum_minimum_list",
                                "day_time_list",
                                "scenario",
                                "scenario_list")))
      gc()
    }

    # Clear all but commuters_sum_minimum_list, day_time_list, and scenario variables
    rm(list = setdiff(ls(), c("commuters_sum_minimum_list",
                              "day_time_list",
                              "scenario",
                              "scenario_list")))
    gc()
  }

  # Clear all but scenario_list, day_time_list, and commuters_sum_minimum_list
  rm(list = setdiff(ls(), c("scenario_list",
                            "day_time_list",
                            "commuters_sum_minimum_list")))
  gc()
}



# ----- Facet map with different times of day

source("R/study_area_geographies.R")
source("R/filter_od_matrix.R")
# ---------------------- Load in data

# --------  1. administrative boundaries
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


# -------- 2. gtfs
gtfs_bus <- st_read("data/interim/gtfs_freq/gtfs_bus_sf_temporal.geojson")


# -------- 3. Pop density base layer

# --- pop density 1km grid
oa_pop_density <- stars::read_stars("data/external/population_density_grid_uk/gbr_pd_2020_1km_UNadj.tif")
# crop to study area
oa_pop_density_crop <- st_crop(oa_pop_density, study_area)

# -------- 4. DRT operating zone polygons

for (scenario in scenario_list) {
  print(paste0("Plotting scenario: ", scenario))

  # Loop through each value of commuters_sum_minimum
  for (commuters_sum_minimum in commuters_sum_minimum_list) {
    print(paste0("Plotting commuters_sum_minimum: ", commuters_sum_minimum))

    directory_path <- paste0("data/processed/plots/eda/od_clustering/MSOA/temporal/polygons_combined/min_commuters_", commuters_sum_minimum, "/")

    # Get the list of all GeoJSON files of DRT boundaries (1 file per scenario)
    geojson_files <- list.files(directory_path,    pattern = paste0("*", scenario, ".geojson$"), full.names = TRUE)

    # Read each GeoJSON file into a list of sf objects
    geojson_list <- purrr::map(geojson_files, st_read)
    # prepare for plotting
    combined_geojson <- do.call(rbind, geojson_list)

    combined_geojson <- combined_geojson %>%
      st_transform(st_crs(study_area)) %>%
      st_make_valid()

    # plot
    tm_shape(st_union(study_area)) +
      tm_borders(lwd =2,
                 col = "grey15") +
      tm_shape(oa_pop_density_crop) +
      tm_raster(title = "People / Km2",
                palette = "Blues",
                alpha = 0.5,
                style = "log10_pretty") +
      # bus layer
      tm_shape(gtfs_bus %>%
                 filter(startsWith(scenario, "pt_wkday")) %>%
                 mutate(headway_inv = (1/headway_secs) * 3600) %>%
                 filter(headway_secs < 7200)) +
      tm_lines(col = "darkred",
               lwd = "headway_inv",
               scale = 5.5,
               palette = "-YlOrRd",
               style = "pretty",
               legend.col.show = FALSE,
               alpha = 0.1,
               title.lwd = "Buses/Hour",
               #legend.lwd.is.portrait = FALSE
      ) +
      tm_facets(by = "scenario",
                #by = "commute_all",
                free.coords = FALSE,
                nrow = 2,
                showNA = FALSE) +
      # ---- clusters
      # poly border
      tm_shape(combined_geojson %>%
                 st_buffer(1000)) +
      tm_borders(col = "darkgreen",
                 lwd = 3.5,
                 lty = "dashed") +
      tm_facets(by = "scenario",
                #by = "commute_all",
                free.coords = FALSE,
                nrow = 2,
                showNA = FALSE) +
      tm_layout(fontfamily = 'Georgia',
                main.title = paste0("Potential DRT Operating Zones (Temporal Variation)"),
                main.title.size = 1.1,
                main.title.color = "azure4",
                main.title.position = "left",
                #legend.outside = TRUE,
                #legend.outside.position = "bottom",
                #legend.stack = "horizontal",
                # remove panel headers
                # panel.show = FALSE,
                panel.label.size = 1,
                panel.label.bg.color = NA,
                #panel.labels = 1:length(unique(clusters_vis_mode_poly_filt3$cluster)),
                frame = FALSE)  +
      # add a couple of legends
      tm_add_legend(type = "line", labels = 'Potential DRT service area', col = 'darkgreen', lwd = 2) -> map_cluster_results_gtfs_overline_poly_bus_diff_pop_density_facet_ALL_TIMES

    map_cluster_results_gtfs_overline_poly_bus_diff_pop_density_facet_ALL_TIMES

    plots_path <- paste0("data/processed/plots/eda/od_clustering/", geography, "/temporal/", "min_commuters_", commuters_sum_minimum)


    tmap_save(tm = map_cluster_results_gtfs_overline_poly_bus_diff_pop_density_facet_ALL_TIMES, filename = paste0(plots_path, "/map_cluster_results_gtfs_overline_poly_bus_diff_pop_density_facet_ALL_TIMES_", "scenario_", scenario, ".png"), width = 12, dpi = 1080, asp = 0)


  }
}










#  ------------------------ Test clustering input  -------------------------- #


od_demand_jittered <- st_read(paste0("data/interim/travel_demand/", geography, "/od_demand_jittered_for_clustering_scenarios_temporal.geojson"))


od_demand_jittered %>%
  st_drop_geometry() %>%
  pivot_longer(cols = c(scenario_1, scenario_2, scenario_3),
               names_to = "scenario") %>%
  group_by(combination, scenario) %>%
  summarise(number_of_ods = sum(value == 1))

