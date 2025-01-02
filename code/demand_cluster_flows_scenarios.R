# the purpose of this script is to prepare the flow data for spatial clustering #
# This is done by jittering the flows so that it is not concentrated at zone centroids
#
library(tidyverse)
library(sf)
#library(lwgeom)


########## ----------------------- Read in the data ----------------------- ##########

# is the data disaggregated by mode?
mode <- FALSE


# ----------- 1. Study area

# convert to desired resolution
geography = "MSOA"

# ----------- 2.  Census OD data

# Demand (census) + supply (travel time) data

od_demand <- st_read(paste0("data/raw/travel_demand/cpc_matrices_2019/demand_study_area_", tolower(geography), "_with_speed_and_pd.geojson"))

od_demand = od_demand %>%
  mutate(od_id = paste0(Origin, "-", Destination, "-", combination, "-", row_number()))



########## ----------------------- Decide on the SCENARIOS we want to analyse ----------------------- ##########

# Scenario 1: All OD pairs
# Scenario 2: All OD pairs with poor PT supply
# Scenario 3: All OD pairs with poor PT supply and low potential demand

# ----- Option 1: All OD pairs
od_demand_1 <- od_demand

# ----- Option 2: OD pairs with poor PT supply (many transfers or low travel speed)
od_demand_2 <- od_demand %>%
  # transfers - NA transfers means there is no option to go by bus
  filter(n_rides > 2 | is.na(n_rides) |
           speed_percentile < 0.5 | is.na(speed_percentile))

# ----- Option 3: OD pairs with poor PT supply and low potential demand
# get percentiles
od_demand_3 <- od_demand %>%
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
od_demand_scenarios <- od_demand %>%
  mutate(scenario_1 = case_when(od_id %in% od_demand_1$od_id ~ 1,
                                TRUE ~ 0),
         scenario_2 = case_when(od_id %in% od_demand_2$od_id ~ 1,
                                TRUE ~ 0),
         scenario_3 = case_when(od_id %in% od_demand_3$od_id ~ 1,
                                TRUE ~ 0)
  )

# remove rows that have no flows
od_demand_scenarios = od_demand_scenarios %>%
  filter(total_flow != 0)

### Save the sfs for each scenario

st_write(od_demand_scenarios, paste0("data/interim/travel_demand/", geography, "/od_demand_jittered_for_clustering_scenarios_temporal.geojson"), delete_dsn = TRUE)






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
