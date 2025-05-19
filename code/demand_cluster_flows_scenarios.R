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








# ------------------------------------------------------------------------------
# SENSITIVITY ANALYSIS OF OD FILTERING: This section performs a sensitivity
# analysis for the speed and demand percentile cutoffs used to filter OD pairs.
# We create a grid of % cutoffs and calculate the number of OD pairs that
# meet the criteria for each combination of cutoffs. This should help us
# understand the impact of these cutoffs on the number of OD pairs retained.
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Function to filter OD demand and return number of rows meeting criteria
# ------------------------------------------------------------------------------
filter_od_demand <- function(speed_cutoff, demand_cutoff, od_data,
                             percentile_method = c("all", "nonzero_only")) {

  percentile_method <- match.arg(percentile_method)

  # ----------------------------------------------------------------------------
  # Add percentile ranks for potential demand using one of two methods:
  # ----------------------------------------------------------------------------
  # 1. "all"           → Rank all OD pairs, including those with zero demand.
  #                      This spreads the full range of percentiles across all values.
  #
  # 2. "nonzero_only"  → Compute percentiles **only for non-zero** values of
  #                      potential_demand_equal_split.
  #                      Zero-demand OD pairs are assigned a percentile of 0.
  #
  # This affects how many OD pairs are considered "low demand" when applying
  # the demand_cutoff threshold.
  # ----------------------------------------------------------------------------


  od_data <- od_data %>%
    mutate(
      demand_route_percentile = case_when(
        percentile_method == "all" ~ percent_rank(potential_demand_equal_split),
        percentile_method == "nonzero_only" ~ {
          tmp <- potential_demand_equal_split
          ranks <- rep(0, length(tmp))
          non_zero <- tmp > 0
          ranks[non_zero] <- percent_rank(tmp[non_zero])
          ranks
        }
      )
    )

  od_data %>%
    filter(
      # Poor PT supply logic
      n_rides > 2 | is.na(n_rides) |
        speed_percentile < speed_cutoff | is.na(speed_percentile),
      # Low demand logic
      demand_route_percentile < demand_cutoff
    ) %>%
    nrow()
}

# ------------------------------------------------------------------------------
# Generate cutoff grid and calculate total OD pairs
# ------------------------------------------------------------------------------
cutoffs <- seq(0.30, 1.00, by = 0.05)
total_od_pairs <- nrow(od_demand)

sensitivity_grid <- expand.grid(
  speed_cutoff = cutoffs,
  demand_cutoff = cutoffs
)

# ------------------------------------------------------------------------------
# Function to apply filter logic and generate results table for a given method
# ------------------------------------------------------------------------------
get_sensitivity_results <- function(method = c("all", "nonzero_only")) {
  method <- match.arg(method)

  sensitivity_grid %>%
    rowwise() %>%
    mutate(
      n_filtered = filter_od_demand(speed_cutoff, demand_cutoff, od_demand,
                                    percentile_method = method),
      pct_filtered = round(n_filtered / total_od_pairs * 100, 1)
    ) %>%
    ungroup()
}

# ------------------------------------------------------------------------------
# Get results for both percentile methods
# ------------------------------------------------------------------------------
results_all <- get_sensitivity_results("all")
results_nonzero <- get_sensitivity_results("nonzero_only")

# ------------------------------------------------------------------------------
# Create label and percentage tables (wide format) for display
# ------------------------------------------------------------------------------
make_display_tables <- function(results_df) {
  label_table <- results_df %>%
    mutate(cell_label = paste0(n_filtered, " (", pct_filtered, "%)")) %>%
    select(speed_cutoff, demand_cutoff, cell_label) %>%
    pivot_wider(
      names_from = speed_cutoff,
      values_from = cell_label,
      names_prefix = "Speed < "
    )

  pct_table <- results_df %>%
    select(speed_cutoff, demand_cutoff, pct_filtered) %>%
    pivot_wider(
      names_from = speed_cutoff,
      values_from = pct_filtered,
      names_prefix = "Speed < "
    )

  list(label = label_table, pct = pct_table)
}

# Get formatted tables
tables_all <- make_display_tables(results_all)
tables_nonzero <- make_display_tables(results_nonzero)

# ------------------------------------------------------------------------------
# Display results using gt
# ------------------------------------------------------------------------------
gt_pct_coloured <- function(pct_table) {
  gt(pct_table) %>%
    data_color(
      columns = -c(demand_cutoff),
      method = "numeric",
      palette = "Blues",
      direction = "row",
      domain = c(0, 100),
      bins = 5
    )
}

# View tables (You can assign these to gt objects or view inline in RStudio)
gt_pct_coloured(tables_all$pct)      # For "all" method
gt_pct_coloured(tables_nonzero$pct)  # For "nonzero_only" method

gt(tables_all$label)                 # With labels for "all"
gt(tables_nonzero$label)            # With labels for "nonzero_only"




