# This code visualises the output from code/demand_on_buses.R . The datasets we want to explore together are:
    # demographic data
    # travel time data
    # number of destinations reachable
    # accessibility to different services
    # travel demand data


source("R/study_area_geographies.R")
source("R/filter_od_matrix.R")

library(tidyverse)
library(sf)
library(tmap)

# --- decide on geographic resolution
geography <- "MSOA"

# where do we want to save the plots?
plots_path <- paste0("data/processed/plots/eda/demand/", geography, "/")


# -------------------- read in the outputs

# read in geography
study_area <- st_read("data/interim/study_area_boundary.geojson")
# convert to necessary resolution
study_area <- study_area_geographies(study_area = study_area,
                                     geography = geography)

study_area <- study_area %>%
  st_cast("MULTIPOLYGON")


########### ------------------------ Travel time and travel demand data ------------------------ ###########

# ---------------------- Loading in travel time and travel demand data


# --- travel time and demand (ttd) matrix

#ttd_matrix <- arrow::read_parquet(paste0("data/raw/travel_demand/od_census_2021/demand_study_area_", tolower(geography), ".parquet"))
ttd_matrix <- arrow::read_parquet(paste0("data/raw/travel_demand/od_census_2021/demand_study_area_", tolower(geography), "_with_speed_and_pd.parquet"))
# TODO: move this line of code to script that creates n_rides (probably routing_r5r or r5r_routing_wrappers (where rows are summarised))
ttd_matrix <- ttd_matrix %>%
  mutate(n_rides = round(n_rides))

# from_id_col = paste0(geography, "21CD_home")
# to_id_col = paste0(geography, "21CD_work")
from_id_col = "Origin"
to_id_col = "Destination"
# -------------------- Analysis


# Get median values per origin
ttd_matrix_o <- ttd_matrix %>%
  group_by(across(all_of(from_id_col)), combination, departure_time) %>%
  summarise(across(c(ends_with("_time"), n_rides), median, na.rm = TRUE),
            # number of destinations that can be reached
            reachable_destinations = n(),
            # TODO: number of destinations that can be reached directly
            # reachable_destinations_direct = count(n_rides == 1),
            commute_all = sum(commute_all)) %>%
  ungroup() %>%
  mutate(n_rides = round(n_rides))


# Get median values per destination
ttd_matrix_d <- ttd_matrix %>%
  group_by(across(all_of(to_id_col)), combination, departure_time) %>%
  summarise(across(c(ends_with("_time"), n_rides), median, na.rm = TRUE),
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

########### ------------------------ Bus coverage of travel demand ------------------------ ###########


# --- data on which OD pairs are covered by direct buses
od_supply <- arrow::read_parquet(paste0("data/interim/travel_demand/", toupper(geography), "/od_pairs_bus_coverage.parquet"))


# get number of unique trips (directional routes) that serve each OD pair, and their headway
od_supply_avg <- od_supply %>%
  group_by(Origin, Destination, start_time) %>%
  summarise(route_options = n(),
            headway_sec_avg = round(mean(headway_secs)),
            headway_sec_min = min(headway_secs),
            headway_sec_med = median(headway_secs),
            bus_per_hr_avg = round(3600/headway_sec_avg),
            bus_per_hr_min = round(3600/headway_sec_min)) %>%
  ungroup()

# filter od pairs by euclidian distance
od_supply_filtered = filter_matrix_by_distance(zones = study_area,
                                               od_matrix = od_supply_avg,
                                               dist_threshold = 1000)


########## --------------- Combine ttd matrices with bus service provision matrix --------------- ##########

# combine ttd matrix with matrix showing bus provision for ODs
od_sd <- od_supply_filtered %>%
  #TODO: check if we need an inner join (we only have 1 start time atm)
  #left_join(ttd_matrix,
  # full join so that we include all OD pairs, not just the ones with a direct bus
  full_join(ttd_matrix,
            by = c("Origin" = from_id_col,
                   "Destination" = to_id_col,
                   "start_time" = "departure_time"))

# ---------- create factor column for demand (for facet plots)

od_sd <- od_sd %>%
  mutate(commute_all_fct = cut_interval(commute_all, n = 3))




########### ------------------------ Travel demand aggregated onto bus routes ------------------------ ###########


# ----------- 5. Save the output
trips_sd_sf <- st_read(paste0("data/processed/travel_demand/trips_potential_demand_census_", geography, ".geojson"))

# If we want to get the total demand per day let's try and group by shape_id (same trip at different time has a different trip_id)
trips_sd_sf_shape_sum <- trips_sd_sf %>%
  group_by(shape_id) %>%
  summarise(across(contains("potential_demand"), ~ sum(.x, na.rm = TRUE))) %>%
  ungroup()



########## --------------- Plots --------------- ##########

# --------------- Preprocessing

# ---------- get desire lines

od_sd_sf <- od::od_to_sf(x = od_sd %>% st_drop_geometry(),
                         z = study_area %>% st_cast("MULTIPOLYGON"),
                         crs = 4326)



# ----------- prep the layer
# TODO: remove this when we have multiple start times (currently the od_supply df only has 7:30 - WHY? )
od_sd_sf <- od_sd_sf %>%
  filter(combination == "pt_wkday_morning")

# ---------------------------- MAP 1: OD pairs with the highest level of commuting


tm_shape(study_area) +
  tm_borders(col = "grey80",
             alpha = 0.5) +
tm_shape(od_sd_sf) +
  tm_lines(col = "grey80",
           alpha = 0.05) +
  tm_shape(od_sd_sf %>%
             filter(commute_all > quantile(commute_all, probs = 0.90, na.rm = TRUE))) +
  tm_lines(col = "commute_all",
           title.col = "No. of commuters \n(census 2021)",
           lwd = "commute_all",
           legend.lwd.show = FALSE,
           #legend.col.is.portrait = FALSE,
           scale = 10) +
  tm_layout(fontfamily = 'Georgia',
            main.title = "Commuting - Top 10% of OD pairs (Census 2021)", # this works if you need it
            main.title.size = 1.3,
            main.title.color = "azure4",
            main.title.position = "left",
            #legend.outside = TRUE,
            #legend.outside.position = "bottom",
            frame = FALSE) -> map_desire_commuting

map_desire_commuting

tmap_save(tm = map_desire_commuting, filename = paste0(plots_path, "map_desire_commuting.png"), width = 10, dpi = 720, asp = 0)



# ---------------------------- MAP 2: OD pairs by number of buses (transfers) required to connect them

tm_shape(study_area) +
  tm_borders(col = "grey60",
             alpha = 0.5) +
tm_shape(od_sd_sf) +
  tm_lines(col = "grey80",
           alpha = 0.02) +
  tm_shape(od_sd_sf %>%
             #filter(commute_all > quantile(commute_all, probs = 0.50, na.rm = TRUE)) %>%
             mutate(transfers = n_rides - 1)) +
  tm_lines(col = "commute_all",
           title.col = "No. of commuters \n(census 2021)",
           lwd = "commute_all",
           legend.lwd.show = FALSE,
           #legend.col.is.portrait = FALSE,
           palette = "YlOrRd",
           alpha = 0.8,
           scale = 10) +
  tm_facets(by = "transfers",
            free.coords = FALSE,
            # for "Missing label"
            textNA = paste0(max(od_sd_sf$n_rides, na.rm = TRUE), "+ or not possible"),
            nrow = 1)  +  # so that the maps aren't different sizes
  tm_layout(fontfamily = 'Georgia',
            main.title = "Minimum number of transfers required to reach destination", # this works if you need it
            main.title.size = 1.3,
            main.title.color = "azure4",
            main.title.position = "left",
            #legend.outside = TRUE,
            #legend.outside.position = "bottom",
            frame = FALSE)  -> map_desire_commuting_facet_transfers

map_desire_commuting_facet_transfers

tmap_save(tm =map_desire_commuting_facet_transfers, filename = paste0(plots_path, "map_desire_commuting_facet_transfers.png"), height = 4, dpi = 720, asp = 0)


tm_shape(study_area) +
  tm_borders(col = "grey60",
             alpha = 0.5) +
  tm_shape(od_sd_sf) +
  tm_lines(col = "grey80",
           alpha = 0.02) +
  tm_shape(od_sd_sf %>%
             #filter(commute_all > quantile(commute_all, probs = 0.50, na.rm = TRUE)) %>%
             mutate(transfers = n_rides - 1) %>%
             filter(transfers > 1 | is.na(transfers))) +
  tm_lines(col = "commute_all",
           title.col = "No. of commuters \n(census 2021)",
           lwd = "commute_all",
           legend.lwd.show = FALSE,
           #legend.col.is.portrait = FALSE,
           palette = "YlOrRd",
           scale = 10) +
  tm_facets(by = "transfers",
            free.coords = FALSE,
            # for "Missing label"
            textNA = paste0(max(od_sd_sf$n_rides, na.rm = TRUE), "+ or not possible"),
            nrow = 1)  +  # so that the maps aren't different sizes
  tm_layout(fontfamily = 'Georgia',
            main.title = "Minimum number of transfers required to reach destination", # this works if you need it
            main.title.size = 1.3,
            main.title.color = "azure4",
            main.title.position = "left",
            # legend.outside = TRUE,
            # legend.outside.position = "bottom",
            frame = FALSE)  -> map_desire_commuting_mult_rides_facet_transfers

map_desire_commuting_mult_rides_facet_transfers

tmap_save(tm = map_desire_commuting_mult_rides_facet_transfers, filename = paste0(plots_path, "map_desire_commuting_mult_rides_facet_transfers.png"), width = 10, dpi = 1080, asp = 0)




# ---------------------------- MAP 3: OD pairs NOT served by a direct bus


tm_shape(study_area) +
  tm_borders(col = "grey60",
           alpha = 0.5) +
tm_shape(od_sd_sf %>%
           filter(n_rides > 1)) +
  tm_lines(col = "commute_all",
           lwd = "commute_all",
           title.col = "No. of commuters \n(census 2021)",
           legend.lwd.show = FALSE,
           legend.col.is.portrait = FALSE,
           scale = 5) +
  tm_layout(fontfamily = 'Georgia',
            main.title = "OD pairs NOT served by \na direct bus",
            main.title.size = 1.3,
            main.title.color = "azure4",
            main.title.position = "left",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            frame = FALSE) -> map_desire_commuting_no_direct_bus

map_desire_commuting_no_direct_bus

tmap_save(tm =map_desire_commuting_no_direct_bus, filename = paste0(plots_path, "map_desire_commuting_no_direct_bus.png"), width = 15, dpi = 1080)




# ---------------------------- MAP 4: No. of direct bus routes serving OD pair (faceted) by no. of commuters


tm_shape(study_area) +
  tm_borders(col = "grey60",
             alpha = 0.5) +
tm_shape(od_sd_sf) +
  tm_lines(col = "grey90",
           alpha = 0.5) +
tm_shape(od_sd_sf) +
  tm_lines(col = "route_options",
           lwd = "bus_per_hr_min",
           title.col = "No. of different DIRECT bus \nroutes serving OD pair",
           title.lwd = "Frequency of best route\n(Buses / hr)",
           legend.col.is.portrait = FALSE,
           palette = "Blues",
           scale = 5) +
tm_facets(by = "commute_all_fct",
          free.coords = FALSE,
          nrow = 1) +
tm_layout(fontfamily = 'Georgia',
          main.title = "No. of different DIRECT bus routes serving OD pair\nFaceted by no. of commuters",
          main.title.size = 1.3,
          main.title.color = "azure4",
          main.title.position = "left",
          legend.outside = TRUE,
          legend.outside.position = "bottom",
          legend.stack = "horizontal",
          frame = FALSE) -> map_desire_direct_bus_facet_commuting

map_desire_direct_bus_facet_commuting

tmap_save(tm =map_desire_direct_bus_facet_commuting, filename = paste0(plots_path, "map_desire_direct_bus_facet_commuting.png"), width = 15, dpi = 1080)






# ---------------------------- MAP 5: Fastest vs direct routes

tm_shape(study_area) +
  tm_borders(col = "grey60",
             alpha = 0.5) +
tm_shape(od_sd_sf) +
  tm_lines(col = "route_options",
           lwd = "bus_per_hr_min",
           title.col = "No. of different DIRECT bus \nroutes serving OD pair",
           title.lwd = "Frequency of best route\n(Buses / hr)",
           legend.col.is.portrait = FALSE,
           palette = "Blues",
           scale = 5) +
  tm_facets(by = "n_rides",
            free.coords = FALSE,
            nrow = 1) +
  tm_layout(fontfamily = 'Georgia',
            main.title = "No. of different DIRECT bus routes serving OD pair. \nFaceted by no. of rides (transfers + 1) involved in fastest trip",
            main.title.size = 1.3,
            main.title.color = "azure4",
            main.title.position = "left",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.stack = "horizontal",
            frame = FALSE) -> map_desire_direct_bus_facet_transfers

map_desire_direct_bus_facet_transfers

tmap_save(tm =map_desire_direct_bus_facet_transfers, filename = paste0(plots_path, "map_desire_direct_bus_facet_transfers.png"), width = 15, dpi = 1080)



# ---------------------------- MAP 6: Demand Aggregated onto bus routes


tm_shape(study_area) +
  tm_borders(alpha = 0.1) +
  tm_shape(trips_sd_sf %>%
             filter(start_time == "07:30:00")) +
  tm_lines(col = "potential_demand_freq_based",
           lwd = "potential_demand_freq_based",
           scale = 3,
           legend.is.portrait = FALSE,
           alpha = 0.4) +
  tm_layout(fontfamily = 'Georgia',
            main.title = "Potential Ridership on Different Trips",
            main.title.size = 1.3,
            main.title.color = "azure4",
            main.title.position = "left",
            #legend.outside = TRUE,
            #legend.outside.position = "bottom",
            frame = FALSE) -> map_potential_demand_bus_routes_freq

map_potential_demand_bus_routes_freq

tmap_save(tm =map_potential_demand_bus_routes_freq, filename = paste0(plots_path, "map_potential_demand_bus_routes_freq.png"), width = 15, dpi = 1080)



# ----- b)  potential ridership on all different trips - facet by method

trips_sd_sf_long <- trips_sd_sf %>%
  pivot_longer(cols = contains("potential_demand"),
               names_to = "method",
               values_to = "potential_demand")


tm_shape(study_area) +
  tm_borders(alpha = 0.1) +
  tm_shape(trips_sd_sf_long %>%
             filter(start_time == "07:30:00")) +
  tm_lines(col = "potential_demand",
           alpha = 0.8,
           #palette = "Blues",
           style = "quantile",
           legend.col.is.portrait = FALSE) +
  tm_facets(by="method",
            ncol = 2,
            free.coords=FALSE)+
  tm_layout(fontfamily = 'Georgia',
            main.title = "Potential ridership on trips using different methods", # this works if you need it
            main.title.size = 1.3,
            main.title.color = "azure4",
            main.title.position = "left",
            legend.text.size = 1,
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            frame = FALSE) -> map_potential_demand_bus_routes_facet_method

map_potential_demand_bus_routes_facet_method

tmap_save(tm = map_potential_demand_bus_routes_facet_method, filename = paste0(plots_path, "map_potential_demand_bus_routes_facet_mathod.png"), width = 15, dpi = 1080)



# -----------  # ---------------------------- MAP 7: Redundancy: How many routes serve each OD pair


# Convert columns to factors for facet plotting
od_no_of_routes <- od_sd %>%
  mutate(route_options_fct = cut(route_options, breaks = seq(0, 10, by = 2)),
         #bus_per_hr_min_fct = cut_interval(bus_per_hr_min, n = 3),
         bus_per_hr_min_fct = cut(bus_per_hr_min, breaks = seq(0, 10, by = 2)),
         headway_sec_min_fct = cut(headway_sec_min, breaks = seq(0, 12000, by = 600), dig.lab = 5),
         headway_sec_med_fct = cut(headway_sec_med, breaks = seq(0, 6000, by = 600), dig.lab = 5))



# Plot OD pairs by the number of routes serving them
tm_shape(study_area) +
  tm_borders(alpha = 0.2) +
  tm_shape(od_no_of_routes %>%
             filter(route_options <= 10)) +
  tm_lines(col = "headway_sec_med",
           title.col = "Median headway of routes serving OD pair",
           alpha = 0.6,
           legend.col.is.portrait = FALSE) +
  tm_facets(by="route_options",
            nrow = 2,
            free.coords=FALSE)+
  tm_layout(fontfamily = 'Georgia',
            main.title = "No. of routes Serving each OD pair", # this works if you need it
            main.title.size = 1.3,
            main.title.color = "azure4",
            main.title.position = "left",
            # legend title
            legend.text.size = 1,
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            frame = FALSE)


tm_shape(study_area) +
  tm_borders(alpha = 0.2) +
  tm_shape(od_no_of_routes %>%
             filter(route_options <= 1)) +
  tm_lines(col = "headway_sec_min",
           title.col = "Headway of (only) route serving OD pair",
           alpha = 0.6,
           legend.col.is.portrait = FALSE) +
  tm_facets(by="headway_sec_med_fct",
            nrow = 2,
            free.coords=FALSE)+
  tm_layout(fontfamily = 'Georgia',
            main.title = "Headway of routes serving OD pairs.\nData filtered to OD pairs served by only one route", # this works if you need it
            main.title.size = 1.3,
            main.title.color = "azure4",
            main.title.position = "left",
            # legend title
            legend.text.size = 1,
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            frame = FALSE) ->  map_od_pairs_one_route_facet_headway

map_od_pairs_one_route_facet_headway

tmap_save(tm = map_od_pairs_one_route_facet_headway, filename = paste0(plots_path, "map_od_pairs_one_route_facet_headway.png"), width = 15, dpi = 1080)



# Lets plot the potential demand on the busiest route serving each OD pair
# This is equivalent to scenario 3 in the paper

ttd_matrix_sf <- filter_matrix_by_distance(study_area,
                                           ttd_matrix %>% select(-distance_m),
                                           1000)

ttd_matrix_sf <- ttd_matrix_sf %>%
  mutate(across(contains("potential_demand"), ~ . / 1000))

tm_shape(study_area) +
  tm_borders(alpha = 0.2) +
  # background: plot od pairs that can't be reached by PT
tm_shape(ttd_matrix_sf %>%
           filter(is.na(speed_percentile_fct))) +
  tm_lines(col = "grey90",
           alpha = 0.5) +
tm_shape(ttd_matrix_sf %>%
           filter(!is.na(speed_percentile_fct))) +
  tm_lines(col = "potential_demand_equal_split",
           #lwd = "potential_demand_equal_split",
           palette = "RdYlGn",
           title.col = "No. of potential commuters on busiest route serving OD pair ('000)          ",
           alpha = 0.3,
           scale = 3,
           legend.col.is.portrait = FALSE) +
  tm_facets(by="speed_percentile_fct",
            nrow = 2,
            free.coords=FALSE)+
  tm_layout(fontfamily = 'Georgia',
            main.title = "Potential demand (Faceted by speed (percentiles) of travel between OD pairs using PT)", # this works if you need it
            main.title.size = 1.3,
            main.title.color = "azure4",
            main.title.position = "left",
            # legend title
            legend.text.size = 1,
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            frame = FALSE) ->  map_od_pairs_route_demand

map_od_pairs_route_demand

tmap_save(tm = map_od_pairs_route_demand, filename = paste0(plots_path, "map_od_pairs_demand_busiest_route.png"), width = 15, dpi = 720, asp = 0)


# same map but faceted by demand (col = speed)

tm_shape(study_area) +
  tm_borders(alpha = 0.2) +
  # background: plot od pairs that can't be reached by PT
  tm_shape(ttd_matrix_sf %>%
             filter(is.na(speed_percentile_fct))) +
  tm_lines(col = "grey90",
           alpha = 0.5) +
  tm_shape(ttd_matrix_sf %>%
             filter(!is.na(speed_percentile_fct)) %>%
             mutate(potential_demand_equal_split_fct = cut(potential_demand_equal_split, breaks = c(0, 10, 20, 30,  40)))
           ) +
  tm_lines(col = "speed_percentile_fct",
           #lwd = "potential_demand_equal_split",
           palette = "-RdYlGn",
           title.col = "Travel speed by PT \nbetween OD pairs \n(percentiles)",
           alpha = 0.3,
           scale = 3,
           #legend.col.is.portrait = FALSE
           ) +
  tm_facets(by="potential_demand_equal_split_fct",
            nrow = 1,
            free.coords=FALSE)+
  tm_layout(fontfamily = 'Georgia',
            main.title = "Travel speed between OD pairs \nFaceted by potential demand on busiest route directly serving each OD pair", # this works if you need it
            main.title.size = 1.3,
            main.title.color = "azure4",
            main.title.position = "left",
            # legend title
            legend.text.size = 1,
            legend.outside = TRUE,
            legend.outside.position = "right",
            frame = FALSE) ->  map_od_pairs_route_demand_speed

map_od_pairs_route_demand_speed

tmap_save(tm = map_od_pairs_route_demand_speed, filename = paste0(plots_path, "map_od_pairs_demand_busiest_route_speed.png"), width = 10, dpi = 720, asp = 0)









# ----- bar plot of transfers | length | commute_all




ttd_matrix %>%
  mutate(transfers = n_rides - 1) %>%
  # length categories (km)
  mutate(length_category = cut(distance_m / 1000, breaks = seq(0, max(distance_m, na.rm = TRUE), by = 10)),
         commute_category = cut(commute_all, breaks = seq(0, max(commute_all, na.rm = TRUE), by = 150)),
         transfers_category = as.factor(case_when(is.na(transfers) ~ "3+",
                                                  transfers <= 2 ~ as.character(transfers),
                                                  TRUE ~ "3+"))) %>%
  #filter(commute_all >= 50) %>%
  ggplot(aes(x = transfers_category, fill = factor(length_category))) +
  geom_bar(position = position_stack(reverse = TRUE)) +  # Flip the order of stack +
  scale_fill_brewer(palette = "Greys", direction = -1) +
  #facet_wrap(~ commute_category, scales = "fixed", nrow = 2) +
  labs(title = "Transfers required to commute \nbetween OD pairs",
       x = "Transfers",
       y = "No. of OD pairs",
       fill = "Euclidian \nDistance (km)") +
  theme_minimal() +
  coord_flip()


ggsave(filename = paste0(plots_path, "plot_bar_transfers_stacked_distance.png"), width = 4, height = 3)








ttd_matrix %>%
  mutate(transfers = n_rides - 1) %>%
  mutate(transfers = replace_na(transfers, 5)) %>%
  mutate(length_category = cut(distance_m / 1000, breaks = seq(0, max(distance_m, na.rm = TRUE), by = 5)),
         commute_category = cut(commute_all, breaks = seq(0, max(commute_all, na.rm = TRUE), by = 100))) %>%
  #filter(commute_all > 150) %>%
  ggplot(aes(x = commute_category, fill = factor(length_category))) +
  geom_bar() +
  facet_wrap(~ transfers, scales = "fixed", nrow = 2) +
  labs(title = "Stacked Histogram of Transfers by Commuters Category",
       x = "Transfers",
       y = "Frequency",
       fill = "Length") +
  theme_minimal()

