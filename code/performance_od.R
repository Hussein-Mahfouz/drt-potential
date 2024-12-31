### the purpose of this script is to analyse the performance of OD pairs ###
### performance is measure by: speed of travl ...

library(tidyverse)
library(sf)
library(tmap)

# library(tidygraph)
# library(sfnetworks)


source("R/study_area_geographies.R")
source("R/filter_od_matrix.R")

geography = "MSOA"
# are we processing the data that is disaggregated by mode?
mode = FALSE
source = "cpc"

# --- where do we want to save the plots?
plots_path <- paste0("data/processed/plots/eda/od_performance/", geography, "/", source, "/")
travel_time_path <- paste0("data/processed/travel_times/")



# ----------- 1. Load in the data


# ----- Study area

# --- administrative boundaries
study_area <- st_read("data/interim/study_area_boundary.geojson")

# convert to desired resolution
study_area = study_area_geographies(study_area = study_area,
                                    geography = geography)

study_area <- study_area %>%
  st_cast("MULTIPOLYGON")



# ----- Travel time and demand matrix

# od_demand <- arrow::read_parquet(paste0("data/raw/travel_demand/cpc_matrices_2019/demand_study_area_", tolower(geography), ".parquet"))
od_demand <- st_read(paste0(travel_time_path, geography, "/travel_time_matrix_expanded_temporal_jitter_demand.geojson"))



# ----------- 2. Add desire lines and filter OD pairs

# we don't want OD pairs below a certain distance (say 1km)


# from_id_col = paste0(geography, "21CD_home")
# to_id_col = paste0(geography, "21CD_work")
#
# # rename columns for function below
# od_demand <- od_demand %>%
#   rename(Origin = all_of(from_id_col),
#          Destination = all_of(to_id_col)) %>%
#   relocate(Origin, Destination)


#
# # filter od pairs by euclidian distance
# od_demand_sf = filter_matrix_by_distance(zones = study_area,
#                                          od_matrix = od_demand,
#                                          dist_threshold = 500)

od_demand_sf = od_demand %>%
  st_transform(3857) %>%
  mutate(distance_m = units::drop_units(sf::st_length(.)))

# ----------- 3. Get travel speeds

od_demand_sf <- od_demand_sf %>%
  mutate(speed_mps = distance_m / (total_time*60),
         speed_kph = speed_mps * 3.6)


# --- get travel time as fraction of total time

od_demand_sf <- od_demand_sf %>%
  mutate(ride_time_frac = ride_time / total_time,
         ride_time_frac_fct = cut(ride_time_frac,
                                    breaks = seq(0, 1, by = 0.25),
                                    include.lowest = TRUE))



# ----------- 4. Rank OD pairs

# group by combination as pt schedules differ throughout the day
# get percentiles for (a) speed and (b) demand

od_demand_sf_rank <- od_demand_sf %>%
  group_by(combination) %>%
  mutate(speed_percentile = percent_rank(speed_mps),
         speed_percentile_fct = cut(speed_percentile,
                                    breaks = seq(0, 1, by = 0.25),
                                    include.lowest = TRUE),
         demand_percentile = percent_rank(total_flow),
         demand_percentile_fct = cut(demand_percentile,
                                     breaks = seq(0, 1, by = 0.25),
                                     include.lowest = TRUE),
         ) %>%
  ungroup()


# ---------- 5. Save the output

# arrow::write_parquet(st_drop_geometry(od_demand_sf_rank), paste0("data/raw/travel_demand/cpc_matrices_2019/demand_study_area_", tolower(geography), "_with_speed.parquet"))
st_write(od_demand_sf_rank, paste0("data/raw/travel_demand/cpc_matrices_2019/demand_study_area_", tolower(geography), "_with_speed.geojson"), delete_dsn = TRUE)
# ---------- 6. Map the results


# Lwd: Demand between OD pairs,
# Col: No. of rides required,
# Facet: speed_percentile_fct

tm_shape(study_area) +
  tm_fill(col = "grey95") +
tm_shape(study_area) +
  tm_borders(col = "grey60",
             alpha = 0.5) +
  tm_shape(od_demand_sf_rank %>%
             filter(combination == "pt_wkday_06_30") %>%
             mutate(n_rides = as.factor(round(n_rides)))) +
  tm_lines(col = "n_rides",
           lwd = "total_flow",
           #legend.lwd.show = FALSE,
           alpha = 0.5,
           title.col = "No. of rides required",
           title.lwd = "Demand between OD pair",
           legend.col.is.portrait = FALSE,
           palette = "Dark2", #-RdYlGn
           style = "quantile",
           scale = 15) +
  tm_facets(by = "speed_percentile_fct",
            free.coords = FALSE,
            nrow = 2) +
  tm_layout(fontfamily = 'Georgia',
            main.title = "Demand between OD pairs\nfaceted by speed (percentile) between OD pairs",
            main.title.size = 1.3,
            main.title.color = "azure4",
            main.title.position = "left",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.stack = "horizontal",
            legend.title.size = 0.9,
            frame = FALSE) -> map_desire_commuting_facet_speed_col_rides

map_desire_commuting_facet_speed_col_rides
tmap_save(tm = map_desire_commuting_facet_speed_col_rides, filename = paste0(plots_path, "map_desire_commuting_facet_speed_col_rides.png"), width = 15, dpi = 1080)




# Lwd: Demand between OD pairs,
# Col: No. of rides required,
# Facet: speed_percentile_fct
# Filter: n_rides == 1 (only OD pairs served by direct trips)

tm_shape(study_area) +
  tm_fill(col = "grey95") +
  tm_shape(study_area) +
  tm_borders(col = "grey60",
             alpha = 0.5) +
  tm_shape(od_demand_sf_rank %>%
             filter(combination == "pt_wkday_06_30", n_rides == 1)) +
  tm_lines(col = "total_flow",
           lwd = "total_flow",
           legend.lwd.show = FALSE,
           alpha = 0.8,
           title.col = "Demand between OD pair",
           legend.col.is.portrait = FALSE,
           palette = "-RdYlGn",
           style = "quantile",
           scale = 10) +
  tm_facets(by = "speed_percentile_fct",
            free.coords = FALSE,
            nrow = 2) +
  tm_layout(fontfamily = 'Georgia',
            main.title = "Demand between OD pairs\nthat are served by direct bus\nFaceted by speed (percentile) between OD pairs",
            main.title.size = 1.3,
            main.title.color = "azure4",
            main.title.position = "left",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.stack = "horizontal",
            legend.title.size = 0.9,
            frame = FALSE) -> map_desire_commuting_direct_bus_facet_speed

map_desire_commuting_direct_bus_facet_speed

tmap_save(tm =  map_desire_commuting_direct_bus_facet_speed, filename = paste0(plots_path, "map_desire_commuting_direct_bus_facet_speed.png"), width = 15, dpi = 1080)


# Lwd | col: Demand between OD pairs, demand between od pairs
# Filter: speed_percentile <= 0.25 (SLOWEST OD PAIRS)

tm_shape(study_area) +
  tm_fill(col = "grey95") +
tm_shape(study_area) +
  tm_borders(col = "grey60",
             alpha = 0.5) +
  tm_shape(study_area) +
  tm_fill(col = "grey80",
          alpha = 0.5) +
  tm_shape(od_demand_sf_rank %>%
             filter(combination == "pt_wkday_06_30", speed_percentile <= 0.25)) +
  tm_lines(col = "total_flow",
           lwd = "total_flow",
           scale = 10,
           palette = "RdYlGn",
           legend.lwd.show = FALSE,
           alpha = 1,
           title.col = "Demand between OD pair",
           legend.col.is.portrait = FALSE) +
  #palette = "Blues",
  #style = "quantile",
  tm_layout(fontfamily = 'Georgia',
            main.title = "Demand between OD pairs\n(slowest 25% of OD pairs)",
            main.title.size = 1.3,
            main.title.color = "azure4",
            main.title.position = "left",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.stack = "horizontal",
            frame = FALSE) -> map_desire_commuting_low_speed

map_desire_commuting_low_speed
tmap_save(tm =  map_desire_commuting_low_speed, filename = paste0(plots_path, "map_desire_commuting_low_speed.png"), width = 15, dpi = 1080)




# --- HOW MANY RIDES NEEDED (FOR SLOWEST OD PAIRS)

# Lwd | Col: Demand between OD pairs,
# Facet: n_ride
# Filter: speed_percentile <= 0.25 (SLOWEST OD PAIRS)

tm_shape(study_area) +
  tm_borders(col = "grey60",
             alpha = 0.5) +
  tm_shape(study_area) +
  tm_fill(col = "grey80",
          alpha = 0.5) +
  tm_shape(od_demand_sf_rank %>%
             mutate(n_rides = round(n_rides)) %>%
             filter(speed_percentile <= 0.25, combination == "pt_wkday_morning")) +
  tm_lines(col = "total_flow",
           lwd = "total_flow",
           scale = 10,
           #palette = "Blues",
           #style = "quantile",
           legend.lwd.show = FALSE,
           alpha = 1,
           title.col = "Demand between OD pair",
           legend.col.is.portrait = FALSE) +
  tm_facets(by = "n_rides",
            free.coords = FALSE,
            nrow = 2) +
  tm_layout(fontfamily = 'Georgia',
            main.title = "Demand between OD pairs\n(slowest 25% of OD pairs)\nFaceted by no of rides",
            main.title.size = 1.3,
            main.title.color = "azure4",
            main.title.position = "left",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.stack = "horizontal",
            frame = FALSE)  -> map_desire_commuting_low_speed_facet_rides

map_desire_commuting_low_speed_facet_rides
tmap_save(tm =  map_desire_commuting_low_speed_facet_rides, filename = paste0(plots_path, "map_desire_commuting_low_speed_facet_rides.png"), width = 15, dpi = 1080)



# --- Separate data into demand and speed quadrants

#TODO replace 0.5 by different number
od_demand_sf_rank <- od_demand_sf_rank %>%
  mutate(quadrant = case_when(speed_percentile <= 0.5 & demand_percentile <= 0.5 ~ "low speed low demand",
                              speed_percentile <= 0.5 & demand_percentile > 0.5 ~ "low speed high demand",
                              speed_percentile > 0.5 & demand_percentile <= 0.5 ~ "high speed low demand",
                              speed_percentile > 0.5 & demand_percentile > 0.5 ~ "high speed high demand"))



# FACET BY COMBINATION OF DEMAND AND SPEED

# Lwd: Demand between OD pairs,
# Col: No. of rides required,
# Facet: quadrant

tm_shape(study_area) +
  tm_fill(col = "grey95") +
  tm_shape(study_area) +
  tm_borders(col = "grey60",
             alpha = 0.5) +
  tm_shape(od_demand_sf_rank %>%
             filter(combination == "pt_wkday_morning", !is.na(quadrant)) %>%
             mutate(n_rides = as.factor(round(n_rides)))) +
  tm_lines(col = "n_rides",
           lwd = "total_flow",
           #legend.lwd.show = FALSE,
           alpha = 0.5,
           title.col = "No. of rides required",
           title.lwd = "Demand between OD pair",
           legend.col.is.portrait = FALSE,
           palette = "Dark2", #-RdYlGn
           style = "quantile",
           scale = 15) +
  tm_facets(by = "quadrant",
            free.coords = FALSE,
            nrow = 2) +
  tm_layout(fontfamily = 'Georgia',
            main.title = "Demand between OD pairs\nfaceted by combination of speed and demand (percentiles)\nbetween OD pairs",
            main.title.size = 1.3,
            main.title.color = "azure4",
            main.title.position = "left",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.stack = "horizontal",
            legend.title.size = 0.9,
            frame = FALSE) -> map_desire_commuting_facet_speed_and_demand

map_desire_commuting_facet_speed_and_demand
tmap_save(tm =  map_desire_commuting_facet_speed_and_demand, filename = paste0(plots_path, "map_desire_commuting_facet_speed_and_demand.png"), width = 15, dpi = 1080)




# --- OD PAIRS WITH HIGH DEMAND AND LOW SPEED

# Lwd: Demand between OD pairs,
# Col: No. of rides required,
# Filter: quadrant == "low speed high demand"

tm_shape(study_area) +
  tm_fill(col = "grey95") +
  tm_shape(study_area) +
  tm_borders(col = "grey60",
             alpha = 0.5) +
  tm_shape(od_demand_sf_rank %>%
             filter(combination == "pt_wkday_morning", quadrant == "low speed high demand") %>%
             mutate(n_rides = as.factor(round(n_rides)))) +
  tm_lines(col = "n_rides",
           lwd = "total_flow",
           #legend.lwd.show = FALSE,
           alpha = 0.5,
           title.col = "No. of rides required",
           title.lwd = "Demand between OD pair",
           legend.col.is.portrait = FALSE,
           palette = "Dark2", #-RdYlGn
           style = "quantile",
           scale = 10) +
  tm_layout(fontfamily = 'Georgia',
            main.title = "Demand between OD pairs\nwith low speed and high demand (percentiles)",
            main.title.size = 1.3,
            main.title.color = "azure4",
            main.title.position = "left",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.stack = "horizontal",
            legend.title.size = 0.9,
            frame = FALSE) -> map_desire_commuting_low_speed_high_demand

map_desire_commuting_low_speed_high_demand
tmap_save(tm =  map_desire_commuting_low_speed_high_demand, filename = paste0(plots_path, "map_desire_commuting_low_speed_high_demand.png"), width = 15, dpi = 1080)




# --- OD PAIRS WITH HIGH DEMAND AND LOW SPEED

# Lwd | Col: Demand between OD pairs,
# Filter: quadrant == "low speed high demand"
# Facet: n_rides

tm_shape(study_area) +
  tm_fill(col = "grey95") +
tm_shape(study_area) +
  tm_borders(col = "grey60",
             alpha = 0.5) +
  tm_shape(od_demand_sf_rank %>%
             filter(combination == "pt_wkday_morning", quadrant == "low speed high demand", n_rides != 0) %>%
             mutate(n_rides = as.factor(round(n_rides)))) +
  tm_lines(col = "total_flow",
           lwd = "total_flow",
           #legend.lwd.show = FALSE,
           alpha = 0.5,
           title.col = "Demand between OD pair",
           legend.lwd.show = FALSE,
           legend.col.is.portrait = FALSE,
           palette = "-YlOrRd", #-RdYlGn
           style = "pretty",
           scale = 10) +
  tm_facets(by = "n_rides",
            free.coords = FALSE,
            nrow = 2) +
  tm_layout(fontfamily = 'Georgia',
            main.title = "Demand between OD pairs\nwith low speed and high demand (percentiles)\nFaceted by no. of rides",
            main.title.size = 1.3,
            main.title.color = "azure4",
            main.title.position = "left",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.stack = "horizontal",
            legend.title.size = 0.9,
            frame = FALSE) -> map_desire_commuting_low_speed_high_demand_facet_rides

map_desire_commuting_low_speed_high_demand_facet_rides
tmap_save(tm =  map_desire_commuting_low_speed_high_demand_facet_rides, filename = paste0(plots_path, "map_desire_commuting_low_speed_high_demand_facet_rides.png"), width = 15, dpi = 1080)





# --- OD PAIRS WITH HIGH DEMAND AND LOW SPEED, FACETED BY DEMAND Filter BY COMBINATION OF DEMAND AND SPEED


# Lwd: Demand between OD pairs,
# Col: No. of rides required,
# Filter: quadrant == "low speed high demand"
# Facet: demand_percentile_fct


tm_shape(study_area) +
  tm_fill(col = "grey95") +
  tm_shape(study_area) +
  tm_borders(col = "grey60",
             alpha = 0.5) +
  tm_shape(od_demand_sf_rank %>%
             filter(combination == "pt_wkday_morning", quadrant == "low speed high demand") %>%
             mutate(n_rides = as.factor(round(n_rides)))) +
  tm_lines(col = "n_rides",
           lwd = "total_flow",
           #legend.lwd.show = FALSE,
           alpha = 0.5,
           title.col = "No. of rides required",
           title.lwd = "Demand between OD pair",
           legend.col.is.portrait = FALSE,
           palette = "Dark2", #-RdYlGn
           style = "quantile",
           scale = 10) +
  tm_facets(by = "demand_percentile_fct",
            free.coords = FALSE,
            ncol = 2) +
  tm_layout(fontfamily = 'Georgia',
            main.title = "Demand between OD pairs with low speed and high demand (percentiles)\nFaceted by demand percentile",
            main.title.size = 1.3,
            main.title.color = "azure4",
            main.title.position = "left",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.stack = "horizontal",
            legend.title.size = 0.9,
            frame = FALSE)



# TODO: improve / remove this map. What are we trying to show? Should we add speed?

# Ride time as fraction of total trip time

# Lwd: Demand between OD pairs,
# Col: No. of rides required,
# Filter: quadrant == "low speed high demand"
# Facet: ride_time_frac_fct


tm_shape(study_area) +
  tm_fill(col = "grey95") +
  tm_shape(study_area) +
  tm_borders(col = "grey60",
             alpha = 0.5) +
  tm_shape(od_demand_sf_rank %>%
             filter(combination == "pt_wkday_morning") %>%
             mutate(n_rides = as.factor(round(n_rides)))) +
  tm_lines(col = "n_rides",
           lwd = "total_flow",
           #legend.lwd.show = FALSE,
           alpha = 0.5,
           title.col = "No. of rides required",
           title.lwd = "Demand between OD pair",
           legend.col.is.portrait = FALSE,
           palette = "Dark2", #-RdYlGn
           style = "quantile",
           scale = 10) +
  tm_facets(by = "ride_time_frac_fct",
            free.coords = FALSE,
            ncol = 2) +
  tm_layout(fontfamily = 'Georgia',
            main.title = "Demand between OD pairs with low speed and high demand (percentiles)\nFaceted by ride time / total time",
            main.title.size = 1.3,
            main.title.color = "azure4",
            main.title.position = "left",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.stack = "horizontal",
            legend.title.size = 0.9,
            frame = FALSE)





# ----- New plots after getting CPC temporal data

tm_shape(study_area) +
  tm_fill(col = "grey95") +
  tm_shape(study_area) +
  tm_borders(col = "grey60",
             alpha = 0.5) +
  tm_shape(od_demand_sf_rank %>%
             filter(!is.na(quadrant)) %>%
             mutate(n_rides = as.factor(round(n_rides)))) +
  tm_lines(col = "n_rides",
           lwd = "total_flow",
           #legend.lwd.show = FALSE,
           alpha = 0.5,
           title.col = "No. of rides required",
           title.lwd = "Demand between OD pair",
           legend.col.is.portrait = FALSE,
           palette = "Dark2", #-RdYlGn
           style = "quantile",
           scale = 15) +
  tm_facets_grid("quadrant", "combination",
                 type = "stack",
                 free.coords = FALSE) +
  tm_layout(fontfamily = 'Georgia',
            main.title = "Demand between OD pairs\nfaceted by combination of speed and demand (percentiles)\nbetween OD pairs",
            main.title.size = 1.3,
            main.title.color = "azure4",
            main.title.position = "left",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.stack = "horizontal",
            legend.title.size = 0.9,
            frame = FALSE)

