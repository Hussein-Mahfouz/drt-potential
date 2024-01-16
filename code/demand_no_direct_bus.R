# This script proposes exploratory methods for answering the "PT vs DRT" question on a route-level #
# It focuses on aggregating all OD demand without a direct bus onto the road network #

library(tidyverse)
library(sf)
library(stplanr)
library(tmap)
# network analysis - merging nodes and edges
library(sfnetworks)
library(dbscan)
library(tidygraph)


source("R/study_area_geographies.R")
source("R/filter_od_matrix.R")
source("R/simplify_graph.R")

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

# ----- shortest path geometries

od_shortest_paths <- readRDS(paste0("data/processed/travel_times/", geography, "/shortest_path_car.Rds"))

# --- path to save plots
plots_path <- paste0("data/processed/plots/eda/flows_no_direct/", geography, "/")


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

# ---------- 3. METHOD A: Overline

# --- a. Filter OD pairs to those needing an improved service

# OD pairs without direct route   ---   is.na(n_rides) to include od pairs that can't be routed between
od_demand_no_direct = od_demand %>%
  filter(n_rides > 1 | is.na(n_rides))

# --- b. add shortest path geometry

od_demand_no_direct <- od_demand_no_direct %>%
  left_join(od_shortest_paths, by = c("from_id", "to_id")) %>%
  st_as_sf()


# --- c. OPTION 1: use overline to get total flow on road network

od_demand_overline <- od_demand_no_direct %>%
  # nest sf data by combination and apply overline per nested df
  group_by(combination) %>%
  nest() %>%
  # use map to apply overline function to all groups
  mutate(demand = map(data, ~ stplanr::overline(sl = .x,
                                                attrib = "commute_all",
                                                ncores = 3,
                                                fun = sum))) %>%
  select(-data) %>%
  # turn back into one big sf
  unnest(demand) %>%
  ungroup() %>%
  st_as_sf()

st_write(od_demand_overline, paste0("data/interim/travel_demand/", geography, "/demand_no_direct_overline.geojson"), delete_dsn =TRUE)




# --- d. OPTION 2: use overline + network simplification methods to remove parallel lines

od_demand_overline_merge <-od_demand_no_direct %>%
  group_by(combination) %>%
  nest() %>%
  # use map to apply overline() + simplify_graph() function to all groups
  # apply overline with simplify = FALSE. Keep split vertices so that you apply sfnetwork morphers
  mutate(demand = map(data, ~ stplanr::overline(sl = .x,
                                                attrib = "commute_all",
                                                ncores = 3,
                                                simplify = FALSE,
                                                fun = sum))) %>%
  mutate(demand_merged = map(demand, ~ simplify_graph(layer = .x,
                                                      radius = 35))) %>%
  select(-c(data, demand)) %>%
  # turn back into one big sf
  unnest(demand_merged) %>%
  ungroup() %>%
  st_as_sf()


st_write(od_demand_overline_merge, paste0("data/interim/travel_demand/", geography, "/demand_no_direct_overline_merge.geojson"), delete_dsn =TRUE)


# compare the results



od_compare_baseline <- od_demand_no_direct %>%
  select(commute_all) %>%
  mutate(approach = "no_processing") %>%
  bind_rows(od_demand_overline %>%
              filter(combination == "pt_wkday_morning") %>%
              mutate(approach = "overline")) %>%
  bind_rows(od_demand_overline_merge %>%
              filter(combination == "pt_wkday_morning") %>%
              mutate(approach = "overline_merge")) %>%
  select(-combination)


#tmap_mode("view")
#tmap_mode("plot")


# Compare overline to base
tm_shape(study_area) +
  tm_borders(col = "grey60",
             alpha = 0.5) +
tm_shape(study_area) +
  tm_fill(col = "indianred2", #grey80
          alpha = 0.5) +
tm_shape(od_compare_baseline %>%
           filter(approach %in% c("no_processing", "overline"))) +
  tm_lines(col = "commute_all",
           lwd = "commute_all",
           scale = 15,
           palette = "Greens",
           style = "fisher", # jenks
           legend.lwd.show = FALSE,
           alpha = 1,
           title.col = "Aggregated demand",
           legend.col.is.portrait = FALSE) +
  tm_facets(by = "approach",
            free.coords = FALSE,
            nrow = 1) +
  tm_layout(fontfamily = 'Georgia',
            main.title = "OD demand aggregated along shortest paths",
            main.title.size = 1.3,
            main.title.color = "azure4",
            main.title.position = "left",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.stack = "horizontal",
            frame = FALSE) -> map_aggregate_flows_overline_vs_base

map_aggregate_flows_overline_vs_base

tmap_save(tm =  map_aggregate_flows_overline_vs_base, filename = paste0(plots_path, "map_aggregate_flows_overline_vs_base.png"), width = 15, dpi = 1080)



# compare overline to overline + merge
tm_shape(study_area) +
  tm_borders(col = "grey60",
             alpha = 0.5) +
tm_shape(study_area) +
  tm_fill(col = "indianred2",
          alpha = 0.5) +
  tm_shape(od_compare_baseline %>%
             filter(approach %in% c("overline", "overline_merge"))) +
  tm_lines(col = "commute_all",
           lwd = "commute_all",
           scale = 10,
           palette = "Greens",
           style = "fisher", # jenks
           legend.lwd.show = FALSE,
           alpha = 1,
           title.col = "Aggregated demand",
           title.lwd = "",
           legend.col.is.portrait = FALSE) +
  tm_facets(by = "approach",
            free.coords = FALSE,
            nrow = 1) +
  tm_layout(fontfamily = 'Georgia',
            main.title = "OD demand aggregated along shortest paths",
            main.title.size = 1.3,
            main.title.color = "azure4",
            main.title.position = "left",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.stack = "horizontal",
            frame = FALSE) -> map_aggregate_flows_overline_vs_merge

map_aggregate_flows_overline_vs_merge

tmap_save(tm =  map_aggregate_flows_overline_vs_merge, filename = paste0(plots_path, "map_aggregate_flows_overline_vs_merge.png"), width = 15, dpi = 1080)



tm_shape(study_area) +
  tm_borders(col = "grey60",
             alpha = 0.5) +
tm_shape(study_area) +
  tm_fill(col = "indianred2",
          alpha = 0.5) +
tm_shape(od_demand_overline_merge %>%
             filter(commute_all > 1000)) +
  tm_lines(col = "commute_all",
           lwd = "commute_all",
           scale = 10,
           palette = "Greens",
           style = "fisher",
           legend.lwd.show = FALSE,
           alpha = 1,
           title.col = "Aggregated demand",
           legend.col.is.portrait = FALSE) +
  tm_layout(fontfamily = 'Georgia',
            main.title = "OD demand aggregated along shortest paths",
            main.title.size = 1.3,
            main.title.color = "azure4",
            main.title.position = "left",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.stack = "horizontal",
            frame = FALSE) -> map_aggregate_flows_merge

map_aggregate_flows_merge
# save interactive map
htmlwidgets::saveWidget(map_aggregate_flows_merge, file = paste0(plots_path, "map_aggregate_flows_merge.html"))




# compare different times of day

od_compare <- od_demand_overline %>%
  mutate(approach = "overline") %>%
  bind_rows(od_demand_overline_merge %>%
              mutate(approach = "overline_merge")) %>%
  st_as_sf()


# check that the unsatisfied demand is different at different times of day (i.e that the code is working as expected)
od_demand_no_direct %>%
  st_drop_geometry() %>%
  group_by(combination) %>%
  summarise(od_pairs = n())



ggplot(od_compare %>%
         filter(approach == "overline"),
       aes(x = commute_all)) +
  geom_histogram(binwidth = 50) +
  facet_wrap(~combination, nrow = 4)

ggplot(od_compare %>%
         filter(approach == "overline", commute_all > 2000),
       aes(x = commute_all)) +
  geom_histogram(binwidth = 100) +
  facet_wrap(~combination, nrow = 4)

ggplot(od_compare %>%
         filter(approach == "overline"),
       aes(x = commute_all, color = combination)) +
  geom_density()


# plot maps
tm_shape(study_area) +
  tm_borders(col = "grey60",
             alpha = 0.5) +
tm_shape(study_area) +
  tm_fill(col = "grey85",
          alpha = 0.5) +
  tm_shape(od_compare %>%
             filter(approach == "overline")) +
  tm_lines(col = "commute_all",
           lwd = "commute_all",
           scale = 10,
           palette = "YlOrRd",
           style = "fisher",
           legend.lwd.show = FALSE,
           alpha = 1,
           title.col = "Aggregated demand",
           legend.col.is.portrait = FALSE) +
  tm_facets(by = "combination",
            free.coords = FALSE,
            ncol = 3) +
  tm_layout(fontfamily = 'Georgia',
            main.title = "OD demand aggregated along shortest paths",
            main.title.size = 1.3,
            main.title.color = "azure4",
            main.title.position = "left",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.stack = "horizontal",
            frame = FALSE) -> map_aggregate_flows_combination

map_aggregate_flows_combination

tmap_save(tm =  map_aggregate_flows_combination, filename = paste0(plots_path, "map_aggregate_flows_combinations_all.png"), dpi = 1080)


# compare 2 specific times of day
tm_shape(study_area) +
  tm_borders(col = "grey60",
             alpha = 0.5) +
  tm_shape(study_area) +
  tm_fill(col = "grey85",
          alpha = 0.5) +
  tm_shape(od_compare %>%
             filter(approach == "overline",
                    combination %in% c("pt_wkday_morning", "pt_wkday_night"))) +
  tm_lines(col = "commute_all",
           lwd = "commute_all",
           scale = 10,
           palette = "YlOrRd",
           style = "fisher",
           legend.lwd.show = FALSE,
           alpha = 1,
           title.col = "Aggregated demand",
           legend.col.is.portrait = FALSE) +
  tm_facets(by = "combination",
            free.coords = FALSE,
            nrow = 1) +
  tm_layout(fontfamily = 'Georgia',
            main.title = "OD demand aggregated along shortest paths",
            main.title.size = 1.3,
            main.title.color = "azure4",
            main.title.position = "left",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.stack = "horizontal",
            frame = FALSE)  -> map_aggregate_flows_combination_wkdaymorningnight

map_aggregate_flows_combination_wkdaymorningnight


tmap_save(tm =  map_aggregate_flows_combination_wkdaymorningnight, filename = paste0(plots_path, "map_aggregate_flows_combination_wkday_ampm.png"), width = 15, dpi = 1080)




ggplot(od_compare %>%
         filter(approach == "overline_merge"),
       aes(x = commute_all)) +
  geom_histogram(binwidth = 50) +
  facet_wrap(~combination, nrow = 4)

od_demand %>%
  group_by(combination) %>%
  summarise(od_pairs = n())



ggplot(od_compare %>%
         filter(approach == "overline"),
       aes(x = commute_all)) +
  geom_histogram(binwidth = 200) +
  facet_wrap(~combination, nrow = 4)












