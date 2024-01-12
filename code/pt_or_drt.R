# This script proposes exploratory methods for answering the "PT vs DRT" question on a route-level #

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

# OD pairs without direct route
od_demand_no_direct = od_demand %>%
  filter(n_rides > 1)

# --- b. add shortest path geometry

od_demand_no_direct <- od_demand_no_direct %>%
  left_join(od_shortest_paths, by = c("from_id", "to_id")) %>%
  st_as_sf()

# --- placeholder: nest sf by combination in order to apply overline and merge per group

# --- c. OPTION 1: use overline to get total flow on road network

# nest sf data by combination and apply overline per nested df
od_demand_filtered <- od_demand_no_direct %>%
  group_by(combination) %>%
  nest()

# use map to apply overline function to all groups
od_demand_overline <- od_demand_filtered %>%
  # create a new df for each group
  mutate(demand = map(data, ~ stplanr::overline(sl = .x,
                                                attrib = "commute_all",
                                                ncores = 3,
                                                fun = sum))) %>%
  select(-data) %>%
  # turn back into one big sf
  unnest(demand)







# --- d. OPTION 2: use overline + network simplification methods to remove parallel lines


# use map to apply overline() + simplify_graph() function to all groups
od_demand_overline_merge <- od_demand_filtered %>%
  # create a new df for each group
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
  unnest(demand_merged)




# compare the results



od_compare <- od_demand_filtered %>%
  select(commute_all) %>%
  mutate(approach = "no_processing") %>%
  bind_rows(od_demand_overline %>%
              mutate(approach = "overline")) %>%
  bind_rows(od_demand_overline_merge %>%
              mutate(approach = "overline_merge"))


ggplot(od_compare %>%
         filter(approach != "no_processing", combination = "pt_wkday_morning"),
       aes(x = commute_all, fill = approach)) +
  geom_histogram(binwidth = 50) +
  facet_wrap(~approach, nrow = 4)


tmap_mode("view")
tmap_mode("plot")


# Compare overline to base
tm_shape(study_area) +
  tm_borders(col = "grey60",
             alpha = 0.5) +
tm_shape(study_area) +
  tm_fill(col = "indianred2", #grey80
          alpha = 0.5) +
tm_shape(od_compare %>%
           filter(approach %in% c("no_processing", "overline"))) +
  tm_lines(col = "commute_all",
           lwd = "commute_all",
           scale = 15,
           palette = "Greens",
           style = "jenks", # pretty
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
  tm_shape(od_compare %>%
             filter(approach %in% c("overline", "overline_merge"))) +
  tm_lines(col = "commute_all",
           lwd = "commute_all",
           scale = 10,
           palette = "Greens",
           style = "jenks", # pretty
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
tm_shape(od_demand_overline2_merge %>%
             filter(commute_all > 1000)) +
  tm_lines(col = "commute_all",
           lwd = "commute_all",
           scale = 10,
           palette = "Greens",
           style = "jenks",
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





# compare

od_compare2 <- od_demand_fi %>%
  select(commute_all) %>%
  mutate(approach = "no_processing") %>%
  bind_rows(od_demand_merge %>%
              mutate(approach = "sfnet_merge"))




tm_shape(study_area) +
  tm_borders(col = "grey60",
             alpha = 0.5) +
  tm_shape(study_area) +
  tm_fill(col = "grey80",
          alpha = 0.5) +
  tm_shape(od_compare2) +
  tm_lines(col = "commute_all",
           lwd = "commute_all",
           scale = 10,
           #palette = "Blues",
           style = "quantile",
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
            frame = FALSE)
























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

