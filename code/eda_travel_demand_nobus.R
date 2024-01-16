# This script proposes exploratory methods for answering the "PT vs DRT" question on a route-level #
# It builds on demand_no_direct_bus.R and visualises results overlayed with GTFS shapes

library(tidyverse)
library(sf)
library(tmap)
library(tidytransit)
library(UK2GTFS)



source("R/study_area_geographies.R")
source("R/read_gtfs.R")
source("R/filter_feed_date_time.R")


# ----------- 1. Load in the data

# ----- Study area

# --- administrative boundaries
study_area <- st_read("data/interim/study_area_boundary.geojson")

# convert to desired resolution
geography = "MSOA"
study_area = study_area_geographies(study_area = study_area,
                                    geography = geography)


# ----- Travel demand aggregated

od_demand_overline <- st_read(paste0("data/interim/travel_demand/", geography, "/demand_no_direct_overline.geojson"))

# ----- GTFS

# ------ read in the feeds in tidytransit compatible format + give them names
# gtfs_bus <- read_gtfs_feeds(feed_dir = "data/interim/", package = "tidytransit")

# --- define directory with gtfs feeds
feed_dir = "data/interim/gtfs_freq/"
# --- get names of feeds in a specific directory
feed_names <- list.files(feed_dir, ".zip$", full.names = FALSE)
# --- read in the gtfs feeds
gtfs_feeds <- read_gtfs_feeds(feed_dir = feed_dir, package = "tidytransit")
# --- assign each feed in the list a name (this is it's name in the directory)
names(gtfs_feeds) <- feed_names

# --- path to save plots
plots_path <- paste0("data/processed/plots/eda/flows_no_direct/", geography, "/")


# ---------- 2. Filter GTFS by time of day (one feed for each combination)

# feeds
gtfs_bus <- gtfs_feeds$study_area_gtfs_bus_f.zip
gtfs_rail <- gtfs_feeds$study_area_gtfs_rail_f.zip
# add shapes.txt to the rail feed
gtfs_rail <- UK2GTFS::ATOC_shapes(gtfs_rail)


# Define different scenarios for filtering
scenarios <- tribble(
  ~scenario, ~date, ~min_departure_time, ~max_arrival_time,
  # public transport at different times of day / week
  "pt_wkday_morning", "2023-08-14", "06:00:00", "09:00:00",
  "pt_wkday_afternoon", "2023-08-14", "11:00:00", "14:00:00",
  "pt_wkday_evening", "2023-08-14", "17:00:00", "20:00:00",
  "pt_wkday_night", "2023-08-14", "21:30:00", "23:59:00:00",
  "pt_wkend_morning", "2023-08-13", "06:00:00", "09:00:00",
  "pt_wkend_evening", "2023-08-13", "17:00:00", "20:00:00",
)


# Bus
gtfs_bus_filtered <- filter_gtfs_feed_internal(gtfs_feed = gtfs_bus,
                                               scenarios = scenarios,
                                               shapes_only = TRUE)





# Rail

gtfs_rail_filtered <- filter_gtfs_feed_internal(gtfs_feed = gtfs_rail,
                                                scenarios = scenarios,
                                                shapes_only = TRUE)



# ---------- 3. Get GTFS shape for each time of day

gtfs_bus_filtered_df <- bind_rows(gtfs_bus_filtered)
gtfs_rail_filtered_df <- bind_rows(gtfs_rail_filtered)

# Bus headway (morning peak)

tm_shape(study_area) +
  tm_borders(col = "grey60",
             alpha = 0.5) +
tm_shape(study_area) +
  tm_fill(col = "grey85",
          alpha = 0.5) +
  tm_shape(gtfs_bus_filtered_df %>%
             mutate(headway_inv = 1/headway_secs) %>%
             filter(headway_secs < 1800, scenario == "pt_wkday_morning")) +
  tm_lines(col = "headway_secs",
           lwd = "headway_inv",
           scale = 5,
           palette = "-YlOrRd",
           style = "fisher",
           legend.lwd.show = FALSE,
           alpha = 1,
           title.col = "Headway (seconds)",
           legend.col.is.portrait = FALSE) +
  tm_layout(fontfamily = 'Georgia',
            main.title = "Bus Headway (Morning Peak)",
            main.title.size = 1.3,
            main.title.color = "azure4",
            main.title.position = "left",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.stack = "horizontal",
            frame = FALSE)


# Bus headway (all scenarios)


tm_shape(study_area) +
  tm_borders(col = "grey60",
             alpha = 0.5) +
  tm_shape(study_area) +
  tm_fill(col = "grey85",
          alpha = 0.5) +
  tm_shape(gtfs_bus_filtered_df %>%
             mutate(headway_inv = 1/headway_secs) %>%
             filter(headway_secs < 3600)) +
  tm_lines(col = "headway_secs",
           lwd = "headway_inv",
           scale = 5,
           palette = "-YlOrRd",
           style = "fisher",
           legend.lwd.show = FALSE,
           alpha = 1,
           title.col = "Headway (seconds)",
           legend.col.is.portrait = FALSE) +
  tm_facets(by = "scenario",
            free.coords = FALSE,
            nrow = 2) +
  tm_layout(fontfamily = 'Georgia',
            main.title = "Bus Headway",
            main.title.size = 1.3,
            main.title.color = "azure4",
            main.title.position = "left",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.stack = "horizontal",
            frame = FALSE) -> map_headways_combinations_all_bus

map_headways_combinations_all_bus

tmap_save(tm =  map_headways_combinations_all_bus, filename = paste0(plots_path, "map_headways_combinations_all_bus.png"), width = 15, dpi = 1080)


# Rail headway (all scenarios)

tm_shape(study_area) +
  tm_borders(col = "grey60",
             alpha = 0.5) +
  tm_shape(study_area) +
  tm_fill(col = "grey85",
          alpha = 0.5) +
  tm_shape(gtfs_rail_filtered_df %>%
             mutate(headway_inv = 1/headway_secs) %>%
             filter(headway_secs < 7200)) +
  tm_lines(col = "headway_secs",
           lwd = "headway_inv",
           scale = 5,
           palette = "-YlOrRd",
           style = "pretty",
           legend.lwd.show = FALSE,
           alpha = 1,
           title.col = "Headway (seconds)",
           legend.col.is.portrait = FALSE) +
  tm_facets(by = "scenario",
            free.coords = FALSE,
            nrow = 2) +
  tm_layout(fontfamily = 'Georgia',
            main.title = "Bus Headway",
            main.title.size = 1.3,
            main.title.color = "azure4",
            main.title.position = "left",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.stack = "horizontal",
            frame = FALSE) -> map_headways_combinations_all_rail

map_headways_combinations_all_rail

tmap_save(tm =  map_headways_combinations_all_rail, filename = paste0(plots_path, "map_headways_combinations_all_rail.png"), width = 15, dpi = 1080)




# Bus headway overlayed on aggregate demand (morning peak)

tm_shape(study_area) +
  tm_borders(col = "grey60",
             alpha = 0.5) +
tm_shape(study_area) +
  tm_fill(col = "grey85",
          alpha = 0.5) +
tm_shape(od_demand_overline %>%
           filter(combination == "pt_wkday_morning")) +
  tm_lines(col = "commute_all",
           #lwd = "commute_all",
           scale = 5,
           palette = "Blues",
           style = "fisher",
           #legend.lwd.show = FALSE,
           alpha = 1,
           title.col = "Aggregated demand",
           legend.col.is.portrait = FALSE) +
tm_shape(gtfs_bus_filtered_df %>%
             mutate(vehicles_per_hour = round(1/(headway_secs/3600))) %>%
             filter(headway_secs < 3600, scenario == "pt_wkday_morning")) +
  tm_lines(col = "darkred",
           lwd = "vehicles_per_hour",
           scale = 15,
           #palette = "-YlOrRd",
           style = "fisher",
           alpha = 0.2,
           title.lwd = "Vehicles / Hour",
           legend.lwd.is.portrait = FALSE) +
tm_layout(fontfamily = 'Georgia',
            main.title = "Bus headway overlayed over aggregate demand\n(Morning Peak)",
            main.title.size = 1.3,
            main.title.color = "azure4",
            main.title.position = "left",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.stack = "horizontal",
            frame = FALSE)

# Bus + Rail headway overlayed on aggregate demand (morning peak)



tm_shape(study_area) +
  tm_borders(col = "grey60",
             alpha = 0.5) +
  tm_shape(study_area) +
  tm_fill(col = "grey85",
          alpha = 0.5) +
  tm_shape(od_demand_overline %>%
             filter(combination == "pt_wkday_morning")) +
  tm_lines(col = "commute_all",
           #lwd = "commute_all",
           scale = 5,
           palette = "Blues",
           style = "fisher",
           #legend.lwd.show = FALSE,
           alpha = 1,
           title.col = "Aggregated demand",
           legend.col.is.portrait = FALSE) +
tm_shape(gtfs_bus_filtered_df %>%
             mutate(vehicles_per_hour = round(1/(headway_secs/3600))) %>%
             filter(headway_secs < 3600, scenario == "pt_wkday_morning")) +
  tm_lines(col = "darkred",
           lwd = "vehicles_per_hour",
           scale = 15,
           #palette = "-YlOrRd",
           style = "fisher",
           alpha = 0.2,
           title.lwd = "Vehicles / Hour",
           legend.lwd.is.portrait = FALSE) +
tm_shape(gtfs_rail_filtered_df %>%
             mutate(vehicles_per_hour = round(1/(headway_secs/3600))) %>%
             filter(headway_secs < 7200, scenario == "pt_wkday_morning")) +
  tm_lines(col = "darkgreen",
           lwd = "vehicles_per_hour",
           scale = 5,
           #palette = "-YlOrRd",
           style = "fisher",
           alpha = 0.2,
           title.lwd = "Vehicles / Hour",
           legend.lwd.is.portrait = FALSE) +
  tm_layout(fontfamily = 'Georgia',
            main.title = "Bus headway overlayed over aggregate demand\n(Morning Peak)",
            main.title.size = 1.3,
            main.title.color = "azure4",
            main.title.position = "left",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.stack = "horizontal",
            frame = FALSE)


# Bus headway overlayed on aggregate demand (facet by time of day)

tm_shape(study_area) +
  tm_borders(col = "grey60",
             alpha = 0.5) +
  tm_shape(study_area) +
  tm_fill(col = "grey90",
          alpha = 0.5) +
  tm_shape(od_demand_overline %>%
             mutate(commute_all = round(commute_all / 1000))) +
  tm_lines(col = "commute_all",
           #lwd = "commute_all",
           scale = 5,
           palette = "Greens", #YlGn
           style = "fisher",
           #legend.lwd.show = FALSE,
           alpha = 1,
           title.col = "Aggregated demand ('000)",
           legend.col.is.portrait = FALSE) +
  tm_facets(by = "combination",
            free.coords = FALSE,
            ncol = 3) +
  tm_shape(gtfs_bus_filtered_df %>%
             mutate(vehicles_per_hour = round(1/(headway_secs/3600))) %>%
             filter(headway_secs < 3600)) +
  tm_lines(col = "darkred",
           lwd = "vehicles_per_hour",
           scale = 10,
           #palette = "-YlOrRd",
           style = "fisher",
           alpha = 0.3,
           title.lwd = "Vehicles/hr",
           legend.lwd.is.portrait = FALSE) +
  tm_facets(by = "scenario",
            free.coords = FALSE,
            ncol = 3) +
  tm_layout(fontfamily = 'Georgia',
            main.title = "Bus headway overlayed over aggregate demand",
            main.title.size = 1.3,
            main.title.color = "azure4",
            main.title.position = "left",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.stack = "horizontal",
            frame = FALSE) -> map_headways_demand_combinations_all_bus

map_headways_demand_combinations_all_bus

tmap_save(tm =  map_headways_demand_combinations_all_bus, filename = paste0(plots_path, "map_headways_demand_combinations_all_bus.png"), width = 15, dpi = 1080)




# ------------------ Identify areas with no bus routes

gtfs_bus_filtered_df <- gtfs_bus_filtered_df %>%
  st_transform(3857) %>% st_buffer(500)






