#  This script proposes exploratory methods for answering the "PT vs DRT" question on a route-level   #
#  It builds on demand_no_direct_bus.R and visualises results overlayed with GTFS shapes              #
#  It also shows the overlap between bus routes and od demand:
#       To what extent does each demand pair overlap with existing bus routes (fraction of overlap)?  #
#       What is the headway of the buses that overlap?                                                #

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
             filter(headway_secs < 3600, scenario == "pt_wkday_morning")) +
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




########## ------------------ Identify areas with no bus routes ------------------ ##########

# Buffer gtfs feed

##### ---------- OPTION 1: Aggregated demand (od_demand_overline)


# Logic:

# # Perform spatial join for each group in 'a' with the corresponding geometry in 'b'
# result_list <- map(1:nrow(gtfs_bus_filtered_df_buff), function(i) {
#   # get the names of the combinations that exist in both sf features (for filtering)
#   combinations <- unique(od_demand_overline$combination)
#   # filter the demand data so that it only includes values from a specific combination / scenario
#   od_demand_oveline_filt <- od_demand_overline %>% filter(combination == combinations[i])
#   # spatial filter the demand data by the gtfs bus geometry
#   result <- st_filter(od_demand_oveline_filt,
#                       gtfs_bus_filtered_df_buff[gtfs_bus_filtered_df_buff$scenario == combinations[i],],
#                       .predicate = st_disjoint)
# })
#
# # Combine the results into a single sf object
# final_result <- do.call(rbind, result_list)



#' Function to get all geometries from a that do not intersect with b
#'
#' It is a basic st_disjoint but it is applied per group
#'
#' @param a an sf with columns "commute_all", "combination"
#' @param b an sf with column "scenario" that matches onto "combination in "a"
#' @param buffer how big should the buffer around b be?
#' @return an sf with all geometries in a that do not intersect with a. geometry operation is done per group#'
#' @examples
#'
#' @export
filter_by_element = function(a, b, buffer){

  # create buffer around geometry for intersection operation
  b_buffer <- b %>%
    st_transform(3857) %>% st_buffer(buffer) %>%
    group_by(scenario) %>%
    summarise(geometry = st_union(geometry)) %>%
    st_transform(4326) %>%
    st_make_valid()
  # Perform spatial join for each group in 'a' with the corresponding geometry in 'b'
  result_list <- map(1:nrow(b_buffer), function(i) {
    # get the names of the combinations that exist in both sf features (for filtering)
    combinations <- unique(a$combination)
    # filter the demand data so that it only includes values from a specific combination / scenario
    a_filt <- a %>% filter(combination == combinations[i])
    # spatial filter the demand data by the gtfs bus geometry
    result <- st_filter(a_filt,
                        b_buffer[b_buffer$scenario == combinations[i],],
                        .predicate = st_disjoint)
  })

  # Combine the results into a single sf object
  final_result <- do.call(rbind, result_list)
  return(final_result)
}

# ----- All demand that does not overlap with any bus route - with buses

demand_disjoint_buses_all <- filter_by_element(od_demand_overline, gtfs_bus_filtered_df, buffer = 500)

# plot

tm_shape(study_area) +
  tm_borders(col = "grey60",
             alpha = 0.5) +
  tm_shape(study_area) +
  tm_fill(col = "grey90",
          alpha = 0.5) +
  tm_shape(demand_disjoint_buses_all %>%
             mutate(commute_all = round(commute_all / 1000))) +
  tm_lines(col = "commute_all",
           #lwd = "commute_all",
           scale = 10,
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
          main.title = "Travel demand not overlapping\n with any bus routes",
          main.title.size = 1.3,
          main.title.color = "azure4",
          main.title.position = "left",
          legend.outside = TRUE,
          legend.outside.position = "bottom",
          legend.stack = "horizontal",
          frame = FALSE)


# ----- All demand that does not overlap with any bus route - without buses
tm_shape(study_area) +
  tm_borders(col = "grey60",
             alpha = 0.5) +
  tm_shape(study_area) +
  tm_fill(col = "grey95",
          alpha = 0.5) +
  tm_shape(demand_disjoint_buses_all %>%
             mutate(commute_all = round(commute_all / 1000))) +
  tm_lines(col = "commute_all",
           #lwd = "commute_all",
           scale = 10,
           palette = "Greens", #YlGn
           style = "fisher",
           #legend.lwd.show = FALSE,
           alpha = 1,
           title.col = "Aggregated demand ('000)",
           legend.col.is.portrait = FALSE) +
  tm_facets(by = "combination",
            free.coords = FALSE,
            ncol = 3) +
  tm_layout(fontfamily = 'Georgia',
            main.title = "Travel demand not overlapping\n with any bus routes",
            main.title.size = 1.3,
            main.title.color = "azure4",
            main.title.position = "left",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.stack = "horizontal",
            frame = FALSE) -> map_demand_disjoint_all_buses_combinations_nobus

map_demand_disjoint_all_buses_combinations_nobus

tmap_save(tm =  map_demand_disjoint_all_buses_combinations_nobus, filename = paste0(plots_path, "map_demand_disjoint_all_buses_combinations_nobus.png"), width = 15, dpi = 1080)




# ----- All demand that does not overlap with any HIGH-FREQUENCY bus route (2 bus per hour) - With buses

demand_disjoint_buses_highfreq <- filter_by_element(od_demand_overline,
                                                    gtfs_bus_filtered_df %>%
                                                      filter(headway_secs < 1800),
                                                    buffer = 500)


# ----- All demand that does not overlap with any HIGH-FREQUENCY bus route (2 bus per hour) - With buses


# plot

tm_shape(study_area) +
  tm_borders(col = "grey60",
             alpha = 0.5) +
  tm_shape(study_area) +
  tm_fill(col = "grey90",
          alpha = 0.5) +
  tm_shape(demand_disjoint_buses_highfreq %>%
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
             filter(headway_secs < 1800)) +
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
            main.title = "Travel demand not overlapping with\n any HIGH FREQUENCY bus routes",
            main.title.size = 1.3,
            main.title.color = "azure4",
            main.title.position = "left",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.stack = "horizontal",
            frame = FALSE) -> map_demand_disjoint_high_freq_buses_combinations_bus

map_demand_disjoint_high_freq_buses_combinations_bus

tmap_save(tm =  map_demand_disjoint_high_freq_buses_combinations_bus, filename = paste0(plots_path, "map_demand_disjoint_high_freq_buses_combinations_bus.png"), width = 15, dpi = 1080)


# ----- All demand that does not overlap with any HIGH-FREQUENCY bus route (2 bus per hour) - Without buses

tm_shape(study_area) +
  tm_borders(col = "grey60",
             alpha = 0.5) +
  tm_shape(study_area) +
  tm_fill(col = "grey95",
          alpha = 0.5) +
  tm_shape(demand_disjoint_buses_highfreq %>%
             mutate(commute_all = round(commute_all / 1000))) +
  tm_lines(col = "commute_all",
           #lwd = "commute_all",
           scale = 10,
           palette = "Greens", #YlGn
           style = "fisher",
           #legend.lwd.show = FALSE,
           alpha = 1,
           title.col = "Aggregated demand ('000)",
           legend.col.is.portrait = FALSE) +
  tm_facets(by = "combination",
            free.coords = FALSE,
            ncol = 3) +
  tm_layout(fontfamily = 'Georgia',
            main.title = "Travel demand not overlapping with\n any HIGH FREQUENCY bus routes",
            main.title.size = 1.3,
            main.title.color = "azure4",
            main.title.position = "left",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.stack = "horizontal",
            frame = FALSE)  -> map_demand_disjoint_high_freq_buses_combinations_nobus

map_demand_disjoint_high_freq_buses_combinations_nobus

tmap_save(tm =  map_demand_disjoint_high_freq_buses_combinations_nobus, filename = paste0(plots_path, "map_demand_disjoint_high_freq_buses_combinations_nobus.png"), width = 15, dpi = 1080)



##### ---------- OPTION 2: Demand on each route (od_demand_no_direct)
# --- for each demand route: get high frequency bus that intersects with it the most + get %of demand that is covered by that bus

od_demand_no_direct = readRDS(paste0("data/interim/travel_demand/", geography, "/demand_no_direct.Rds"))



#' Function to get the bus route that overlaps most with each od_demand shortest path
#'
#' @param od_demand an sf with the shortest path for each od pair
#' @param gtfs an sf with all the bus routes and their respective headways. Column "scenario" in gtfs matches onto column "combination" in od_demand
#' @param buffer radius (meters) that we want to buffer gtfs by before intersection operation
#' @param max_headway which routes from the gtfs do we keep? all those with headway_secs < max_headway
#'
#' @return an sf with "Origin" "Destination" "combination" "commute_all" "length_total" from the od_demand sf.
#' The geometry column is a LINESTRING with the (largest intersection) between the shortest path and the route
#' that intersected with it most. Columns taken from the intersected gtfs sf are "trip_id" "headway_secs" "length_int" "geometry"
#' @examples
#' @export
get_most_overlapping_route = function(od_demand, gtfs, buffer = 500, max_headway = 3600){

  # vector to store the results
  results = vector(mode = "list", length = nrow(gtfs))

  # --- keep only the columns we want
  od_demand <- od_demand %>%
    select(Origin, Destination, commute_all, combination) %>%
    mutate(length_total = st_length(.))

  # get the names of the combinations that exist in both sf features (for filtering)
  combinations <- unique(od_demand$combination)

  # --- Perform spatial join for each group in 'od_demand' with the corresponding geometry in 'gtfs_buff'
  for(i in 1:length(combinations)){

    print(paste0(Sys.time(), " ... Buffering GTFS in preperation for spatial join for scenario: ", combinations[i] ))
    # --- create buffer around gtfs geometry for intersection operation
    gtfs_buff = gtfs %>%
      filter(scenario == combinations[i], headway_secs <= max_headway) %>%
      st_transform(3857) %>% st_buffer(buffer) %>%
      st_transform(4326) %>%
      st_make_valid()

    print(paste0(Sys.time(), " ... Filtering geometries for scenario: ", combinations[i]))
    # get geometries from demand and gtfs that match scenario i
    od_demand_filt_i <- od_demand[od_demand$combination == combinations[i],]
    gtfs_buff_filt_i <- gtfs_buff[gtfs_buff$scenario == combinations[i],]

    print(paste0(Sys.time(), " ... Intersecting geometries for scenario: ", combinations[i]))
    # --- get the geometry from od_demand that intersects with the gtfs buffer (this returns a multilinestring for each od_demand_row)
    od_demand_int_i <- st_intersection(od_demand_filt_i, gtfs_buff_filt_i)

    # --- get largest intersection: the largest overlap between a bus route and an OD demand

    # convert from "MULTILINESTRING" to "LINESTRING"
    od_demand_int_ls_i <- od_demand_int_i %>% st_cast("MULTILINESTRING") %>% st_cast("LINESTRING")

    # keep the longest linestring intersection only
    od_demand_int_ls_i <- od_demand_int_ls_i %>%
      mutate(length_int = st_length(.)) %>%
      group_by(Origin, Destination, combination) %>%
      arrange(desc(length_int)) %>%
      slice(1) %>%
      ungroup() %>%
      select(Origin, Destination, combination, trip_id, headway_secs, length_int)

    print(paste0(Sys.time(), " ... Joining result onto  for scenario: ", combinations[i]))
    # --- merge length of intersection back onto the original sf
    od_demand_int_ls_i_joined <- od_demand_int_ls_i %>%
      left_join(od_demand_filt_i %>%
                  st_drop_geometry(),
                by = c("Origin", "Destination", "combination"))

    # add ttm to results list
    results[[i]] <- od_demand_int_ls_i_joined
    #status updates
    print(paste0("COMPLETED SCENARIO: ", combinations[i], " ....."))

  }

  # combine list into 1 dataframe
  results <- bind_rows(results)
  return(results)

}

od_demand_highest_route_overlap <- get_most_overlapping_route(od_demand = od_demand_no_direct,
                                                              gtfs = gtfs_bus_filtered_df,
                                                              buffer = 500,
                                                              max_headway = 7200)

# add some stats on overlap length / %
od_demand_highest_route_overlap <- od_demand_highest_route_overlap %>%
  mutate(overlap_frac = as.numeric(round(length_int / length_total, 1)))

# save
saveRDS(od_demand_highest_route_overlap, paste0("data/interim/travel_demand/", geography, "/od_demand_highest_route_overlap.Rds"))

#' Function to get the difference between two sfs PER ROW
#'
#' @param od_demand
#' @param od_intersection an sf with "Origin", "Destination", and "combination"
#' @return an sf with "Origin", "Destination", and "combination" and a gepmetry column
#' that represents the difference between od_Demand and od_intersection
#' @examples
#'
#' @export
get_overlap_inverse = function(od_demand, od_intersection){
  # prepare layers
  od_demand_df <- od_demand %>% as.data.frame()
  od_intersection_df <- od_intersection %>%
    st_transform(3857) %>% st_buffer(500) %>% st_transform(4326) %>%
    as.data.frame()

  # join layers so that we have a geometry.x and a geometry.y
  od_demand_joined <- od_demand_df %>%
    inner_join(od_intersection_df,
               by = c("Origin", "Destination", "combination"))

  # apply st_difference per row
  od_demand_diff <- od_demand_joined %>%
    rowwise() %>%
    # if the geometries overlap completely, then there is no "difference" geometry. This causes an error which we avoid using an ifelse
    mutate(difference = ifelse(
      length(st_difference(geometry.x, geometry.y)) > 0,
      # If there is a geometry
      st_difference(geometry.x, geometry.y) %>%
        st_collection_extract("LINESTRING"),
      # Otherwise
      NA)) %>%
    # # remove rows with no difference geom
    # filter(!is.null(difference)) %>%
    # convert to one MULTILINESTRING (instead of one LINESTRING). In mutate, we can only have one value per row
    group_by(Origin, Destination, combination) %>%
    summarise(difference = st_cast(do.call(st_sfc, difference), "MULTILINESTRING")) %>%
    ungroup() %>%
    st_as_sf() %>%
    st_set_crs(st_crs(od_intersection))

  # add metadata back
  od_demand_diff <- od_demand_diff %>%
    left_join(od_intersection %>%
                st_drop_geometry(),
              by = c("Origin", "Destination", "combination"))

  return(od_demand_diff)
  }



# apply the function that gets the parts of the demand shortest path that DON'T overlap with the bus geometry
od_demand_highest_route_overlap_diff <- get_overlap_inverse(od_demand = od_demand_no_direct,
                                                            od_intersection = od_demand_highest_route_overlap)


# prepare layers
a_df <- od_demand_no_direct[1:100,] %>% as.data.frame()
b_df <- od_demand_highest_route_overlap[1:100,] %>%
  st_transform(3857) %>% st_buffer(500) %>% st_transform(4326) %>%
  as.data.frame()

# join layers so that we have a geometry.x and a geometry.y
c_df <- a_df %>%
  inner_join(b_df,
             by = c("Origin", "Destination", "combination"))

# apply st_difference per row
d_df <- c_df %>%
  rowwise() %>%
  # if the geometries overlap completely, then there is no "difference" geometry. This causes an error which we avoid using an ifelse
  mutate(difference = ifelse(
    length(st_difference(geometry.x, geometry.y)) > 0,
    # If there is a geometry
    st_difference(geometry.x, geometry.y) %>%
      st_collection_extract("LINESTRING"),
    # Otherwise
    NA)) %>%
  # convert to one MULTILINESTRING (instead of one LINESTRING). In mutate, we can only have one value per row
  group_by(Origin, Destination, combination) %>%
  summarise(difference = st_cast(do.call(st_sfc, difference), "MULTILINESTRING")) %>%
  ungroup() %>%
  st_as_sf() %>%
  st_set_crs(st_crs(od_intersection))


test_og <- od_demand_no_direct %>% filter(Origin == "E02002330", Destination == "E02002403", combination == "pt_wkday_afternoon")
test_filt <- od_demand_highest_route_overlap %>% filter(Origin == "E02002330", Destination == "E02002403", combination == "pt_wkday_afternoon")
test_filt_inv <- od_demand_highest_route_overlap_diff %>% filter(Origin == "E02002330", Destination == "E02002403", combination == "pt_wkday_afternoon")

plot(st_geometry(study_area))
plot(st_geometry(test_og))
plot(st_geometry(test_og), add = TRUE, col = "blue")
plot(st_geometry(test_filt), add = TRUE, col = "red")
plot(st_geometry(test_filt_inv), add = TRUE, col = "green")




# Plot


# Weekday Morning Peak

tm_shape(study_area) +
  tm_borders(col = "grey60",
             alpha = 0.5) +
tm_shape(study_area) +
  tm_fill(col = "grey95",
          alpha = 0.5) +
  tm_shape(od_demand_highest_route_overlap_diff %>%
             mutate(overlap_frac_fct = cut(overlap_frac, breaks = seq(0, 1, by = 0.25)),
                    vehicles_per_hour = round(1/(headway_secs/3600))) %>%
             filter(combination == "pt_wkday_morning")) +
  tm_lines(col = "commute_all",
           lwd = "vehicles_per_hour",
           scale = 5,
           palette = "Reds", #YlGn
           style = "pretty",
           alpha = 1,
           title.col = "Aggregated demand",
           title.lwd = "Vehicles per hour",
           legend.col.is.portrait = FALSE) +
  tm_facets(by = "overlap_frac_fct",
            free.coords = FALSE,
            ncol = 2) +
  tm_layout(fontfamily = 'Georgia',
            main.title = "Travel demand not overlapping with any bus routes (Weekday Morning). \nFacets show fraction of demand that overlaps with a bus route",
            main.title.size = 1.1,
            main.title.color = "azure4",
            main.title.position = "left",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.stack = "horizontal",
            frame = FALSE) -> map_demand_overlap_gtfs_st_diff_morning_wkday

map_demand_overlap_gtfs_st_diff_morning_wkday

tmap_save(tm =  map_demand_overlap_gtfs_st_diff_morning_wkday, filename = paste0(plots_path, "map_demand_overlap_gtfs_st_diff_morning_wkday.png"), width = 15, dpi = 1080)



# Weekday Evening


tm_shape(study_area) +
  tm_borders(col = "grey60",
             alpha = 0.5) +
  tm_shape(study_area) +
  tm_fill(col = "grey95",
          alpha = 0.5) +
  tm_shape(od_demand_highest_route_overlap_diff %>%
             mutate(overlap_frac_fct = cut(overlap_frac, breaks = seq(0, 1, by = 0.25)),
                    vehicles_per_hour = round(1/(headway_secs/3600))) %>%
             filter(combination == "pt_wkday_evening")) +
  tm_lines(col = "commute_all",
           lwd = "vehicles_per_hour",
           scale = 5,
           palette = "Reds", #YlGn
           style = "pretty",
           alpha = 1,
           title.col = "Aggregated demand",
           title.lwd = "Vehicles per hour",
           legend.col.is.portrait = FALSE) +
  tm_facets(by = "overlap_frac_fct",
            free.coords = FALSE,
            ncol = 2) +
  tm_layout(fontfamily = 'Georgia',
            main.title = "Travel demand not overlapping with any bus routes (Weekday Evening). \nFacets show fraction of demand that overlaps with a bus route",
            main.title.size = 1.1,
            main.title.color = "azure4",
            main.title.position = "left",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.stack = "horizontal",
            frame = FALSE) -> map_demand_overlap_gtfs_st_diff_evening_wkday

map_demand_overlap_gtfs_st_diff_evening_wkday

tmap_save(tm =  map_demand_overlap_gtfs_st_diff_evening_wkday, filename = paste0(plots_path, "map_demand_overlap_gtfs_st_diff_evening_wkday.png"), width = 15, dpi = 1080)




# Weekend Evening


tm_shape(study_area) +
  tm_borders(col = "grey60",
             alpha = 0.5) +
  tm_shape(study_area) +
  tm_fill(col = "grey95",
          alpha = 0.5) +
  tm_shape(od_demand_highest_route_overlap_diff %>%
             mutate(overlap_frac_fct = cut(overlap_frac, breaks = seq(0, 1, by = 0.25)),
                    vehicles_per_hour = round(1/(headway_secs/3600))) %>%
             filter(combination == "pt_wkend_evening")) +
  tm_lines(col = "commute_all",
           lwd = "vehicles_per_hour",
           scale = 10,
           palette = "Reds", #YlGn
           style = "pretty",
           alpha = 1,
           title.col = "Aggregated demand",
           title.lwd = "Vehicles per hour",
           legend.col.is.portrait = FALSE) +
  tm_facets(by = "overlap_frac_fct",
            free.coords = FALSE,
            ncol = 2) +
  tm_layout(fontfamily = 'Georgia',
            main.title = "Travel demand not overlapping with any bus routes (Weekend Evening). \nFacets show fraction of demand that overlaps with a bus route",
            main.title.size = 1.1,
            main.title.color = "azure4",
            main.title.position = "left",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.stack = "horizontal",
            frame = FALSE) -> map_demand_overlap_gtfs_st_diff_evening_wkend

map_demand_overlap_gtfs_st_diff_evening_wkend

tmap_save(tm =  map_demand_overlap_gtfs_st_diff_evening_wkend, filename = paste0(plots_path, "map_demand_overlap_gtfs_st_diff_evening_wkend.png"), width = 15, dpi = 1080)




