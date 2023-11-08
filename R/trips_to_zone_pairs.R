

#' Identify which OD pairs are served by each trip
#'
#' this function is used to identify all OD pairs that can be served directly by
#' each trip in the gtfs feed. It uses the \code{gtfs_trips_zone_coverage} and
#' the \code{generate_ordered_pairs} functions defined below.
#'
#'
#' @param gtfs a gtfs feed with a frequencies.txt file
#' @param zones a zonal sf layer of our study area
#' @param zone_column the column with the zone_id
#'
#' @return a dataframe with one row for each od pair served by a trip. The row has trip_id, start_time, headway_sec, Origin, Destination
#' @examples
#'
#' @export
gtfs_trips_od_coverage = function(gtfs, zones, zone_column){
  # --- 1: Identify which zones are served by each trip
  trips_zones <- gtfs_trips_zone_coverage(gtfs = gtfs, zones = zones)
  message(paste0("... identified ", length(zone_column), " zones that are along each trip itinerary"))
  # --- 2: Identify which zones each trip passes by
  message(paste0("identifying od zone pairs that are along each trip itinerary: ..."))

  # group the zones by trip_id
  trips_zones_grouped <- trips_zones %>% group_by(trip_id, start_time, headway_secs)
  # nest each group
  trips_zones_grouped <- nest(trips_zones_grouped)
  # get od pairs served by each trip
  trips_zones_grouped <- mutate(trips_zones_grouped,
                                od_pairs = map(data, ~ generate_ordered_pairs(.x[[zone_column]])))
  # unnest and drop unneseccary data
  trips_zones_unnested <- unnest(trips_zones_grouped, od_pairs) %>%
    select(-data) %>%
    ungroup()
  message("... done!")

  return(trips_zones_unnested)
}

# ---------- Function 1: Identify which zones are served by each trip

#' Identify which zones are served by each trip
#'
#' # each trip passes through a number of zones. We want a list of these zones. We do this by:
#'  a) identifying the stops associated with each trip
#'  b) doing a spatial join between stops and zones and using that to match trips to zones
#'
#'  This gives us a spatial df that has, for each trip, the unique stops that serve
#'  it and the zone that the stop falls in
#'
#' @param gtfs: a gtfs feed
#' @param zones: a zonal sf layer of our study area
#'
#' @return a dataframe with the trip_id and data on the intersected zones. The df has one row for each zone that a trip intersects
#' @examples
#'
#' @export
gtfs_trips_zone_coverage = function(gtfs, zones){
  # get stop geometry
  gtfs_stops <- gtfstools::convert_stops_to_sf(gtfs, crs = st_crs(zones))

  # --- identify which zone each stop is in
  gtfs_stops_int <- st_join(gtfs_stops, zones,
                            join = st_intersects,
                            left = FALSE)

  # ----------- 2. Identify which zones each trip passes by

  # ------ stop_times has all the stops visited by each trip. Join the stop geometry onto that data
  gtfs_trips_int <- left_join(gtfs$stop_times, gtfs_stops_int, by = "stop_id")

  # ----------- 3.  add info from frequencies.txt (start_time and headway_secs)
  gtfs_trips_int <- left_join(gtfs_trips_int, gtfs$frequencies, by = "trip_id")

  return(gtfs_trips_int)
}


# ---------- Function 2: Identify which OD pairs are served by each trip

# We have data on the zones served by each trip. We also know the order in which
# the zones are served (through the stop sequence). We use this information to get
# the OD pairs served by each trip. To obtain OD pairs, we need to respect the order
# in which zones are served. If a trip follows the itinerary: "zone1", "zone4", "zone8",
# then it serves

# zone1 : zone4
# zone1 : zone8
# zone4 : zone8

# It DOES NOT serve the return journeys (i.e. zone8 : zone4)



#' Function to generate valid pairs respecting original order
#'
#' This function is used to determine all OD pairs served directly by a trip
#' @param zones the row in a df with the zone_id
#'
#' @return a df with the following columns: trip_id, Origin, Destination. One row
#' for each valid OD pair + trip combination
#' @details We have data on the zones served by each trip. We also know the order in which
#' the zones are served (through the stop sequence). We use this information to get
#' the OD pairs served by each trip. To obtain OD pairs, we need to respect the order
#' in which zones are served. If a trip follows the itinerary: "zone1", "zone4", "zone8",
#' then it serves:
#'    zone1 : zone4
#'    zone1 : zone8
#'    zone4 : zone8
#'
#' It DOES NOT serve the return journeys (i.e. zone8 : zone4)
#' @examples
#'
#' @export
generate_ordered_pairs <- function(zones) {
  valid_pairs <- list()
  for (i in 1:(length(zones))) {
    for (j in seq(i,length(zones))) {
      valid_pairs[[length(valid_pairs) + 1]] <- data.frame(Origin = zones[i], Destination = zones[j])
    }
  }
  # turn into a df
  valid_pairs <- do.call(rbind, valid_pairs)
  # remove duplicate pairs
  valid_pairs <- distinct(valid_pairs, Origin, Destination)
  return(valid_pairs)
}


#' Function to generate valid pairs respecting original order
#'
#' This function is used to determine all OD pairs served directly by a trip
#' @param zones the row in a df with the zone_id
#'
#' @return a df with the following columns: trip_id, Origin, Destination. One row
#' for each valid OD pair + trip combination
#' @details We have data on the zones served by each trip. We also know the order in which
#' the zones are served (through the stop sequence). We use this information to get
#' the OD pairs served by each trip. To obtain OD pairs, we need to respect the order
#' in which zones are served. If a trip follows the itinerary: "zone1", "zone4", "zone8",
#' then it serves:
#'    zone1 : zone4
#'    zone1 : zone8
#'    zone4 : zone8
#'
#' It DOES NOT serve the return journeys (i.e. zone8 : zone4)
#' @examples
#'
#' @export
# generate_ordered_pairs2 <- function(zones) {
#   # Determine the number of pairs in order to create a vector with pre-allocated size:
#     # # we use the combinations with replacement function
#     # combinations <- choose(n + k - 1, k):
#     # n = total number of elements in the set;
#     # k = number of elements in each combination
#   combinations <- choose(length(zones) + 2 - 1, 2)
#   # empty list to store dataframe result
#   valid_pairs <- vector(mode = "list", length = combinations)
#   for (i in 1:(length(zones))) {
#     # j always starts at the position of i, never before
#     for (j in seq(i,length(zones))) {
#       valid_pairs[[j + ((i-1)*length(zones)) ]] <- data.frame(Origin = zones[i], Destination = zones[j])
#     }
#   }
#   # turn into a df
#   valid_pairs <- do.call(rbind, valid_pairs)
#   # remove duplicate pairs
#   valid_pairs <- distinct(valid_pairs, Origin, Destination)
#   return(valid_pairs)
#
# }





