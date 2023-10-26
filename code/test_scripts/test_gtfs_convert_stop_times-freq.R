### The purpose of this script is to explore how trips and routes are coded in UK gtfs feeds. This is  ###
### because i'm exploring how to create a stop_times_to_frequencies() function                         ###
### See this issue: https://github.com/Hussein-Mahfouz/drt-potential/issues/11                         ###


source("R/read_gtfs.R")

library(sf)
library(tmap)

feeds <- read_gtfs_feeds(feed_dir = "data/interim/", package = "tidytransit")
#feeds <- read_gtfs_feeds(feed_dir = "data/raw/gtfs/rail/", package = "tidytransit")


feed_bus <- feeds[[1]]
feed_rail <- feeds[[2]]

# get trips df
trips <- feed_bus$trips

# we have many trips with the same trip_id and service_id (more than 2)
# Example route_id == 19472, service_id == 101

trips_sample <- trips %>%
  filter(route_id == "19472", service_id == "101")

# get stop_times df
stop_times <- feed_bus$stop_times
# filter to the trips in the trips_sample df
stop_times_sample <- stop_times %>%
  filter(trip_id %in% trips_sample$trip_id)

# check the agency id
agency_sample <- trips_sample %>%
  left_join(feed_bus$routes %>% select(route_id, agency_id), by = "route_id") %>%
  left_join(feed_bus$agency, by = "agency_id")

# add stops geometry to plot an sf

stop_times_sample <- stop_times_sample %>%
  left_join(feed_bus$stops %>% select(stop_id, stop_lon, stop_lat))

# convert to sf
stop_times_sample <- st_as_sf(stop_times_sample, coords = c("stop_lon", "stop_lat"))


trips %>% group_by(trip_id, service_id) %>%
  summarise(trips = n()) %>%
  ungroup() %>%
  arrange(desc(trips))
# plot the trips to see how they are different

# one plot
tm_shape(stop_times_sample) +
  tm_bubbles(col = "trip_id")

tm_shape(stop_times_sample) +
  tm_bubbles(col = "trip_id")

# plot the trips to see how they are different

# facet plot
tm_shape(stop_times_sample) +
  tm_bubbles(col = "stop_sequence") +
  tm_facets('trip_id')


# ---------- understand how calendar and services affect how many times a trip is repeated

calendar_bus <- feed_bus$calendar
calendar_dates_bus <- feed_bus$calendar_dates
services_bus <- feed_bus$.$dates_services


# both of the feeds here don't have frequencies.txt file
data_path <- system.file("extdata/poa", package = "r5r")
list.files(data_path)

r5rgtfs = gtfstools::read_gtfs(paste0(data_path, "/poa_eptc.zip"))

# try here
data_path <- system.file("extdata/spo", package = "r5r")
list.files(data_path)


r5rgtfs = gtfstools::read_gtfs(paste0(data_path, "/spo.zip"))
r5rgtfs
freq <- r5rgtfs$frequencies
stoptimes <- r5rgtfs$stop_times
# ---------------- build the logic



# --- read in the feed
gtfs <- tidytransit::read_gtfs("data/interim/study_area_gtfs_bus.zip")
#gtfs <- gtfstools::read_gtfs("data/interim/study_area_gtfs_bus.zip")

gtfs <- tidytransit::read_gtfs("data/interim/study_area_gtfs_rail.zip")

# ----- what time intervals do we want to calculate headway for?

time_ranges <- tibble(start_time = c("00:00:00", "05:00:00", "07:00:00", "09:00:00", "12:00:00", "15:00:00", "19:00:00", "21:00:00"),
                      end_time = c("05:00:00", "07:00:00", "09:00:00", "12:00:00", "15:00:00", "19:00:00", "21:00:00", "23:59:00")) %>%
  mutate(across(everything(), hms::as_hms))


# --- Calculate the headway

# 1. identify trips with the same itinerary. Trip IDs are unique for every departing vehicle,
#    so we group vehicles that have the same stop sequence

# Create a column to identify same trips (trips with the same stop sequence)

trips_stop_sequence <- gtfs$stop_times %>% group_by(trip_id) %>%
  mutate(stop_id_order = paste0(stop_id, collapse = '-')) %>%
  ungroup()

# keep only one row per unique trip
trips_stop_sequence <- trips_stop_sequence %>%
  # we use stop_sequence == min(stop_sequence) instead of == 0, as stop_sequence doesn't have to start from 0
  filter(stop_sequence == min(stop_sequence)) %>%
  # some arrival times are bigger than 24 - these cause errors when converting to time
  filter(as.character(arrival_time) <= "23:59:59") %>%
  mutate(arrival_time = hms::as_hms(arrival_time))

# 2. Assign a time range to each trip based on the departure from the first stop

trips_time_ranges <- trips_stop_sequence %>%
  inner_join(time_ranges,
             join_by(arrival_time >= start_time, arrival_time < end_time))

# 3. Get the headway of each trip

# add the service_id to each trip

trips_time_ranges <- trips_time_ranges %>%
  left_join(gtfs$trips %>% select(trip_id, service_id), by = "trip_id")


# calculate number of buses for each unique trip + time range + service_id combination
trips_headways <- trips_time_ranges %>%
  group_by(stop_id_order, service_id, start_time, end_time) %>%
  summarise(vehicles = n(),
            # we don't need all of the trip IDs (all trips in the same group have the same itinerary)
            trip_id = first(trip_id)) %>%
  ungroup()

# get the headway (time between buses = time period / no. of buses)
trips_headways <- trips_headways %>%
  mutate(headway_secs = round(as.numeric(end_time - start_time) / vehicles)) %>%
  # keep necessary columns only
  select(trip_id, start_time, end_time, headway_secs)

hist(trips_headways$headway_secs)
# 4. edit the gtfs feed to produce a frequency-based feed

# remove stop_times file
gtfs_edited[names(gtfs_edited) != "stop_times"]

# add "frequencies" file to the gtfs feed
gtfs_edited$frequencies <- trips_headways

# filter feed to only keep the trip ids in the new "frequencies" file
gtfs_edited <- gtfs_edited %>%
  #gtfstools::filter_by_trip_id(trip_id = trips_headways$trip_id)
  tidytransit::filter_feed_by_trips(trip_ids = .$frequencies$trip_id)



x <- gtfs$trips

y <- gtfs$stop_times
trips <- gtfs$trips



