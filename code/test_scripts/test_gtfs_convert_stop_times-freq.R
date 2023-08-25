### The purpose of this script is to explore how trips and routes are coded in UK gtfs feeds. This is  ###
### because i'm exploring how to create a stop_times_to_frequencies() function                         ###
### See this issue: https://github.com/Hussein-Mahfouz/drt-potential/issues/11                         ###


source("R/read_gtfs.R")

library(sf)
library(tmap)

feeds <- read_gtfs_feeds(feed_dir = "data/interim/", package = "tidytransit")
#feeds <- read_gtfs_feeds(feed_dir = "data/raw/gtfs/rail/", package = "tidytransit")


feed_bus <- feeds[[1]]

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

