### The purpose of this script is to convert the stop_times gtfs file to a frequencies gtfs file. This ###
### is because the time_window( parameter in r5r::travel_time_matrix needs a frequency based gtfs file ###
### See this issue: https://github.com/Hussein-Mahfouz/drt-potential/issues/11                         ###

library(tidyverse)


gtfs <- tidytransit::read_gtfs("data/interim/study_area_gtfs_bus.zip")

# one option is to use the stop_times.txt and count the number of departures per trip_id. This only works if we have two trip_ids that match onto
# each route_id. In this gtfs feed, each vehicle trip has a unique trip_id

# get the id of the first stop in the trip's stop sequence
first_stop_id <- gtfs$stop_times %>%
  group_by(trip_id) %>%
  summarise(stop_id = stop_id[which.min(stop_sequence)])

# add the stop name
first_stop <- first_stop_id %>%
  left_join(gtfs$stops %>% select(trip_origin = stop_name, stop_id), by = "stop_id")



# generate a trip headsign column (if it does not exist)

if(!exists("trip_headsign", where = gtfs$trips)) {
  # get the last id of the trip's stop sequence
  trip_headsigns <- gtfs$stop_times %>%
    group_by(trip_id) %>%
    summarise(stop_id = stop_id[which.max(stop_sequence)]) %>%
    left_join(gtfs$stops, by="stop_id") %>%
    select(trip_id, trip_headsign.computed = stop_name)

  # assign the headsign to the gtfs object
  gtfs$trips <- left_join(gtfs$trips, trip_headsigns, by = "trip_id")
}


# ----- get headway per route

# add route_id and service_id to df with the first stop of each trip
first_stop <- first_stop %>%
  left_join(gtfs$trips %>% select(route_id, service_id, trip_id, trip_headsign),
            by = "trip_id")

# group by route id, service_id, and trip_headsign to get headway per route
departures <- first_stop %>%
  group_by(route_id, service_id, trip_origin, trip_headsign) %>%
  summarise(n_departures = n())

# add trip id back onto the data (does this make sense? NO - there are too many trip_ids for the same "trip")
x <- departures %>% left_join(first_stop,
                              by = c("route_id", "service_id", "trip_origin", "trip_headsign")) %>%
  relocate(n_departures, .after = everything())

# TODO:
  # add start_time and stop_time from stop_times.txt AND add dates from calendar.txt
  # get headway for different day and time intervals

# check out
   # https://r-transit.github.io/tidytransit/reference/filter_stop_times.html
   # https://ipeagit.github.io/gtfstools/reference/filter_by_time_of_day.html   (this is better)



start_time = "06:00:00"
end_time = "22:00:00"

if(is.character(start_time)) start_time <- hms::as_hms(start_time)
if(is.character(end_time)) end_time <- hms::as_hms(end_time)

stop_times = gtfs_obj$stop_times %>%
  #filter(trip_id %in% trips$trip_id) %>%
  filter(departure_time >= start_time & arrival_time <= end_time) #%>%
  left_join(trips[c("trip_id", "route_id", "direction_id", "service_id")], "trip_id")


# hacking away - 12/10/2023




# ----- what time intervals do we want to calculate headway for?

time_ranges <- tibble(start_time = c("00:00:00", "05:00:00", "07:00:00", "09:00:00", "12:00:00", "15:00:00", "19:00:00", "21:00:00"),
                      end_time = c("05:00:00", "07:00:00", "09:00:00", "12:00:00", "15:00:00", "19:00:00", "21:00:00", "23:59:00")) %>%
  mutate(across(everything(), hms::as_hms))


# --- Create column with start_time at first stop


# --- Create a column to identify same trips (trips with the same stop sequence)

trips_stop_sequence <- gtfs$stop_times %>% group_by(trip_id) %>%
  mutate(stop_id_order = paste0(stop_id, collapse = '-')) %>%
  ungroup()

# keep only one row per unique trip
trips_stop_sequence <- trips_stop_sequence %>%
  filter(stop_sequence == 0)

# --- Assign a time range to a trip based on the departure from the first stop

trips_time_ranges <- trips_stop_sequence %>%
  inner_join(time_ranges,
             join_by(arrival_time >= start_time, arrival_time < end_time))

# --- get the headway of each trip

# calculate number of buses for each unique trip + time range combination
trips_headways <- trips_time_ranges %>%
  group_by(stop_id_order, start_time, end_time) %>%
  summarise(frequency = n(),
            trip_id = first(trip_id)) %>%
  ungroup()

# get the headway
trips_headways <- trips_headways %>%
  mutate(headway_secs = as.numeric(end_time - start_time) / frequency)
