# PURPOSE: convert a stop_times based feed to a frequency based feed

stop_times_to_frequencies <- function(gtfs,
                                      time_ranges = tibble(start_time = c("00:00:00", "09:00:00", "12:00:00", "19:00:00"),
                                                           end_time =   c("09:00:00", "12:00:00", "19:00:00", "23:59:00"))){
  # PURPOSE: convert a stop_times based feed to a frequency based feed
  # INPUT:
  # gtfs: the gtfs feed you want to edit
  # time_ranges: the day is split into multiple time slots. We calculate the frequency of trip in each of these slots
  #              format: a tibble with columns "start_time" and "end_time"
  # OUTPUT
  # a frequency based gtfs feed

  message(" ... converting time ranges to hms ... ")
  # ----- convert time ranges to hms
  time_ranges <- time_ranges %>%
    mutate(across(everything(), hms::as_hms))


  # --- Calculate the headway

  # 1. identify trips with the same itinerary. Trip IDs are unique for every departting vehicle,
  #    so we group vehicles that have the same stop sequence

  # Create a column to identify same trips (trips with the same stop sequence)
  message("... identifying trips with same stop sequence ...")

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
  message("... assigning trips to time ranges ... ")

  trips_time_ranges <- trips_stop_sequence %>%
    inner_join(time_ranges,
               join_by(arrival_time >= start_time, arrival_time < end_time))

  # 3. Get the headway of each trip

  # add the service_id to each trip
  trips_time_ranges <- trips_time_ranges %>%
    left_join(gtfs$trips %>% select(trip_id, service_id), by = "trip_id")

  # calculate number of buses for each unique trip + time range + service_id combination
  message("... calculating headways ... ")

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


  # 4. edit the gtfs feed to produce a frequency-based feed
  message("... replacing stop_times with frequencies ... ")

  # # remove stop_times file
  # gtfs <- gtfs[names(gtfs) != "stop_times"]

  # add "frequencies" file to the gtfs feed
  gtfs$frequencies <- trips_headways

  # filter feed to only keep the trip ids in the new "frequencies" file
  gtfs <- gtfs %>%
    #gtfstools::filter_by_trip_id(trip_id =  .$frequencies$trip_id)
    tidytransit::filter_feed_by_trips(trip_ids = .$frequencies$trip_id)

  return(gtfs)

}
