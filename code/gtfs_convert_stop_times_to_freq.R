### The purpose of this script is to convert the stop_times gtfs file to a frequencies gtfs file. This ###
### is because the time_window( parameter in r5r::travel_time_matrix needs a frequency based gtfs file ###
### See this issue: https://github.com/Hussein-Mahfouz/drt-potential/issues/11                         ###

# ----- load in necessary functions (by sourcing all scripts in the R/ directory)
file.sources <- dir("R/", ".R$", full.names = TRUE)
sapply(file.sources, source)


# --------------- BUS feed

# --- read in the feed

#
gtfs_bus <- tidytransit::read_gtfs("data/interim/study_area_gtfs_bus.zip")
#gtfs_bus <- gtfstools::read_gtfs("data/interim/study_area_gtfs_bus.zip")

# rail
gtfs_rail <- tidytransit::read_gtfs("data/interim/study_area_gtfs_rail.zip")


# apply the function
gtfs_freq_bus = stop_times_to_frequencies(gtfs = gtfs_bus,
                                      time_ranges = tibble(start_time = c("00:00:00", "05:00:00", "07:00:00", "09:00:00", "12:00:00", "15:00:00", "19:00:00", "21:00:00"),
                                                           end_time = c("05:00:00", "07:00:00", "09:00:00", "12:00:00", "15:00:00", "19:00:00", "21:00:00", "23:59:00")))

# apply the function
gtfs_freq_rail = stop_times_to_frequencies(gtfs = gtfs_rail,
                                          time_ranges = tibble(start_time = c("00:00:00", "05:00:00", "07:00:00", "09:00:00", "12:00:00", "15:00:00", "19:00:00", "21:00:00"),
                                                               end_time = c("05:00:00", "07:00:00", "09:00:00", "12:00:00", "15:00:00", "19:00:00", "21:00:00", "23:59:00")))


# save the new feed
tidytransit::write_gtfs(gtfs_freq_bus, "data/interim/gtfs_freq/study_area_gtfs_bus_f.zip")
tidytransit::write_gtfs(gtfs_freq_rail, "data/interim/gtfs_freq/study_area_gtfs_rail_f.zip")

