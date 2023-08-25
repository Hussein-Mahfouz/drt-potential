### The purpose of this script is to convert the frequencies gtfs file to a stop_times gtfs file. Some ###
### routing algorithms can only use stop_times (e.g. the algorithm behind r5r::detailed_itineraries()) ###

# ---------------  Identify GTFS file paths

# parent directory where feeds are saved
gtfs_dir <- "data/interim/"
# get name of gtfs feeds
gtfs_names = list.files(gtfs_dir, pattern = ".zip")
# get path for each feed
gtfs_paths = paste0(gtfs_dir, gtfs_names)


# ----------------------------- Convert frequencies to stop_times

# function to convert frequencies to stop_times and replace a gtfs feed with the edited one
freq_to_stop_times = function(feeds){
  # INPUT: list of gtfs file paths
  # OUTPUT: edited feed that is saved in place of the old feed
  for (i in 1:length(feeds)){
    # read in the gtfs feed
    gtfs_feed <- gtfstools::read_gtfs(path = feeds[i])
    if("frequencies" %in% names(gtfs_feed)){

      # status
      print(paste0("Converting frequencies to stop_times for feed: ", i))
      # convert frequencies to stop_times
      # this function expands stop_times.txt and also creates trips.txt - One trip for each bus departure (adds _<n> to the trip_id)
      # https://ipeagit.github.io/gtfstools/reference/frequencies_to_stop_times.html
      gtfs_feed_edited <- gtfstools::frequencies_to_stop_times(gtfs_feed)
      # status
      # print(paste0("Removing frequencies.txt for feed: ", i))
      # # remove frequencies.txt [FUNCTION REMOVES IT ON ITS OWN]
      # gtfs_feed_edited <- gtfs_feed_edited[names(gtfs_feed_edited) %in% "frequencies" == FALSE]
      # save the feed, overwriting the existing feed
      gtfstools::write_gtfs(gtfs = gtfs_feed_edited,
                            path = feeds[i])
      # status
      print(paste0("Saved feed: ", i))

    } else{
      print(paste0("There is not frequencies.txt file in feed: ", i, ". Leaving it as is."))
    }

  }
}


# apply the function

freq_to_stop_times(feeds = gtfs_paths)
