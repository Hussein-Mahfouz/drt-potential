# Function to read in multiple gtfs feeds
library(tidyverse)

read_gtfs_feeds <- function(feed_dir){
  #INPUT:
  # feed_dir: folder where gtfs feeds are stored
  # OUTPUT:
  # list of gtfs feeds
  # ----- 1. get relative paths of all gtfs feeds
  feeds <- dir(feed_dir, ".zip$", full.names = TRUE)
  # ----- 2. read in the feeds
  # ----- check that there are zip files in the path provided
  if(length(feeds) > 0){
    gtfs_feeds <- purrr::map(feeds, gtfstools::read_gtfs)
    return(gtfs_feeds)
  } else{
    # ----- Error message if there are no zip files in the provided directory
    stop("There are no zip files in the feed directory provided. Check {feed_dir} variable")

  }

}
