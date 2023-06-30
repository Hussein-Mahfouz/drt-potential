library(tidyverse)
library(gtfstools)
library(sf)


# Download the GTFS feed
gh_release_download(tag = "gtfs-feeds",
                    pattern = "gtfs_england_06_23.zip",
                    dest = "data_raw")


# Function to read in multiple feeds
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


gtfs_feed <- read_gtfs_feeds(feed_dir = "data_raw/")




# get UK boundaries
   # city
   # combined authority
   # region
# add to release

# Sources

 # https://github.com/humaniverse/geographr




uk_cities <- sf::st_read("data_raw/Counties_and_Unitary_Authorities_(December_2022)_EN_BUC.geojson")

leeds <- uk_cities %>% filter(CTYUA22NM == "Leeds")
wy <- uk_cities %>% filter(CTYUA22NM %in% c("Leeds", "Bradford", "Calderdale", "Kirklees", "Wakefield"))

# filter uk gtfs to specific region
gtfs_leeds <- gtfs_feed[[1]] %>%
  gtfstools::filter_by_sf(geom = leeds)

gtfs_wy <- gtfs_feed[[1]] %>%
  gtfstools::filter_by_sf(geom = wy)

# convert to shapes and plot
shapes_uk <- gtfs_feed[[1]] %>%
  gtfstools::convert_shapes_to_sf()

shapes_wy <- gtfs_wy %>%
  gtfstools::convert_shapes_to_sf()

shapes_leeds <- gtfs_leeds %>%
  gtfstools::convert_shapes_to_sf()

plot(st_geometry(wy))
plot(st_geometry(shapes_uk), add = TRUE, col = "grey")
plot(st_geometry(shapes_wy), add = TRUE, col = "red")
plot(st_geometry(shapes_leeds), add = TRUE, col = "green")


