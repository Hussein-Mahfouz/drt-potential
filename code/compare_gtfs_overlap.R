###################################################################################################
###    The purpose of this script is to compare different gtfs feeds and see if they have       ###
###    services that are operating in the same period. Routing engines will build a graph       ###
###    and only include the services that are operating during the datetime specified. We need  ###
###    to choose a date that works for all feeds that we want to use. If the feeds don't        ###
###    overlap we can either: (a) find different feeds, (b) edit the calendar.txt of one of     ###
###    the feeds (https://github.com/transportforcairo/wri-numo_access-analysis/blob/main/code/ ###
###    1_Preprocessing/0_edit_gtfs_calendar.R)                                                  ###
###################################################################################################


library(tidyverse)
library(tidytransit)

# ------------ compare bus and rail gtfs feeds

source("R/read_gtfs.R")

# ------ read in the feeds in tidytransit compatible format + give them names
#gtfs_bus <- read_gtfs_feeds(feed_dir = "data/raw/gtfs/bus/", package = "tidytransit")

# --- define directory with gtfs feeds
feed_dir = "data/interim/"
# --- get names of feeds in a specific directory
feed_names <- list.files(feed_dir, ".zip$", full.names = FALSE)
# --- read in the gtfs feeds
gtfs_feeds <- read_gtfs_feeds(feed_dir = feed_dir, package = "tidytransit")
# --- assign each feed in the list a name (this is it's name in the directory)
names(gtfs_feeds) <- feed_names


# ------- Function to get top servicepatterns from a gtfs feed

get_top_services = function(gtfs_feed, n = 10){
  # INPUT:
  # gtfs_feed: the feed we want to analyze
  # n: we extract the top n service patterns
  # OUTPUT
  # dataframe with top 10 services. For each service, we have a row for each unique date it operates on

  # set the service patterns
  gtfs_feed <- tidytransit::set_servicepattern(gtfs_feed)

  # --- extract most popular service patterns for plotting

  # get service patterns table
  service_patterns <- gtfs_feed$.$servicepatterns
  # get number of times each pattern is used
  unique_services <- service_patterns %>%
    group_by(servicepattern_id) %>%
    # get number of times each service pattern is repeated
    mutate(services = n()) %>%
    ungroup() %>%
    # keep one row per servicepattern
    distinct(servicepattern_id, .keep_all = TRUE)

  # get top n most popular service patterns
  unique_services_top <- unique_services %>%
    arrange(desc(services)) %>%
    head(n)


  # get dates of each servicepattern_id

  # --- join top services onto "dates_service_patterns" to see which days they run on
  date_servicepattern_table <- gtfs_feed$.$dates_servicepatterns %>%
    inner_join(unique_services_top, by = "servicepattern_id") %>%
    # add weekday
    mutate(weekday = weekdays(date))

  return(date_servicepattern_table)
}


# apply the function an the different gtfs feeds
services_bus <- get_top_services(gtfs_feed = gtfs_feeds$study_area_gtfs_bus.zip, n = 75)
services_rail <-  get_top_services(gtfs_feed = gtfs_feeds$study_area_gtfs_rail.zip, n = 75)

# combine the feeds for a facet plot
services_all <- services_bus %>% mutate(mode = "bus") %>%
  bind_rows(services_rail %>% mutate(mode = "rail"))

# --- plot
ggplot(services_rail) + theme_bw() +
  geom_point(aes(x = date, y = servicepattern_id, color = weekday), size = 1) +
  labs(title = "Rail Services") +
  scale_x_date(breaks = scales::date_breaks("1 month")) +
  theme(legend.position = "bottom",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1))

ggsave("data/processed/plots/gtfs_exploration/service_patterns_rail.png", width = 14, height = 10)


ggplot(services_bus) + theme_bw() +
  geom_point(aes(x = date, y = servicepattern_id, color = weekday), size = 1) +
  labs(title = "Bus Services") +
  scale_x_date(breaks = scales::date_breaks("1 month")) +
  theme(legend.position = "bottom",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1))

ggsave("data/processed/plots/gtfs_exploration/service_patterns_bus.png", width = 14, height = 10)


# --- plot   (I have edited n in the get_top_services function to include more rows in this plot)
ggplot(services_all) + theme_bw() +
  geom_point(aes(x = date, y = servicepattern_id, color = weekday), size = 1) +
  scale_x_date(breaks = scales::date_breaks("1 month")) +
  theme(legend.position = "bottom",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  facet_wrap(vars(mode), nrow = 2)

ggsave("data/processed/plots/gtfs_exploration/service_patterns_facet.png", width = 14, height = 10)


