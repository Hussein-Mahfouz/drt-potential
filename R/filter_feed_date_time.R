#' Filtering a GTFS feed by date and time
#'
#' @param scenarios a tibble with columns  ~scenario, ~date, ~min_departure_datetime, ~max_arrival_time. Check tidytransit::filter_feed_by_date for
#' valid input format for the last 3 columns. Scenario is a custom name provided by you
#' departure_datetime should be a character following "dd-mm-yyyy hh:mm:ss"
#' @param gtfs_feed a gtfs feed with a frequencies.txt file. Use R/stop_times_to_frequencies.R to create one
#' if yo have a stop_times based file
#' @param shapes_only if TRUE, return one sf per scenario. The sf will include the shape geometry along with the metadata from trips.txt and frequencies.txt.
#' If FALSE, return the entire filtered gtfs feed. Defaults to TRUE
#'
#' @return
#' @examples
#'
#' scenarios <- tribble(
#' ~scenario, ~date, ~min_departure_time, ~max_arrival_time,
#' # public transport at different times of day / week
#' "pt_wkday_morning", "2023-08-14", "06:00:00", "09:00:00",
#' "pt_wkday_afternoon", "2023-08-14", "11:00:00", "14:00:00",
#' "pt_wkday_evening", "2023-08-14", "17:00:00", "20:00:00",
#' "pt_wkday_night", "2023-08-14", "21:30:00", "23:59:00:00",
#' "pt_wkend_morning", "2023-08-13", "06:00:00", "09:00:00",
#' "pt_wkend_evening", "2023-08-13", "17:00:00", "20:00:00",
#' )

#' @export
#'
filter_gtfs_feed_internal = function(scenarios,
                                     gtfs_feed,
                                     shapes_only = TRUE){


  # empty list to store results for each combination
  results <- vector(mode = "list", length = nrow(scenarios))

  print("Filtering feeds...")
  # Filter feed differently dpending on scenario
  for(i in 1:nrow(scenarios)){
    print(paste0("Filtering feed ", i, ": ", scenarios$scenario[i], " ....."))


    filtered_feed <- tidytransit::filter_feed_by_date(gtfs_feed,
                                                      extract_date = scenarios$date[i],
                                                      min_departure_time = scenarios$min_departure_time[i],
                                                      max_arrival_time = scenarios$max_arrival_time[i]
    )
    # convert gtfs to sf
    filtered_feed <- tidytransit::gtfs_as_sf(filtered_feed)

    #status updates
    print(paste0("Done filtering: ", scenarios$scenario[i]))

    # Extract shapes from feed and join trip and frequency info
    if(shapes_only & ("shapes" %in% names(gtfs_feed))){

      filtered_feed <- filtered_feed$shapes %>%
        select(shape_id) %>%
        left_join(filtered_feed$trips, by = "shape_id") %>%
        left_join(filtered_feed$frequencies, by = "trip_id") %>%
        mutate(scenario = scenarios$scenario[i])

    } else if(!("shapes" %in% names(gtfs_feed))) {
      message("no shapes.txt. Feed will not have a spatial column")
    }

    # add results to list
    results[[i]] <- filtered_feed
    #status updates
    print(paste0("Filtered feed: ", scenarios$scenario[i], " ....."))

  }

  # name the list
  names(results) <- scenarios$scenario
  # return the list
  return(results)

}
