#' Calculate travel time matrix for different mode combinations
#'
#' Wrapper function around r5r::travel_time_matrix() that allow you to pass multiple mode combinations.
#' It loops over each combination and calculate a travel time matrix
#'
#' @param data_path the path with the osm.pbf and gtfs feeds (optional elevation file should be here also)
#' @param scenarios a tibble with columns  ~scenario, ~mode,  ~departure_datetime.
#' departure_datetime should be a character following "dd-mm-yyyy hh:mm:ss"
#' @param zone_layer an sf point layer. An OD matrix will be created by routing
#' between all points
#' @param time_window see r5r::travel_time_matrix()
#' @param percentiles see r5r::travel_time_matrix()
#'
#' @return A dataframe with the travel time between all OD combinations. One column is added for each different
#' percentiles value (e.g. travel_time_p50 for the 50th percentile)
#' @examples
#'
#' @export
tt_matrix = function(scenarios,
                     zone_layer,
                     time_window = 20,
                     percentiles = c(25, 50, 75)){


  # # stop any running r5 instances
  # r5r::stop_r5()
  # setup r5
  # print("Setting up r5...")
  # r5r_core <- setup_r5(data_path = data_path,
  #                      verbose = TRUE,
  #                      overwrite = TRUE) # turn to true once we have elevation

  # print("r5 graph built...")
  if(!exists("r5r_core")){
    stop("No graph detected. You need to build the graph using r5r::setup_r5()")
  }

  # empty list to store results for each combination
  results <- vector(mode = "list", length = nrow(scenarios))

  print("Calculating travel times...")
  # calculate travel time matrix for each combination
  for(i in 1:nrow(scenarios)){
    #status updates
    print(paste0("CALCULATING TRAVEL TIME FOR SCENARIO: ", scenarios$scenario[i], " ....."))
    # calculate a travel time matrix
    ttm <- r5r::travel_time_matrix(r5r_core = r5r_core,
                                   origins = zone_layer ,
                                   destinations = zone_layer,
                                   time_window = time_window,
                                   percentiles = percentiles,
                                   mode = scenarios$mode[i][[1]],
                                   departure_datetime = as.POSIXct(scenarios$departure_datetime[i][[1]],
                                                                   format = "%d-%m-%Y %H:%M:%S"),
                                   max_walk_time = 10, #max_walk_dist,
                                   max_trip_duration = 120,
                                   # number of threads (all - 1)
                                   #n_threads = parallel::detectCores() - 1,
                                   # to suppress r5 output. Change to true if debugging
                                   verbose = FALSE,
                                   # slow down function by ~20%
                                   progress = TRUE)

    # add column with combination name / number
    ttm$combination <- scenarios$scenario[i]

    # add ttm to results list
    results[[i]] <- ttm
    #status updates
    print(paste0("COMPLETED SCENARIO: ", scenarios$scenario[i], " ....."))

  }
  # # stop r5
  # r5r::stop_r5(r5r_core)
  # # java garbage collector to free up memory
  # rJava::.jgc(R.gc = TRUE)

  # combine list into 1 dataframe
  results <- bind_rows(results)

  # # pivot wider to get 1 row per OD pair
  # results <- results %>% pivot_wider(names_from = combination,
  #                                      values_from = matches("*travel_time"))

  return(results)

}



#' Calculate EXPANDED travel time matrix for different mode combinations
#'
#' This includes access_time, egress_time, waiting_time, ride_time, and routes used
#'
#' @param scenarios a tibble with columns  ~scenario, ~mode,  ~departure_datetime.
#' @param zone_layer an sf point layer. An OD matrix will be created by routing
#' @param time_window see r5r::travel_time_matrix()
#' @param storage_option character string to define whethe to store results in memory or save them to disk. Options are "memory" and "save"
#' @param save_format used if storage option is "save". Options are "csv" and "parquet"
#' @param folder_path Used if storage option = "save". The path of the folder where the results will be saved.
#'
#' @return A dataframe with the expanded travel time between all OD combinations. One row exists for each minute in a time
#' window (e.g. 7:30, 7:31, 7:32). The results are either a df stored in memory, or files (csv or parquet) saved on disk
#' @examples
#'
#' @export
tt_matrix_expanded = function(scenarios,
                              zone_layer,
                              time_window = 20,
                              storage_option,
                              save_format,
                              folder_path){


  # # stop any running r5 instances
  # r5r::stop_r5()
  # setup r5
  # print("Setting up r5...")
  # r5r_core <- setup_r5(data_path = data_path,
  #                      verbose = TRUE,
  #                      overwrite = TRUE) # turn to true once we have elevation

  # print("r5 graph built...")
  if(!exists("r5r_core")){
    stop("No graph detected. You need to build the graph using r5r::setup_r5()")
  }

  # empty list to store results for each combination
  results <- vector(mode = "list", length = nrow(scenarios))
  # folder to save the results on disk
  if(storage_option == "save"){
    dir.create(path = folder_path)
  }

  print("Calculating travel times...")
  # calculate travel time matrix for each combination
  for(i in 1:nrow(scenarios)){
    #status updates
    print(paste0("CALCULATING TRAVEL TIME FOR SCENARIO: ", scenarios$scenario[i], " ....."))
    # calculate a travel time matrix
    ttm <- r5r::expanded_travel_time_matrix(r5r_core = r5r_core,
                                            origins = zone_layer,
                                            destinations = zone_layer,
                                            time_window = time_window,
                                            mode = scenarios$mode[i][[1]],
                                            departure_datetime = as.POSIXct(scenarios$departure_datetime[i][[1]],
                                                                            format = "%d-%m-%Y %H:%M:%S"),
                                            max_walk_time = 10, #max_walk_dist,
                                            max_trip_duration = 120,
                                            # number of threads (all - 1)
                                            #n_threads = parallel::detectCores() - 1,
                                            # draws will all be the same as this is not a frequency based feed. see r5r documentation
                                            draws_per_minute = 1,
                                            breakdown = TRUE,
                                            # to suppress r5 output. Change to true if debugging
                                            verbose = FALSE,
                                            # slow down function by ~20%
                                            progress = TRUE)

    # add column with combination name / number
    ttm$combination <- scenarios$scenario[i]

    # ---------- save the output

    # OPTION 1. Save in memory as list of dataframes
    if(storage_option == "memory"){
      # add ttm to results list
      results[[i]] <- ttm
    }     # save results
    # OPTION 2. Save to disk
    else if (storage_option == "save"){
      # 2a. save as one csv file, and append to it at every loop (very large)
      if(save_format == "csv"){
        if(i == 1){
          write_csv(ttm, file = paste0(folder_path, ".csv"))
        }
        else{
          write_csv(ttm, file = paste0(folder_path, ".csv"), append = TRUE)
        }}
      # 2b. save as multiple parquet files in a subdirectory (smaller files, but they can't be appended)
      else if(save_format == "parquet"){
        # save the results
        arrow::write_parquet(ttm, paste0(folder_path, "/travel_time_matrix_expanded_", i, ".parquet"))
      }
    }

    #status updates
    print(paste0("COMPLETED SCENARIO: ", scenarios$scenario[i], " ....."))


  }

  if(storage_option == "memory"){
    # combine list into 1 dataframe
    results <- bind_rows(results)

    return(results)
  }
}






#' Summarise results from tt_matrix_expanded() - which is a arapper arounf r5r::expanded_travel_time_matrix()
#'
#' @param ttm_expanded_results a tibble with the output of r5r::expanded_travel_time_matrix().
#' columns: "from_id" "to_id" "departure_time" "draw_number" "access_time" "wait_time" "ride_time"
#' "transfer_time" "egress_time" "routes" "n_rides" "total_time" "combination"
#'
#' @return a dataframe with summarised results. All time (numeric columns) are summarised to return the mode by group.
#' The grouping is done by "from_id", "to_id" and "combination"
#' @examples
#'
#' @export
summarise_ttm_expanded = function(ttm_expanded_results){
  message(paste0("coverting to tidytable ... ", Sys.time()))
  # convert to tidytable to use data.table performance
  ttm_expanded_results = tidytable::as_tidytable(ttm_expanded_results)
  message(paste0("conversion done. Summarising results ... "), Sys.time())
  # Define a custom function to calculate the mode
  get_mode <- function(v) {
    uniq_v <- unique(v)
    uniq_v[which.max(tabulate(match(v, uniq_v)))]
  }

  # expanded travel_time_matrix returns the results for each minute in a time_window (unlike
  # travel_time_matrix which aggregates the results internally). We need to get averages for all the time columns
  results_summarised <- ttm_expanded_results %>%
    tidytable::group_by(from_id, to_id, combination) %>%
    tidytable::summarise(
      # different route combinations will exist depending on start time. we select the most popular one using the function defined above
      routes = get_mode(routes),
      # the times are e.g. 7:30, 7:31. 7:32 - we take the first one
      departure_time = tidytable::first(departure_time),
      # get the median of all time variables
      tidytable::across(tidytable::where(is.numeric), ~median(as.double(.), na.rm = TRUE))) %>%
    tidytable::ungroup() %>%
    tidytable::select(from_id, to_id, combination, routes, matches("time"), n_rides)

  message(paste0("summary done. converting back to tibble ... ", Sys.time()))
  # covert back to df
  results_summarised <- as_tibble(results_summarised)
  message(paste0("conversion done ... ", Sys.time()))

  return(results_summarised)

}















