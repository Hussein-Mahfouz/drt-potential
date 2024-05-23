#' Sensitivity analysis of DBSCAN parameters
#'
#' @param distance_matrix a precalculated distance matrix between desire lines
#' @param options_epsilon a vector of options for the epsilon paramter
#' @param options_minpts a vector of options for the minPts paramter
#' @param weights a vector where each value represents the number of people going between an OD pair (to pass to the weights argument of dbscan)
#' @param flows the original od matrix with a column that represents flows between od pairs
#'
#' @return a df with columns {id} (to identify the eps and minpts used), {cluster}, {size} (no of desire lines in cluster), commuters_sum (total no. of commuters in cluster)
#'
#' @examples
#'
#' @export
dbscan_sensitivity = function(distance_matrix, options_epsilon, options_minpts, weights, flows){

  # dataframe with all combinations of eps and minpts
  options_parameters <- tidyr::expand_grid(eps = options_epsilon, minpts = options_minpts)

  # create empty list to store the results for each eps and minpts combination
  results <- vector(mode = "list", length = nrow(options_parameters))

  # get results for each eps and minpts combination
  for(i in 1:nrow(options_parameters)){
    # print iteration
    print(paste0("running dbscan for option ", i, " of ", nrow(options_parameters),
                 " : eps = ", options_parameters$eps[i],
                 " | minpts = ", options_parameters$minpts[i]))

    # clustering using dbscan
    cluster_dbscan_i = dbscan::dbscan(distance_matrix,
                                      minPts = options_parameters$minpts[i], # 125
                                      eps = options_parameters$eps[i], # 9.5
                                      weights = weights)


    # Get results

    # add column with clustering results to distance matrix
    cluster_dbscan_res_i <- distance_matrix %>%
      mutate(cluster = cluster_dbscan_i$cluster)

    # prepare data for joining
    cluster_dbscan_res_i <- cluster_dbscan_res_i %>%
      rownames_to_column(var = "flow_ID") %>%
      select(flow_ID, cluster)


    # add flow data to get commuters in each cluster
    cluster_dbscan_res_i <- flows %>%
      inner_join(cluster_dbscan_res_i, by = "flow_ID")

    # check size per cluster and total commuters per cluster
    cluster_res_i <- cluster_dbscan_res_i %>%
      st_drop_geometry() %>%
      group_by(cluster) %>%
      summarise(size = n(), commuters_sum = sum(commute_all)) %>%
      arrange(desc(size)) %>%
      ungroup()

      # add id that identifies which parameters were used to get this result
    cluster_res_i <- cluster_res_i %>%
      mutate(id = paste0("eps_", options_parameters$eps[i], "_minpts_", options_parameters$minpts[i]))

    # save in list
    results[[i]] <- cluster_res_i

  }
  # turn results into one df
  results = bind_rows(results)
  # return this output
  return(results)
}
