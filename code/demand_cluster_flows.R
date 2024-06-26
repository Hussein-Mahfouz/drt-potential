library(tidyverse)
library(sf)
library(lwgeom)
library(geos)
# library(spatstat)
library(dbscan)
library(tmap)


source("R/study_area_geographies.R")
source("R/filter_od_matrix.R")
source("R/dbscan_sensitivity.R")
#source("code/demand_cluster_flows_prep.R")


########## ----------------------- Read in the data ----------------------- ##########
geography <- "MSOA"

# od_demand_jittered <- st_read(paste0("data/interim/travel_demand/", geography, "/od_demand_jittered_for_clustering_scenarios.geojson"))
od_demand_jittered <- st_read(paste0("data/interim/travel_demand/", geography, "/od_demand_jittered_for_clustering_scenarios_mode.geojson"))

# path tp save plots
plots_path <- paste0("data/processed/plots/eda/od_clustering/", geography, "/")

# ----------- 1. Study area

# --- administrative boundaries
study_area <- st_read("data/interim/study_area_boundary.geojson")

# convert to desired resolution
geography = "MSOA"
study_area = study_area_geographies(study_area = study_area,
                                    geography = geography)

study_area <- study_area %>%
  st_cast("MULTIPOLYGON")

# move the geographic ID to the first column. od::points_to_od() only keeps the first column as ID

geoid_col = paste0(geography, "21CD")

study_area <- study_area %>%
  relocate(all_of(geoid_col), .before = everything())

########## ----------------------- Different parameter combinations  ----------------------- ##########

# # 1) All flows + equal weight to origins and destinations (for flow distance)
# scenario <- 1
# clustering <- "equal"
# distance_threshold <- round(max(od_demand_jittered$distance_m), -3)
#
# # 2) Flows with poor PT supply + equal weight to origins and destinations (for flow distance)
# scenario <- 2
# clustering <- "equal"
# distance_threshold <- round(max(od_demand_jittered$distance_m), -3)

# # 3) Flows with poor PT supply and low potential demand + and equal weight to origins and destinations (for flow distance)
scenario <- 3
clustering <- "equal"
distance_threshold <- round(max(od_demand_jittered$distance_m), -3)
#
# # 2) Focusing on shorter distances
# scenario <- 3
# clustering <- "equal"
# distance_threshold <- 5000

# # 2) Focusing on shorter distances
# scenario <- 3
# clustering <- "equal"
# distance_threshold <- 7500

# # 2) Focusing on shorter distances
# scenario <- 3
# clustering <- "equal"
# distance_threshold <- 10000
#
# # 3) Changing alpha and beta
# scenario <- 3
# clustering <- "origin"
# distance_threshold <- 5000

# # 3) Changing alpha and beta
# scenario <- 3
# clustering <- "origin"
# distance_threshold <- 10000



######### ---------------------- What scenario are we analyzing? ------------------ ##########

# Scenario 1: All OD pairs
# Scenario 2: All OD pairs with poor PT supply
# Scenario 3: All OD pairs with poor PT supply and low potential demand

if(scenario == 1){
  od_demand_jittered <- od_demand_jittered %>%
    filter(scenario_1 == 1)
} else if(scenario == 2){
  od_demand_jittered <- od_demand_jittered %>%
    filter(scenario_2 == 1)
} else if(scenario == 3){
  od_demand_jittered <- od_demand_jittered %>%
    filter(scenario_3 == 1)
}


########## ------------ "Spatial Cluster Detection in Spatial Flow Data" ---------- ##########

# ----- STEP 1: Get distance between flows


# --- a. prepare the data

# - metric crs
od_demand_jittered <- od_demand_jittered %>%
  st_transform(3857) %>%
  # add distance column
  mutate(distance_m =  units::drop_units(sf::st_length(.)))

# # split by distance
od_demand_jittered <- od_demand_jittered %>%
  #filter(distance_m < 0.25*max(distance_m))
  filter(distance_m < distance_threshold) # distance in meters



# - Add columns for coordinates of startpoint (X, Y) and endpoint (U, V)
od_demand_xyuv <- od_demand_jittered %>%
  mutate(x = as_tibble(st_coordinates(st_startpoint(od_demand_jittered)))$X,
         y = as_tibble(st_coordinates(st_startpoint(od_demand_jittered)))$Y,
         u = as_tibble(st_coordinates(st_endpoint(od_demand_jittered)))$X,
         v = as_tibble(st_coordinates(st_endpoint(od_demand_jittered)))$Y) %>%
  st_drop_geometry()


# - Each OD pair needs a unique ID (for distance matrix)

# assign unique id to each point
od_demand_xyuv <- od_demand_xyuv %>%
  group_by(Origin, x, y) %>%
  mutate(O_ID = cur_group_id()) %>%
  ungroup() %>%
  group_by(Destination, u, v) %>%
  mutate(D_ID = cur_group_id()) %>%
  ungroup()

# assign unique id to each OD pair
od_demand_xyuv <- od_demand_xyuv %>%
  mutate(flow_ID = paste0(Origin, "_", O_ID, "-", Destination, "_", D_ID))

# add the unique ID back onto the od sf because we will need it later
od_demand_jittered <- od_demand_jittered %>%
  bind_cols(od_demand_xyuv %>%
              select(flow_ID))


# --- b. Calculate distance matrix

# Function to get flow distance and flow dissimilarity by iterating over Origins
flow_distance = function(flows, alpha = 1, beta = 1){

    print("creating unique pairs of flows ...")
    # create combination pairs of all flows
    flows_grid <- expand_grid(flow_ID_a = flows$flow_ID,
                              flow_ID_b = flows$flow_ID)

    print("adding coordinate data back onto the unique pairs ...")
    # add the coordinate data
    flows_grid <- flows_grid %>%
      # --- add flow_a coordinates
      inner_join(flows %>%
                   select(flow_ID, x, y, u, v, distance_m),
                 by = c("flow_ID_a" = "flow_ID")) %>%
      # rename coordinate columns by adding a suffix
      rename_with(.fn = ~ paste0(.x, "_i"), .cols = c("x", "y", "u", "v", "distance_m")) %>%
      # --- add flow_b coordinates
      inner_join(flows %>%
                   select(flow_ID, x, y, u, v, distance_m),
                 by = c("flow_ID_b" = "flow_ID")) %>%
      # rename coordinate columns by adding a suffix
      rename_with(.fn = ~ paste0(.x, "_j"), .cols = c("x", "y", "u", "v", "distance_m"))

    # ----- get "Flow Distance" and "Flow Dissimilarity"

    print("calculating flow distance and flow dissimiliarity ...")


    flows_grid <- flows_grid %>%
      # Flow Distance
      mutate(fd = sqrt(alpha * ((x_i - x_j)^2 + (y_i - y_j)^2) +
                         beta * ((u_i - u_j)^2 + (v_i - v_j)^2)),
             # Flow Dissimilarity
             fds = sqrt((alpha * ((x_i - x_j)^2 + (y_i - y_j)^2) +
                           beta * ((u_i - u_j)^2 + (v_i - v_j)^2)) / (distance_m_i * distance_m_j)))


  return(flows_grid)
}


# -------- Apply function to get distances:

# --- step 1: Set alpha and Beta:

# which one are we using?

if(clustering == "equal"){
  # Equal weight to Origins and Destinations
  alpha = 1
  beta = 1
} else if(clustering == "origin"){
  # Focus on flows that START in the same area
  alpha = 1.5
  beta = 0.5
} else if(clustering == "destination"){
  # Focus on flows that END in the same area
  alpha = 0.5
  beta = 1.5
}


# --- step 2: Apply the function
distances <- flow_distance(flows = od_demand_xyuv,
                           alpha = alpha,
                           beta = beta)



# --- step 3: Convert df to a distance matrix
dist_mat <- distances %>%
  select(flow_ID_a, flow_ID_b, fds) %>%
  pivot_wider(names_from = flow_ID_b, values_from = fds) %>%
  column_to_rownames(var = "flow_ID_a")



# --- step 4: Remove NAs from distance matrix

# # this is a sparse matrix (lot's of OD pairs without flow, so most pairs of OD pairs don't exist)
# # replace NA with a very high number?
# max(dist_mat, na.rm = TRUE)
#
# dist_mat[is.na(dist_mat)] <- max(dist_mat, na.rm = TRUE) * 3


# ------------------------- CLUSTERING USING DBSCAN ------------------------- #




### ---------- STEP 1: Create Weights for dbscan ---------- ###

# We have an OD matrix where each flow vector represents n commuters. We need to weight each vector
# by the number of commuters so that minPTS works correctly and doesn't treat each vector as 1 item.

# The weights vector needs to match the rows of the distance matrix

# --- get weights
w <- dist_mat %>%
  rownames_to_column(var = "flow_ID") %>%
  select(flow_ID) %>%
  # add commuting data to
  inner_join(od_demand_jittered %>%
               select(flow_ID, commute_all),
             by = "flow_ID")

# weight vector
w_vec <- as.vector(w$commute_all)



###  ---------- STEP 2: Decide on Epsilon (ε) and minPTS ---------- ###


# epsilon (ε) is a parameter used to define the maximum distance threshold
# for points to be considered as neighbors. minPTS is the minimum number of points
# needed in a radius (ε) for a cluster to be formed

# ----- K-nearest neighbor knee plot
# kNNdistplot(dist_mat, k = 100)

# ----- Explore distance distribution of matrix (for epsilon)

# distance distribution of the flows
hist(distances$fds, breaks = 100)

# ----- Monte Carlo simulation to get mean value of distances (for epsilon)

# # if we consider all flows, the distance is skewed by flows that START at the same O
# # or end at the same D, and this will give us clusters of flows that either start or end at the same point
#
# n <- nrow(distances)  # Number of rows in your dataframe
# num_samples <- 100  # Number of random samples
# repetitions <- 3000  # Number of times to repeat the process
#
# # Define a function to calculate the sum of "distance" column for random samples
# mean_distance <- function() {
#   sample_indices <- sample(1:n, num_samples, replace = FALSE)
#   subset_df <- distances[sample_indices, ]
#   return(mean(subset_df$fds))
# }
#
# # Use replicate to repeat the process
# results <- replicate(repetitions, mean_distance())
# hist(results)

#
# # ----- Sensitivity analysis(for different epsilon and minpts combinaitons)
#
# # function to get clustering results for many combinations
# dbscan_sensitivity_res <- dbscan_sensitivity(distance_matrix = dist_mat,
#                                              options_epsilon <- c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 5, 6, 7, 7.5, 8, 9),
#                                              options_minpts <- c(50, 75, 100, 150, 175, 200, 250, 300, 400, 500, 700, 1000, 1500, 3000, 5000, 10000),
#                                              weights = w_vec,
#                                              flows = st_drop_geometry(od_demand_jittered)
#                                              )
#
#
# arrow::write_parquet(dbscan_sensitivity_res, paste0("data/interim/travel_demand/", geography, "/od_demand_clustering_sensitivity.parquet"))
# # dbscan_sensitivity_res <- arrow::read_parquet(paste0("data/interim/travel_demand/", geography, "/od_demand_clustering_sensitivity.parquet"))
#
#
# # All results plotted together
# dbscan_sensitivity_res %>%
#   filter(cluster != 0) %>%
#   ggplot(aes(x = cluster, y = size, fill = commuters_sum)) +
#   geom_col() +
#   scale_y_continuous(trans='log10') +
#   facet_wrap(~id, scales = "fixed") +
#   labs(title = "Sensitivity analysis for clustering - Varying {eps} and {minPts}",
#        subtitle = "Parameter combinations that returned more than 1 cluster",
#        x = "Cluster no.",
#        y = "No. of od pairs in cluster",
#        fill= "No. of commuters") +
#  theme_minimal()
#
# ggsave("data/processed/plots/eda/od_clustering/sensitivity_analysis_eps_minpts_all.png", width = 14, height = 10)
#
# dbscan_sensitivity_res %>%
#   filter(cluster != 0) %>%
#   group_by(id) %>%
#   #mutate(clusters = n()) %>%
#   # How many clusters have more than 5 od pairs in them?
#   mutate(clusters = sum(size > 5)) %>%
#   ungroup() %>%
#   filter(clusters > 5) %>%
#   ggplot(aes(x = cluster, y = size, fill = commuters_sum)) +
#   geom_col() +
#   scale_y_continuous(trans='log10') +
#   facet_wrap(~id, scales = "fixed") +
#   labs(title = "Sensitivity analysis for clustering - Varying {eps} and {minPts}",
#        subtitle = "Parameter combinations with > 5 clusters having at least 5 od pairs each",
#        x = "Cluster no.",
#        y = "No. of od pairs in cluster",
#        fill= "No. of commuters") +
#   theme_minimal()
#
# ggsave("data/processed/plots/eda/od_clustering/sensitivity_analysis_eps_minpts_filtered.png", width = 14, height = 10)
#


###  --------------------  STEP 3: Cluster  -------------------- ###


# cluster option 1: border points assigned to cluster
cluster_dbscan = dbscan::dbscan(dist_mat,
                                minPts = 70, # 125
                                eps = 8, # 9.5
                                #borderPoints = FALSE,
                                weights = w_vec)
# # for splitted distance
# cluster_dbscan = dbscan::dbscan(dist_mat,
#                                 minPts = 70, # 125
#                                 eps = 6.5, # 9.5
#                                 #borderPoints = FALSE,
#                                 weights = w_vec)

# # cluster option 2: border points not assigned to cluster
# cluster_dbscan = dbscan::dbscan(dist_mat,
#                                 minPts = 250,
#                                 eps = 1,
#                                 weights = w_vec,
#                                 borderPoints = FALSE)


#cluster_dbscan = dbscan::dbscan(dist_mat, minPts = 500, eps = (max(distances$fds) * 5/8), weights = w_vec)


unique(cluster_dbscan$cluster)


###  --------------------  STEP 4: Save results  -------------------- ###


# get results
cluster_dbscan_res <- dist_mat %>%
  mutate(cluster = cluster_dbscan$cluster)

# prepare data for joining
cluster_dbscan_res <- cluster_dbscan_res %>%
  rownames_to_column(var = "flow_ID") %>%
  select(flow_ID, cluster)


# add geometry back
cluster_dbscan_res <- od_demand_jittered %>%
  inner_join(cluster_dbscan_res, by = "flow_ID")


# save
st_write(cluster_dbscan_res, paste0("data/processed/clustering/scenario_", scenario, "_distance_", distance_threshold, "_", clustering, ".geojson"), delete_dsn = TRUE)






