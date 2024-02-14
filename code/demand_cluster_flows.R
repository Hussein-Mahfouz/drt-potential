library(tidyverse)
library(sf)
library(lwgeom)
# library(spatstat)
library(dbscan)
library(tmap)


source("R/study_area_geographies.R")
source("R/filter_od_matrix.R")
source("code/demand_cluster_flows_prep.R")


########## ----------------------- Read in the data ----------------------- ##########
# geography <- "MSOA"
#
# od_demand_jittered <- st_read(paste0("data/interim/travel_demand/", geography, "/od_demand_jittered_for_clustering.geojson"))
# od_demand_jittered <- st_read(paste0("data/interim/travel_demand/", geography, "/od_demand_jittered_for_clustering_mode.geojson"))

########## ------------ "Spatial Cluster Detection in Spatial Flow Data" ---------- ##########

# ----- STEP 1: Get distance between flows


# --- a. prepare the data

# - metric crs
od_demand_jittered <- od_demand_jittered %>%
  st_transform(3857) %>%
  # add distance column
  mutate(distance_m =  units::drop_units(sf::st_length(.)))




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

clustering <- "equal"
# clustering <- "origin"
# clustering <- "destination"

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

#  ---------- STEP 1: Decide on Epsilon (ε) and minPTS

# epsilon (ε) is a parameter used to define the maximum distance threshold
# for points to be considered as neighbors. minPTS is the minimum number of points
# needed in a radius (ε) for a cluster to be formed

# ----- K-nearest neighbor knee plot
# kNNdistplot(dist_mat, k = 100)

# ----- Explore distance distribution of matrix

# distance distribution of the flows
hist(distances$fds, breaks = 100)

# ----- Monte Carlo simulation to get mean value of distances

# if we consider all flows, the distance is skewed by flows that START at the same O
# or end at the same D, and this will give us clusters of flows that either start or end at the same point

n <- nrow(distances)  # Number of rows in your dataframe
num_samples <- 100  # Number of random samples
repetitions <- 3000  # Number of times to repeat the process

# Define a function to calculate the sum of "distance" column for random samples
mean_distance <- function() {
  sample_indices <- sample(1:n, num_samples, replace = FALSE)
  subset_df <- distances[sample_indices, ]
  return(mean(subset_df$fds))
}

# Use replicate to repeat the process
results <- replicate(repetitions, mean_distance())
hist(results)


# ---------- STEP 2: Create Weights

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


# ---------- STEP 3: Cluster

# cluster option 1: border points assigned to cluster
cluster_dbscan = dbscan::dbscan(dist_mat,
                                minPts = 70, # 125
                                eps = 8, # 9.5
                                #borderPoints = FALSE,
                                weights = w_vec)

# # cluster option 2: border points not assigned to cluster
# cluster_dbscan = dbscan::dbscan(dist_mat,
#                                 minPts = 250,
#                                 eps = 1,
#                                 weights = w_vec,
#                                 borderPoints = FALSE)


#cluster_dbscan = dbscan::dbscan(dist_mat, minPts = 500, eps = (max(distances$fds) * 5/8), weights = w_vec)


unique(cluster_dbscan$cluster)



# ------------------------- VISUALISE RESULTS ------------------------- #


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

# check size per cluster and total commuters per cluster
cluster_dbscan_res %>%
  st_drop_geometry() %>%
  group_by(cluster) %>%
  summarise(size = n(), commuters_sum = sum(commute_all)) %>%
  arrange(desc(size))

# add size and total commuters columns
cluster_dbscan_res <- cluster_dbscan_res %>%
  group_by(cluster) %>%
  mutate(size = n(),
         commuters_sum = sum(commute_all)) %>%
  ungroup()

# plot
plot(cluster_dbscan_res["cluster"])

tm_shape(study_area) +
  tm_borders(col = "grey60",
             alpha = 0.5) +
  tm_shape(study_area) +
  tm_fill(col = "grey95",
          alpha = 0.5) +
  tm_shape(cluster_dbscan_res %>%
             filter(cluster == 10) %>%
             #filter(distance_m > 20000) %>%
             mutate(cluster = as.factor(cluster)),) +
  tm_lines(lwd = "commute_all")


### ---------- Plot 1: Top n Clusters (Facet Plot) ---------- ###

tm_shape(study_area) +
  tm_borders(col = "grey60",
             alpha = 0.5) +
  tm_shape(study_area) +
  tm_fill(col = "grey95",
          alpha = 0.5) +
tm_shape(cluster_dbscan_res %>%
           filter(size > 5, size < 1000) %>%
           filter(commuters_sum > 200) %>%
           mutate(cluster = as.factor(cluster))) +
  tm_lines(lwd = "commute_all",
           col = "cluster",
           scale = 10,
           palette = "Accent", #YlGn
           #style = "pretty",
           alpha = 1,
           title.col = "Cluster",
           #title.lwd = "Vehicles per hour",
           legend.col.is.portrait = FALSE) +
  tm_facets(by = "cluster",
            free.coords = FALSE,
            nrow = 2,
            showNA = FALSE) +
  tm_layout(fontfamily = 'Georgia',
            main.title = "Clustering flows using DBSCAN (clusters with > 200 commuters)",
            main.title.size = 1.1,
            main.title.color = "azure4",
            main.title.position = "left",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.stack = "horizontal",
            frame = FALSE)


### ---------- Plot 2: Compare mode composition of each cluster (desire line level) ---------- ###




# get bus ridership as a fraction of car ridership
cluster_dbscan_res_mode <- cluster_dbscan_res %>%
  mutate(commute_frac = commute_bus / commute_car)



tm_shape(study_area) +
  tm_borders(col = "grey60",
             alpha = 0.5) +
  tm_shape(study_area) +
  tm_fill(col = "grey95",
          alpha = 0.5) +
  tm_shape(cluster_dbscan_res_mode %>%
             filter(size > 5, size < 1000) %>%
             filter(commuters_sum > 200) %>%
             mutate(cluster = as.factor(cluster))) +
  tm_lines(lwd = "commute_all",
           col = "commute_frac",
           scale = 10,
           palette = "RdYlGn", #Accent
           #style = "pretty",
           alpha = 1,
           title.col = "Fraction of bus to car users",
           #title.lwd = "Vehicles per hour",
           legend.col.is.portrait = FALSE) +
  tm_facets(by = "cluster",
            free.coords = FALSE,
            nrow = 2,
            showNA = FALSE) +
  tm_layout(fontfamily = 'Georgia',
            main.title = "Clustering flows using DBSCAN (clusters with > 200 commuters)",
            main.title.size = 1.1,
            main.title.color = "azure4",
            main.title.position = "left",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.stack = "horizontal",
            frame = FALSE)


### ---------- Plot 3: Compare mode composition of each cluster (cluster level) ---------- ###


# Get mode composition of entire cluster

cluster_dbscan_res_mode_summary <- cluster_dbscan_res_mode %>%
  st_drop_geometry() %>%
  group_by(cluster, size) %>%
  summarise(across(starts_with("commute_"), sum)) %>%
  ungroup() %>%
  mutate(commute_frac_cluster = round(commute_bus / commute_car, 2))

# add commuting fraction
cluster_dbscan_res_mode <- cluster_dbscan_res_mode %>%
  inner_join(cluster_dbscan_res_mode_summary %>% select(cluster, commute_frac_cluster), by = "cluster")


tm_shape(study_area) +
  tm_borders(col = "grey60",
             alpha = 0.5) +
  tm_shape(study_area) +
  tm_fill(col = "grey95",
          alpha = 0.5) +
  tm_shape(cluster_dbscan_res_mode %>%
             filter(size > 5, size < 1000) %>%
             filter(commuters_sum > 200) %>%
             mutate(cluster = as.factor(cluster))) +
  tm_lines(lwd = "commute_all",
           col = "commute_frac_cluster",
           scale = 10,
           palette = "RdYlGn", #Accent
           #style = "pretty",
           alpha = 1,
           title.col = "Fraction of bus to car",
           #title.lwd = "Vehicles per hour",
           legend.col.is.portrait = FALSE) +
  tm_facets(by = "cluster",
            free.coords = FALSE,
            nrow = 2,
            showNA = FALSE) +
  tm_layout(fontfamily = 'Georgia',
            main.title = "Clustering flows using DBSCAN",
            main.title.size = 1.1,
            main.title.color = "azure4",
            main.title.position = "left",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.stack = "horizontal",
            frame = FALSE)









# Get bounding box of cluster
big_clusters %>%
  filter(cluster == 31) %>%
  st_union() %>%
  st_convex_hull() -> x

plot(st_geometry(study_area %>% st_transform(3857)))
plot(st_geometry(x), add = TRUE, col = "red")
