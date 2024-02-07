library(tidyverse)
library(sf)
library(lwgeom)
# library(spatstat)
library(dbscan)
library(tmap)


source("R/study_area_geographies.R")
source("R/filter_od_matrix.R")


########## ----------------------- Read in the data ----------------------- ##########

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

# ----------- 2.  Census OD data

# Demand (census) + supply (travel time) data

od_demand <- arrow::read_parquet(paste0("data/raw/travel_demand/od_census_2021/demand_study_area_", tolower(geography), ".parquet"))

# filter to specific combination
# TODO: get seperate flows for car and pt, and keep two combinations
od_demand <- od_demand %>%
  filter(combination == "pt_wkday_morning")

# rename columns as most functions are applied on generic column names
from_id_col = paste0(geography, "21CD_home")
to_id_col = paste0(geography, "21CD_work")

od_demand = od_demand %>%
  rename("Origin" = all_of(from_id_col),
         "Destination" = all_of(to_id_col))
########## ----------------------- Convert df to sf desire lines ----------------------- ##########


# --- create desire lines and remove od pairs with very short distance

# TODO: edit this to avoid clusters of very short flows
  # "Density-based clustering for bivariate-flow data" (section 5.2): preprocessing step to avoid
  # clusters of very short flows. this involves splitting the data into 3 chunks
  # based on length (
od_demand_filtered = filter_matrix_by_distance(zones = study_area,
                                               od_matrix = od_demand,
                                               dist_threshold = 500)

# # try removing ODs with low flows
# od_demand_filtered <- od_demand_filtered %>%
#   filter(commute_all < 10)

od_demand_filtered <- od_demand_filtered %>%
  filter(distance_m <= max(distance_m) / 7)

########## ------------ "Spatial Cluster Detection in Spatial Flow Data" ---------- ##########

# ----- STEP 1: Get distance between flows


# --- a. prepare the data

# metric crs
od_demand_filtered <- od_demand_filtered %>%
  st_transform(3857)

# Add columns for coordinates of startpoint (X, Y) and endpoint (U, V)
od_demand_xyuv <- od_demand_filtered %>%
  mutate(x = as_tibble(st_coordinates(st_startpoint(od_demand_filtered)))$X,
         y = as_tibble(st_coordinates(st_startpoint(od_demand_filtered)))$Y,
         u = as_tibble(st_coordinates(st_endpoint(od_demand_filtered)))$X,
         v = as_tibble(st_coordinates(st_endpoint(od_demand_filtered)))$Y) %>%
  st_drop_geometry()

# create flow ID column
od_demand_xyuv <- od_demand_xyuv %>%
  mutate(flow_ID = paste(Origin, Destination, sep = "-"))


# --- b. Calculate distance matrix

# Function to get flow distance and flow dissimilarity by iterating over Origins
flow_distance = function(study_area, flows, alpha = 1, beta = 1, geoid){
  # empty list to store results for each origin
  results <- vector(mode = "list", length = nrow(study_area))

  for (i in 1:nrow(study_area)){

    #status updates
    print(paste0("Getting distances for Origin: ", i, " ....."))

    # filter to specific origin
    flows_origin_i <- flows %>% filter(Origin == study_area[[geoid]][i])

    # create grid
    flows_origin_i_grid <- expand_grid(flow_ID_a = flows_origin_i$flow_ID,
                                       flow_ID_b = flows_origin_i$flow_ID)

    # add the coordinate data
    flows_origin_i_grid <- flows_origin_i_grid %>%
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

    flows_origin_i_grid <- flows_origin_i_grid %>%
      # Flow Distance
      mutate(fd = sqrt(alpha * ((x_i - x_j)^2 + (y_i - y_j)^2) +
                         beta * ((u_i - u_j)^2 + (v_i - v_j)^2)),
      # Flow Dissimilarity
             fds = sqrt((alpha * ((x_i - x_j)^2 + (y_i - y_j)^2) +
                           beta * ((u_i - u_j)^2 + (v_i - v_j)^2)) / (distance_m_i * distance_m_j)))

    # add results to list
    results[[i]] <- flows_origin_i_grid

  }
  return(results)
}

# apply function to get distances
distances <- flow_distance(study_area = study_area,
                      flows = od_demand_xyuv,
                      alpha = 1,
                      beta = 1,
                      geoid = geoid_col)

# convert from list of dfs to one df
distances <- bind_rows(distances)



# convert df to a distance matrix
dist_mat <- distances %>%
  select(flow_ID_a, flow_ID_b, fds) %>%
  pivot_wider(names_from = flow_ID_b, values_from = fds) %>%
  column_to_rownames(var = "flow_ID_a")



# this is a sparse matrix (lot's of OD pairs without flow, so most pairs of OD pairs don't exist)
# replace NA with a very high number?
max(dist_mat, na.rm = TRUE)

dist_mat[is.na(dist_mat)] <- max(dist_mat, na.rm = TRUE) * 2


# ------------------------- CLUSTERING USING DBSCAN ------------------------- #

#  ---------- STEP 1: Decide on Epsilon (ε) and minPTS

# epsilon (ε) is a parameter used to define the maximum distance threshold
# for points to be considered as neighbors. minPTS is the minimum number of points
# needed in a radius (ε) for a cluster to be formed

# ----- K-nearest neighbor knee plot
kNNdistplot(dist_mat, k = 100)

# ----- Explore distance distribution of matrix

# distance distribution of the flows
hist(distances$fds)

# ----- Monte Carlo simulation to get mean value of distances

# if we consider all flows, the distance is skewed by flows that START at the same O
# or end at the same D, and this will give us clusters of flows that either start or end at the same point

n <- nrow(distances)  # Number of rows in your dataframe
num_samples <- 100  # Number of random samples
repetitions <- 1000  # Number of times to repeat the process

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
  inner_join(od_demand_filtered %>%
               st_drop_geometry() %>%
               mutate(flow_ID = paste0(Origin, "-", Destination)) %>%
               select(flow_ID, commute_all),
             by = "flow_ID")

# weight vector
w_vec <- as.vector(w$commute_all)

# cluster option 1: border points assigned to cluster
cluster_dbscan = dbscan::dbscan(dist_mat,
                                minPts = 500,
                                eps = 1.2,
                                weights = w_vec)

# # cluster option 2: border points not assigned to cluster
# cluster_dbscan = dbscan::dbscan(dist_mat,
#                                 minPts = 200,
#                                 eps = 1.3,
#                                 weights = w_vec,
#                                 borderPoints = FALSE)


#cluster_dbscan = dbscan::dbscan(dist_mat, minPts = 500, eps = (max(distances$fds) * 5/8), weights = w_vec)


unique(cluster_dbscan$cluster)










# ----------------------------------------



# get results
cluster_dbscan_res <- dist_mat %>%
  mutate(cluster = cluster_dbscan$cluster)

# prepare data for joining
cluster_dbscan_res <- cluster_dbscan_res %>%
  rownames_to_column(var = "flow_ID") %>%
  select(flow_ID, cluster)


# add geometry back

cluster_dbscan_res <- od_demand_filtered %>%
  mutate(flow_ID = paste0(Origin, "-", Destination)) %>%
  inner_join(cluster_dbscan_res, by = "flow_ID")

# plot
plot(cluster_dbscan_res["cluster"])

tm_shape(study_area) +
  tm_borders(col = "grey60",
             alpha = 0.5) +
  tm_shape(study_area) +
  tm_fill(col = "grey95",
          alpha = 0.5) +
  tm_shape(cluster_dbscan_res %>%
             filter(cluster == 7) %>%
             #filter(distance_m > 20000) %>%
             mutate(cluster = as.factor(cluster)),) +
  tm_lines(lwd = "commute_all")


tm_shape(study_area) +
  tm_borders(col = "grey60",
             alpha = 0.5) +
  tm_shape(study_area) +
  tm_fill(col = "grey95",
          alpha = 0.5) +
tm_shape(cluster_dbscan_res %>%
           #filter(cluster < 20) %>%
           #filter(distance_m > 20000) %>%
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
            nrow = 3,
            showNA = FALSE) +
  tm_layout(fontfamily = 'Georgia',
            main.title = "Clustering flows using HDBSCAN",
            main.title.size = 1.1,
            main.title.color = "azure4",
            main.title.position = "left",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.stack = "horizontal",
            frame = FALSE)


# Option 2: Flow Dissimilarity


# ----- STEP 2: Hotspot detection

# may have to compute Ripley's K from scratch using the equation (as we have the distance, not the actual points)
# see wikipedia for eqn

# alternatively, spatstat::kest() is a function for Ripley's K (unclear if useful)
# https://andrewmaclachlan.github.io/CASA0005repo/detecting-spatial-patterns.html



hist(od_demand_filtered$commute_all, breaks = 200)
