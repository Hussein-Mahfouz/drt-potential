library(tidyverse)
library(sf)
library(lwgeom)
library(spatstat)
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



########## ------------ "Spatial Cluster Detection in Spatial Flow Data" ---------- ##########

# ----- STEP 1: Get distance between flows

# metric crs
od_demand_filtered <- od_demand_filtered %>%
  st_transform(3857)

# Add columns for coordinates of startpoint (X, Y) and endpoint (U, V)

x <- od_demand_filtered %>%
  mutate(x = as_tibble(st_coordinates(st_startpoint(od_demand_filtered)))$X,
         y = as_tibble(st_coordinates(st_startpoint(od_demand_filtered)))$Y,
         u = as_tibble(st_coordinates(st_endpoint(od_demand_filtered)))$X,
         v = as_tibble(st_coordinates(st_endpoint(od_demand_filtered)))$Y) %>%
  st_drop_geometry()

# create flow ID column
x <- x %>%
  mutate(flow_ID = paste(Origin, Destination, sep = "-"))


# filter to specific origin
x_origin_i <- x %>% filter(Origin == study_area$MSOA21CD[1])

# create grid
x_origin_i_grid <- expand_grid(flow_ID_a = x_origin_i$flow_ID,
                               flow_ID_b = x_origin_i$flow_ID)

# add the coordinate data
x_origin_i_grid2 <- x_origin_i_grid %>%
  # --- add flow_a coordinates
  inner_join(x %>%
              select(flow_ID, x, y, u, v, distance_m),
            by = c("flow_ID_a" = "flow_ID")) %>%
  # rename coordinate columns by adding a suffix
  rename_with(.fn = ~ paste0(.x, "_i"), .cols = c("x", "y", "u", "v", "distance_m")) %>%
  # --- add flow_b coordinates
  inner_join(x %>%
               select(flow_ID, x, y, u, v, distance_m),
             by = c("flow_ID_b" = "flow_ID")) %>%
  # rename coordinate columns by adding a suffix
  rename_with(.fn = ~ paste0(.x, "_j"), .cols = c("x", "y", "u", "v", "distance_m"))

# ----- get "Flow Distance" and "Flow Dissimilarity"

# Flow Distance

x_origin_i_grid2 <- x_origin_i_grid2 %>%
  mutate(fd = sqrt(0.5 * ((x_i - x_j)^2 + (y_i - y_j)^2) +
              0.5 * ((u_i - u_j)^2 + (v_i - v_j)^2)),
         fds = sqrt((0.5 * ((x_i - x_j)^2 + (y_i - y_j)^2) +
                      0.5 * ((u_i - u_j)^2 + (v_i - v_j)^2)) / (distance_m_i * distance_m_j)))



flow_distance = function(study_area, flows, geoid){
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

    # Flow Distance
    flows_origin_i_grid <- flows_origin_i_grid %>%
      mutate(fd = sqrt(0.5 * ((x_i - x_j)^2 + (y_i - y_j)^2) +
                         0.5 * ((u_i - u_j)^2 + (v_i - v_j)^2)),
             fds = sqrt((0.5 * ((x_i - x_j)^2 + (y_i - y_j)^2) +
                           0.5 * ((u_i - u_j)^2 + (v_i - v_j)^2)) / (distance_m_i * distance_m_j)))

    # add results to list
    results[[i]] <- flows_origin_i_grid

  }
  return(results)
}


test <- flow_distance(study_area = study_area,
                      flows = x,
                      geoid = geoid_col)

test2 <- bind_rows(test)

# replace na values with


# convert from long to wide
test2 %>%
  select(flow_ID_a, flow_ID_b, fds) %>%
  pivot_wider(names_from = flow_ID_b, values_from = fds) %>%
  column_to_rownames(var = "flow_ID_a") -> test3



# this is a sparse matrix (lot's of OD pairs without flow, so most pairs of OD pairs don't exist)
# replace NA with a very high number?
max(test3, na.rm = TRUE)

test3[is.na(test3)] <- max(test3, na.rm = TRUE) * 2


# hdbscan

# sample
test4 <- test3[1:5000, 1:5000]
test4 <- test3


# CLUSTERING

# ---------- HDBSCAN

res = dbscan::hdbscan(test4, minPts = 50)


# ---------- OTHER (DBSAN / OPTICS)

# in dbscan / optics, we need to define eps. epsilon (ε) is a parameter used to
# define the maximum distance threshold for points to be considered as neighbors

# we need to look at the distance distribution of the flows
hist(test2$fds)

# if we consider all flows, the distance is skewed by flows that START at the same O
# or end at the same D, and this will give us clusters of flows that either start or end at the same point

n <- nrow(test2)  # Number of rows in your dataframe
num_samples <- 100  # Number of random samples
repetitions <- 1000  # Number of times to repeat the process

# Define a function to calculate the sum of "distance" column for random samples
mean_distance <- function() {
  sample_indices <- sample(1:n, num_samples, replace = FALSE)
  subset_df <- test2[sample_indices, ]
  return(mean(subset_df$fds))
}

# Use replicate to repeat the process
results <- replicate(repetitions, mean_distance())
hist(results)


# ---------- DBSCAN (with wights)

# get weights
test4 %>%
  rownames_to_column(var = "flow_ID") %>%
  select(flow_ID) %>%
  inner_join(od_demand_filtered %>%
               st_drop_geometry() %>%
               mutate(flow_ID = paste0(Origin, "-", Destination)) %>%
               select(flow_ID, commute_all),
             by = "flow_ID") -> w


w_vec <- as.vector(w$commute_all)

res = dbscan::dbscan(test4, minPts = 400, eps = 6, weights = w_vec)



# ---------- OPTICS
res = dbscan::optics(test4, eps = 1, minPts = 20)
plot(res)
# get eps_cl from plot y axis (threshold that seperates the clusters)
res = extractDBSCAN(res, eps_cl = 1.7)
# plot again to see clustering results
plot(res)

# attempt to precomute weighted distance matrix

# # Apply weights to the distance matrix
# weighted_distance_matrix <- test4 * weights %*% t(weights)














# ----------------------------------------



# get results
test5 <- test4 %>%
  mutate(cluster = res$cluster)

# prepare data for joining
test5 %>%
  rownames_to_column(var = "flow_ID") %>%
  select(flow_ID, cluster) -> test5


# add geometry back

od_demand_filtered %>%
  mutate(flow_ID = paste0(Origin, "-", Destination)) %>%
  inner_join(test5, by = "flow_ID") -> test6

# plot
plot(test6["cluster"])




tm_shape(study_area) +
  tm_borders(col = "grey60",
             alpha = 0.5) +
  tm_shape(study_area) +
  tm_fill(col = "grey95",
          alpha = 0.5) +
tm_shape(test6 %>%
           filter(distance_m > 20000, cluster < 20) %>%
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
            nrow = 4,
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


test6 %>% group_by(cluster) %>%
  st_drop_geometry() %>%
  summarise(commute_all = sum(commute_all)) -> p





# Option 2: Flow Dissimilarity


# ----- STEP 2: Hotspot detection

# may have to compute Ripley's K from scratch using the equation (as we have the distance, not the actual points)
# see wikipedia for eqn

# alternatively, spatstat::kest() is a function for Ripley's K (unclear if useful)
# https://andrewmaclachlan.github.io/CASA0005repo/detecting-spatial-patterns.html

