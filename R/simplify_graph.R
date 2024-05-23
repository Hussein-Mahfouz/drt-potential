# TODO: edit this function so that the column to morph is supplied as a fn argument
# I am attampting this in the second function below but getting stuck in:
# summarise_attributes = list({{column_name}} = "sum", "first"))


# function to simplify graph (merge parallel edges)
simplify_graph = function(layer, radius){
  # --- 1. convert to sfnetwork
  layer_net <- layer %>%
    st_transform(3857) %>%  # I want metric crs for grouping nodes based on distance
    as_sfnetwork(., directed = FALSE)

  # --- 2. group nodes based on proximity
  # -- a. Retrieve the coordinates of the nodes.
  node_coords = layer_net %>%
    activate("nodes") %>%
    st_coordinates()
  # -- b. Cluster the nodes with the DBSCAN spatial clustering algorithm.
  # We set eps = x such that:
  # Nodes within a distance of x meters from each other will be in the same cluster.
  # We set minPts = 1 such that:
  # A node is assigned a cluster even if it is the only member of that cluster.
  clusters = dbscan(node_coords, eps = radius, minPts = 1)$cluster
  # Add the cluster information to the nodes of the network.
  clustered =  layer_net %>%
    activate("nodes") %>%
    mutate(cls = clusters)
  # -- c. contract: merge nodes together based on clustering results
  layer_cont = tidygraph::convert(
    clustered,
    to_spatial_contracted,
    cls,
    # simplify - FALSE so that we keep multiple edges.
    # All edges that have the same from and to node will then be grouped in the next step
    simplify = FALSE
  )

  # replace NA values with 0 so that summation works!
  layer_cont <- layer_cont %>%
    activate("edges") %>%
    mutate(commute_all = replace_na(commute_all, 0))

  # --- 3. merge multiple edges (edges that have the same end nodes)

  # combine edges that have the same origin and destination node.
  # This allows us to merge directions (as well as multiple lanes e.g. having flow
  # on 1 segment on the ring road instead of 4 lanes)

  # Simplify the network with to_spatial_simple.
  layer_simp = tidygraph::convert(
    layer_cont,
    to_spatial_simple,
    # `Vehicles/Hour` will be summed, all other attributes we will take the first row in the group
    summarise_attributes = list(commute_all = "sum", "first"))

  # --- 4. Convert to sf in order to save
  layer_simp <- layer_simp %>%
    activate("edges") %>%
    st_as_sf() %>% st_transform(4326) %>%
    select(commute_all)

  return(layer_simp)
}



#
#
# # function to simplify graph (merge parallel edges)
# simplify_graph2 = function(layer, radius, column_name){
#   # --- 1. convert to sfnetwork
#   layer_net <- layer %>%
#     sf::st_transform(3857) %>%  # I want metric crs for grouping nodes based on distance
#     sfnetworks::as_sfnetwork(., directed = FALSE)
#
#   # --- 2. group nodes based on proximity
#   # -- a. Retrieve the coordinates of the nodes.
#   node_coords = layer_net %>%
#     tidygraph::activate("nodes") %>%
#     sf::st_coordinates()
#   # -- b. Cluster the nodes with the DBSCAN spatial clustering algorithm.
#   # We set eps = x such that:
#   # Nodes within a distance of x meters from each other will be in the same cluster.
#   # We set minPts = 1 such that:
#   # A node is assigned a cluster even if it is the only member of that cluster.
#   clusters = dbscan::dbscan(node_coords, eps = radius, minPts = 1)$cluster
#   # Add the cluster information to the nodes of the network.
#   clustered =  layer_net %>%
#     tidygraph::activate("nodes") %>%
#     dplyr::mutate(cls = clusters)
#   # -- c. contract: merge nodes together based on clustering results
#   layer_cont = tidygraph::convert(
#     clustered,
#     sfnetworks::to_spatial_contracted,
#     cls,
#     # simplify - FALSE so that we keep multiple edges.
#     # All edges that have the same from and to node will then be grouped in the next step
#     simplify = FALSE
#   )
#
#   # replace NA values with 0 so that summation works!
#   layer_cont <- layer_cont %>%
#     tidygraph::activate("edges") %>%
#     mutate({{column_name}} := replace_na({{column_name}}, 0))
#
#   # --- 3. merge multiple edges (edges that have the same end nodes)
#
#   # combine edges that have the same origin and destination node.
#   # This allows us to merge directions (as well as multiple lanes e.g. having flow
#   # on 1 segment on the ring road instead of 4 lanes)
#
#   # Simplify the network with to_spatial_simple.
#   layer_simp = tidygraph::convert(
#     layer_cont,
#     sfnetworks::to_spatial_simple,
#     # Chosen column will be summed, all other attributes we will take the first row in the group
#     # TODO: pass variable column name instead:
#     summarise_attributes = list({{column_name}} = "sum", "first"))
#
#   # --- 4. Convert to sf in order to save
#   layer_simp <- layer_simp %>%
#     tidygraph::activate("edges") %>%
#     st_as_sf() %>% st_transform(4326) %>%
#     dplyr::select({{column_name}})
#
#   return(layer_simp)
# }
#

