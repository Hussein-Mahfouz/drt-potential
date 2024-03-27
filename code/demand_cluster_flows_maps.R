##########################################################################################
### The purpose of this script is to visualize the results from demand_cluster_flows.R ###
##########################################################################################

library(tidyverse)
library(sf)
library(lwgeom)
library(geos)
# library(spatstat)
library(tmap)



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

# ----- assign number of rows dynamically for facet plot

# get clusters to map
cluster_dbscan_res %>%
  filter(size > 7, size < 5000) %>%
  filter(commuters_sum > 200) %>%
  filter(cluster != 0) -> clusters_vis

# we want maximum 4 maps per row
rows <- round(length(unique(clusters_vis$cluster)) / 4)

### ---------- Plot 1: Top n Clusters (Facet Plot) ---------- ###

tm_shape(study_area) +
  tm_borders(col = "grey60",
             alpha = 0.5) +
  tm_shape(study_area) +
  tm_fill(col = "grey95",
          alpha = 0.5) +
  tm_shape(cluster_dbscan_res %>%
             filter(size > 7, size < 5000) %>%
             filter(commuters_sum > 200) %>%
             filter(cluster != 0) %>%
             mutate(cluster = as.factor(cluster))) +
  tm_lines(lwd = "commute_all",
           col = "cluster",
           #col = "darkgreen",
           scale = 10,
           palette = "Accent", #YlGn
           #style = "pretty",
           alpha = 1,
           title.col = "Cluster",
           title.lwd = "No. of commuters",
           legend.col.show = FALSE,
           # remove "missing from legend
           showNA = FALSE) +
  tm_facets(by = "cluster",
            free.coords = FALSE,
            nrow = rows,
            showNA = FALSE) +
  tm_layout(fontfamily = 'Georgia',
            main.title = paste0("Clustered flows (OD", scenario, ")"),
            main.title.size = 1.1,
            main.title.color = "azure4",
            main.title.position = "left",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.stack = "horizontal",
            # remove panel headers
            panel.show = FALSE,
            frame = FALSE) -> map_cluster_results

map_cluster_results

tmap_save(tm = map_cluster_results, filename = paste0(plots_path, "map_clusters_scenario_", scenario, "_", clustering, "_length_", distance_threshold, ".png"), width = 12, dpi = 1080, asp = 0)



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
             filter(size > 7, size < 5000) %>%
             filter(commuters_sum > 200) %>%
             filter(cluster != 0) %>%
             mutate(cluster = as.factor(cluster)) %>%
             arrange(commute_frac)) +
  tm_lines(lwd = "commute_all",
           col = "commute_frac",
           scale = 10,
           palette = "RdYlGn", #Accent
           alpha = 1,
           title.col = "Fraction of Bus to Car users",
           title.lwd = "No. of commuters",
           #legend.col.is.portrait = FALSE,
           # remove "missing from legend
           showNA = FALSE) +
  tm_facets(by = "cluster",
            free.coords = FALSE,
            nrow = rows,
            showNA = FALSE) +
  tm_layout(fontfamily = 'Georgia',
            main.title = paste0("Clustered flows (OD", scenario, ")"),
            main.title.size = 1.1,
            main.title.color = "azure4",
            main.title.position = "left",
            #legend.outside = TRUE,
            #legend.outside.position = "bottom",
            #legend.stack = "horizontal",
            # remove panel headers
            panel.show = FALSE,
            frame = FALSE) -> map_cluster_results_bus_frac

map_cluster_results_bus_frac

tmap_save(tm = map_cluster_results_bus_frac, filename = paste0(plots_path, "map_clusters_scenario_", scenario, "_", clustering, "_length_", distance_threshold, "_bus_frac.png"), width = 12, dpi = 1080, asp = 0)

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
             filter(size > 7, size < 5000) %>%
             filter(commuters_sum > 200) %>%
             filter(cluster != 0) %>%
             mutate(cluster = as.factor(cluster))) +
  tm_lines(lwd = "commute_all",
           col = "commute_frac_cluster",
           scale = 10,
           palette = "RdYlGn", #Accent
           #style = "pretty",
           alpha = 1,
           title.col = "Fraction of Bus to Car users",
           title.lwd = "No. of commuters",
           #legend.col.is.portrait = FALSE,
           # remove "missing from legend
           showNA = FALSE) +
  tm_facets(by = "cluster",
            free.coords = FALSE,
            nrow = rows,
            showNA = FALSE) +
  tm_layout(fontfamily = 'Georgia',
            main.title = paste0("Clustered flows (OD", scenario, ")"),
            main.title.size = 1.1,
            main.title.color = "azure4",
            main.title.position = "left",
            # legend.outside = TRUE,
            # legend.outside.position = "bottom",
            # legend.stack = "horizontal",
            # remove panel headers
            panel.show = FALSE,
            frame = FALSE) -> map_cluster_results_bus_frac_grouped

map_cluster_results_bus_frac_grouped

tmap_save(tm = map_cluster_results_bus_frac_grouped, filename = paste0(plots_path, "map_clusters_scenario_", scenario, "_", clustering, "_length_", distance_threshold, "_bus_frac_grouped.png"), width = 12, dpi = 1080, asp = 0)


# --- Map with clusters ovelayed onto bus routes



gtfs_bus <- st_read("data/interim/gtfs_freq/gtfs_bus_sf.geojson")

# gtfs_bus <- gtfs_bus %>%
#   st_filter(st_union(study_area), .predicate = st_within)

tm_shape(study_area) +
  tm_borders(col = "grey60",
             alpha = 0.5) +
  tm_shape(study_area) +
  tm_fill(col = "grey95",
          alpha = 0.5) +
  # bus layer
  tm_shape(gtfs_bus %>%
             filter(scenario == "pt_wkday_morning") %>%
             mutate(headway_inv = (1/headway_secs) * 3600) %>%
             filter(headway_secs < 7200)) +
  tm_lines(col = "grey75",
           lwd = "headway_inv",
           scale = 5,
           palette = "-YlOrRd",
           style = "pretty",
           legend.col.show = FALSE,
           alpha = 1,
           title.lwd = "Buses/Hour",
           #legend.lwd.is.portrait = FALSE
  ) +
  # clusters
  tm_shape(cluster_dbscan_res_mode %>%
             filter(size > 7, size < 5000) %>%
             filter(commuters_sum > 200) %>%
             filter(cluster != 0) %>%
             mutate(cluster = as.factor(cluster))) +
  tm_lines(lwd = "commute_all",
           col = "commute_frac_cluster",
           scale = 10,
           palette = "RdYlGn", #Accent
           #style = "pretty",
           alpha = 1,
           title.col = "Fraction of Bus to Car users",
           title.lwd = "No. of commuters",
           #legend.col.is.portrait = FALSE,
           # remove "missing from legend
           showNA = FALSE) +
  tm_facets(by = "cluster",
            free.coords = FALSE,
            nrow = rows,
            showNA = FALSE) +
  tm_layout(fontfamily = 'Georgia',
            main.title = paste0("Clustered flows (OD", scenario, ")"),
            main.title.size = 1.1,
            main.title.color = "azure4",
            main.title.position = "left",
            #legend.outside = TRUE,
            #legend.outside.position = "bottom",
            #legend.stack = "horizontal",
            # remove panel headers
            panel.show = FALSE,
            frame = FALSE) -> map_cluster_results_bus_frac_grouped_gtfs

map_cluster_results_bus_frac_grouped_gtfs

tmap_save(tm = map_cluster_results_bus_frac_grouped_gtfs, filename = paste0(plots_path, "map_clusters_scenario_", scenario, "_", clustering, "_length_", distance_threshold, "_bus_frac_grouped_gtfs.png"), width = 12, dpi = 1080, asp = 0)





# --- Map with clusters as polygons (convex_hull())

# turn clusters into polygons using convex hull

cluster_dbscan_res_mode_poly <- cluster_dbscan_res_mode %>%
  filter(size > 7, size < 5000) %>%
  filter(commuters_sum > 200) %>%
  filter(cluster != 0) %>%
  mutate(cluster = as.factor(cluster)) %>%
  #head(1000) %>%
  group_by(cluster) %>%
  summarize(commuters_sum = first(commuters_sum),
            commute_frac_cluster = first(commute_frac_cluster),
            geometry = st_convex_hull(st_union(geometry))) %>%
  ungroup()


# plot

tm_shape(study_area) +
  tm_borders(col = "grey60",
             alpha = 0.5) +
  tm_shape(study_area) +
  tm_fill(col = "grey95",
          alpha = 0.5) +
  # bus layer
  tm_shape(gtfs_bus %>%
             filter(scenario == "pt_wkday_morning") %>%
             mutate(headway_inv = (1/headway_secs) * 3600) %>%
             filter(headway_secs < 7200)) +
  tm_lines(col = "grey70",
           lwd = "headway_inv",
           scale = 5,
           palette = "-YlOrRd",
           style = "pretty",
           legend.col.show = FALSE,
           alpha = 1,
           title.lwd = "Buses/Hour",
           #legend.lwd.is.portrait = FALSE
  ) +
  # clusters
  tm_shape(cluster_dbscan_res_mode_poly) +
  tm_polygons(col = "commute_frac_cluster",
              palette = "RdYlGn", #Accent
              #style = "pretty",
              alpha = 0.3,
              title = "Fraction of Bus \nto Car users",
              #legend.col.is.portrait = FALSE,
              # remove "missing from legend
              showNA = FALSE) +
  tm_facets(by = "cluster",
            #by = "commuters_sum",
            free.coords = FALSE,
            nrow = rows,
            showNA = FALSE) +
  tm_layout(fontfamily = 'Georgia',
            main.title = paste0("Clustered flows (OD", scenario, ")"),
            main.title.size = 1.1,
            main.title.color = "azure4",
            main.title.position = "left",
            #legend.outside = TRUE,
            #legend.outside.position = "bottom",
            #legend.stack = "horizontal",
            # remove panel headers
            #panel.show = FALSE,
            panel.label.size = 1,
            panel.label.bg.color = NA,
            panel.labels = 1:length(unique(cluster_dbscan_res_mode_poly$cluster)),
            frame = FALSE) -> map_cluster_results_bus_frac_grouped_gtfs_poly

map_cluster_results_bus_frac_grouped_gtfs_poly

tmap_save(tm = map_cluster_results_bus_frac_grouped_gtfs_poly, filename = paste0(plots_path, "map_clusters_scenario_", scenario, "_", clustering, "_length_", distance_threshold, "_bus_frac_grouped_gtfs_poly.png"), width = 12, dpi = 1080, asp = 0)





# --- Map with clusters as polygons (convex_hull()) + lines in background


tm_shape(study_area) +
  tm_borders(col = "grey60",
             alpha = 0.5) +
  tm_shape(study_area) +
  tm_fill(col = "grey95",
          alpha = 0.5) +
  # bus layer
  tm_shape(gtfs_bus %>%
             filter(scenario == "pt_wkday_morning") %>%
             mutate(headway_inv = (1/headway_secs) * 3600) %>%
             filter(headway_secs < 7200)) +
  tm_lines(col = "grey70",
           lwd = "headway_inv",
           scale = 5,
           palette = "-YlOrRd",
           style = "pretty",
           legend.col.show = FALSE,
           alpha = 1,
           title.lwd = "Buses/Hour",
           #legend.lwd.is.portrait = FALSE
  ) +
  # ---- clusters
  # lines
  tm_shape(cluster_dbscan_res_mode %>%
             filter(size > 7, size < 5000) %>%
             filter(commuters_sum > 200) %>%
             filter(cluster != 0) %>%
             mutate(cluster = as.factor(cluster)) %>%
             arrange(commute_frac)) +
  tm_lines(lwd = "commute_all",
           col = "commute_frac",
           scale = 5,
           breaks = c(0, 0.25, 0.5, 0.75, 1, Inf),
           palette = "RdYlGn", #Accent
           alpha = 0.4,
           title.col = "Fraction of Bus to Car users",
           #title.lwd = "No. of commuters",
           legend.col.show = FALSE,
           legend.lwd.show = FALSE,
           # remove "missing from legend
           showNA = FALSE) +
  tm_facets(by = "cluster",
            #by = "commuters_sum",
            free.coords = FALSE,
            nrow = rows,
            showNA = FALSE) +
  # poly
  tm_shape(cluster_dbscan_res_mode_poly) +
  tm_polygons(col = "commute_frac_cluster",
              palette = "RdYlGn", #Accent
              breaks = c(0, 0.25, 0.5, 0.75, 1, Inf),
              #style = "pretty",
              alpha = 0.2,
              title = "Fraction of Bus \nto Car users",
              # remove "missing from legend
              showNA = FALSE) +
  tm_facets(by = "cluster",
            #by = "commuters_sum",
            free.coords = FALSE,
            nrow = rows,
            showNA = FALSE) +
  tm_layout(fontfamily = 'Georgia',
            main.title = paste0("Clustered flows (OD", scenario, ")"),
            main.title.size = 1.1,
            main.title.color = "azure4",
            main.title.position = "left",
            #legend.outside = TRUE,
            #legend.outside.position = "bottom",
            #legend.stack = "horizontal",
            # remove panel headers
            panel.label.size = 1,
            panel.label.bg.color = NA,
            panel.labels = 1:length(unique(cluster_dbscan_res_mode_poly$cluster)),
            #panel.show = FALSE,
            frame = FALSE) -> map_cluster_results_bus_frac_grouped_gtfs_poly_lines

map_cluster_results_bus_frac_grouped_gtfs_poly_lines

tmap_save(tm = map_cluster_results_bus_frac_grouped_gtfs_poly_lines, filename = paste0(plots_path, "map_clusters_scenario_", scenario, "_", clustering, "_length_", distance_threshold, "_bus_frac_grouped_gtfs_poly_lines.png"), width = 12, dpi = 1080, asp = 0)



# ------ Map with clusters as polygons (convex_hull()) + lines in background - CROP TO AREAS NOT OVERLAPPING GTFS BUS


# GTFS: Approach 1 - st_union()
# get high frequency buses
gtfs_bus_freq <- gtfs_bus  %>%
  filter(scenario == "pt_wkday_morning") %>%
  mutate(headway_inv = (1/headway_secs) * 3600) %>%
  filter(headway_secs < 3600)

# convert to one geom in order to intersect by
gtfs_bus_freq1 <- gtfs_bus_freq %>%
  st_transform(3857) %>%
  st_buffer(1000) %>%
  st_union()

cluster_dbscan_res_mode_poly_filt <- st_difference(cluster_dbscan_res_mode_poly, gtfs_bus_freq1)



# convert from MULTIPOLYGON to POLYGON and retain largest geom only for each multipolygon
cluster_dbscan_res_mode_poly_filt <- cluster_dbscan_res_mode_poly_filt %>%
  st_cast("MULTIPOLYGON") %>%
  st_cast("POLYGON") %>%
  st_make_valid()

# retain largest poly in each cluster
cluster_dbscan_res_mode_poly_filt_max <- cluster_dbscan_res_mode_poly_filt %>%
  mutate(area = st_area(.)) %>%
  group_by(cluster) %>%
  filter(area == max(area))

# convex hull for aesthetic
cluster_dbscan_res_mode_poly_filt_max <- st_convex_hull(cluster_dbscan_res_mode_poly_filt_max)




tm_shape(study_area) +
  tm_borders(col = "grey60",
             alpha = 0.5) +
  tm_shape(study_area) +
  tm_fill(col = "grey95",
          alpha = 0.5) +
  # bus layer
  tm_shape(gtfs_bus %>%
             filter(scenario == "pt_wkday_morning") %>%
             mutate(headway_inv = (1/headway_secs) * 3600) %>%
             filter(headway_secs < 7200)) +
  tm_lines(col = "grey70",
           lwd = "headway_inv",
           scale = 5,
           palette = "-YlOrRd",
           style = "pretty",
           legend.col.show = FALSE,
           alpha = 1,
           title.lwd = "Buses/Hour",
           #legend.lwd.is.portrait = FALSE
  ) +
  # ---- clusters
  # lines
  tm_shape(cluster_dbscan_res_mode %>%
             filter(size > 7, size < 5000) %>%
             filter(commuters_sum > 200) %>%
             filter(cluster != 0) %>%
             filter(cluster %in% cluster_dbscan_res_mode_poly_filt_max$cluster) %>%
             mutate(cluster = as.factor(cluster)) %>%
             arrange(commute_frac)) +
  tm_lines(lwd = "commute_all",
           col = "commute_frac",
           scale = 5,
           breaks = c(0, 0.25, 0.5, 0.75, 1, Inf),
           palette = "RdYlGn", #Accent
           alpha = 0.4,
           title.col = "Fraction of Bus to Car users",
           #title.lwd = "No. of commuters",
           legend.col.show = FALSE,
           legend.lwd.show = FALSE,
           # remove "missing from legend
           showNA = FALSE) +
  tm_facets(by = "cluster",
            #by = "commuters_sum",
            free.coords = FALSE,
            nrow = rows,
            showNA = FALSE) +
  # poly border
  tm_shape(cluster_dbscan_res_mode_poly %>%
             filter(cluster %in% cluster_dbscan_res_mode_poly_filt_max$cluster)) +
  tm_borders(col = "black",
             lwd = 2,
             lty = "dashed") +
  tm_facets(by = "cluster",
            #by = "commuters_sum",
            free.coords = FALSE,
            nrow = rows,
            showNA = FALSE) +
  # poly fill
  tm_shape(cluster_dbscan_res_mode_poly_filt_max %>%
             st_buffer(1000)) +
  tm_polygons(col = "commute_frac_cluster",
              palette = "RdYlGn", #Accent
              breaks = c(0, 0.25, 0.5, 0.75, 1, Inf),
              #style = "pretty",
              alpha = 0.2,
              title = "Fraction of Bus \nto Car users \n(in potential \nDRT zone)",
              # remove "missing from legend
              showNA = FALSE) +
  tm_facets(by = "cluster",
            #by = "commuters_sum",
            free.coords = FALSE,
            nrow = rows,
            showNA = FALSE) +
  tm_layout(fontfamily = 'Georgia',
            main.title = paste0("Clustered flows (OD", scenario, ")"),
            main.title.size = 1.1,
            main.title.color = "azure4",
            main.title.position = "left",
            #legend.outside = TRUE,
            #legend.outside.position = "bottom",
            #legend.stack = "horizontal",
            # remove panel headers
            # panel.show = FALSE,
            panel.label.size = 1,
            panel.label.bg.color = NA,
            panel.labels = 1:length(unique(cluster_dbscan_res_mode_poly_filt_max$cluster)),
            frame = FALSE)  +
  # add a couple of legends
  tm_add_legend(type = "line", labels = 'Cluster', col = 'black', lwd = 2, lty = "dashed")-> map_cluster_results_bus_frac_grouped_gtfs_poly_lines_bus_diff

map_cluster_results_bus_frac_grouped_gtfs_poly_lines_bus_diff

tmap_save(tm = map_cluster_results_bus_frac_grouped_gtfs_poly_lines_bus_diff, filename = paste0(plots_path, "map_clusters_scenario_", scenario, "_", clustering, "_length_", distance_threshold, "_bus_frac_grouped_gtfs_poly_lines_bus_diff.png"), width = 12, dpi = 1080, asp = 0)




# --- Map with clusters as polygons (convex_hull()) + WITHOUT lines in background - CROP TO AREAS NOT OVERLAPPING GTFS BUS


tm_shape(study_area) +
  tm_borders(col = "grey60",
             alpha = 0.5) +
  tm_shape(study_area) +
  tm_fill(col = "grey95",
          alpha = 0.5) +
  # bus layer
  tm_shape(gtfs_bus %>%
             filter(scenario == "pt_wkday_morning") %>%
             mutate(headway_inv = (1/headway_secs) * 3600) %>%
             filter(headway_secs < 7200)) +
  tm_lines(col = "grey70",
           lwd = "headway_inv",
           scale = 5,
           palette = "-YlOrRd",
           style = "pretty",
           legend.col.show = FALSE,
           alpha = 1,
           title.lwd = "Buses/Hour",
           #legend.lwd.is.portrait = FALSE
  ) +
  # ---- clusters
  # poly border
  tm_shape(cluster_dbscan_res_mode_poly %>%
             filter(cluster %in% cluster_dbscan_res_mode_poly_filt_max$cluster)) +
  tm_borders(col = "black",
             lwd = 2,
             lty = "dashed") +
  tm_facets(by = "cluster",
            #by = "commuters_sum",
            free.coords = FALSE,
            nrow = rows,
            showNA = FALSE) +
  # poly fill
  tm_shape(cluster_dbscan_res_mode_poly_filt_max %>%
             st_buffer(1000)) +
  tm_polygons(col = "commute_frac_cluster",
              palette = "RdYlGn", #Accent
              breaks = c(0, 0.25, 0.5, 0.75, 1, Inf),
              #style = "pretty",
              alpha = 0.2,
              title = "Fraction of Bus \nto Car users \n(in potential \nDRT zone)",
              # remove "missing from legend
              showNA = FALSE) +
  tm_facets(by = "cluster",
            #by = "commuters_sum",
            free.coords = FALSE,
            nrow = rows,
            showNA = FALSE) +
  tm_layout(fontfamily = 'Georgia',
            main.title = paste0("Clustered flows (OD", scenario, ")"),
            main.title.size = 1.1,
            main.title.color = "azure4",
            main.title.position = "left",
            #legend.outside = TRUE,
            #legend.outside.position = "bottom",
            #legend.stack = "horizontal",
            # remove panel headers
            # panel.show = FALSE,
            panel.label.size = 1,
            panel.label.bg.color = NA,
            panel.labels = 1:length(unique(cluster_dbscan_res_mode_poly_filt_max$cluster)),
            frame = FALSE)  +
  # add a couple of legends
  tm_add_legend(type = "line", labels = 'Cluster', col = 'black', lwd = 2, lty = "dashed")  -> map_cluster_results_bus_frac_grouped_gtfs_poly_bus_diff

map_cluster_results_bus_frac_grouped_gtfs_poly_bus_diff

tmap_save(tm = map_cluster_results_bus_frac_grouped_gtfs_poly_bus_diff, filename = paste0(plots_path, "map_clusters_scenario_", scenario, "_", clustering, "_length_", distance_threshold, "_bus_frac_grouped_gtfs_poly_bus_diff.png"), width = 12, dpi = 1080, asp = 0)





# --- Map with clusters as polygons (convex_hull()) + lines in background - CROP TO AREAS NOT OVERLAPPING GTFS BUS


# GTFS: APPROACH 2a: concave_hull() AFTER st_union()
gtfs_bus_freq2 <- gtfs_bus_freq  %>%
  st_transform(3857) %>%
  #st_union() %>%
  geos::geos_concave_hull(ratio = 0.5, allow_holes = FALSE) %>%
  st_as_sf() %>%
  st_union() %>%
  st_set_crs(3857) %>%
  st_buffer(250)


# st_difference to get non overlapping geoms
cluster_dbscan_res_mode_poly_filt <- st_difference(cluster_dbscan_res_mode_poly, gtfs_bus_freq2)



# convert from MULTIPOLYGON to POLYGON and retain largest geom only for each multipolygon
cluster_dbscan_res_mode_poly_filt <- cluster_dbscan_res_mode_poly_filt %>%
  st_cast("MULTIPOLYGON") %>%
  st_cast("POLYGON") %>%
  st_make_valid()

# union geoms per cluster
cluster_dbscan_res_mode_poly_filt <- cluster_dbscan_res_mode_poly_filt %>%
  group_by(cluster, commuters_sum, commute_frac_cluster) %>%
  summarise(geometry = st_union(geometry)) %>%
  ungroup()



tm_shape(study_area) +
  tm_borders(col = "grey60",
             alpha = 0.5) +
  tm_shape(study_area) +
  tm_fill(col = "grey95",
          alpha = 0.5) +
  # bus layer
  tm_shape(gtfs_bus %>%
             filter(scenario == "pt_wkday_morning") %>%
             mutate(headway_inv = (1/headway_secs) * 3600) %>%
             filter(headway_secs < 7200)) +
  tm_lines(col = "grey70",
           lwd = "headway_inv",
           scale = 5,
           palette = "-YlOrRd",
           style = "pretty",
           legend.col.show = FALSE,
           alpha = 1,
           title.lwd = "Buses/Hour",
           #legend.lwd.is.portrait = FALSE
  ) +
  # ---- clusters
  # lines
  tm_shape(cluster_dbscan_res_mode %>%
             filter(size > 7, size < 5000) %>%
             filter(commuters_sum > 200) %>%
             filter(cluster != 0) %>%
             filter(cluster %in% cluster_dbscan_res_mode_poly_filt$cluster) %>%
             mutate(cluster = as.factor(cluster)) %>%
             arrange(commute_frac)) +
  tm_lines(lwd = "commute_all",
           col = "commute_frac",
           scale = 5,
           breaks = c(0, 0.25, 0.5, 0.75, 1, Inf),
           palette = "RdYlGn", #Accent
           alpha = 0.4,
          # title.col = "Fraction of Bus to Car users",
           #title.lwd = "No. of commuters",
           legend.col.show = FALSE,
           legend.lwd.show = FALSE,
           # remove "missing from legend
           showNA = FALSE) +
  tm_facets(by = "cluster",
            #by = "commuters_sum",
            free.coords = FALSE,
            nrow = rows,
            showNA = FALSE) +
  # poly border
  tm_shape(cluster_dbscan_res_mode_poly  %>%
             filter(cluster %in% cluster_dbscan_res_mode_poly_filt$cluster)) +
  tm_borders(col = "black",
             lwd = 2,
             lty = "dashed") +
  tm_facets(by = "cluster",
            #by = "commuters_sum",
            free.coords = FALSE,
            nrow = rows,
            showNA = FALSE) +
  # poly fill
  tm_shape(cluster_dbscan_res_mode_poly_filt %>%
             st_buffer(1000)) +
  tm_polygons(col = "commute_frac_cluster",
              palette = "RdYlGn", #Accent
              breaks = c(0, 0.25, 0.5, 0.75, 1, Inf),
              #style = "pretty",
              alpha = 0.2,
              title = "Fraction of Bus \nto Car users \n(in potential \nDRT zone)",
              # remove "missing from legend
              showNA = FALSE) +
  tm_facets(by = "cluster",
            #by = "commuters_sum",
            free.coords = FALSE,
            nrow = rows,
            showNA = FALSE) +
  tm_layout(fontfamily = 'Georgia',
            main.title = paste0("Clustered flows (OD", scenario, ")"),
            main.title.size = 1.1,
            main.title.color = "azure4",
            main.title.position = "left",
            #legend.outside = TRUE,
            #legend.outside.position = "bottom",
            #legend.stack = "horizontal",
            # remove panel headers
            # panel.show = FALSE,
            panel.label.size = 1,
            panel.label.bg.color = NA,
            panel.labels = 1:length(unique(cluster_dbscan_res_mode_poly_filt$cluster)),
            frame = FALSE) +
  # add a couple of legends
  tm_add_legend(type = "line", labels = 'Cluster', col = 'black', lwd = 2, lty = "dashed") -> map_cluster_results_bus_frac_grouped_gtfs_poly_lines_bus_diff_concave

map_cluster_results_bus_frac_grouped_gtfs_poly_lines_bus_diff_concave

tmap_save(tm = map_cluster_results_bus_frac_grouped_gtfs_poly_lines_bus_diff_concave, filename = paste0(plots_path, "map_clusters_scenario_", scenario, "_", clustering, "_length_", distance_threshold, "_bus_frac_grouped_gtfs_poly_lines_bus_diff_concave.png"), width = 12, dpi = 1080, asp = 0)




# --- Map with clusters as polygons (convex_hull()) + WITHOUT lines in background - CROP TO AREAS NOT OVERLAPPING GTFS BUS  ----- CONCAVE HULL


tm_shape(study_area) +
  tm_borders(col = "grey60",
             alpha = 0.5) +
  tm_shape(study_area) +
  tm_fill(col = "grey95",
          alpha = 0.5) +
  # bus layer
  tm_shape(gtfs_bus %>%
             filter(scenario == "pt_wkday_morning") %>%
             mutate(headway_inv = (1/headway_secs) * 3600) %>%
             filter(headway_secs < 7200)) +
  tm_lines(col = "grey70",
           lwd = "headway_inv",
           scale = 5,
           palette = "-YlOrRd",
           style = "pretty",
           legend.col.show = FALSE,
           alpha = 1,
           title.lwd = "Buses/Hour",
           #legend.lwd.is.portrait = FALSE
  ) +
  # ---- clusters
  # poly border
  tm_shape(cluster_dbscan_res_mode_poly  %>%
             filter(cluster %in% cluster_dbscan_res_mode_poly_filt$cluster)) +
  tm_borders(col = "black",
             lwd = 2,
             lty = "dashed") +
  tm_facets(by = "cluster",
            #by = "commuters_sum",
            free.coords = FALSE,
            nrow = rows,
            showNA = FALSE) +
  # poly fill
  tm_shape(cluster_dbscan_res_mode_poly_filt %>%
             st_buffer(1000)) +
  tm_polygons(col = "commute_frac_cluster",
              palette = "RdYlGn", #Accent
              breaks = c(0, 0.25, 0.5, 0.75, 1, Inf),
              #style = "pretty",
              alpha = 0.2,
              title = "Fraction of Bus \nto Car users \n(in potential \nDRT zone)",
              # remove "missing from legend
              showNA = FALSE) +
  tm_facets(by = "cluster",
            #by = "commuters_sum",
            free.coords = FALSE,
            nrow = rows,
            showNA = FALSE) +
  tm_layout(fontfamily = 'Georgia',
            main.title = paste0("Clustered flows (OD", scenario, ")"),
            main.title.size = 1.1,
            main.title.color = "azure4",
            main.title.position = "left",
            #legend.outside = TRUE,
            #legend.outside.position = "bottom",
            #legend.stack = "horizontal",
            # remove panel headers
            # panel.show = FALSE,
            panel.label.size = 1,
            panel.label.bg.color = NA,
            panel.labels = 1:length(unique(cluster_dbscan_res_mode_poly_filt$cluster)),
            frame = FALSE)  +
  # add a couple of legends
  tm_add_legend(type = "line", labels = 'Cluster', col = 'black', lwd = 2, lty = "dashed") -> map_cluster_results_bus_frac_grouped_gtfs_poly_bus_diff_concave

map_cluster_results_bus_frac_grouped_gtfs_poly_bus_diff_concave

tmap_save(tm = map_cluster_results_bus_frac_grouped_gtfs_poly_bus_diff_concave, filename = paste0(plots_path, "map_clusters_scenario_", scenario, "_", clustering, "_length_", distance_threshold, "_bus_frac_grouped_gtfs_poly_bus_diff_concave.png"), width = 12, dpi = 1080, asp = 0)









# GTFS: APPROACH 2b: concave_hull() AFTER st_union()
gtfs_bus_freq3 <- gtfs_bus_freq  %>%
  st_transform(3857) %>%
  st_buffer(100) %>%
  st_union() %>%
  geos::geos_concave_hull(ratio = 0.1, allow_holes = TRUE) %>%
  st_as_sf() %>%
  st_set_crs(3857) %>%
  st_make_valid()


# st_difference to get non overlapping geoms
cluster_dbscan_res_mode_poly_filt <- st_difference(cluster_dbscan_res_mode_poly, gtfs_bus_freq3)



# convert from MULTIPOLYGON to POLYGON and retain largest geom only for each multipolygon
cluster_dbscan_res_mode_poly_filt <- cluster_dbscan_res_mode_poly_filt %>%
  st_cast("MULTIPOLYGON") %>%
  st_cast("POLYGON") %>%
  st_make_valid()



tm_shape(study_area) +
  tm_borders(col = "grey60",
             alpha = 0.5) +
  tm_shape(study_area) +
  tm_fill(col = "grey95",
          alpha = 0.5) +
  # bus layer
  tm_shape(gtfs_bus %>%
             filter(scenario == "pt_wkday_morning") %>%
             mutate(headway_inv = (1/headway_secs) * 3600) %>%
             filter(headway_secs < 7200)) +
  tm_lines(col = "grey70",
           lwd = "headway_inv",
           scale = 5,
           palette = "-YlOrRd",
           style = "pretty",
           legend.col.show = FALSE,
           alpha = 1,
           title.lwd = "Buses/Hour",
           #legend.lwd.is.portrait = FALSE
  ) +
  # ---- clusters
  # lines
  tm_shape(cluster_dbscan_res_mode %>%
             filter(size > 7, size < 5000) %>%
             filter(commuters_sum > 200) %>%
             filter(cluster != 0) %>%
             filter(cluster %in% cluster_dbscan_res_mode_poly_filt$cluster) %>%
             mutate(cluster = as.factor(cluster)) %>%
             arrange(commute_frac)) +
  tm_lines(lwd = "commute_all",
           col = "commute_frac",
           scale = 5,
           breaks = c(0, 0.25, 0.5, 0.75, 1, Inf),
           palette = "RdYlGn", #Accent
           alpha = 0.4,
           title.col = "Fraction of Bus to Car users",
           #title.lwd = "No. of commuters",
           legend.col.show = FALSE,
           legend.lwd.show = FALSE,
           # remove "missing from legend
           showNA = FALSE) +
  tm_facets(by = "cluster",
            #by = "commuters_sum",
            free.coords = FALSE,
            nrow = rows,
            showNA = FALSE) +
  # poly border
  tm_shape(cluster_dbscan_res_mode_poly  %>%
             filter(cluster %in% cluster_dbscan_res_mode_poly_filt$cluster)) +
  tm_borders(col = "black",
             lwd = 2,
             lty = "dashed") +
  tm_facets(by = "cluster",
            #by = "commuters_sum",
            free.coords = FALSE,
            nrow = rows,
            showNA = FALSE) +
  # poly fill
  tm_shape(cluster_dbscan_res_mode_poly_filt %>%
             st_buffer(1000)) +
  tm_polygons(col = "commute_frac_cluster",
              palette = "RdYlGn", #Accent
              breaks = c(0, 0.25, 0.5, 0.75, 1, Inf),
              #style = "pretty",
              alpha = 0.2,
              title = "Fraction of Bus \nto Car users \n(in potential \nDRT zone)",
              # remove "missing from legend
              showNA = FALSE) +
  tm_facets(by = "cluster",
            #by = "commuters_sum",
            free.coords = FALSE,
            nrow = rows,
            showNA = FALSE) +
  tm_layout(fontfamily = 'Georgia',
            main.title = paste0("Clustered flows (OD", scenario, ")"),
            main.title.size = 1.1,
            main.title.color = "azure4",
            main.title.position = "left",
            #legend.outside = TRUE,
            #legend.outside.position = "bottom",
            #legend.stack = "horizontal",
            # remove panel headers
            # panel.show = FALSE,
            panel.label.size = 1,
            panel.label.bg.color = NA,
            panel.labels = 1:length(unique(cluster_dbscan_res_mode_poly_filt$cluster)),
            frame = FALSE)  +
  # add a couple of legends
  tm_add_legend(type = "line", labels = 'Cluster', col = 'black', lwd = 2, lty = "dashed") -> map_cluster_results_bus_frac_grouped_gtfs_poly_lines_bus_diff_concave2

map_cluster_results_bus_frac_grouped_gtfs_poly_lines_bus_diff_concave2

tmap_save(tm = map_cluster_results_bus_frac_grouped_gtfs_poly_lines_bus_diff_concave2, filename = paste0(plots_path, "map_clusters_scenario_", scenario, "_", clustering, "_length_", distance_threshold, "_bus_frac_grouped_gtfs_poly_lines_bus_diff_concave2.png"), width = 12, dpi = 1080, asp = 0)




# --- Map with clusters as polygons (convex_hull()) + WITHOUT lines in background - CROP TO AREAS NOT OVERLAPPING GTFS BUS  ----- CONCAVE HULL


tm_shape(study_area) +
  tm_borders(col = "grey60",
             alpha = 0.5) +
  tm_shape(study_area) +
  tm_fill(col = "grey95",
          alpha = 0.5) +
  # bus layer
  tm_shape(gtfs_bus %>%
             filter(scenario == "pt_wkday_morning") %>%
             mutate(headway_inv = (1/headway_secs) * 3600) %>%
             filter(headway_secs < 7200)) +
  tm_lines(col = "grey70",
           lwd = "headway_inv",
           scale = 5,
           palette = "-YlOrRd",
           style = "pretty",
           legend.col.show = FALSE,
           alpha = 1,
           title.lwd = "Buses/Hour",
           #legend.lwd.is.portrait = FALSE
  ) +
  # ---- clusters
  # poly border
  tm_shape(cluster_dbscan_res_mode_poly %>%
             filter(cluster %in% cluster_dbscan_res_mode_poly_filt$cluster)) +
  tm_borders(col = "black",
             lwd = 2,
             lty = "dashed") +
  tm_facets(by = "cluster",
            #by = "commuters_sum",
            free.coords = FALSE,
            nrow = rows,
            showNA = FALSE) +
  # poly fill
  tm_shape(cluster_dbscan_res_mode_poly_filt %>%
             filter(cluster %in% cluster_dbscan_res_mode_poly_filt$cluster) %>%
             st_buffer(1000)) +
  tm_polygons(col = "commute_frac_cluster",
              palette = "RdYlGn", #Accent
              breaks = c(0, 0.25, 0.5, 0.75, 1, Inf),
              #style = "pretty",
              alpha = 0.2,
              title = "Fraction of Bus \nto Car users \n(in potential \nDRT zone)",
              # remove "missing from legend
              showNA = FALSE) +
  tm_facets(by = "cluster",
            #by = "commuters_sum",
            free.coords = FALSE,
            nrow = rows,
            showNA = FALSE) +
  tm_layout(fontfamily = 'Georgia',
            main.title = paste0("Clustered flows (OD", scenario, ")"),
            main.title.size = 1.1,
            main.title.color = "azure4",
            main.title.position = "left",
            #legend.outside = TRUE,
            #legend.outside.position = "bottom",
            #legend.stack = "horizontal",
            # remove panel headers
            # panel.show = FALSE,
            panel.label.size = 1,
            panel.label.bg.color = NA,
            panel.labels = 1:length(unique(cluster_dbscan_res_mode_poly_filt$cluster)),
            frame = FALSE) +
  # add a couple of legends
  tm_add_legend(type = "line", labels = 'Cluster', col = 'black', lwd = 2, lty = "dashed") -> map_cluster_results_bus_frac_grouped_gtfs_poly_bus_diff_concave2

map_cluster_results_bus_frac_grouped_gtfs_poly_bus_diff_concave2

tmap_save(tm = map_cluster_results_bus_frac_grouped_gtfs_poly_bus_diff_concave2, filename = paste0(plots_path, "map_clusters_scenario_", scenario, "_", clustering, "_length_", distance_threshold, "_bus_frac_grouped_gtfs_poly_bus_diff_concave2.png"), width = 12, dpi = 1080, asp = 0)





########## - ADD RURAL URBAN AS BASE MAP

# --- 2011 map and urban rural classification
basemap <- st_read("data/external/oa_england_2011/OA_2011_EW_BGC_V2.shp") %>%
  st_transform(st_crs(study_area)) %>%
  st_make_valid()

urban_rural_csv <- read_csv("data/external/census_2011_rural_urban.csv")


# add numeric columns for coloring
urban_rural_csv <- urban_rural_csv %>%
  mutate(RUC11CD_NM = case_when(RUC11 == "Urban major conurbation" ~ 10,
                                RUC11 == "Urban minor conurbation" ~ 9,
                                RUC11 == "Urban city and town" ~ 8,
                                RUC11 == "Urban city and town in a sparse setting" ~ 7,
                                RUC11 == "Rural town and fringe"  ~ 6,
                                RUC11 == "Rural town and fringe in a sparse setting" ~ 5,
                                RUC11 == "Rural village" ~ 4,
                                RUC11 == "Rural village in a sparse setting" ~ 3,
                                RUC11 == "Rural hamlets and isolated dwellings" ~ 2,
                                RUC11 == "Rural hamlets and isolated dwellings in a sparse setting" ~ 1)) %>%
  arrange(RUC11CD_NM) %>%
  # turn column to factor
  mutate(RUC11CD_NM_FCT = as.factor(RUC11CD_NM))
# join
basemap_urban_rural <- basemap %>%
  left_join(urban_rural_csv, by = "OA11CD")


# --- filter to study area
basemap_urban_rural <- basemap_urban_rural %>%
  st_filter(st_union(study_area) %>%
              st_make_valid(),
            .predicate = st_intersects)






tm_shape(basemap_urban_rural) +
  tm_fill(col = "RUC11CD_NM",
          palette = "Greys",
          alpha = 0.5,
          title = "Level of \nUrbanisation") +
  # bus layer
  tm_shape(gtfs_bus %>%
             filter(scenario == "pt_wkday_morning") %>%
             mutate(headway_inv = (1/headway_secs) * 3600) %>%
             filter(headway_secs < 7200)) +
  tm_lines(col = "darkred",
           lwd = "headway_inv",
           scale = 5,
           palette = "-YlOrRd",
           style = "pretty",
           legend.col.show = FALSE,
           alpha = 0.1,
           title.lwd = "Buses/Hour",
           #legend.lwd.is.portrait = FALSE
  ) +
  # ---- clusters
  # poly border
tm_shape(cluster_dbscan_res_mode_poly %>%
             filter(cluster %in% cluster_dbscan_res_mode_poly_filt$cluster)) +
  tm_borders(col = "black",
             lwd = 3,
             lty = "dashed") +
  tm_facets(by = "cluster",
            #by = "commuters_sum",
            free.coords = FALSE,
            nrow = rows,
            showNA = FALSE) +
  # poly fill
# tm_shape(cluster_dbscan_res_mode_poly_filt_max %>%
#              filter(cluster %in% cluster_dbscan_res_mode_poly_filt$cluster) %>%
tm_shape(cluster_dbscan_res_mode_poly_filt %>%
             st_buffer(1000)) +
  tm_borders(col = "darkgreen",
             lwd = 2) +
  tm_facets(by = "cluster",
            #by = "commuters_sum",
            free.coords = FALSE,
            nrow = rows,
            showNA = FALSE) +
  tm_layout(fontfamily = 'Georgia',
            main.title = paste0("Clustered flows (OD", scenario, ")"),
            main.title.size = 1.1,
            main.title.color = "azure4",
            main.title.position = "left",
            #legend.outside = TRUE,
            #legend.outside.position = "bottom",
            #legend.stack = "horizontal",
            # remove panel headers
            # panel.show = FALSE,
            panel.label.size = 1,
            panel.label.bg.color = NA,
            panel.labels = 1:length(unique(cluster_dbscan_res_mode_poly_filt$cluster)),
            frame = FALSE)  +
  # add a couple of legends
  tm_add_legend(type = "line", labels = 'Cluster', col = 'black', lwd = 2, lty = "dashed") +
  tm_add_legend(type = "line", labels = 'Potential DRT\nservice area', col = 'darkgreen', lwd = 2) -> map_cluster_results_bus_frac_grouped_gtfs_poly_bus_diff_concave_urbanisation

map_cluster_results_bus_frac_grouped_gtfs_poly_bus_diff_concave_urbanisation

tmap_save(tm = map_cluster_results_bus_frac_grouped_gtfs_poly_bus_diff_concave_urbanisation, filename = paste0(plots_path, "map_clusters_scenario_", scenario, "_", clustering, "_length_", distance_threshold, "_bus_frac_grouped_gtfs_poly_bus_diff_concave_urbanisation.png"), width = 12, dpi = 1080, asp = 0)



