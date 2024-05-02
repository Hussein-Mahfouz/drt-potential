library(tidyverse)
library(sf)
library(tmap)


######### BOUNDARY of Leeds

study_area <- st_read("data/interim/study_area_boundary.geojson")

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


basemap_urban_rural <- st_intersection(basemap_urban_rural, st_union(study_area))



tm_shape(basemap_urban_rural) +
  tm_fill(col = "RUC11CD_NM_FCT",
          palette = "Greys",
          alpha = 1,
          legend.is.portrait = TRUE,
          title = "Level of \nUrbanisation") +
tm_shape(study_area %>%
           st_union()) +
  tm_borders(col = "black") +
  tm_layout(fontfamily = 'Georgia',
            main.title = "Leeds City Region - Rural Urban Classification",
            main.title.size = 1.1,
            main.title.color = "azure4",
            main.title.position = "left",
            #legend.outside = TRUE,
            #legend.outside.position = "bottom",
            frame = FALSE) +
  tm_credits("Urban Rural Classification \n(DEFRA, 2011)") -> map_rural_urban

map_rural_urban

tmap_save(tm = map_rural_urban, filename = "data/processed/plots/diagrams/rural_urban.png", width = 12, dpi = 1080, asp = 0)




