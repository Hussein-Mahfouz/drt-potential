library(tidyverse)
library(sf)
library(tmap)


# ---------- Read in data

# --- 1. CPC data template - 2011 MSOA layer
cpc_zones <- read_csv("data/external/cpc/ZoningTemplate.csv")

# --- 2. 2011 MSOA boundaries (geo)
msoa_shp_2011 <- st_read("data/external/msoa_england_2011/infuse_msoa_lyr_2011_clipped.shp")

# --- 3. Commuting matrices (2021)
msoa_commute_2021 <- read_csv("data/raw/travel_demand/od_census_2021/ODWP15EW_MSOA_v1.zip")

# simplify
msoa_commute_2021 <- msoa_commute_2021 %>%
  rename(msoa_origin = "Middle layer Super Output Areas code", msoa_destination = "MSOA of workplace code") %>%
  # aggregate: we don't care about gender and age breakdown
  group_by(msoa_origin, msoa_destination) %>%
  summarise(commuters = sum(Count)) %>%
  ungroup()

# add 2011 msoa names
msoa_2011_2021 <- read_csv("data/external/cpc/MSOA_2011_2021.csv") %>%
  select(MSOA11CD, MSOA21CD, LAD22CD)

msoa_commute_2021_2011 <- msoa_commute_2021 %>%
  # add 2011 msoa codes (origin)
  left_join(msoa_2011_2021 %>%
              rename_with(~ paste0(.x, "_origin")),
            by = c("msoa_origin" = "MSOA21CD_origin")) %>%
  # add 2011 msoa codes (destination)
  left_join(msoa_2011_2021 %>%
              rename_with(~ paste0(.x, "_destination")),
            by = c("msoa_destination" = "MSOA21CD_destination"))




# ---------- Join the geo data (2) onto (1)
cpc_zones_geo <- cpc_zones %>%
  left_join(msoa_shp_2011  %>%
              select(geo_code),
            by = c("msoa" = "geo_code")) %>%
  st_as_sf()


cps_zones_yh_region <- cpc_zones_geo %>%
  filter(region_name == "Yorkshire and The Humber")

cps_zones_wy_county <- cpc_zones_geo %>%
  filter(county_name == "West Yorkshire")



# ---------- Commuting matrices

# which OD pairs have an origin or destination inside our study area
msoa_commute_2021_2011_region = msoa_commute_2021_2011 %>%
  filter(MSOA11CD_origin %in% cps_zones_wy_county$msoa |
           MSOA11CD_destination %in% cps_zones_wy_county$msoa)


# ----------

msoas_of_interest <- cpc_zones_geo %>%
  filter(msoa %in% msoa_commute_2021_2011_region$MSOA11CD_origin |
           msoa %in% msoa_commute_2021_2011_region$MSOA11CD_destination)



# label msoas in west yorkshire

msoas_of_interest %>%
  group_by(lad) %>%
  mutate(zone_id = n()) %>%
  mutate(zone_id = as.character(zone_id)) %>%
  ungroup() -> x


tm_shape(x)+
  tm_fill(col = "zone_id",
          palette = "Dark2")


# ------------------------- APPROACH (msoa -> lad -> county)

# ----- study area
study_area_county = "E11000006"
study_area_region = "E12000003"

# 1.LEVEL = MSOA

# --- west yorkshire
cpc_zones_wy = cpc_zones_geo %>%
  filter(county == study_area_county) %>%
  mutate(id_col = row_number(),
         zone_type = 1) # internal to the model


# 2.LEVEL = LAD

# --- yorkshire humber region
cpc_zones_yh_region = cpc_zones_geo %>%
  filter(region == study_area_region) %>%
  filter(! msoa %in% cpc_zones_wy$msoa) %>%
  group_by(lad) %>%
  mutate(id_col = cur_group_id() + max(cpc_zones_wy$id_col)) %>%
  ungroup() %>%
  mutate(zone_type = 0) # external to the model


# --- greater manchester and lancashire and derbyshire
cpc_zones_other_neighbors =  cpc_zones_geo %>%
  filter(county_name %in% c("Greater Manchester", "Derbyshire", "Lancashire")) %>%
  #filter(! msoa %in% cpc_zones_wy$msoa) %>%
  group_by(lad) %>%
  mutate(id_col = cur_group_id() + max(cpc_zones_yh_region$id_col)) %>%
  ungroup() %>%
  mutate(zone_type = 0) # external to the model


# Combine (LEVEL 1) and (LEVEL 2)

cpc_zones_study_plus = cpc_zones_wy %>%
  bind_rows(cpc_zones_yh_region) %>%
  bind_rows(cpc_zones_other_neighbors)

# 3.LEVEL = COUNTY (OR REGION - check which one I used)

# ----- Other areas

cpc_zones_other = cpc_zones_geo %>%
  filter(country_name == "England") %>%
  filter(! msoa %in% cpc_zones_study_plus$msoa) %>%
  group_by(region) %>%
  mutate(id_col = cur_group_id() + max(cpc_zones_study_plus$id_col)) %>%
  ungroup() %>%
  mutate(zone_type = 0) # external to the model


# 4.LEVEL = COUNTRY

# ----- Scotland and Wales

cpc_zones_countries = cpc_zones_geo %>%
  filter(country_name != "England") %>%
  group_by(country_name) %>%
  mutate(id_col = cur_group_id() + max(cpc_zones_other$id_col)) %>%
  ungroup() %>%
  mutate(zone_type = 0) # external to the model


# bring all together
cpc_zones_all = cpc_zones_study_plus %>%
  bind_rows(cpc_zones_other) %>%
  bind_rows(cpc_zones_countries)


cpc_zones_all <- cpc_zones_all %>%
  mutate(zone_id = id_col)


cpc_zones_all_df = cpc_zones_all %>%
  select(msoa:msoa_name) %>%
  st_drop_geometry()

# Save
write_csv(cpc_zones_all_df, "data/external/cpc/ZoningTemplateFilled_lad_region.csv")


cpc_zones_all %>% filter(zone_id %in% c(347, 348)) -> x

tm_shape(x)+
  tm_fill(col = "zone_id",
          palette = "Dark2",
          legend.show = FALSE)


# ------------------------- APPROACH (msoa -> lad) + scot wales as countries (could not use them as LADs due to an error on cpc website) ---------- #
# ------- THIS IS THE LAYER I SUBMITTED ------- #

# ----- study area
study_area_county = "E11000006"

# 1.LEVEL = MSOA

# --- west yorkshire
cpc_zones_wy = cpc_zones_geo %>%
  filter(county == study_area_county) %>%
  mutate(id_col = row_number(),
         zone_type = 1) # internal to the model


# 2.LEVEL = LAD

# --- yorkshire humber region
cpc_zones_other = cpc_zones_geo %>%
  filter(country_name == "England") %>%
  filter(! msoa %in% cpc_zones_wy$msoa) %>%
  group_by(lad) %>%
  mutate(id_col = cur_group_id() + max(cpc_zones_wy$id_col)) %>%
  ungroup() %>%
  mutate(zone_type = 0) # external to the model


# ----- Scotland and Wales

cpc_zones_scot_wales = cpc_zones_geo %>%
  filter(country_name != "England") %>%
  group_by(country_name) %>%
  mutate(id_col = cur_group_id() + max(cpc_zones_other$id_col)) %>%
  ungroup() %>%
  mutate(zone_type = 0) # external to the model



# Combine

cpc_zones_all = cpc_zones_wy %>%
  bind_rows(cpc_zones_other) %>%
  bind_rows(cpc_zones_scot_wales) %>%
  mutate(zone_id = id_col)

cpc_zones_all_df = cpc_zones_all %>%
  select(msoa:msoa_name) %>%
  st_drop_geometry()

# Save
write_csv(cpc_zones_all_df, "data/external/cpc/ZoningTemplateFilled_lad_scot_wales.csv")


cpc_zones_all %>% filter(zone_id %in% c(629, 643, 652)) -> x

tm_shape(x)+
  tm_fill(col = "lad",
          palette = "Dark2")












tm_shape(cpc_zones_all %>%
           filter(zone_id == 375))+
  tm_fill(col = "zone_id",
          palette = "Dark2",
          legend.show = FALSE)

cpc_zones_all %>%
  filter(zone_id == 375) -> x
# PLOTS

tm_shape(cpc_zones_all)+
  tm_fill(col = "zone_id",
          palette = "Dark2",
          legend.show = FALSE)


tm_shape(cpc_zones_all %>%
           #filter(county_name == "West Yorkshire") %>%
           filter(region_name == "Yorkshire and The Humber") %>%
           mutate(id_col = as.character(id_col)))+
  tm_fill(col = "lad",
          palette = "Dark2")


tm_shape(cpc_zones_all %>%
           filter(county_name == "Inner London" | county_name == "Outer London")) +
  tm_fill(col = "lad",
          palette = "Dark2")


unique(cpc_zones_all2$zone_id)
