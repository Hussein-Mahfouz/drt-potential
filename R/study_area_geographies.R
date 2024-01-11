
# --- edit the sf to match the requested geographic resolution. The original layer is at the OA level, but we may want LSOA or MSOA

study_area_geographies <- function(study_area,
                                   geography){
  # get a resolved geometry and retain the first value in each group (the values are the same for all elements in the same group - only the OA is different)
  if(geography == "MSOA"){
    message("converting from OA to MSOA")
    study_area <- study_area %>%
      group_by(MSOA21CD) %>%
      summarise(across(c(OBJECTID, where(is.character)), ~first(.x))) %>%
      select(-c(LSOA21CD, OA21CD)) %>%
      ungroup()

  } else if(geography == "LSOA"){
    message("converting from OA to LSOA")
    study_area <- study_area %>%
      group_by(LSOA21CD) %>%
      summarise(across(c(OBJECTID, where(is.character)), ~first(.x))) %>%
      select(-OA21CD) %>%
      ungroup()

  } else if(geography == "OA"){
    message("keeping original OA boundaries")
  }
  return(study_area)
}
