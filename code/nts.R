### working with data from the National Travel Survey ###

library(tidyverse)
library(haven)

# path with all files
nts_path <- "data/external/UKDA-5340-spss/spss/spss25/"

# common suffix + extension for all tables
suffix <- "_eul_2002-2021.sav"

# ---- Load in datasets

# individual
individual <- haven::read_sav(paste0(nts_path, "individual", suffix))

# trip
trips <- haven::read_sav(paste0(nts_path, "trip", suffix))

