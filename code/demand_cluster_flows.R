library(tidyverse)
library(sf)

# Get od demand data (should be by mode)




# ------------ "Spatial Cluster Detection in Spatial Flow Data" ---------- #


# ----- STEP 1: Get distance between flows

# Option 1: Flow distance

# check: st_distance(by_element = FALSE)


# Option 2: Flow Dissimilarity


# ----- STEP 2: Hotspot detection

# may have to compute Ripley's K from scratch using the equation (as we have the distance, not the actual points)
# see wikipedia for eqn

# alternatively, spatstat::kest() is a function for Ripley's K (unclear if useful)
# https://andrewmaclachlan.github.io/CASA0005repo/detecting-spatial-patterns.html

