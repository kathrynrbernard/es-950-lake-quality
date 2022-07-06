# load packages
library(tidyverse)

### parcel data ###
# read in full parcel dataset - has data for all 3 lakes
parcel_data_full <- read.csv("data/950_parcel_habitat.csv")

# see what lakes we have
unique(parcel_data_full["LAKE_NAME"])

# separate big portage lake
portage_parcel_data <- parcel_data_full %>% filter(LAKE_NAME == "Big Portage Lake")
portage_parcel_data

# separate papoose lake
papoose_parcel_data <- parcel_data_full %>% filter(LAKE_NAME == "Papoose Lake")

# separate big arbor vitae lake
arbor_parcel_data <- parcel_data_full %>% filter(LAKE_NAME == "Big Arbor Vitae Lake")

# using papoose for now to prototype
# see what variables we have
colnames(papoose_parcel_data)

# [1] "PARCELID"            "CANOPY_PCT"          "SHRUB_PRESENCE"      "HERB_PRESENCE"       "SHRUB_HERB_PCT"     
# [6] "IMPERVIOUS_PCT"      "MANI_LAWN_PCT"       "AG_PCT"              "OTHER_PCT"           "BUILDINGS_CNT"      
# [11] "BOAT_SHORE_CNT"      "FIRE_PIT_CNT"        "OTHER_STRUCTURE_CNT" "POINT_SOURCE_PRES"   "CHANNEL_FLOW_PRES"  
# [16] "STAIR_LAKE_PRES"     "LAWN_LAKE_PRES"      "SAND_DEP_PRES"       "OTHER_RUNOFF_PRES"   "VERTICAL_WALL_LEN"  
# [21] "RIPRAP_LEN"          "EROSION_CNTRL_LEN"   "ART_BEACH_LEN"       "GREAT_ERO_LEN"       "LESS_ERO_LEN"       
# [26] "PIERS_CNT"           "BOAT_LIFT_CNT"       "SWIM_RAFT_CNT"       "BOATHOUSE_CNT"       "MARINAS_CNT"        
# [31] "STRUCTURE_OTHER_CNT" "EMERGENT_VEG_PRES"   "FLOATING_VEG_PRES"   "PLANT_REMOVAL_PRES"  "FLOAT_EMERG_PRES"   
# [36] "EXPOSED_CANOPY_PRES" "EXPOSED_SHRUB_PRES"  "EXPOSED_HERB_PRES"   "EXPOSED_MOW_PRES"    "EXPOSED_TILL_PRES"  
# [41] "NOTES"               "PLACENAME"           "COUNTY"              "LAKE_NAME"           "WBIC"           

### woody habitat data ###
woody_data_full <- read.csv("data/950_woody_habitat.csv")

# see what lakes we have
unique(woody_data_full["LAKE_NAME"])

# separate big portage lake
portage_woody_data <- woody_data_full %>% filter(LAKE_NAME == "Big Portage Lake")
portage_woody_data

# separate papoose lake
papoose_woody_data <- woody_data_full %>% filter(LAKE_NAME == "Papoose Lake")

# separate big arbor vitae lake
arbor_woody_data <- woody_data_full %>% filter(LAKE_NAME == "Big Arbor Vitae Lake")

# using papoose for now to prototype
# see what variables we have
colnames(papoose_woody_data)

# [1] "WBIC"        "LAKE_NAME"   "UNIQUE_ID"   "BRANCH"      "TOUCH_SHORE" "IN_WATER"    "LATITUDE"    "LONGITUDE"  


# note from the protocol:
# Further lake-wide statistics can be generated from this such as:
# - percent cover of impervious surface, mowed lawn, or plants in the Riparian Buffer Zone
# - number of parcels with erosion concerns
# - total length of modified banks
# - density of human structures (piers, buildings, etc.)
# - general distribution of floating and emergent aquatic plants
# - density of coarse woody habitat.
# For each metric, a threshold identifying healthy habitat will be developed.



