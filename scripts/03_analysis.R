## This script reads in the cleaned data files (generated in 02_data_cleaning.R) and does additional processing
## Run this script first


# Setup -------------------------------------------------------------------
# Load packages
library(tidyverse)
library(gridExtra)

# Read in data
parcel_data <- read.csv("data/950_parcel_habitat_clean.csv")
woody_data <- read.csv("data/950_woody_habitat_clean.csv")

# Separate our group's lake - Big Arbor Vitae Lake
arbor_parcel <-
  parcel_data %>% filter(LAKE_NAME == "Big Arbor Vitae Lake")
arbor_woody <- woody_data %>% filter(LAKE_NAME == "Big Arbor Vitae")



# Development Status ------------------------------------------------------
# manually identified these parcels by looking at the DNR lake viewer
nondeveloped_ids <-
  c(
    "2-2562-02",
    "2-2562-01",
    "2-2562",
    "2-2556",
    "2-2553-06",
    "2-2565",
    "2-2566",
    "2-2582",
    "2-2597",
    "2-2648"
  )

arbor_parcel$DEVELOPED <-
  !arbor_parcel$PARCELID %in% nondeveloped_ids


# Land Processing ----------------------------------------------------------
# define land structures
land_structures <- select(arbor_parcel,c(BUILDINGS_CNT, BOAT_SHORE_CNT, FIRE_PIT_CNT, OTHER_STRUCTURE_CNT))

# add column to dataset for total number of land structures
arbor_parcel <-
  arbor_parcel %>% mutate(STRUCTURES_TOTAL_LAND = rowSums(land_structures[,]))

# Water Processing --------------------------------------------------------
# define aquatic vegetation
aquatic_veg <-select(arbor_parcel,c(PARCELID,EMERGENT_VEG_PRES,FLOATING_VEG_PRES,FLOAT_EMERG_PRES))

# define aquatic structures
aquatic_structures <-select(arbor_parcel,c(PARCELID,PIERS_CNT,BOAT_LIFT_CNT,SWIM_RAFT_CNT,BOATHOUSE_CNT,MARINAS_CNT,STRUCTURE_OTHER_CNT))

# add column to overall dataset for total number of structures in the water
arbor_parcel <-
  arbor_parcel %>% mutate(STRUCTURES_TOTAL_WATER = rowSums(aquatic_structures[, -(1)]))

# group structures
arbor_parcel <- arbor_parcel %>% mutate(STRUCTURES_CLASS =
                                          case_when(STRUCTURES_TOTAL_WATER <= 1 ~ "Low", 
                                                    STRUCTURES_TOTAL_WATER > 1 & STRUCTURES_TOTAL_WATER <= 5 ~ "Medium",
                                                    STRUCTURES_TOTAL_WATER > 5 ~ "High")
)

# create new column for whether any aquatic veg is present (floating or emergent)
arbor_parcel <- arbor_parcel %>% mutate(FLOAT_OR_EMERG_PRES = case_when(EMERGENT_VEG_PRES==TRUE | FLOATING_VEG_PRES==TRUE ~ TRUE,
                                                                        EMERGENT_VEG_PRES==FALSE & FLOATING_VEG_PRES==FALSE ~ FALSE))


# Erosion Processing ------------------------------------------------------
# define erosion control structures
erosion_control <-select(arbor_parcel,c(PARCELID, VERTICAL_WALL_LEN, RIPRAP_LEN, EROSION_CNTRL_LEN))

# define erosion risk factors
erosion_risks <- select(arbor_parcel,c(PARCELID,POINT_SOURCE_PRES,CHANNEL_FLOW_PRES,STAIR_LAKE_PRES,LAWN_LAKE_PRES,SAND_DEP_PRES,OTHER_RUNOFF_PRES))

# erosion measurements
erosion_meas <- select(arbor_parcel, c(PARCELID, GREAT_ERO_LEN, LESS_ERO_LEN))

# add column for erosion risk presence
erosion_risks <- erosion_risks %>% replace(is.na(.), 0) %>%
  mutate(EROSION_RISK_SUM = rowSums(across(where(is.numeric))),
         EROSION_RISK_PRES=EROSION_RISK_SUM > 0)

# add column for erosion control presence
erosion_control <- erosion_control %>% replace(is.na(.), 0) %>%
  mutate(EROSION_CTRL_SUM = rowSums(across(where(is.numeric))),
         EROSION_CTRL_PRES=EROSION_CTRL_SUM > 0)

# Parcel Drilldown --------------------------------------------------------
# pick three parcels of interest to explore in depth
## 2-2686-16
# land: lots of structures, lots of manicured lawn, medium canopy, few shrubs/herbs
# water: lots of structures, some veg
# erosion: lots of control structures, no concerns, no actual erosion
parcel_one <- arbor_parcel[arbor_parcel$PARCELID=="2-2686-16",]

# 2-2562-02
# land: lots of canopy and shrub/herb, no structures
# water: no structures, no veg
# erosion: a lot of erosion (from the notes), channel flow present but no great-ero-len documented
parcel_two <- arbor_parcel[arbor_parcel$PARCELID=="2-2562-02",]

#2-2649
# land: lots of canopy, lots of shrub/herb, no structures
# water: has veg, has aquatic structures
# erosion: no concerns, some controls
parcel_three <- arbor_parcel[arbor_parcel$PARCELID=="2-2649",]

# create a small dataframe with just parcels of interest
parcel_dd <- arbor_parcel %>% filter(PARCELID==parcel_one$PARCELID | PARCELID==parcel_two$PARCELID | PARCELID==parcel_three$PARCELID)


# Parcel Dataframes -------------------------------------------------------

# Land
parcel_veg_struc <- select(parcel_dd,c(PARCELID,SHRUB_HERB_PCT,IMPERVIOUS_PCT,MANI_LAWN_PCT,AG_PCT,OTHER_PCT,
                                       STRUCTURES_TOTAL_LAND))
parcel_pivot <- parcel_veg_struc %>% pivot_longer(!c(PARCELID,STRUCTURES_TOTAL_LAND), names_to="Vegetation", values_to="Pct")

# Erosion
parcel_control_pivot <- select(arbor_parcel, c(PARCELID, VERTICAL_WALL_LEN, RIPRAP_LEN, EROSION_CNTRL_LEN)) %>% 
  filter(PARCELID %in% parcel_dd$PARCELID) %>% 
  pivot_longer(!PARCELID, names_to="Control", values_to="Length")
parcel_risk_pivot <- select(
  arbor_parcel,
  c(PARCELID,POINT_SOURCE_PRES,CHANNEL_FLOW_PRES,STAIR_LAKE_PRES,LAWN_LAKE_PRES,SAND_DEP_PRES,OTHER_RUNOFF_PRES)) %>% 
  filter(PARCELID %in% parcel_dd$PARCELID) %>% 
  pivot_longer(!PARCELID, names_to="Risk", values_to="Presence")

