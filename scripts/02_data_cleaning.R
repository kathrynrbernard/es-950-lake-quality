# Author: Kathryn Bernard
# Purpose: This script does preliminary cleaning and pre-processing of the DNR's datasets
# Inputs: Two files - the parcel habitat data and the woody habitat data
# Outputs: Two files - a clean version of each input file


# Setup -------------------------------------------------------------------
# Load necessary packages
library(tidyverse)


# Read in Parcel Data ------------------------------------------------------------
parcel_data_raw <- read.csv("data/950_parcel_habitat.csv")


# Preliminary Investigation -----------------------------------------------
# Variables
colnames(parcel_data_raw)

# Datatypes
str(parcel_data_raw)

# Some datatypes need to be changed.
# SHRUB_PRESENCE and HERB_PRESENCE are stored as "Y" and "N" instead of 0 and 1
# They should be converted to logical types
parcel_data_raw$SHRUB_PRESENCE <- parcel_data_raw$SHRUB_PRESENCE=="Y"
parcel_data_raw$HERB_PRESENCE <- parcel_data_raw$HERB_PRESENCE=="Y"

# The other Boolean columns are mostly stored as integers 
# They should also be converted to logical types
to_logical <- c("EMERGENT_VEG_PRES", "FLOATING_VEG_PRES", "PLANT_REMOVAL_PRES", "FLOAT_EMERG_PRES", "EXPOSED_CANOPY_PRES",
             "EXPOSED_SHRUB_PRES", "EXPOSED_HERB_PRES", "EXPOSED_MOW_PRES", "EXPOSED_TILL_PRES")

parcel_data_raw <- parcel_data_raw %>% mutate(across(.cols=all_of(to_logical), .fns=as.logical))

# Check the structure again to make sure everything was converted correctly
str(parcel_data_raw) 


# Output ------------------------------------------------------------------
# Write out a clean file with the new data types
write.csv(parcel_data_raw, "data/950_parcel_habitat_clean.csv")



# Read in Woody Habitat Data ----------------------------------------------
woody_data_raw <- read.csv("data/950_woody_habitat.csv")


# Preliminary Investigation -----------------------------------------------
# Variables
colnames(woody_data_raw)

# Datatypes
str(woody_data_raw) # All these datatypes look correct - no conversions are needed

# Parcel mapping
## TODO - map the log data to parcels


# Output ------------------------------------------------------------------
# Write out a clean file for naming consistency's sake
write.csv(woody_data_raw, "data/950_woody_habitat_clean.csv")

