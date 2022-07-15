## Setup
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


# Erosion control structures vs. risks to erosion
erosion_control <-
  select(arbor_parcel,
         c(PARCELID, VERTICAL_WALL_LEN, RIPRAP_LEN, EROSION_CNTRL_LEN))
erosion_risks <- select(
  arbor_parcel,
  c(
    PARCELID,
    POINT_SOURCE_PRES,
    CHANNEL_FLOW_PRES,
    STAIR_LAKE_PRES,
    LAWN_LAKE_PRES,
    SAND_DEP_PRES,
    OTHER_RUNOFF_PRES
  )
)

erosion_risks %>% ggplot(aes(x = OTHER_RUNOFF_PRES)) +
  geom_histogram(binwidth = 1)

# Most parcels do not have any runoff concerns; a few have POINT_SOURCE and a few more have CHANNEL_FLOW but none have the others

erosion_meas <-
  select(arbor_parcel, c(PARCELID, GREAT_ERO_LEN, LESS_ERO_LEN))
# more parcels have GREAT_ERO_LEN than have concerns documented - could still include this

erosion_control %>% summarize(
  mean_wall_len = mean(VERTICAL_WALL_LEN),
  min_wall_len = min(VERTICAL_WALL_LEN),
  max_wall_len = max(VERTICAL_WALL_LEN),
  mean_riprap_len = mean(RIPRAP_LEN),
  min_riprap_len = min(RIPRAP_LEN),
  max_riprap_len = max(RIPRAP_LEN),
  mean_ctrl_len = mean(EROSION_CNTRL_LEN),
  min_ctrl_len = min(EROSION_CNTRL_LEN),
  max_ctrl_len = max(EROSION_CNTRL_LEN)
)

# a decent number of people have riprap
erosion_control %>% ggplot(aes(x = RIPRAP_LEN)) +
  geom_histogram(binwidth = 5)
