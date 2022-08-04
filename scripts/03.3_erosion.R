
# Setup -------------------------------------------------------------------
# Load packages
library(tidyverse)
library(gridExtra)
library(dplyr)
library(RColorBrewer)


# Preprocessing -----------------------------------------------------------
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


# Length of Riprap Across Parcels Plot ------------------------------------
erosion_control %>% ggplot(aes(x = RIPRAP_LEN)) +
  geom_histogram(binwidth = 5) +
ggtitle("Amount of Riprap in Parcels") +
  labs(x = "Riprap Length", y = "Number of Parcels")

# additional colors
erosion_control %>% ggplot(aes(x = RIPRAP_LEN)) +
  geom_histogram(binwidth = 5, fill = 'chocolate4')+
  ggtitle("Length of Riprap across Parcels") +
  labs(x = "Riprap Length (Feet)", y = "Count") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5,size=15))
  
  
# Plotting erosion control in one block
e1 <- erosion_control %>% ggplot(aes(x=RIPRAP_LEN)) + 
  geom_histogram(binwidth = 5, fill = 'chocolate4') +
  ggtitle("Riprap Length")+
  labs(x = "Riprap Length", y = "Count")
e2 <- erosion_control %>% ggplot(aes(x=VERTICAL_WALL_LEN)) +
  geom_histogram(binwidth = 5, fill = 'orangered')+
  ggtitle("Vertical Wall")+
  labs(x = "Vertical Wall")+
  theme(axis.text.y=element_blank())
e3 <- erosion_control %>% ggplot(aes(x=EROSION_CNTRL_LEN)) +
  geom_histogram(binwidth = 5, fill = 'red1') +
  ggtitle("Erosion Control") + 
  labs(x = "Erosion Control")
grid.arrange(e1,e2,e3, nrow=1)

# By selected parcels
parcel_ero <- select(parcel_dd,c(PARCELID,POINT_SOURCE_PRES,CHANNEL_FLOW_PRES))
parcel_ero %>% ggplot(aes(x=parcel_ero, y=Presence,fill=PARCELID)) +
  geom_bar(stat="identity",position="dodge")
