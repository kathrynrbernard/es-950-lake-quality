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

# Aquatic vegetation
aquatic_veg <-
  select(arbor_parcel,
         c(
           PARCELID,
           EMERGENT_VEG_PRES,
           FLOATING_VEG_PRES,
           FLOAT_EMERG_PRES
         ))

# Structures in the water
aquatic_structures <-
  select(
    arbor_parcel,
    c(
      PARCELID,
      PIERS_CNT,
      BOAT_LIFT_CNT,
      SWIM_RAFT_CNT,
      BOATHOUSE_CNT,
      MARINAS_CNT,
      STRUCTURE_OTHER_CNT
    )
  )

# add column to overall dataset for total number of structures in the water
arbor_parcel <-
  arbor_parcel %>% mutate(STRUCTURES_TOTAL = rowSums(aquatic_structures[, -(1)]))


# create new column for whether any aquatic veg is present (floating or emergent)
arbor_parcel <- arbor_parcel %>% mutate(FLOAT_OR_EMERG_PRES = case_when(EMERGENT_VEG_PRES==TRUE | FLOATING_VEG_PRES==TRUE ~ TRUE,
                                                                        EMERGENT_VEG_PRES==FALSE & FLOATING_VEG_PRES==FALSE ~ FALSE))

# want to plot number of structures vs presence of aquatic vegetation
# would be easier as pct of veg but we don't have that

# one idea- group number of structures into low/med/high and have 3 bar plots
# low = 0-1; med=2-5, high=6+
arbor_parcel %>% ggplot(aes(x=STRUCTURES_TOTAL)) +
  geom_histogram(binwidth=2)
arbor_parcel <- arbor_parcel %>% mutate(STRUCTURES_CLASS =
                                          case_when(STRUCTURES_TOTAL <= 1 ~ "Low", 
                                                    STRUCTURES_TOTAL > 1 & STRUCTURES_TOTAL <= 5 ~ "Medium",
                                                    STRUCTURES_TOTAL > 5 ~ "High")
)
arbor_parcel %>% ggplot(aes(x=STRUCTURES_CLASS)) +
  geom_histogram(stat = "count")
arbor_parcel %>% ggplot(aes(x=STRUCTURES_CLASS, fill=FLOAT_OR_EMERG_PRES)) +
  geom_histogram(stat = "count",position="dodge") +
  scale_x_discrete(limits=c("Low","Medium", "High"))



# parcel drilldown
## 2-2686-16
# land: lots of structures, lots of manicured lawn, medium canopy, few shrubs/herbs
# water: lots of structures, some veg
# erosion: lots of control structures, no concerns, no actual erosion
parcel_one <- arbor_parcel[arbor_parcel$STRUCTURES_TOTAL==max(arbor_parcel$STRUCTURES_TOTAL),]

# 2-2562-02
# land: lots of canopy and shrub/herb, no structures
# water: no structures, no veg
# erosion: a lot of erosion (from the notes), channel flow present but no great-ero-len documented
parcel_two <- arbor_parcel[22,]

# create a small dataframe with just parcels of interest
parcel_dd <- arbor_parcel %>% filter(PARCELID==parcel_one$PARCELID | PARCELID==parcel_two$PARCELID)

# investigate what kinds of structures are here
parcel_structures <- select(parcel_dd,c(PARCELID,PIERS_CNT,BOAT_LIFT_CNT,SWIM_RAFT_CNT,BOATHOUSE_CNT,MARINAS_CNT,STRUCTURE_OTHER_CNT))
parcel_struc_pivot <- pivot_longer(parcel_structures,cols=!PARCELID,names_to = "Type", values_to = "Count")

parcel_struc_pivot %>% ggplot(aes(x=Type,y=Count,fill=PARCELID)) + geom_bar(stat="identity") +
  scale_y_continuous(breaks=seq(1:10))

# investigate what kind of veg is here
select(parcel_dd,c(PARCELID,FLOATING_VEG_PRES,EMERGENT_VEG_PRES))
