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
blues <- brewer.pal(n=9,name="Blues")
blues2 <- blues[c(6,9)]
names(blues2) <- levels(as.factor(arbor_parcel$FLOAT_OR_EMERG_PRES))

arbor_parcel %>% ggplot(aes(x=STRUCTURES_CLASS, fill=as.factor(FLOAT_OR_EMERG_PRES))) +
  geom_histogram(stat = "count",position="dodge") +
  scale_x_discrete(limits=c("Low","Medium", "High"),labels=c("Low (0-1)","Medium (2-5)", "High (6+)")) +
  labs(title="Presence of Aquatic Vegetation by Structures in the Water",x="Number of Structures", y="Number of Parcels") +
  scale_fill_manual(name="Aquatic Vegetation Present", values = blues2)

# investigate what kinds of structures are here
parcel_structures <- select(parcel_dd,c(PARCELID,PIERS_CNT,BOAT_LIFT_CNT,SWIM_RAFT_CNT,BOATHOUSE_CNT,MARINAS_CNT,STRUCTURE_OTHER_CNT))
parcel_struc_pivot <- pivot_longer(parcel_structures,cols=!PARCELID,names_to = "Type", values_to = "Count")

parcel_struc_pivot %>% ggplot(aes(x=Type,y=Count,fill=PARCELID)) + geom_bar(stat="identity") +
  scale_y_continuous(breaks=seq(1:10))

# investigate what kind of veg is here
select(parcel_dd,c(PARCELID,FLOATING_VEG_PRES,EMERGENT_VEG_PRES))

