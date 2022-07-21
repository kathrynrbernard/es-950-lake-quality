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
blues2 <- blues[c(7,3)]
names(blues2) <- levels(as.factor(arbor_parcel$FLOAT_OR_EMERG_PRES))

arbor_parcel %>% ggplot(aes(x=STRUCTURES_CLASS, fill=as.factor(FLOAT_OR_EMERG_PRES))) +
  geom_histogram(stat = "count",position="dodge", color=blues[8]) +
  scale_x_discrete(limits=c("Low","Medium", "High"),labels=c("Low (0-1)","Medium (2-5)", "High (6+)")) +
  labs(title="Presence of Aquatic Vegetation by Structures in the Water",x="Number of Structures", y="Number of Parcels") +
  scale_fill_manual(name="Aquatic Vegetation Present", values = blues2) +
  guides(color="none") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5,size=15))

# parcel drilldown - show distribution of total numbers of aquatic structures for each parcel
# color manipulation to make it the right length
blues20 <- blues
i <- 1
for (color in blues){
  blues20[i] <- color
  blues20[i+1] <- color
  i <- i + 2
}
blues20[c(19,20)] <- blues[c(9,9)]
  
arbor_parcel %>% ggplot(aes(x=STRUCTURES_TOTAL)) +
  geom_histogram(binwidth=1,fill=blues20, color=blues20[20]) +
  labs(x="Total Number of Structures in the Water", y="Number of Parcels", title="Distribution of Structures in the Water") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5,size=15))

# then go into 3 parcels specifically to show exactly what kinds they have
parcel_structures <- select(parcel_dd,c(PARCELID,PIERS_CNT,BOAT_LIFT_CNT,SWIM_RAFT_CNT,BOATHOUSE_CNT,MARINAS_CNT,STRUCTURE_OTHER_CNT))
parcel_struc_pivot <- pivot_longer(parcel_structures,cols=!PARCELID,names_to = "Type", values_to = "Count")

parcel_struc_pivot %>% ggplot(aes(x=Count,y=Type,fill=PARCELID)) + 
  geom_bar(stat="identity", position="dodge", color=blues[9]) +
  scale_x_continuous(breaks=seq(0:10)) + # show axis in whole numbers
  scale_y_discrete(limits=c("BOATHOUSE_CNT", "MARINAS_CNT", "SWIM_RAFT_CNT", "STRUCTURE_OTHER_CNT", "BOAT_LIFT_CNT", "PIERS_CNT"), # reverse order
                   labels=c("BOATHOUSE_CNT"="Boathouses", "MARINAS_CNT"="Marinas", 
                            "SWIM_RAFT_CNT"="Swim Rafts", "STRUCTURE_OTHER_CNT"="Other Structures", 
                            "BOAT_LIFT_CNT"="Boat Lifts", "PIERS_CNT"="Piers")) + 
  scale_fill_manual(values=blues[c(7,3,1)], name="Parcel ID",breaks=c("2-2686-16", "2-2649", "2-2562-02")) + # re-title and re-order legend
  labs(x="Number of Structures", y="Type of Structure", title="Count of Each Type of Structure per Parcel") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5,size=15))
  

# investigate what kind of veg is here
parcel_veg <- select(parcel_dd,c(PARCELID,FLOATING_VEG_PRES,EMERGENT_VEG_PRES))
parcel_veg_pivot <- pivot_longer(parcel_veg, cols=!PARCELID, names_to="Type", values_to="Presence")
parcel_veg_pivot %>% ggplot(aes(x=Type,y=Presence,fill=PARCELID)) +
  geom_bar(stat="identity",position="dodge")


parcel_veg %>% ggplot(aes(x=PARCELID,y=FLOATING_VEG_PRES,size=EMERGENT_VEG_PRES)) + 
  geom_point() 

