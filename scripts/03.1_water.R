## This script does all the plotting for the littoral zone information
## Run 03_analysis.R first to load in data and do necessary pre-processing


# Setup -------------------------------------------------------------------
# Load packages
library(tidyverse)
library(gridExtra)

# Structures vs Vegetation Presence Plot ----------------------------------
# group number of structures into low/med/high and have 3 bar plots
# low = 0-1; med=2-5, high=6+

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



# Structures Distribution  --------------------------------------------------
# color manipulation to make it the right length
blues20 <- blues
i <- 1
for (color in blues){
  blues20[i] <- color
  blues20[i+1] <- color
  i <- i + 2
}
blues20[c(19,20)] <- blues[c(9,9)]

# bar plot  
arbor_parcel %>% ggplot(aes(x=STRUCTURES_TOTAL_WATER)) +
  geom_histogram(binwidth=1,fill=blues20, color=blues20[20]) +
  labs(x="Total Number of Structures in the Water", y="Number of Parcels", title="Distribution of Structures in the Water") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5,size=15))

# lollipop plot of the same data as above
arbor_parcel %>% 
  count(STRUCTURES_TOTAL_WATER) %>% 
  ggplot(aes(x=STRUCTURES_TOTAL_WATER, y=n)) +
  geom_segment(aes(x=STRUCTURES_TOTAL_WATER,xend=STRUCTURES_TOTAL_WATER,y=0,yend=n),color=blues[9]) +
  geom_point(size=5, color=blues20[20],fill=c(blues,blues[9], blues[9]) ,alpha=0.7,shape=21,stroke=1) +
  labs(x="Total Number of Structures in the Water", y="Number of Parcels", title="Distribution of Structures in the Water") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5,size=15))


# Parcel Drilldown - Structures Plot --------------------------------------
# what specific types of structures are on each of the 3 specified parcels
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
  


# Parcel Drilldown - Vegetation Presence Plot -----------------------------
# prototype plot
parcel_veg <- select(parcel_dd,c(PARCELID,FLOATING_VEG_PRES,EMERGENT_VEG_PRES))
parcel_veg_pivot <- pivot_longer(parcel_veg, cols=!PARCELID, names_to="Type", values_to="Presence")
parcel_veg_pivot %>% ggplot(aes(x=Type,y=Presence,fill=PARCELID)) +
  geom_bar(stat="identity",position="dodge")


parcel_veg %>% ggplot(aes(x=PARCELID,y=FLOATING_VEG_PRES,size=EMERGENT_VEG_PRES)) + 
  geom_point() 
select(parcel_dd,c(PARCELID,FLOATING_VEG_PRES,EMERGENT_VEG_PRES))


