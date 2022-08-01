# Purpose - load packages and do pre-processing for Shiny app


# Packages  ---------------------------------------------------------------
library(shiny)
library(tidyverse)
library(RColorBrewer)
library(gridExtra)
library(grid)
library(plotly)


# Data --------------------------------------------------------------------
parcel_data <- read.csv("data/950_parcel_habitat_clean.csv")
woody_data <- read.csv("data/950_woody_habitat_clean.csv")

# Separate our group's lake - Big Arbor Vitae Lake
arbor_parcel <-
  parcel_data %>% filter(LAKE_NAME == "Big Arbor Vitae Lake")
arbor_woody <- woody_data %>% filter(LAKE_NAME == "Big Arbor Vitae")

# Add development column
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

# add column for total structures on land
arbor_parcel <-
  arbor_parcel %>% mutate(STRUCTURES_TOTAL_LAND = rowSums(land_structures[,]))

# add column for total structures in water
arbor_parcel <-
  arbor_parcel %>% mutate(STRUCTURES_TOTAL_WATER = rowSums(aquatic_structures[, -(1)]))

# group number of structures in water
arbor_parcel <- arbor_parcel %>% mutate(
  STRUCTURES_CLASS =
    case_when(
      STRUCTURES_TOTAL_WATER <= 1 ~ "Low",
      STRUCTURES_TOTAL_WATER > 1 &
        STRUCTURES_TOTAL_WATER <= 5 ~ "Medium",
      STRUCTURES_TOTAL_WATER > 5 ~ "High"
    )
)

# combine floating and emergent veg presence
arbor_parcel <-
  arbor_parcel %>% mutate(
    FLOAT_OR_EMERG_PRES = case_when(
      EMERGENT_VEG_PRES == TRUE | FLOATING_VEG_PRES == TRUE ~ TRUE,
      EMERGENT_VEG_PRES ==
        FALSE & FLOATING_VEG_PRES == FALSE ~ FALSE
    )
  )


# Parcel Drilldown --------------------------------------------------------
parcel_one <- arbor_parcel[arbor_parcel$PARCELID=="2-2686-16",]
parcel_two <- arbor_parcel[arbor_parcel$PARCELID=="2-2562-02",]
parcel_three <- arbor_parcel[arbor_parcel$PARCELID=="2-2649",]
parcel_dd <- arbor_parcel %>% filter(PARCELID==parcel_one$PARCELID | PARCELID==parcel_two$PARCELID | PARCELID==parcel_three$PARCELID)

# Colors ------------------------------------------------------------------
# Green (land)
greens <- brewer.pal(n=9,name="Greens")
greens18 <- greens # developed parcels
i <- 1
for (color in greens){
  greens18[i] <- color
  greens18[i+1] <- color
  i <- i + 2
}
greens4 <- greens[c(5,7,9,9)] # undeveloped parcels

greens15 <- greens
i <- 1
for(i in 1:nrow(parcel_pivot)){
  if(parcel_pivot[i,"Pct"] == 0){
    greens15[i] <- greens[1]
  }
  else if(parcel_pivot[i,"Pct"] < 10){
    greens15[i] <- greens[3]
  }
  else if(parcel_pivot[i,"Pct"] >= 10 & parcel_pivot[i,"Pct"] < 70){
    greens15[i] <- greens[6]
  }
  else if(parcel_pivot[i,"Pct"] >= 70){
    greens15[i] <- greens[8]
  }
  i <- i + 1
}

# Blue (water)
blues <- brewer.pal(n=9,name="Blues")
blues20 <- blues
i <- 1
for (color in blues){
  blues20[i] <- color
  blues20[i+1] <- color
  i <- i + 2
}
blues20[c(19,20)] <- blues[c(9,9)]

blues2 <- blues[c(6,9)]
names(blues2) <- levels(as.factor(arbor_parcel$FLOAT_OR_EMERG_PRES))

# Tan (erosion)
tans <- brewer.pal(n=9,name="BrBG")

# Subset the data ---------------------------------------------------------
# Land
land_cover <- select(arbor_parcel,
                     c(PARCELID, SHRUB_HERB_PCT,IMPERVIOUS_PCT,MANI_LAWN_PCT,OTHER_PCT))
land_cover <- pivot_longer(land_cover,cols=!PARCELID,names_to="VEG_TYPE",values_to="PCT_COVERAGE")

parcel_veg_struc <- select(parcel_dd,c(PARCELID,SHRUB_HERB_PCT,IMPERVIOUS_PCT,MANI_LAWN_PCT,AG_PCT,OTHER_PCT,
                                       STRUCTURES_TOTAL_LAND))
parcel_pivot <- parcel_veg_struc %>% pivot_longer(!c(PARCELID,STRUCTURES_TOTAL_LAND), names_to="Vegetation", values_to="Pct")


# Water
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

parcel_structures <- select(parcel_dd,c(PARCELID,PIERS_CNT,BOAT_LIFT_CNT,SWIM_RAFT_CNT,BOATHOUSE_CNT,MARINAS_CNT,STRUCTURE_OTHER_CNT))
parcel_struc_pivot <- pivot_longer(parcel_structures,cols=!PARCELID,names_to = "Type", values_to = "Count")

# Erosion
erosion_control <-
  select(arbor_parcel,
         c(PARCELID, VERTICAL_WALL_LEN, RIPRAP_LEN, EROSION_CNTRL_LEN))


