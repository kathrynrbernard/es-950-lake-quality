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

# Shoreline vegetation - shrubs/herbs/manicured lawn
shore_veg <-
  select(
    arbor_parcel,
    c(
      PARCELID,
      CANOPY_PCT,
      SHRUB_PRESENCE,
      HERB_PRESENCE,
      SHRUB_HERB_PCT,
      MANI_LAWN_PCT
    )
  )

shore_veg %>% summarize(
  mean_sh_pct = mean(SHRUB_HERB_PCT),
  min_sh_pct = min(SHRUB_HERB_PCT),
  max_sh_pct = max(SHRUB_HERB_PCT),
  mean_canopy_pct = mean(CANOPY_PCT),
  min_canopy_pct = min(CANOPY_PCT),
  max_canopy_pct = max(CANOPY_PCT)
)

shore_veg %>% ggplot(aes(x = SHRUB_HERB_PCT)) +
  geom_histogram(binwidth = 5)

shore_veg %>% ggplot(aes(x = CANOPY_PCT)) +
  geom_histogram(binwidth = 5)

shore_veg %>% ggplot(aes(x = MANI_LAWN_PCT)) +
  geom_histogram(binwidth = 5)



## Developed vs non developed parcels

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

arbor_parcel %>%
  group_by(DEVELOPED) %>%
  summarize(
    mean_canopy = mean(CANOPY_PCT),
    mean_erosion = mean(GREAT_ERO_LEN),
    mean_float = mean(FLOATING_VEG_PRES),
    mean_EMERG = mean(EMERGENT_VEG_PRES)
  )

## plotting notes: https://datacarpentry.org/R-ecology-lesson/04-visualization-ggplot2.html

## plot canopy coverage by development status
# basic plot
arbor_parcel %>%
  ggplot(aes(x=CANOPY_PCT,fill=DEVELOPED)) + 
  geom_histogram(position="dodge",binwidth=5)
# faceting
arbor_parcel %>%
  ggplot(aes(x=CANOPY_PCT)) + 
  geom_histogram(position="dodge",binwidth=5) +
  facet_wrap(facets=vars(DEVELOPED))
# graph as percents instead of counts -> not really what I want; the percentages are being calculated from the entire df
# instead of % developed vs % non developed
arbor_parcel %>% 
  ggplot(aes(x = CANOPY_PCT)) +  
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L)) +
  facet_wrap(facets=vars(DEVELOPED))
# separate plots
arbor_parcel %>% filter(DEVELOPED=="TRUE") %>% 
  ggplot(aes(x = CANOPY_PCT)) +  
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L))
arbor_parcel %>% filter(DEVELOPED=="FALSE") %>% 
  ggplot(aes(x = CANOPY_PCT)) +  
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L))
# align axis scales, clean up labels, add colors, etc.
#color stuff
greens <- brewer.pal(n=9,name="Greens")
developed_greens <- c(rep(greens[1],3),rep(greens[2],3),rep(greens[3],2),rep(greens[4],2),rep(greens[5],2),rep(greens[6],2),rep(greens[7],2),greens[8],rep(greens[9],2))

devel_canopy_plot <- arbor_parcel %>% filter(DEVELOPED=="TRUE") %>% 
  ggplot(aes(x = CANOPY_PCT)) +  
  geom_bar(fill=developed_greens,aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L),limits=c(0,.75)) +
  xlim(0,100) +
  labs(title="Developed Parcels",x="Percent Canopy Cover", y="Percent of Parcels") +
  theme_minimal()
undevel_canopy_plot <- arbor_parcel %>% filter(DEVELOPED=="FALSE") %>% 
  ggplot(aes(x = CANOPY_PCT)) +  
  geom_bar(fill=greens[7:9],aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L),limits=c(0,.75)) +
  xlim(0,100) +
  labs(title="Undeveloped Parcels",x="Percent Canopy Cover", y="") +
  theme_minimal()
# plot side by side
grid.arrange(devel_canopy_plot, undevel_canopy_plot, ncol=2)


# mean canopy - developed vs undeveloped
arbor_parcel %>% 
  group_by(DEVELOPED) %>% 
  summarize(mean_canopy = mean(CANOPY_PCT)) %>% 
  ggplot(aes(x=DEVELOPED,y=mean_canopy)) + 
  geom_bar(fill=c(greens[9],greens[7]),stat="identity") +
  labs(x="Parcel Development Status",y="Average Canopy Coverage") +
  scale_x_discrete(limits=c(TRUE,FALSE),labels=c("Developed","Undeveloped")) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) + # show % signs since the variable is measured in %s: https://stackoverflow.com/questions/50627529/add-a-percent-to-y-axis-labels
  theme_minimal()





sum(arbor_parcel$DEVELOPED) # 79 developed, 10 nondeveloped
arbor_parcel[arbor_parcel$DEVELOPED,"FLOATING_VEG_PRES"]


arbor_parcel %>%
  ggplot(aes(x=FLOATING_VEG_PRES,fill=DEVELOPED)) + geom_bar(aes(y=..count../sum(..count..)),position="dodge",stat="count")
