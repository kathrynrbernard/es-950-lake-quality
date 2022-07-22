## Setup
# Load packages
library(tidyverse)
library(gridExtra)
library(grid)
library(plotly)

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
greens19 <- greens # developed parcels
i <- 1
for (color in greens){
  greens19[i] <- color
  greens19[i+1] <- color
  i <- i + 2
}
greens19[19] <- greens[9]

greens3 <- greens[c(7,9,9)] # undeveloped parcels

devel_canopy_plot <-
  arbor_parcel %>% filter(DEVELOPED == "TRUE") %>%
  ggplot(aes(x = CANOPY_PCT)) +
  geom_bar(fill = greens19, color=greens[9], aes(y = (..count..) / sum(..count..))) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L),
                     limits = c(0, .75)) +
  xlim(0, 100) +
  labs(title = "Developed Parcels", x = "Percent Canopy Cover", y =
         "Percent of Parcels") +
  theme_minimal()
undevel_canopy_plot <-
  arbor_parcel %>% filter(DEVELOPED == "FALSE") %>%
  ggplot(aes(x = CANOPY_PCT)) +
  geom_bar(fill = greens3, color=greens[9], aes(y = (..count..) / sum(..count..))) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L),
                     limits = c(0, .75)) +
  xlim(0, 100) +
  labs(title = "Undeveloped Parcels", x = "Percent Canopy Cover", y =
         "") +
  theme_minimal()
# plot side by side
grid.arrange(devel_canopy_plot, undevel_canopy_plot, ncol=2, 
             top=textGrob("Percent Canopy Coverage by Development Status",gp = gpar(fontsize = 15)))



# mean canopy - developed vs undeveloped
arbor_parcel %>% 
  group_by(DEVELOPED) %>% 
  summarize(mean_canopy = mean(CANOPY_PCT)) %>% 
  ggplot(aes(x=DEVELOPED,y=mean_canopy)) + 
  geom_bar(fill=c(greens[7],greens[3]),color=greens[9], stat="identity") +
  labs(x="Parcel Development Status",y="Average Canopy Coverage",title="Average Canopy Cover by Parcel Development Status") +
  scale_x_discrete(limits=c(TRUE,FALSE),labels=c("Developed","Undeveloped")) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) + # show % signs since the variable is measured in %s: https://stackoverflow.com/questions/50627529/add-a-percent-to-y-axis-labels
  theme_minimal()  +
  theme(plot.title = element_text(hjust = 0.5,size=15))


# heatmap
land_cover <- select(arbor_parcel,
                     c(CANOPY_PCT,SHRUB_HERB_PCT,IMPERVIOUS_PCT,MANI_LAWN_PCT,OTHER_PCT))
rownames(land_cover) <- arbor_parcel$PARCELID
land_cover <- scale(land_cover)  

# base R
heatmap(land_cover,Rowv = NA, Colv = NA)

# ggplot
# data needs to be in a "long" format
land_cover <- select(arbor_parcel,
                     c(PARCELID, CANOPY_PCT,SHRUB_HERB_PCT,IMPERVIOUS_PCT,MANI_LAWN_PCT,OTHER_PCT))
land_cover <- pivot_longer(land_cover,cols=!PARCELID,names_to="VEG_TYPE",values_to="PCT_COVERAGE")

# ggplot converted to plotly
p <- land_cover %>% ggplot(aes(x=VEG_TYPE,y = PARCELID, fill=PCT_COVERAGE, text=paste0("Parcel ID: ", PARCELID, "%", "\nPercent Covered: ",PCT_COVERAGE))) +
  geom_tile() +
  scale_fill_distiller(palette="Greens",direction=1, name="Percent Coverage") +
  labs(title="Land Cover Percentages per Parcel", x="Vegetation Type", y="Parcel") +
  scale_x_discrete(labels=c("Canopy", "Shrub/Herb", "Impervious", "Manicured Lawn", "Other")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5,size=15),
        axis.ticks.y=element_blank(),
        axis.text.y = element_blank())
ggplotly(p, tooltip="text")

# plotly
plotly::plot_ly(
  data = land_cover,
  x = ~VEG_TYPE, y = ~PARCELID, z = ~PCT_COVERAGE, text = ~paste('Parcel ID: ', PARCELID),
  colors="Greens",
  hoverinfo = "text",
  type = "heatmap"
)
