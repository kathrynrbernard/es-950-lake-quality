## This script does all the plotting for the riparian zone information
## Run 03_analysis.R first to load in data and do necessary pre-processing


# Setup -------------------------------------------------------------------
# Load packages
library(tidyverse)
library(gridExtra)
library(grid)
library(plotly)
library(RColorBrewer)

# Canopy Coverage Plots --------------------------------------------------
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
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme_minimal()  +
  theme(plot.title = element_text(hjust = 0.5,size=15))



# Vegetation Heatmap ------------------------------------------------------
# separate out land cover types
land_cover <- select(arbor_parcel,
                     c(SHRUB_HERB_PCT,IMPERVIOUS_PCT,MANI_LAWN_PCT,OTHER_PCT)) # from Sam - don't include canopy percent here
rownames(land_cover) <- arbor_parcel$PARCELID
land_cover <- scale(land_cover)  

# base R
heatmap(land_cover,Rowv = NA, Colv = NA)

# ggplot
# data needs to be in a "long" format
land_cover <- select(arbor_parcel,
                     c(PARCELID, SHRUB_HERB_PCT,IMPERVIOUS_PCT,MANI_LAWN_PCT,OTHER_PCT))
land_cover <- pivot_longer(land_cover,cols=!PARCELID,names_to="VEG_TYPE",values_to="PCT_COVERAGE")


# ggplot converted to plotly - final plot
p <- land_cover %>% 
  ggplot(aes(x=VEG_TYPE,y = PARCELID, fill=PCT_COVERAGE, text=paste0("Parcel ID: ", PARCELID, "\nPercent Covered: ",PCT_COVERAGE,"%"))) +
  geom_tile() +
  scale_fill_distiller(palette="Greens",direction=1, name="Percent Coverage") +
  labs(title="Land Cover Percentages per Parcel", x="Vegetation Type", y="Parcel") +
  scale_x_discrete(limits=c("SHRUB_HERB_PCT", "MANI_LAWN_PCT", "IMPERVIOUS_PCT", "OTHER_PCT"),
                   labels=c("Shrub/Herb", "Manicured Lawn", "Impervious", "Other")) +
  theme_minimal() +
  theme(text = element_text(family="arial"),
      plot.title = element_text(hjust = 0.5,size=15),
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


# Shrub/Herb Coverage Plots -----------------------------------------------
## % of parcels with each % coverage, split by development status
greens <- brewer.pal(n=9,name="Greens")
greens18 <- greens # developed parcels
i <- 1
for (color in greens){
  greens18[i] <- color
  greens18[i+1] <- color
  i <- i + 2
}

greens4 <- greens[c(5,7,9,9)] # undeveloped parcels

devel_sh_plot <-
  arbor_parcel %>% filter(DEVELOPED == "TRUE") %>%
  ggplot(aes(x = SHRUB_HERB_PCT)) +
  geom_bar(fill = greens18, color=greens[9], aes(y = (..count..) / sum(..count..))) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L),
                     limits = c(0, .45)) +
  xlim(0, 100) +
  labs(title = "Developed Parcels", x = "Percent Shrub/Herbaceous Cover", y =
         "Percent of Parcels") +
  theme_minimal() 

undevel_sh_plot <-
  arbor_parcel %>% filter(DEVELOPED == "FALSE") %>%
  ggplot(aes(x = SHRUB_HERB_PCT)) +
  geom_bar(fill=greens18,color=greens[9],aes(y = (..count..) / sum(..count..))) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L),
                     limits = c(0, .45)) +
  xlim(0, 100) +
  labs(title = "Undeveloped Parcels", x = "Percent Shrub/Herbaceous Cover", y =
         "") +
  theme_minimal()
# plot side by side
grid.arrange(devel_sh_plot, undevel_sh_plot, ncol=2, 
             top=textGrob("Percent Shrub/Herbaceous Coverage by Development Status",gp = gpar(fontsize = 15)))


## mean shrub/herb coverage, split by development status
arbor_parcel %>% 
  group_by(DEVELOPED) %>% 
  summarize(mean_sh = mean(SHRUB_HERB_PCT)) %>% 
  ggplot(aes(x=DEVELOPED,y=mean_sh)) + 
  geom_bar(fill=c(greens[7],greens[3]),color=greens[9], stat="identity") +
  labs(x="Parcel Development Status",y="Average Shrub/Herbaceous Coverage",title="Average Shrub/Herbaceous Cover by Parcel Development Status") +
  scale_x_discrete(limits=c(TRUE,FALSE),labels=c("Developed","Undeveloped")) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) + # show % signs 
  theme_minimal()  +
  theme(plot.title = element_text(hjust = 0.5,size=15))



# Shrub/Herb and Manicured Lawn Comparisons -------------------------------
# Mean shrub/herb and mean manicured lawn by development status

arbor_parcel %>% 
  group_by(DEVELOPED) %>% 
  summarize(shrub_herb=mean(SHRUB_HERB_PCT),
             lawn=mean(MANI_LAWN_PCT)) %>% 
  pivot_longer(!DEVELOPED,names_to="LAND_COVER",values_to="MEAN") %>% 
  ggplot(aes(x=DEVELOPED,y=MEAN,fill=LAND_COVER)) +
  geom_bar(stat="identity", position="dodge",color=greens[9]) +
  scale_fill_manual(values=c(greens[3], greens[7]),labels=c("Manicured Lawn", "Shrub/Herbaceous")) +
  labs(x="Parcel Development Status",y="Average Percent Coverage",title="Average Shrub/Herbaceous and Manicured Lawn Cover by Parcel Development Status",
       fill="Land Cover Type") +
  scale_x_discrete(limits=c(TRUE,FALSE),labels=c("Developed","Undeveloped")) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) + # show % signs 
  theme_minimal()  +
  theme(plot.title = element_text(hjust = 0.5,size=15))




# Parcel Drilldown --------------------------------------------------------

# Plot idea:
# recode manicured lawn pct into presence/absence
# one parcel has mani lawn (the one with the structures), the other 2 do not
# all three have shrub and herb, but the one with a lot of structures has a small pct
# one parcel has a lot of strucutres, the other 2 do not have any 

# Vegetation and total number of structures
# With a label to say how many total structures are on each parcel on top of the facet plotting %s of vegetation
parcel_veg_struc <- select(parcel_dd,c(PARCELID,SHRUB_HERB_PCT,IMPERVIOUS_PCT,MANI_LAWN_PCT,AG_PCT,OTHER_PCT,
                                       STRUCTURES_TOTAL_LAND))
parcel_pivot <- parcel_veg_struc %>% pivot_longer(!c(PARCELID,STRUCTURES_TOTAL_LAND), names_to="Vegetation", values_to="Pct")


labels <- c(paste("Parcel ID: ",factor(parcel_dd$PARCELID), "\nTotal Structures: ", parcel_dd$STRUCTURES_TOTAL_LAND))
parcel_pivot %>% mutate(PARCELID=factor(parcel_pivot$PARCELID, 
                                        levels=c("2-2649", "2-2562-02", "2-2686-16"),
                                        labels=labels)) %>% 
  ggplot(aes(x=Vegetation,y=Pct,fill=PARCELID)) +
  geom_bar(stat="identity", position="dodge",color=greens[9]) +
  scale_fill_manual(values=c(greens[2],greens[4],greens[6]),name="Parcel ID",labels=c("2-2649", "2-2562-02", "2-2686-16")) +
  labs(x="Type of Vegetation", y="Percent Coverage", title="Land Cover Type Per Parcel") +
  scale_x_discrete(limits=c("SHRUB_HERB_PCT", "MANI_LAWN_PCT", "IMPERVIOUS_PCT", "AG_PCT", "OTHER_PCT"), 
                   labels=c("Shrub/Herb", "Lawn", "Impervious", "Agriculture", "Other")) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) + # show % signs
  facet_wrap(facets=vars(PARCELID)) +
  coord_flip() +
  theme_minimal()



# Structures
parcel_structures <- select(parcel_dd,c(PARCELID,BUILDINGS_CNT,BOAT_SHORE_CNT,FIRE_PIT_CNT,OTHER_STRUCTURE_CNT))
parcel_struc_pivot <- pivot_longer(parcel_structures,cols=!PARCELID,names_to = "Structure", values_to = "Count")

parcel_struc_pivot %>% ggplot(aes(x=Structure,y=Count)) +
  geom_bar(stat="identity", position="dodge") +
  facet_wrap(facets=vars(PARCELID)) +
  theme_minimal()

