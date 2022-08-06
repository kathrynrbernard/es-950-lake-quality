
# Setup -------------------------------------------------------------------
# Load packages
library(tidyverse)
library(gridExtra)
library(dplyr)
library(RColorBrewer)
library(plotly)

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

# Heat Map
# Separate Erosion Factors
erosion_prevent <- select(arbor_parcel,
                     c(RIPRAP_LEN,VERTICAL_WALL_LEN,EROSION_CNTRL_LEN)) 
rownames(erosion_prevent) <- arbor_parcel$PARCELID
erosion_prevent <- scale(erosion_prevent)

# base heat map
heatmap(erosion_prevent,Rowv = NA, Colv = NA)

erosion_prevent <- select(arbor_parcel,
                          c(RIPRAP_LEN,VERTICAL_WALL_LEN,EROSION_CNTRL_LEN,PARCELID))
erosion_prevent <- pivot_longer(erosion_prevent,cols=!PARCELID,names_to='ERO_PRE',values_to='LEN')

# Plot Heat map
e <- erosion_prevent %>% ggplot(arbor_parcel, mapping=aes(x='ERO_PRE', y= PARCELID, fill='LEN')) + 
  geom_tile()+
  scale_fill_distiller(palette="BrBG",direction=1, name="Percent Coverage") +
  labs(title="Erosion Control on Big Arbor", x="Control Type", y="Parcel") +
  scale_x_discrete(limits=c("RIPRAP_LEN", "ERTICAL_WALL_LEN", "EROSION_CNTRL_LEN"),
                   labels=c("Riprap Length", "Vertical Wall Length", "Erosion Control Length")) +
  theme_minimal() +
  theme(text = element_text(family="arial"),
        plot.title = element_text(hjust = 0.5,size=15),
        axis.ticks.y=element_blank(),
        axis.text.y = element_blank())
ggplotly(p, tooltip="text")

plotly::plot_ly(
  data = erosion_prevent,
  x = ~ERO_PRE, y = ~PARCELID, z = ~LEN, text = ~paste('Parcel ID: ', PARCELID),
  colors="BrBG",
  hoverinfo = "text",
  type = "heatmap"
)


