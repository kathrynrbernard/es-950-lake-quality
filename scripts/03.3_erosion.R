
# Setup -------------------------------------------------------------------
# Load packages
library(tidyverse)
library(gridExtra)
library(dplyr)
library(RColorBrewer)
library(VennDiagram)


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
  

# All Erosion Control -----------------------------------------------------

# barplots
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

# heatmap
# data needs to be in a "long" format
erosion_control <- select(arbor_parcel, c(PARCELID, VERTICAL_WALL_LEN, RIPRAP_LEN, EROSION_CNTRL_LEN))

erosion_control_pivot <- pivot_longer(erosion_control,cols=!PARCELID,names_to="CONTROL_TYPE",values_to="LENGTH")

tans <- c("wheat", "wheat1", "wheat2", "wheat3", "wheat4")

p <- erosion_control_pivot %>% ggplot(aes(x=CONTROL_TYPE,y=PARCELID,fill=LENGTH,
                                     text=paste0("Parcel ID: ", PARCELID, "\nLength of Erosion Control: ",LENGTH,"ft"))) +
  geom_tile() +
  labs(title="Erosion Control Length per Parcel", x="Erosion Control Type", y="Parcel") +
  scale_fill_gradient(low="cornsilk", high="chocolate4",name="Length of Control Structure (Feet)") +
  scale_x_discrete(limits=c("VERTICAL_WALL_LEN", "RIPRAP_LEN", "EROSION_CNTRL_LEN"),
                   labels=c("Vertical Wall", "Riprap", "Other")) +
  theme_minimal() +
  theme(#text = element_text(family="arial"),
        plot.title = element_text(hjust = 0.5,size=15),
        axis.ticks.y=element_blank(),
        axis.text.y = element_blank())
ggplotly(p, tooltip="text")

# Parcel Drilldown Plot ---------------------------------------------------

erosion_control <- select(arbor_parcel, c(PARCELID, VERTICAL_WALL_LEN, RIPRAP_LEN, EROSION_CNTRL_LEN))
erosion_risks <- select(
  arbor_parcel,
  c(PARCELID,POINT_SOURCE_PRES,CHANNEL_FLOW_PRES,STAIR_LAKE_PRES,LAWN_LAKE_PRES,SAND_DEP_PRES,OTHER_RUNOFF_PRES))
erosion_meas <- select(arbor_parcel, c(PARCELID, GREAT_ERO_LEN, LESS_ERO_LEN))


erosion_risks <- erosion_risks %>% replace(is.na(.), 0) %>%
  mutate(EROSION_RISK_SUM = rowSums(across(where(is.numeric))),
         EROSION_RISK_PRES=EROSION_RISK_SUM > 0)
erosion_control <- erosion_control %>% replace(is.na(.), 0) %>%
  mutate(EROSION_CTRL_SUM = rowSums(across(where(is.numeric))),
         EROSION_CTRL_PRES=EROSION_CTRL_SUM > 0)

erosion_df <- left_join(erosion_risks,erosion_control,by="PARCELID")
erosion_pres <- erosion_df %>% select(EROSION_CTRL_PRES, EROSION_RISK_PRES)

library(VennDiagram)
grid.newpage() 
draw.pairwise.venn(area1 = sum(erosion_pres$EROSION_CTRL_PRES),                        
                   area2 = sum(erosion_pres$EROSION_RISK_PRES),
                   cross.area = nrow(erosion_pres %>% filter(EROSION_CTRL_PRES==1 & EROSION_RISK_PRES==1)), 
                   col = "chocolate4",
                   fill = c("wheat2", "cornsilk"),
                   alpha = 1,
                   category = c("Erosion Control Structures Present", "Erosion Risk Factors Documented"))
grid.newpage()
p1 <- draw.single.venn(area=sum(erosion_pres$EROSION_CTRL_PRES),
                 category = "Erosion Control Structures Present",
                 col = "chocolate4",
                 fill = "wheat2",
                 alpha = 1)
p2 <- draw.single.venn(area=sum(erosion_pres$EROSION_RISK_PRES),
                       category = "Erosion Risk Factors Documented",
                       col = "chocolate4",
                       fill = "cornsilk",
                       alpha = 1)
grid.arrange(gTree(children=p1),gTree(children=p2), nrow=1)

library(cowplot)
grid.newpage()
cowplot::plot_grid(gTree(children=p1),gTree(children=p2), labels=c("Erosion Control Structures Present", "Factors Documented"))

erosion_pres %>% filter(EROSION_CTRL_PRES==1 & EROSION_RISK_PRES==1)

library(ggvenn)
ggvenn(erosion_pres,
       fill_color=c("wheat2", "cornsilk"),
       fill_alpha=1,
       stroke_color="chocolate4")

library(ggVennDiagram)
erosion_pres$EROSION_CTRL_PRES <- as.numeric(erosion_pres$EROSION_CTRL_PRES)
erosion_pres$EROSION_RISK_PRES <- as.numeric(erosion_pres$EROSION_RISK_PRES)
ggVennDiagram(erosion_pres)

venn <- Venn(erosion_pres)
d <- process_data(venn)

ggplot() +
  geom_sf(data = venn_region(d)) +
  geom_sf(data = venn_setedge(d)) +
  geom_sf_text(aes(label = name), data = venn_setlabel(d)) +
  geom_sf_text(aes(label = count), data = venn_region(d))


parcel_ero <- select(parcel_dd,c(PARCELID,POINT_SOURCE_PRES,CHANNEL_FLOW_PRES))
parcel_ero %>% ggplot(aes(x=parcel_ero, y=Presence,fill=PARCELID)) +
  geom_bar(stat="identity",position="dodge")


erosion_parcels <- erosion_df %>% filter(PARCELID %in% parcel_dd$PARCELID)




# ideas
# parcels that have concerns vs parcels that have controls
# heatmap of all erosion control structures


