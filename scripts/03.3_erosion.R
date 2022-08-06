
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

# Control vs Risk ---------------------------------------------------
# venn diagram
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
                   category = c("Erosion Control Structures Present:\n 22 out of 89 parcels",
                                "Erosion Risk Factors\n Documented:\n 6 out of 89 parcels"),
                   cat.fontface = rep("plain", 2),
                   cat.fontfamily=rep("sans", 2),
                   cat.default.pos="text",
                   cex=c(0,0,0),
                   col = "chocolate4",
                   fill = c("wheat2", "cornsilk"),
                   alpha = c(1,1))


# Parcel Drilldown --------------------------------------------------------
parcel_control_pivot <- select(arbor_parcel, c(PARCELID, VERTICAL_WALL_LEN, RIPRAP_LEN, EROSION_CNTRL_LEN)) %>% 
  filter(PARCELID %in% parcel_dd$PARCELID) %>% 
  pivot_longer(!PARCELID, names_to="Control", values_to="Length")
parcel_risk_pivot <- select(
  arbor_parcel,
  c(PARCELID,POINT_SOURCE_PRES,CHANNEL_FLOW_PRES,STAIR_LAKE_PRES,LAWN_LAKE_PRES,SAND_DEP_PRES,OTHER_RUNOFF_PRES)) %>% 
  filter(PARCELID %in% parcel_dd$PARCELID) %>% 
  pivot_longer(!PARCELID, names_to="Risk", values_to="Presence")

p1 <- parcel_risk_pivot %>% replace(is.na(.), 0) %>% 
  mutate(Presence=case_when(Presence==0 ~ 0,
                            Presence==1 | Presence==2 ~ 1)) %>% 
  ggplot(aes(x=Risk,y=Presence, fill=PARCELID)) +
  geom_bar(stat="identity", position="dodge",color="chocolate4") +
  scale_x_discrete(limits=c("OTHER_RUNOFF_PRES","LAWN_LAKE_PRES", "STAIR_LAKE_PRES", "POINT_SOURCE_PRES",
                            "SAND_DEP_PRES","CHANNEL_FLOW_PRES"),
                   labels=c("Other runoff factor", "Lawn sloping into lake", "Stairs sloping into lake", "Point source",
                            "Sand deposits", "Channel flow")) +
  scale_fill_manual(values=c("wheat2", "wheat3", "wheat4")) +
  scale_y_continuous(breaks=c(0,1), labels=c("False", "True")) +
  labs(x="Risk Factor", y="Presence of Risk Factor", title="Erosion Risk Factors per Parcel") +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5,size=15))

p2 <- parcel_control_pivot %>% 
  ggplot(aes(x=Control,y=Length, fill=PARCELID)) +
  geom_bar(stat="identity", position="dodge",color="chocolate4") +
  scale_x_discrete(limits=c("EROSION_CNTRL_LEN","VERTICAL_WALL_LEN", "RIPRAP_LEN"),
                   labels=c("Other erosion control", "Vertical seawall", "Riprap")) +
  scale_fill_manual(values=c("wheat2", "wheat3", "wheat4")) +
  labs(x="Type of Control Structure", y="Length of Control Structure (Feet)", title="Erosion Control Structures per Parcel") +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5,size=15))
grid.arrange(p1,p2,nrow=2)


parcel_control_pivot %>% ggplot(aes(x=Control,y=Length, fill=PARCELID)) +
  geom_bar(stat="identity", position="dodge")

erosion_df <- left_join(erosion_df,erosion_meas,by="PARCELID")
parcel_erosion <- erosion_df %>% filter(PARCELID %in% parcel_dd$PARCELID)

parcel_erosion_sub <- parcel_erosion %>% select(PARCELID,EROSION_RISK_PRES, EROSION_CTRL_PRES)
parcel_erosion_pivot <- parcel_erosion_sub %>% pivot_longer(!PARCELID, names_to="Variable", values_to="Value")
parcel_erosion_pivot %>% ggplot(aes(x=Variable,y=Value,fill=PARCELID)) +
  geom_bar(stat="identity")
