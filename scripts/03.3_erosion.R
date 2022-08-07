## This script does all the plotting for the bank zone information
## Run 03_analysis.R first to load in data and do necessary pre-processing


# Setup -------------------------------------------------------------------
# Load packages
library(tidyverse)
library(gridExtra)
library(RColorBrewer)
library(plotly)
library(VennDiagram)

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
  theme(text = element_text(family="arial"))
ggplotly(e, tooltip="text")
        
# heatmap
# data needs to be in a "long" format
erosion_control <- select(arbor_parcel, c(PARCELID, VERTICAL_WALL_LEN, RIPRAP_LEN, EROSION_CNTRL_LEN))
rownames(erosion_control) <- arbor_parcel$PARCELID
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

erosion_df <- left_join(erosion_risks,erosion_control,by="PARCELID")
erosion_pres <- erosion_df %>% select(EROSION_CTRL_PRES, EROSION_RISK_PRES)

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
# final plot - controls and risks for each of the 3 parcels of interest
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

# prototypes of other plots
parcel_control_pivot %>% ggplot(aes(x=Control,y=Length, fill=PARCELID)) +
  geom_bar(stat="identity", position="dodge")


parcel_erosion_sub <- parcel_erosion %>% select(PARCELID,EROSION_RISK_PRES, EROSION_CTRL_PRES)
parcel_erosion_pivot <- parcel_erosion_sub %>% pivot_longer(!PARCELID, names_to="Variable", values_to="Value")
parcel_erosion_pivot %>% ggplot(aes(x=Variable,y=Value,fill=PARCELID)) +
  geom_bar(stat="identity")

