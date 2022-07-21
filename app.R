library(shiny)
library(tidyverse)
library(RColorBrewer)
library(gridExtra)
library(grid)

# Read in data
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


# water processing
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
arbor_parcel <-
  arbor_parcel %>% mutate(STRUCTURES_TOTAL = rowSums(aquatic_structures[, -(1)]))
arbor_parcel <- arbor_parcel %>% mutate(
  STRUCTURES_CLASS =
    case_when(
      STRUCTURES_TOTAL <= 1 ~ "Low",
      STRUCTURES_TOTAL > 1 &
        STRUCTURES_TOTAL <= 5 ~ "Medium",
      STRUCTURES_TOTAL > 5 ~ "High"
    )
)
arbor_parcel <-
  arbor_parcel %>% mutate(
    FLOAT_OR_EMERG_PRES = case_when(
      EMERGENT_VEG_PRES == TRUE | FLOATING_VEG_PRES == TRUE ~ TRUE,
      EMERGENT_VEG_PRES ==
        FALSE & FLOATING_VEG_PRES == FALSE ~ FALSE
    )
  )


ui <- fluidPage(titlePanel("Big Arbor Vitae Lake Quality"), # Application title
    mainPanel(tabsetPanel(
              tabPanel("Land",
                  fluidRow(style="padding-bottom: 50px; padding-top: 10px;",
                           column(8, plotOutput("land_development_canopy")),
                          column(4, "explanation")),
                  fluidRow(style="padding-bottom: 50px; padding-top: 10px;",
                          column(8, plotOutput("avg_canopy_development")),
                           column(4, "explanation"))
                  ),
                  tabPanel("Water", 
                  fluidRow(style="padding-bottom: 50px; padding-top: 10px;",
                           column(8, plotOutput("aquatic_veg_structures")),
                           column(4,"explanation")),
                  fluidRow(style="padding-bottom: 50px; padding-top: 10px;",
                           column(12,"Let's take a look at some parcel-specific information.")),
                  fluidRow(style="padding-bottom: 50px; padding-top: 10px;",
                          column(8,plotOutput("aquatic_parcel_structures")),
                           column(4,"This graph shows the distribution of number of structures in the littoral zone across the entire lake.
                                  Most parcels on the lake have 1-3 structures in the water on their property. A few parcels have 4-6 aquatic structures,
                                  and there are a handful of parcels with many structures.")),
                  fluidRow(style="padding-bottom: 50px; padding-top: 10px;",
                           column(8,plotOutput("aquatic_parcel_struc_dd")),
                           column(4,"Let's take a deeper dive into three parcels of interest. One of these parcels has a lot of structures, one has a handful,
                                  and one doesn't have any structures in the littoral zone at all."))
                  ),
                  tabPanel("Erosion")
                  
                )))


server <- function(input, output) {
  output$land_development_canopy <- renderPlot({
    # set up colors
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
    
    # plot developed half
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
    
    # plot undeveloped half
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
})
  
  output$avg_canopy_development <- renderPlot({
    # mean canopy - developed vs undeveloped
    arbor_parcel %>% 
      group_by(DEVELOPED) %>% 
      summarize(mean_canopy = mean(CANOPY_PCT)) %>% 
      ggplot(aes(x=DEVELOPED,y=mean_canopy)) + 
      geom_bar(fill=c(greens[7],greens[3]),color=greens[9], stat="identity") +
      labs(x="Parcel Development Status",y="Average Canopy Coverage",title="Average Canopy Cover by Parcel Development Status") +
      scale_x_discrete(limits=c(TRUE,FALSE),labels=c("Developed","Undeveloped")) +
      scale_y_continuous(labels = function(x) paste0(x, "%")) + # show % signs since the variable is measured in %s: https://stackoverflow.com/questions/50627529/add-a-percent-to-y-axis-labels
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5,size=15))
  })
  
  output$aquatic_veg_structures <- renderPlot({
    # color setup
    blues <- brewer.pal(n=9,name="Blues")
    blues2 <- blues[c(6,9)]
    names(blues2) <- levels(as.factor(arbor_parcel$FLOAT_OR_EMERG_PRES))
    
    # plot number of structures and veg presence
    arbor_parcel %>% ggplot(aes(x=STRUCTURES_CLASS, fill=as.factor(FLOAT_OR_EMERG_PRES))) +
      geom_histogram(stat = "count",position="dodge", color=blues[8]) +
      scale_x_discrete(limits=c("Low","Medium", "High"),labels=c("Low (0-1)","Medium (2-5)", "High (6+)")) +
      labs(title="Presence of Aquatic Vegetation by Structures in the Water",x="Number of Structures", y="Number of Parcels") +
      scale_fill_manual(name="Aquatic Vegetation Present", values = blues2) +
      guides(color="none") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5,size=15))
    
  }) # renderPlot
  
  output$aquatic_parcel_structures <- renderPlot({
    # color setup
    blues20 <- blues
    i <- 1
    for (color in blues){
      blues20[i] <- color
      blues20[i+1] <- color
      i <- i + 2
    }
    blues20[c(19,20)] <- blues[c(9,9)]
    
    # plot total number of structures
    arbor_parcel %>% ggplot(aes(x=STRUCTURES_TOTAL)) +
      geom_histogram(binwidth=1,fill=blues20, color=blues20[20]) +
      labs(x="Total Number of Structures in the Water", y="Number of Parcels", title="Distribution of Structures in the Water") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5,size=15))
  })
  
  output$aquatic_parcel_struc_dd <- renderPlot({
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
  })
}

# Run the application
shinyApp(ui = ui, server = server)
