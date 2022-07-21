library(shiny)
library(tidyverse)
library(RColorBrewer)
library(gridExtra)
library(grid)
library(hrbrthemes)
library(shinydashboard)

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


ui <- dashboardPage(
  dashboardHeader(title = "Big Arbor Lake"),
  dashboardSidebar(
    sidebarMenu(
    menuItem("Land", tabName = "Land", icon = icon("tree")),
    menuItem("Water", tabName = "Water", icon = icon("tree")),
  )))
  dashboardBody(
    tabItems(
      tabItem("Land",  box(plotOutput("land_development_canopy")),
              box(
                selectInput("features", "Features:",
                            c("CANOPY_PCT","SHRUB_HERB_PCT")), width = 4
              )
              
      ),
      tabItem("Water",
              
              )
    # Boxes need to be put in a row (or column)
    
      )
)
server <- function(input, output) {

  output$land_development_canopy <- renderPlot({
    greens <- brewer.pal(n = 9, name = "Greens")
    developed_greens <-
      c(
        rep(greens[1], 3),
        rep(greens[2], 3),
        rep(greens[3], 2),
        rep(greens[4], 2),
        rep(greens[5], 2),
        rep(greens[6], 2),
        rep(greens[7], 2),
        greens[8],
        rep(greens[9], 2)
      )
    
    devel_canopy_plot <-
      arbor_parcel %>% filter(DEVELOPED == "TRUE") %>%
      ggplot(aes(x = CANOPY_PCT)) +
      geom_bar(fill = developed_greens, aes(y = (..count..) / sum(..count..))) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1L),
                         limits = c(0, .75)) +
      xlim(0, 100) +
      labs(title = "Developed Parcels", x = "Percent Canopy Cover", y =
             "Percent of Parcels") +
      theme_minimal()
    undevel_canopy_plot <-
      arbor_parcel %>% filter(DEVELOPED == "FALSE") %>%
      ggplot(aes(x = CANOPY_PCT)) +
      geom_bar(fill = greens[7:9], aes(y = (..count..) / sum(..count..))) +
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
      geom_bar(fill=c(greens[9],greens[7]),stat="identity") +
      labs(title="Average Canopy Coverage per Parcel by Development Status", x="Parcel Development Status",y="Average Canopy Coverage") +
      scale_x_discrete(limits=c(TRUE,FALSE),labels=c("Developed","Undeveloped")) +
      scale_y_continuous(labels = function(x) paste0(x, "%")) + # show % signs since the variable is measured in %s: https://stackoverflow.com/questions/50627529/add-a-percent-to-y-axis-labels
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5,size=15))
  })
}


shinyApp(ui, server)

