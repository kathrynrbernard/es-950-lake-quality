library(shiny)
library(tidyverse)
library(RColorBrewer)
library(gridExtra)

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
  arbor_parcel %>% mutate(STRUCTURES_TOTAL = rowSums(aquatic_structures[,-(1)]))
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



# Define UI for application that draws a histogram
ui <- fluidPage(# Application title
  titlePanel("Big Arbor Vitae Lake Quality"),
  
  sidebarLayout(sidebarPanel(
    radioButtons(
      inputId = "radio",
      label = h3("Choose an aspect of the lake to explore"),
      choices = list(
        "Land Attributes" = 1,
        "Water Attributes" = 2,
        "Erosion Attributes" = 3
      ),
      selected = 1
    )
  ),
  mainPanel(column(
    8, plotOutput("plot")
  ))))

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$plot <- renderPlot({
    if (input$radio == "1") { # land
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
      grid.arrange(devel_canopy_plot, undevel_canopy_plot, ncol = 2)
      
    } # if statement
    
    if (input$radio == "2") { # water
      
      # plot
      blues <- brewer.pal(n = 9, name = "Blues")
      blues2 <- blues[c(6, 9)]
      names(blues2) <-
        levels(as.factor(arbor_parcel$FLOAT_OR_EMERG_PRES))
      
      arbor_parcel %>% ggplot(aes(x = STRUCTURES_CLASS, fill = as.factor(FLOAT_OR_EMERG_PRES))) +
        geom_histogram(stat = "count", position = "dodge") +
        scale_x_discrete(
          limits = c("Low", "Medium", "High"),
          labels = c("Low (0-1)", "Medium (2-5)", "High (6+)")
        ) +
        labs(title = "Presence of Aquatic Vegetation by Structures in the Water", x =
               "Number of Structures", y = "Number of Parcels") +
        scale_fill_manual(name = "Aquatic Vegetation Present", values = blues2)
 
    } # if statement
    
      
  }) # renderPlot
}

# Run the application
shinyApp(ui = ui, server = server)
