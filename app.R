#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(RColorBrewer)
library(gridExtra)

#TODO add more inputs to allow switching between overall lake view and parcel view

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


# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Big Arbor Vitae Lake Quality"),
  
  # Radio buttons to select variables of interest
  radioButtons(
    inputId = "radio",
    label = h3("Choose a variable to explore"),
    choices = list(
      "Canopy Percentage" = 1,
      "Floating Aquatic Vegetation" = 2,
      "Floating Emergent Vegetation" = 3
    ),
    selected = 1
  ),
  
  # Show a plot of the generated distribution
  mainPanel(plotOutput("plot"))
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$unused_plot <- renderPlot({
    switch(input$radio,
           "1" = arbor_parcel %>%
             ggplot(aes(x=CANOPY_PCT,fill=DEVELOPED)) + geom_histogram(position="dodge",binwidth=5),
           "2" = arbor_parcel %>%
             ggplot(aes(x=FLOATING_VEG_PRES,fill=DEVELOPED)) + geom_bar(position="dodge",y=..prop..,stat="count")
    )
  })
  
  output$plot <- renderPlot({
    if(input$radio == "1"){
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
      
    }
  })
  
  output$shoreVegPlot <- renderPlot({
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
    shore_veg %>% ggplot(aes(x = CANOPY_PCT)) +
      geom_histogram(binwidth = 5)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
