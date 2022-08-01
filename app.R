# do pre processing
source("shiny_helper.R")

ui <- fluidPage(
  titlePanel("Big Arbor Vitae Lake Quality"), # Application title
  tabsetPanel(
     tabPanel("Home",
              h2("About this Page"),
              HTML("This website displays information about Big Arbor Vitae Lake. All the data visualized here was collected by the 
              Wisconsin Department of Natural Resources as part of their shoreline monitoring program.
              The graphs show metrics related to overall lake quality as well as data from selected parcels.
              Where appropriate, we include recommendations for the lake association or landowners."),
              h2("Navigating the Website"),
              HTML("The app contains three main categories of visualizations: Land, Water, and Erosion.
              These categories correspond to the riparian zone, the littoral zone, and the bank zone of the lake.
              Each category is displayed on a separate tab within the website. Click through the tabs at the top of the page
              to view the graphs for each category."),
              div(style="padding-top: 10px;",img(src='BigArborAerial.png', align = "center", width=600)),
              h3("Deciding Developed vs Undeveloped Parcels"),
              HTML("To get a better sense of Big Arbor Lake, we split the parcels into two categories: Developed and Undeveloped. 
                   In order to decide what parcels fell into these categories, we used a satellite image from the WDNR Lakes and AIS Mapping tool. 
                   We looked for structure presence and canopy coverage in each parcel to see if a parcel was fully developed.
                   Parcels that are highlighted in yellow are the parcels that we decided are 'Undeveloped.' 
                   This encompasses the northern portion of the lake."),
              div(style="padding-top: 10px;",img(src='Undeveloped_Map.png', align = "center", width=600))
              ),
       tabPanel("Land",
                h3("The Riparian Zone at a Glance"),
                HTML("Let's start by taking a look at what types of land cover are present in the riparian zone of Big Arbor."),
               fluidRow(style="padding-bottom: 50px; padding-top: 10px;",
                column(7, plotlyOutput("land_cover_heatmap")), # need to use plotlyOutput instead of plotOutput
                column(5, "This heatmap shows the percentage of each type of land cover for each parcel.
                         Hovering over a square on the heatmap will display the parcel id and percent coverage for the land cover type.
                          There doesn't seem to be much shrub and herbaceous covering on the majority of parcels on this lake.
                          Shrubs are small-ish woody plants and herbaceous plants are things like grasses - both of these types of 
                          land cover are beneficial for controlling erosion, and native plantings are an excellent habitat for
                          animals living near the lakeshore.")),
               h3("Breaking it Down"),
               HTML(""),
               fluidRow(style="padding-bottom: 50px; padding-top: 10px;",
                column(7, plotOutput("land_development_sh")),
                column(5, "Here, we can compare the percentage of shrub/herbaceous coverage on parcels that are developed and non developed.
                          We can see that there is a high percentage of cover on non developed land, but also a good amount on developed land.")),
               fluidRow(style="padding-bottom: 50px; padding-top: 10px;",
                 column(7, plotOutput("avg_sh_lawn_development")),
                 column(5, "This is the mean shrub/herbaceous coverage percent per parcel, split by development status."))
        ),
        tabPanel("Water",
                fluidRow(style="padding-bottom: 50px; padding-top: 10px;",
                  column(7, plotOutput("aquatic_veg_structures")),
                  column(5, "Displayed is the amount of parcels, grouped by their amounts of structures, with vegetation present.
                             We can see that there is very little vegetation present on Big Arbor Lake. Is this a product of removal, or an indication of lake quality issues?")),
                fluidRow(style="padding-bottom: 50px; padding-top: 10px;",
                  column(7,plotOutput("aquatic_parcel_structures")),
                  column(5,"This graph shows the distribution of number of structures in the littoral zone across the entire lake.
                            Most parcels on the lake have 1-3 structures in the water on their property. A few parcels have 4-6 aquatic structures,
                            and there are a handful of parcels with many structures.")),
                fluidRow(style="padding-bottom: 50px; padding-top: 10px;",
                  column(7,plotOutput("aquatic_parcel_struc_dd")),
                  column(5,"Let's take a deeper dive into three parcels of interest. One of these parcels has a lot of structures, one has a handful,
                            and one doesn't have any structures in the littoral zone at all.")),
       ),
       tabPanel("Erosion", 
                fluidRow(style="padding-bottom: 50px; padding-top: 10px;",
                  column(7, plotOutput("erosion_plot")),
                   column(5, "This density graph shows the length of riprap areas present on the lake.")),
       ),
    )
  )


server <- function(input, output) {
  
  output$land_cover_heatmap <- renderPlotly({ # need to use renderPlotly instead of renderPlot
    
    # ggplot converted to plotly (plotly allows a hover bubble for displaying parcel id and percentage)
    p <- land_cover %>% ggplot(aes(x=VEG_TYPE,y = PARCELID, fill=PCT_COVERAGE, text=paste0("Parcel ID: ", PARCELID, "\nPercent Covered: ",PCT_COVERAGE,"%"))) +
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
  })
  
  output$land_development_sh <- renderPlot({
    
    devel_sh_plot <-
      arbor_parcel %>% filter(DEVELOPED == "TRUE") %>%
      ggplot(aes(x = SHRUB_HERB_PCT)) +
      geom_bar(fill = greens18, color=greens[9], aes(y = (..count..) / sum(..count..))) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1L),
                         limits = c(0, .45)) +
      xlim(0, 100) +
      labs(title = "Developed Parcels", x = "Percent Shrub/Herbaceous Cover", y =
             "Percent of Parcels") +
      theme_minimal() +
      theme(axis.title.x = element_text(size=15),
            axis.title.y = element_text(size=15))
    
    undevel_sh_plot <-
      arbor_parcel %>% filter(DEVELOPED == "FALSE") %>%
      ggplot(aes(x = SHRUB_HERB_PCT)) +
      geom_bar(fill = greens4, color=greens[9], aes(y = (..count..) / sum(..count..))) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1L),
                         limits = c(0, .45)) +
      xlim(0, 100) +
      labs(title = "Undeveloped Parcels", x = "Percent Shrub/Herbaceous Cover", y =
             "") +
      theme_minimal() +
      theme(axis.title.x = element_text(size=15))
    # plot side by side
    grid.arrange(devel_sh_plot, undevel_sh_plot, ncol=2, 
                 top=textGrob("Percent Shrub/Herbaceous Coverage by Development Status",gp = gpar(fontsize = 20)))
  })
  
  output$avg_sh_lawn_development <- renderPlot({
    ## mean shrub/herb and manicured lawn coverage, split by development status
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
  })
  
  output$aquatic_veg_structures <- renderPlot({
    
    # plot number of structures and veg presence
    arbor_parcel %>% ggplot(aes(x=STRUCTURES_CLASS, fill=as.factor(FLOAT_OR_EMERG_PRES))) +
      geom_histogram(stat = "count",position="dodge", color=blues[8]) +
      scale_x_discrete(limits=c("Low","Medium", "High"),labels=c("Low (0-1)","Medium (2-5)", "High (6+)")) +
      labs(title="Presence of Aquatic Vegetation by Structures in the Water",x="Number of Structures", y="Number of Parcels") +
      scale_fill_manual(name="Aquatic Vegetation Present", values = blues2) +
      guides(color="none") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5,size=15),
            axis.title.x = element_text(size=15),
            axis.title.y = element_text(size=15))
    
  })
  
  output$aquatic_parcel_structures <- renderPlot({
    
    # plot total number of structures
    arbor_parcel %>% 
      count(STRUCTURES_TOTAL) %>% 
      ggplot(aes(x=STRUCTURES_TOTAL, y=n)) +
      geom_segment(aes(x=STRUCTURES_TOTAL,xend=STRUCTURES_TOTAL,y=0,yend=n),color=blues[9]) +
      geom_point(size=5, color=blues20[20],fill=c(blues,blues[9], blues[9]) ,alpha=0.7,shape=21,stroke=1) +
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
  
  output$erosion_plot <- renderPlot({
    
    erosion_control %>%
      ggplot(aes(x=RIPRAP_LEN)) +
      geom_density(fill=tans[3], color=tans[1]) +
      ggtitle("Length of Riprap across Parcels") +
      labs(x = "Riprap Length (Feet)", y = "Density") + 
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5,size=15))
  })
}

# Run the application
shinyApp(ui = ui, server = server)