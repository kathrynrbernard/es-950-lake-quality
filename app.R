# do pre processing
source("shiny_helper.R")

ui <- fluidPage(
  titlePanel("Big Arbor Vitae Lake Quality"), # Application title
  tabsetPanel(
     tabPanel("Home",
              h2("About this page",style="text-decoration: underline;"),
              p("This website displays information about Big Arbor Vitae Lake. All the data visualized here was collected by the 
              Wisconsin Department of Natural Resources as part of their shoreline monitoring program.
              The graphs show metrics related to overall lake quality as well as data from selected parcels.
              Where appropriate, we include recommendations for the lake association or landowners.",style="font-size:20px;"),
              h2("Navigating the website",style="text-decoration: underline;"),
              p("The app contains three main categories of visualizations: Land, Water, and Erosion.
              These categories correspond to the riparian zone, the littoral zone, and the bank zone of the lake.
              Each category is displayed on a separate tab within the website. Click through the tabs at the top of the page
              to view the graphs for each category.",style="font-size:20px;"),
              p("Each tab has a few visualizations. Some of those visualizations are related to the lake as a whole, and some offer
                in-depth look at a few specific parcels on the lake. We chose three parcels to examine more closely. You can follow the same three parcels
                across the tabs to see how they compare in each shorefront zone.", style="font-size:20px;"),
              div(style="padding-top: 10px;",img(src='BigArborAerial.png', align = "center", width=600))
              ),
       tabPanel("Land",
                h3("The Riparian Zone at a glance",style="text-decoration: underline;"),
                p("Let's start by taking a look at what types of land cover are present in the riparian zone of Big Arbor Vitae."
                     ,style="font-size:18px;"),
               fluidRow(style="padding-bottom: 50px; padding-top: 10px;",
                column(7, plotlyOutput("land_cover_heatmap")), # need to use plotlyOutput instead of plotOutput
                column(5, h5("Summary"),
                        p("This heatmap shows the percentage of each type of land cover for each parcel.
                         Hovering over a square on the heatmap will display the parcel id and percent coverage for the land cover type.
                          There appears to be a substantial amount of shrub and herbaceous covering on Big Arbor Vitae, and not much manicured lawn."
                          ,style="font-size:16px;"),
                       h5("Recommendations"),
                          p("Shrubs are small-ish woody plants and herbaceous plants are things like grasses - both of these types of 
                          land cover are beneficial for controlling erosion, and native plantings are an excellent habitat for
                          animals living near the lakeshore. Manicured lawn has the opposite effect - it can contribute to erosion and runoff
                          concerns, and doesn't provide any habitat benefits. It is great to see so much beneficial land cover present on this lake!"
                            ,style="font-size:16px;"))),
               h3("Breaking it down",style="text-decoration: underline;"),
               p("We split the parcels on this lake into two categories, developed and undeveloped.
                    Developed parcels are primarily located on the south end of the lake where there is a high concentration of built structures, 
                    and undeveloped parcels are primarily located on the north end of the lake, which appears to be largely untouched. We thought
                    that there might be some interesting differences in land cover type based on whether a parcel was developed or not.",
                 style="font-size:18px;"),
               fluidRow(style="padding-bottom: 50px; padding-top: 10px;",
                 column(7, plotOutput("avg_sh_lawn_development")),
                 column(5, h5("Summary"),
                        p("This graph shows the average percent cover of both shrub/herbaceous covering and manicured lawn. 
                          Both developed and undeveloped parcels tend to have a high percent of
                          shrub and herbaceous covering.",style="font-size:16px;"),
                        h5("Recommendations"),
                        p("It seems like landowners are doing a good job of maintaining
                          beneficial land cover on their properties, even in more highly developed areas.
                          There's still some room for improvement, though, Landowners with a lot of manicured lawn could consider
                          re-wilding parts of their property to reintroduce native plants and more land cover that helps combat erosion."
                          ,style="font-size:16px;"))),
               h3("Parcels",style="text-decoration: underline;"),
               p("Let's take a look at some information for our three parcels.",style="font-size:18px;"),
               fluidRow(style="padding-bottom: 50px; padding-top: 10px;",
                 column(7, plotOutput("land_cover_parcels")),
                 column(5, h5("Summary"),
                        p("The graph shows the percent coverage for each land cover type for each of the three
                             parcels. The title on each graph also shows the total number of built structures in the
                             riparian zone for each parcel. It looks like the two parcels with a high percentage of
                             shrub and herbaceous covering don't have any built structures, while the parcel with a high
                             percentage of manicured lawn has 8 total built structures in this zone of the shorefront.",style="font-size:16px;"),
                        h5("Recommendations"),
                        p("Built structures in the riparian zone, especially combined with a lot of manicured lawn and not a lot of
                             benefical land cover, can contribute to erosion concerns. This might be a parcel to keep a closer eye on.
                             ",style="font-size:16px;")))
               
        ),
        tabPanel("Water",
                 h3("The Littoral Zone at a glance",style="text-decoration: underline;"),
                 p("<overview text>"
                   ,style="font-size:18px;"),
                fluidRow(style="padding-bottom: 50px; padding-top: 10px;",
                  column(7, plotOutput("aquatic_veg_structures")),
                  column(5, h5("Summary"),
                            p("Displayed is the amount of parcels, grouped by their amounts of structures, with vegetation present.
                             We can see that there is very little vegetation present on Big Arbor Lake.",style="font-size:16px;"),
                            h5("Recommendations"),
                            p(style="font-size:16px;"))),
                  h3("Breaking it down",style="text-decoration: underline;"),
                  p("<text>.",
                    style="font-size:18px;"),
                fluidRow(style="padding-bottom: 50px; padding-top: 10px;",
                  column(7,plotOutput("aquatic_parcel_structures")),
                  column(5,h5("Summary"),
                            p("This graph shows the distribution of number of structures in the littoral zone across the entire lake.
                            Most parcels on the lake have 1-3 structures in the water on their property. A few parcels have 4-6 aquatic structures,
                            and there are a handful of parcels with many structures.",style="font-size:16px;"),
                            h5("Recommendations"),
                            p(style="font-size:16px;"))),
                  h3("Parcels",style="text-decoration: underline;"),
                  p("Let's take a look at some information for our three parcels.",style="font-size:18px;"),
                fluidRow(style="padding-bottom: 50px; padding-top: 10px;",
                  column(7,plotOutput("aquatic_parcel_struc_dd")),
                  column(5, h5("Summary"),
                          p("Let's take a deeper dive into three parcels of interest. One of these parcels has a lot of structures, one has a handful,
                            and one doesn't have any structures in the littoral zone at all.",style="font-size:16px;"),
                         h5("Recommendations"),
                         p(style="font-size:16px;")))
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
  
  output$land_cover_parcels <- renderPlot({
    
    labels <- c(paste("Parcel ID: ",factor(parcel_dd$PARCELID), "\nTotal Structures: ", parcel_dd$STRUCTURES_TOTAL_LAND))
    parcel_pivot %>% mutate(PARCELID=factor(parcel_pivot$PARCELID, 
                                            levels=c("2-2649", "2-2562-02", "2-2686-16"),
                                            labels=labels)) %>% 
      ggplot(aes(x=Vegetation,y=Pct)) +
      geom_bar(stat="identity", position="dodge",fill=greens15,color=greens[9]) +
      labs(x="Type of Vegetation", y="Percent Coverage", title="Land Cover Type Per Parcel") +
      scale_x_discrete(limits=c("SHRUB_HERB_PCT", "MANI_LAWN_PCT", "IMPERVIOUS_PCT", "AG_PCT", "OTHER_PCT"), 
                       labels=c("Shrub/Herb", "Lawn", "Impervious", "Agriculture", "Other")) +
      facet_wrap(facets=vars(PARCELID)) +
      coord_flip() + 
      theme_minimal()
    
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
      count(STRUCTURES_TOTAL_WATER) %>% 
      ggplot(aes(x=STRUCTURES_TOTAL_WATER, y=n)) +
      geom_segment(aes(x=STRUCTURES_TOTAL_WATER,xend=STRUCTURES_TOTAL_WATER,y=0,yend=n),color=blues[9]) +
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