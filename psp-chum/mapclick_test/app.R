#
# This is some test code for collecting map click data
# and using it to present more granular plots
#

library(shiny) # for Shiny web app
library(tidyverse) # because it's the tidyverse!
library(rgdal) # to import HYDRO shapefiles into R
library(geojsonio) # to convert spatial data to GeoJson
library(spdplyr) # to manipulate attributes of spatial data
library(magrittr) # for lovely magrittr piping
library(leaflet) # for mapping
library(ggplot2) # for plotting
library(forcats) # for changing factor level names

# load saved data from preparation scripts
chumtwo <- readRDS("./data/chumtwo.rds")
tidychum <- readRDS("./data/tidychum.rds")

# Define UI for the app to test map click data
ui <- fluidPage(
   
   # Application title
   titlePanel("Chum map click test"),
   
   # Sidebar with explainer text 
   sidebarLayout(
      sidebarPanel(
         helpText("Test code to link map click data to more granular
                    data in a separate data frame.")
      ),
      
      # Show the map, output from click data, and plot
      mainPanel(
        leafletOutput("chummap"),
        uiOutput("click_text"),
        plotOutput("chumcounts")
      )
   )
)

# Define server logic
server <- function(input, output, session) {
  
  # create reactiveValues data container for click data
  clickdata <- reactiveValues(clickedMarker=NULL)
  
  # render the Leaflet map, passing site name column vector as layerId
  output$chummap <- renderLeaflet({
    leaflet(chumtwo) %>%
      setView(lng = -123, lat = 47.55, zoom = 9) %>%
      addProviderTiles("Esri.WorldImagery") %>% 
      # this is for clustered regular markers and no jittering
      addMarkers(lng = ~lng,
                 lat = ~lat,
                 popup = ~River.site,
                 layerId = ~River.site,
                 clusterOptions = markerClusterOptions(freezeAtZoom=12))
      # this is for blue circle markers with jittering
      #addCircleMarkers(lng = ~jitterlng,
                        #lat = ~jitterlat,
                        #color = "#42d4f4",
                        #popup = ~River.site,
                        #layerId = ~River.site)
   })
  
  # observe the marker click info - store if click on marker
  observeEvent(input$chummap_marker_click,{
    clickdata$clickedMarker <- input$chummap_marker_click}
  )
  
  # observe the map click info - restore to NULL
  observeEvent(input$chummap_click,{
    clickdata$clickedMarker <- NULL}
  )
  
  # text output as a double-check step
  output$click_text <- renderPrint({
    print(clickdata$clickedMarker)
  })
  
  # subset tidychum count data using click data id, which
  # equals the layerId attribute passed in addCircleMarkers
  countsubset <- reactive({
    subset(tidychum, site %in% clickdata$clickedMarker$id)
  })
  
  # render the ggplot using the reactive subset
  output$chumcounts <- renderPlot({
    ggplot(countsubset(),
           aes(x=year,
               y=count,
               color=site)) +
      scale_x_continuous(breaks=c(1960,
                                  1965,
                                  1970,
                                  1975,
                                  1980,
                                  1985,
                                  1990,
                                  1995,
                                  2000,
                                  2005,
                                  2010)) +
      geom_line(size = 1) +
      geom_point(size = 3)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

