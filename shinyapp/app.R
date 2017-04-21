library(shiny)
library(leaflet)
library(plotly)
library(shinythemes)
library(ggmap)
library(tidyverse)
library(stringr)
library(scales)
df <- readRDS("./data/ALL-separate-2.rds")


ui <- navbarPage(theme = shinytheme("sandstone"),
                 "Opening Up the Data",
                 # first tab panel for point data
                 tabPanel("Point View",
                          # water quality data
                          sidebarLayout(
                            sidebarPanel(
                              # feature selection
                              radioButtons(inputId = "mapfeatures",
                                           label = "Select variable:",
                                           c("Turbidity", "TSS", "Investment", "Chum Salmon")),
                              # HUC level selection
                              radioButtons(inputId = "huc",
                                           label = "HUC level:",
                                           c(10, 12)),
                               width = 2),
                            mainPanel(
                              # map of points
                              leafletOutput("watermap",
                                            height = 675,
                                            width = 800),
                              # table output to test dynamic clicking/plot creation
                              tableOutput("myTable")
                              
                            ))),
                 # second tab panel for HUCs
                 tabPanel("HUC View",
                          sidebarLayout(
                            sidebarPanel(
                              # select features to view effects
                              radioButtons(inputId = "mapfeatures1",
                                           label = "Select outcome:",
                                           c("Turbidity", "TSS", "Chum Salmon")),
                              # select HUC level
                              radioButtons(inputId = "huclevel",
                                           label = "HUC level:",
                                           c(10, 12)),
                              checkboxInput(inputID = "colorblind",
                                           label = "Colorblind Friendly Key:",
                                           value = FALSE),
                              width = 2
                            ),
                            # map output
                            mainPanel(leafletOutput("hucmap",
                                                    height = 675,
                                                    width = 800))
                          ))
                 
                 
)



## code for marker size within the df ##
#reactive_df <- reactive({within(reactive_df(), cost_quantile <- as.integer(cut(unique(measurement, quantile(measurement, probs=0:5/5), include.lowest=TRUE))))})
# reactive_df()$marker_size <- reactive({
#   cut(
# #input data
#    reactive_df()$cost_quantile,
# #cut points
#    c( 0, 1, 2, 3, 4, 6) ,
# #label values (character strings work too)
#    labels = c(5,7,8,9,11) ,
# #interval closed on the right?
#    right = FALSE)})
#  
## end marker size code ##

server <- function(input, output, session) {
  
### point code ###
  # read in the shapefiles
  huc10_df <- readRDS("./data/huc10.rds")
  huc12_df <- readRDS("./data/huc12.rds")
  
  # reactive df for those features in the input
  df <- df[df$HUC_id %in% huc10_df$HUC10 | df$HUC_id %in% huc12_df$HUC12, ]
  reactive_df <- reactive({subset(df, result_type %in% input$mapfeatures)}) 

#  reactive_df$marker_size <- reactive({ifelse(midpoint_df()$result_type %in% "Investment", 10,6)})
  
  # subset based on HUC level
  mid_df <- reactive({subset(reactive_df(), HUC_level %in% input$huc)})
  # remove duplicates
  huc_df <- reactive({mid_df()[!duplicated(mid_df()$HUC_id),]})
  # merge the HUC shapefiles and the required information from the reactive df
  shapes_df <- reactive({
    if(10 %in% input$huc){
      huc10_df <- sp::merge(x = huc10_df, y = huc_df()[ , c("HUC_id", 
                                                        "medianyr", 
                                                        "cohensd", 
                                                        "TimePeriod", 
                                                        "effectsize", 
                                                        "meanbefore", 
                                                        "meanafter", 
                                                        "status", 
                                                        "coloreffect")], by.x = "HUC10", by.y = "HUC_id", all.x=TRUE, duplicateGeoms = TRUE)
    } else{
      huc12_df <- sp::merge(x = huc12_df, y = huc_df()[ , c("HUC_id", 
                                                        "medianyr", 
                                                        "cohensd", 
                                                        "TimePeriod", 
                                                        "effectsize", 
                                                        "meanbefore", 
                                                        "meanafter", 
                                                        "status", 
                                                        "coloreffect")], by.x = "HUC12", by.y = "HUC_id", all.x=TRUE, duplicateGeoms = TRUE)
    }
  })
  
  # create map
  output$watermap <- renderLeaflet( m <- leaflet(data = reactive_df()) %>% 
                                      setView(lng = -122.996823, lat = 47.875, zoom = 9) %>%
                                      addTiles() %>%
                                      addProviderTiles("Stamen.Terrain", group = "Terrain") %>%
                                      # HUC outlines beneath the datapoints
                                      addPolygons(data= shapes_df(),
                                                  fillOpacity = 0,
                                                  color = "black", 
                                                  weight = 1,
                                                  opacity = 1)  %>%
                                      # datapoints
                                      addCircleMarkers(~lon, ~lat, 
                                                       popup = content1(),
                                                       # variable radius - possibly not working correctly
                                                       radius = 6,
                                                       color = ~qpal()(measurement),
                                                       stroke = FALSE, fillOpacity = 0.5,
                                                       group = "water quality"
                                                      ) %>%
                                      # legend (quantiles)
                                      addLegend(title = paste("Measurement Quantiles"), pal = qpal(), values = reactive_df()$measurement, opacity = 1, labels = c("turbidity", "TSS"))
  
                                    )
# define the content for the popups
  content1 <- reactive({
    paste(sep = "",
          # name
          "<b>Project Name: </b>",
          reactive_df()$description,
          br(),
          # year
          "<b>Year: </b>",
          reactive_df()$year,
          br(),
          # measurements
          strong(reactive_df()$result_type),
          "<b>: </b>",
          # convert measurement to $ format for investments, else print as normal
          ifelse(reactive_df()$result_type == "Investment",
            dollar(reactive_df()$measurement),
            paste(reactive_df()$measurement,
            " ",
            reactive_df()$unit))
          

    )})
  
  # define color quantile for graphing 
  qpal <- reactive({colorQuantile("Reds", reactive_df()$measurement, n = 5)})
  
### end point code ###
  
  
  
  
### HUC code ###
  
# define reactive df based on mapfeatures1 input
reactive_subset <- reactive({subset(df, result_type %in% input$mapfeatures1)})
# further subset that df to filter only the HUCs selected from huclevel
mid_df <- reactive({subset(reactive_subset(), HUC_level %in% input$huclevel)})
# remove all duplicates (returns only one row per HUC that has enough measurements in the measurement column)
huc_df <- reactive({mid_df()[!duplicated(mid_df()$HUC_id),]})

# dynamically merge the shapefile and the df for mapping
shapefile <- reactive({
  if(10 %in% input$huclevel){
    huc10_df <- sp::merge(x = huc10_df, y = huc_df()[ , c("HUC_id", 
                                                      "medianyr", 
                                                      "cohensd", 
                                                      "TimePeriod", 
                                                      "effectsize", 
                                                      "meanbefore", 
                                                      "meanafter", 
                                                      "status", 
                                                      "coloreffect")], by.x = "HUC10", by.y = "HUC_id", all.x=TRUE, duplicateGeoms = TRUE)
  } else{
    huc12_df <- sp::merge(x = huc12_df, y = huc_df()[ , c("HUC_id", 
                                                      "medianyr", 
                                                      "cohensd", 
                                                      "TimePeriod", 
                                                      "effectsize", 
                                                      "meanbefore", 
                                                      "meanafter", 
                                                      "status", 
                                                      "coloreffect")], by.x = "HUC12", by.y = "HUC_id", all.x=TRUE, duplicateGeoms = TRUE)
  }
})

# create the HUC map
   output$hucmap <- renderLeaflet( m <-  leaflet() %>% 
                                         setView(lng = -122.996823, lat = 47.875, zoom = 9) %>%
                                         addProviderTiles("Stamen.Terrain") %>%
                                     # use the shapefile() df
                                         addPolygons(data= shapefile(),
                                                     popup = content2(),
                                                     # color according to increases/decreases as defined by coloreffect column
                                                     fillColor = ifelse(input$colorblind == FALSE, shapefile()$coloreffect, shapefile()$colorblind),
                                                     fillOpacity = ifelse(is.na(shapefile()$coloreffect), 0, .7),
                                                     color = "black",
                                                     weight = 1,
                                                     opacity = 1
                                                     ) %>%
                                     addLegend(colors = c('#d73027','#fc8d59','#fee08b','#d9ef8b','#91cf60','#1a9850'),
                                               labels = c('large/worse', 'medium/worse', 'small/worse', 
                                                          'small/improving', 'medium/improving', 'large/improving'),
                                               position = 'topright',
                                               title = 'Effect Sizeus',
                                               opacity = .8)
  )
# variable that defines the text for the popup
  content2 <- reactive({
    paste(sep = "",
          # HUC
          "<b>HUC: </b>",
          shapefile()$Name,
          br(),
          # effect size
          "<b>Effect: </b>",
          ifelse(is.na(shapefile()$effectsize),
                 paste("Not enough data for meaningful analysis"),
          paste(shapefile()$effectsize,
          "/",
          shapefile()$status,
          br(),
          # median year of projects
          "<b>Cohen's D: </b>",
          round(shapefile()$cohensd, 2)))

    )})
  
}
# Run the application 
shinyApp(ui = ui, server = server)

