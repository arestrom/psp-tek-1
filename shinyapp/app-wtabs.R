library(shiny)
library(leaflet)
library(plotly)
library(shinythemes)
library(ggmap)
library(tidyverse)
library(stringr)
library(scales)
library(shiny)
df <- readRDS("./data/all-dfs.rds")

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
                              uiOutput("pointslider"),
                               width = 2),
                            mainPanel(
                              # suppress error message
                              tags$style(type="text/css",
                                         ".shiny-output-error { visibility: hidden; }",
                                         ".shiny-output-error:before { visibility: hidden; }"
                              ),
                              # map of points
                              leafletOutput("watermap",
                                            height = 600,
                                            width = 800),
                              # table output to test dynamic clicking/plot creation
                              #uiOutput("click_text"),
                              br(),
                              plotOutput("chumcounts")
                              
                            ))),
                 # second tab panel for HUCs
                 tabPanel("Outcome Data",
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
                              checkboxInput(inputId = "colorblind",
                                           label = "colorblind friendly",
                                           value = FALSE),
                              width = 2
                            ),
                            # map output
                            mainPanel(leafletOutput("hucmap",
                                                    height = 600,
                                                    width = 800))
                          )),
                 tabPanel("Investment Data",
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons(inputId = "hucinv",
                                           label = "HUC level:",
                                           c(10,12)),
                              radioButtons(inputId = "invmeasure",
                                           label = "Investment Measure: ",
                                           c("Total", "Average by Project")),
                            width = 2)
                            ,
                            mainPanel(
                              leafletOutput("invmap",
                                            height = 600,
                                            width = 800)
                            )
                          )),
                 tabPanel("Contact",
                          fluidRow(
                            column(1),
                            column(3,
                                   wellPanel(
                                        )),
                            
                            column(4,
                                   wellPanel(
                                     htmlOutput("contactinfo",
                                                align = "center")
                                   )),
                            
                            column(3,
                                   wellPanel(
                                     ))
                            , align = "center"
                          ))
                 
                 
                 
)


server <- function(input, output, session) {
  
### point code ###
  # read in the shapefiles
  huc10_df <- readRDS("./data/huc10.rds")
  huc12_df <- readRDS("./data/huc12.rds")
  
  # reactive df for those features in the input
  
  df <- df[df$HUC_id %in% huc10_df$HUC10 | df$HUC_id %in% huc12_df$HUC12, ]
  df <- subset(df, !is.na(df$measurement))
  reactive_df_features <- reactive({subset(df, result_type %in% input$mapfeatures)})
  
  # point slides
  output$pointslider <- renderUI({
    sliderInput("slider1", "Select Year:", as.numeric(min(reactive_df_features()$year)), 
                max(reactive_df_features()$year), 
                median(reactive_df_features()$year),
                sep = "")
  })
  
  reactive_df <- reactive({subset(reactive_df_features(), year %in% input$slider1)})
  
  
  # subset based on HUC level
  mid_df <- reactive({subset(df, HUC_level %in% input$huc)})
  # remove duplicates
  huc_df <- reactive({mid_df()[!duplicated(mid_df()$HUC_id),]})
  # merge the HUC shapefiles and the required information from the reactive df
  shapes_df <- reactive({
    if(10 %in% input$huc){
      huc10_df
    } else{
      huc12_df
    }
  })
  
  clickdata <- reactiveValues(clickedMarker = NULL)
  
  # create map
  output$watermap <- renderLeaflet( m <- leaflet(data = shapes_df()) %>% 
                                      setView(lng = -122.996823, lat = 47.8, zoom = 9) %>%
                                      addTiles() %>%
                                      addProviderTiles("Stamen.Terrain", group = "Terrain") %>%
                                      # HUC outlines beneath the datapoints
                                      addPolygons(#data= shapes_df(),
                                                  #popup = hucname(),
                                                  fillOpacity = 0,
                                                  color = "black", 
                                                  weight = 1,
                                                  opacity = 1)  %>%
                                      # datapoints
                                      addCircleMarkers(data = reactive_df(),
                                                       ~lon, ~lat, 
                                                       popup = content1(),
                                                       radius = 6,
                                                       color = ~qpal()(measurement),
                                                       stroke = FALSE, 
                                                       fillOpacity = 0.8,
                                                       layer = ~name,
                                                       clusterOptions = markerClusterOptions()
                                                      ) %>%
                                      
                                      # legend (quantiles)
                                      addLegend(title = paste(pointstitle()), pal = qpal(), values = reactive_df()$measurement, opacity = 1, labels = c("turbidity", "TSS"))
                                    
                                    )
  
  observeEvent(input$watermap_marker_click,{
    clickdata$clickedMarker <- input$watermap_marker_click
  })

  observeEvent(input$watermap_click,{
    clickdata$clickedMarker <- NULL
  })

  output$click_text <- renderPrint({
    print(clickdata$clickedMarker)
  })

  # select everything from reactive_df_features() with the name clicked
  project_sub <- reactive({
    subset(reactive_df_features(), name %in% clickdata$clickedMarker$id)
  })

  # aggregate measurement by year within project_sub()
  project_mean <- reactive({
    aggregate(measurement ~ year, project_sub(), mean)
  })

  # initiate highlight point by getting everything
  # from the pared down df with the name clicked
  highlight_mid <- reactive({
    subset(reactive_df(), name %in% clickdata$clickedMarker$id)
  })
  
  # highlight point data
  highlight_point <- reactive({
    aggregate(measurement ~ year, highlight_mid(), mean)
  })

  output$chumcounts <- renderPlot({
    ggplot(project_mean(),
           aes(x = year,
               y = measurement)) +
      geom_line(size = 1) +
      geom_point(size = 5) +
      geom_point(data = highlight_point(), color = "gold", size = 5) +
      # geom_text(aes(label = round(measurement,2), hjust = 0, vjust = -1.5)) +
      scale_x_continuous(breaks = pretty_breaks(nrow(project_mean())-1)) +
      labs(title = project_sub()$name, 
           x = "Year",
           y = project_sub()$result_type) +
      theme(plot.title = element_text(hjust = .5, size = 18),
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14),
            axis.title.x = element_text(size = 16),
            axis.title.y = element_text(size = 16))
  })
  
# define the content for the popups
  content1 <- reactive({
    paste(sep = "",
          ifelse(reactive_df()$result_type == "Chum Salmon",
                 paste("<b>Location: </b>",
                       reactive_df()$name,
                       br(),
                       "<b>Type: </b>",
                       reactive_df()$project_cat,
                       br(),
                       "<b>Year: </b>",
                       reactive_df()$year,
                       br(),
                       "<b>Chum Count: </b>",
                       reactive_df()$measurement),
                 paste("<b>Project Name: </b>",
                       reactive_df()$name,
                       br(),
                       "<b>Year: </b>",
                       reactive_df()$year,
                       br(),
                       strong(reactive_df()$result_type),
                       "<b>: </b>",
                       ifelse(reactive_df()$result_type == "Investment",
                              dollar(reactive_df()$measurement),
                              paste(reactive_df()$measurement,
                                    " ",
                                    reactive_df()$unit)))
          )
          
          
    )})
  
  hucname <- reactive({
    paste(sep = "",
          "<b>HUC: </b>",
          shapes_df()$Name)
  })
  
  # define color quantile for graphing 
  # qpal <- reactive({ifelse(reactive_df()$result_type == "Chum Salmon",
  #                colorQuantile("Oranges", reactive_df()$measurement, n = 5),
  #                ifelse(reactive_df()$result_type == "Investment",
  #                       colorQuantile("Blues", reactive_df()$measurement, n = 5),
  #                       colorQuantile("Reds", reactive_df()$measurement, n = 5)))
  # })
    
    qpal <- reactive({if(reactive_df()$result_type == "Chum Salmon"){
    colorQuantile("Oranges", reactive_df()$measurement, n = 5)
  } else if(reactive_df()$result_type == "Investment"){
    colorQuantile("Blues", reactive_df()$measurement, n = 5)
  } else if (reactive_df()$result_type == "Turbidity"){
    colorQuantile("Reds", reactive_df()$measurement, n = 5)
  } else{
    colorQuantile("Reds", reactive_df()$measurement, n = 5)
  }
  })
  
  pointstitle <- reactive({if(reactive_df()$result_type == "Turbidity"){
    paste("Turbidity (NTU)",br(),"(Quantiles)") 
  } else if (reactive_df()$result_type == "TSS") {
    paste("TSS (mg/L)",br(),"(Quantiles)")
  } else if (reactive_df()$result_type == "Investment") {
    paste("Investment per Project", br(),"(Quantiles)")
  } else{
    paste("Chum Salmon (Count)",br(),"(Quantiles)")
  }})
  
  
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
                                                      "coloreffect",
                                                      "colorblind")], by.x = "HUC10", by.y = "HUC_id", all.x=TRUE, duplicateGeoms = TRUE)
  } else{
    huc12_df <- sp::merge(x = huc12_df, y = huc_df()[ , c("HUC_id", 
                                                      "medianyr", 
                                                      "cohensd", 
                                                      "TimePeriod", 
                                                      "effectsize", 
                                                      "meanbefore", 
                                                      "meanafter", 
                                                      "status", 
                                                      "coloreffect",
                                                      "colorblind")], by.x = "HUC12", by.y = "HUC_id", all.x=TRUE, duplicateGeoms = TRUE)
  }
})

cb_output <- reactive({input$colorblind})

# create the HUC map
   output$hucmap <- renderLeaflet( m <-  leaflet() %>% 
                                         setView(lng = -122.996823, lat = 47.8, zoom = 9) %>%
                                         addProviderTiles("Stamen.Terrain") %>%
                                     # use the shapefile() df
                                         addPolygons(data= shapefile(),
                                                     popup = content2(),
                                                     # color according to increases/decreases as defined by coloreffect column
                                                     fillColor = if(cb_output() == TRUE){
                                                       shapefile()$colorblind
                                                     } else{
                                                       shapefile()$coloreffect
                                                     },
                                                     fillOpacity = ifelse(is.na(shapefile()$coloreffect), 0, .7),
                                                     color = "black",
                                                     weight = 1,
                                                     opacity = 1
                                                     ) %>%
                                     addLegend(colors = if(cb_output() == TRUE){
                                       c('#c51b7d','#e9a3c9','#fde0ef','#e6f5d0','#a1d76a','#4d9221')
                                     } else{
                                       c('#d73027','#fc8d59','#fee08b','#d9ef8b','#91cf60','#1a9850')
                                     }
                                                 ,
                                               labels = c('large/worse', 'medium/worse', 'small/worse', 
                                                          'small/improving', 'medium/improving', 'large/improving'),
                                               position = 'topright',
                                               title = 'Change in Chum Salmon',
                                               opacity = .8)
  )
# variable that defines the text for the popup
   content2 <- reactive({
     paste(sep = "",
           # HUC
           "<b>HUC: </b>",
           shapefile()$Name,
           br(),
           ifelse(is.na(shapefile()$effectsize),
                  "<b>Not enough data for meaningful analysis</b>",
                  paste("<b>Effect Size: </b>",
                        tools::toTitleCase(shapefile()$effectsize),
                        br(),
                        "<b>Status: </b>",
                        tools::toTitleCase(shapefile()$status),
                        br(),
                        "<b>Cohen's D: </b>",
                        round(shapefile()$cohensd,2))
           ))
     
     
     
     
   })
  
### end HUC code ###  
  
  

### investment code
  
  inv_subset <- subset(df, result_type == "Investment")
  group_inv <- aggregate(measurement ~ HUC_id, inv_subset, "sum")
  avg_df <- aggregate(measurement ~ HUC_id, inv_subset, "mean")
  count_inv <- aggregate(name ~ HUC_id, inv_subset, function(x){NROW(x)})
  inv_subset <- merge(inv_subset, group_inv, by = "HUC_id")
  inv_subset <- merge(inv_subset, count_inv, by = "HUC_id")
  inv_subset <- merge(inv_subset, avg_df, by = "HUC_id")
  inv_subset <- subset(inv_subset, !is.na(inv_subset$measurement.y))
  new_inv <- subset(inv_subset, select = c("HUC_id", "HUC_level", "measurement.y", "name.x", "HUC_Name", "measurement"))
  new_inv <- rename(new_inv, totalinv = measurement.y, projectcount = name.x, avginv = measurement)
  new_inv <- new_inv[!duplicated(new_inv$HUC_id),] 
  
  
  shape_inv <- reactive({
    if(10 %in% input$hucinv){
      inv_10 <- sp::merge(x = huc10_df, y = new_inv, by.x = "HUC10", by.y = "HUC_id", all.x=TRUE, duplicateGeoms = TRUE)
    } else{
      inv_12 <- sp::merge(x = huc12_df, y = new_inv, by.x = "HUC12", by.y = "HUC_id", all.x=TRUE, duplicateGeoms = TRUE)
    }
  })
  
  
  output$invmap <- renderLeaflet( m <-  leaflet() %>% 
                                    setView(lng = -122.996823, lat = 47.8, zoom = 9) %>%
                                    addProviderTiles("Stamen.Terrain") %>%
                                    addPolygons(data= shape_inv(),
                                                popup = content3(),
                                                # color according to increases/decreases as defined by coloreffect column
                                                fillColor = ~invcolor()(as.numeric(var_inv())),
                                                fillOpacity = ifelse(is.na(shape_inv()$totalinv), 0, .7),
                                                color = "black",
                                                weight = 1,
                                                opacity = 1
                                    ) %>%
                                    addLegend(title = paste(invlabel()), 
                                              colors = c("#deebf7","#9ecae1","#3182bd"), 
                                              labels = c("small","medium","large"), opacity = 1)
                                  
  )
  var_inv <- reactive({if(input$invmeasure == "Total"){
    shape_inv()$totalinv} else {
       shape_inv()$avginv}})
  
  content3 <- reactive({
    paste(sep = "",
          # HUC
          "<b>HUC: </b>",
          shape_inv()$Name,
          br(),
          ifelse(is.na(shape_inv()$totalinv),
                 "<b>No project data</b>",
          paste("<b>Total Investment: </b>",
          dollar(shape_inv()$totalinv),
          br(),
          "<b>Number of Projects: </b>",
          shape_inv()$projectcount,
          br(),
          "<b>Average Investment per Project: </b>",
          dollar(shape_inv()$avginv)))
        
    )})
  invcolor <- reactive({if(input$invmeasure == "Total"){
    colorQuantile("Blues", shape_inv()$totalinv, n = 3)} else{
      colorQuantile("Blues", shape_inv()$avginv, n = 3)
    }})
  
  invlabel <- reactive({if(input$invmeasure == "Total"){
    paste("Total Investment") } else{
      paste("Average Investment")
    }
  })
    
  output$contactinfo <- output$datasources <- renderUI({
    str1 <- paste("This is a project in progress developed for a University of Washington Information School capstone project
                  by Tim Blankemeyer, Emma Clarke, and Katrina Gertz. Any questions or comments can be directed to Leska Fore 
                  from the Puget Sound Partnership at leska.fore@psp.wa.gov.")

    
    HTML(paste(str1[1],  sep = "<p/>"))  
  })
  
}
# Run the application 
shinyApp(ui = ui, server = server)

