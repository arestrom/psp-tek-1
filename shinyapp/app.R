library(shiny)
library(leaflet)
library(shinythemes)
library(tidyverse)
library(stringr)
library(scales)

df <- readRDS("./data/all-dfs.rds")

ui <- navbarPage(theme = shinytheme("sandstone"),
                 "Effectiveness and Evaluation Tool",
                 # first tab panel for HUCs
                 tabPanel("Map View",
                          sidebarLayout(
                            sidebarPanel(
                              HTML("<img src = 'eet-banner.png' width = '100%'/>"),
                              # select features to view effects
                              br(),
                              br(),
                              radioButtons(inputId = "mapfeatures1",
                                           label = "Select variable:",
                                           c("Chum Salmon", "Turbidity", "TSS", "Investment")),
                              # select HUC level
                              radioButtons(inputId = "huclevel",
                                           label = "Watershed size:",
                                           choiceNames = c("HUC 10", "HUC 12"),
                                           choiceValues = c("10", "12")),
                              uiOutput("checkbox"),
                              HTML("<b>Colorblind friendly: </b>"),
                              checkboxInput(inputId = "colorblind",
                                           label = "on",
                                           value = FALSE),
                              actionButton("button", "Reset graphs", style='padding:4px; font-size:80%'),
                              br(),
                              br(),
                              selectInput("result_type", "Choose dataset:", 
                                          choices = c("Chum Salmon", "Turbidity", "TSS", "Investment")),
                              downloadButton('downloadData', 'Download', style='padding:4px; font-size:80%;'),
                              # br(),
                              # br(),
                              # "USER GUIDE GOES HERE",
                              br(),
                              br(),
                              HTML("<img class = 'one' src = 'psp-logo.png' width = '100%'/>"),
                              br(),
                              br(),
                              HTML("<img src = 'grso-logo.png' width = '100%'/>"),
                              br(),
                              br(),
                              HTML("<img src = 'doe-logo.png' width = '100%'/>"),

                              
                              width = 2
                            ),
                            # map output
                            mainPanel(
                              tags$style(type="text/css",
                                         ".shiny-output-error { visibility: hidden; }",
                                         ".shiny-output-error:before { visibility: hidden; }"
                              ),
                              fluidRow(
                              splitLayout(cellWidths = c("52%", "1%", "47%"),
                              leafletOutput("hucmap",
                                                    height = 600
                                                    # width = 550
                                            ),
                              br(),
                            plotOutput("inv_plot",
                                        height = 600)),
                            br(),
                            splitLayout(cellWidths = c("70%", "1%", "29%"),
                                        plotOutput("cohensd_plot",
                                                   height = 350)),
                            br(),
                            br()),
                            br(),
                            br(),
                            width = 9
                            
                              
                            #renderText("click_text"))
                          )
                          )),
                 tabPanel("About")
                 
                 
)


server <- function(input, output, session) {
  
### map code ###
  # read in the shapefiles
  huc10_df <- readRDS("./data/huc10.rds")
  huc12_df <- readRDS("./data/huc12.rds")
  
  # reactive df for those features in the input

  
  
  df1 <- df[df$HUC_id %in% huc10_df$HUC10 | df$HUC_id %in% huc12_df$HUC12, ]
  df1 <- subset(df1, !is.null(df1$measurement))
  df1$project_source[df1$result_type == "Chum Salmon"] <- "River Mouths"
  
  df1$color[df1$project_source == "PRISM"] <- "#454546" 
  df1$color[df1$project_source == "EAGL"] <-  "#838284"
  #df1$color[df1$project_source == "HWS"] <- "#636264"


  # create checkbox
  output$checkbox <- renderUI({
    checkboxGroupInput("projects",
                       "Select sites: ",
                       choiceNames = list(HTML("<div>PRISM projects <img src = 'darklegend.png'/></div>"),
                                          HTML("<div>EAGL projects <img src = 'lightlegend.png'/></div>"),
                                          #HTML("<div>HWS projects <img src = 'mediumlegend.png'/></div>"),
                                          HTML("<div>Chum sites <img src = 'arrowlegend.png'/></div>")),
                       choiceValues = list("PRISM", 
                                           "EAGL", 
                                           #"HWS", 
                                           "River Mouths"))
  })  
  # update to null on HUC level change
    observeEvent(input$huclevel, {
      updateCheckboxGroupInput(session,
                               "projects",
                         "Select sites: ",
                         choiceNames = list(HTML("<div>PRISM projects <img src = 'darklegend.png'/></div>"),
                                            HTML("<div>EAGL projects <img src = 'lightlegend.png'/></div>"),
                                            #HTML("<div>HWS projects <img src = 'mediumlegend.png'/></div>"),
                                            HTML("<div>Chum sites <img src = 'arrowlegend.png'/></div>")),
                         choiceValues = list("PRISM", 
                                             "EAGL", 
                                             #"HWS", 
                                             "River Mouths"),
                         selected = NULL)
    })
    # update to NULL on mapfeatures change
    observeEvent(input$mapfeatures1, {
      updateCheckboxGroupInput(session,
                               "projects",
                               "Select sites: ",
                               choiceNames = list(HTML("<div>PRISM projects <img src = 'darklegend.png'/></div>"),
                                                  HTML("<div>EAGL projects <img src = 'lightlegend.png'/></div>"),
                                                  #HTML("<div>HWS projects <img src = 'mediumlegend.png'/></div>"),
                                                  HTML("<div>Chum sites <img src = 'arrowlegend.png'/></div>")),
                               choiceValues = list("PRISM", 
                                                   "EAGL", 
                                                   #"HWS", 
                                                   "River Mouths"),
                               selected = NULL)
    })
  
  

  
  # observeEvent(input$mapfeatures1, {
  #   clickdata$clickedShape <- NULL
  # })
  # 
  
                     
  
  
# define reactive df based on mapfeatures1 input
reactive_subset <- reactive({subset(df1, result_type %in% input$mapfeatures1)})
# further subset that df to filter only the HUCs selected from huclevel
mid_df <- reactive({subset(reactive_subset(), HUC_level %in% input$huclevel)})
# remove all duplicates (returns only one row per HUC that has enough measurements in the measurement column)
huc_df <- reactive({mid_df()[!duplicated(mid_df()$HUC_id),]})

# dynamically merge the shapefile and the df for mapping
shapefile <- reactive({
  if(10 %in% input$huclevel){
    huc10_df <- sp::merge(x = huc10_df, y = huc_df()[ , c("HUC_id", 
                                                      "medianyr", 
                                                      "cohensd_huc_mean", 
                                                      "TimePeriod", 
                                                      "effectsize", 
                                                      "status", 
                                                      "coloreffect",
                                                      "colorblind")], by.x = "HUC10", by.y = "HUC_id", all.x=TRUE, duplicateGeoms = TRUE)
  } else{
    huc12_df <- sp::merge(x = huc12_df, y = huc_df()[ , c("HUC_id", 
                                                      "medianyr", 
                                                      "cohensd_huc_mean", 
                                                      "TimePeriod", 
                                                      "effectsize", 
                                                      "status", 
                                                      "coloreffect",
                                                      "colorblind")], by.x = "HUC12", by.y = "HUC_id", all.x=TRUE, duplicateGeoms = TRUE)
  }
})


river_popup <- reactive({
  paste("Mouth of",
        river_mouths$name)
})

cb_output <- reactive({input$colorblind})

# filter project points
factor_df <- subset(df1, result_type %in% c("Chum Salmon", "Investment"))
proj_df <- reactive({subset(factor_df, project_source %in% input$projects)})

# layer = reactive({if(input$mapfeatures1 == "Investment"){
#   ~shape_inv()$HUC_id
# } else {
#   ~shapefile()$HUC_id
# }})

fill_color <- reactive({if(input$mapfeatures1 == "Investment"){
  ~invcolor()(as.numeric(var_inv()))
} else if (cb_output() == TRUE){
  shapefile()$colorblind
} else{
  shapefile()$coloreffect}})

fill_opacity <- reactive({if(input$mapfeatures1 == "Investment"){
  ifelse(is.na(shape_inv()$totalinv), 0, .7)} else{
    ifelse(is.na(shapefile()$coloreffect), 0, .7)}})

addLegendCustom <- function(map, colors, labels, sizes, opacity = 0.5){
  colorAdditions <- paste0(colors, "; width:", sizes, "px; height:", sizes, "px")
  labelAdditions <- paste0("<div style='display: inline-block;height: ", sizes, "px;margin-top: 4px;line-height: ", sizes, "px;'>", labels, "</div>")
  
  return(addLegend(map, colors = colorAdditions, labels = labelAdditions, opacity = opacity))
}

clickdata <- reactiveValues(clickedShape = NULL)



# create the HUC map
   output$hucmap <- renderLeaflet( m <-  leaflet() %>% 
                                         setView(lng = -122.91, lat = 47.775, zoom = 9) %>%
                                         addProviderTiles("Stamen.Terrain",
                                                          options = providerTileOptions(minZoom = 8)) %>%
                                        # use the shapefile() df
                                         addPolygons(data = shapefile(),
                                           # data= if(input$mapfeatures1 == "Investment"){
                                           #                    shape_inv()
                                           #                 } else{shapefile()},
                                                     popup = content2(),
                                                     # color according to increases/decreases as defined by coloreffect column
                                                     fillColor = fill_color(),
                                                     fillOpacity = fill_opacity(),
                                                     color = "black",
                                                     weight = 1,
                                                     opacity = 1,
                                                     layerId = ~shapefile()$Name
                                                     ) %>%


                                     addLegend(colors = if(input$mapfeatures1 == "Investment"){
                                                           c('#deebf7', '#9ecae1', '#3182bd')
                                                         } else if(cb_output() == TRUE){
                                                           c('#4dac26', '#b8e186', '#f7f7f7', '#f1b6da', '#d01c8b')
                                                         } else{
                                                           c('#1a9641', '#a6d96a', '#d3d3d3', '#fdae61', '#d7191c')
                                                         },
                                               labels = if(input$mapfeatures1 == "Investment"){
                                                          c("small", "medium", "large")
                                                        } else{ 
                                                          c('large improvement', 'small improvement', 'no change', 
                                                            'small decline', 'large decline')
                                                        },
                                               position = 'topright',
                                               title = ifelse(input$mapfeatures1 == "Investment", 
                                                              paste("Total Investment"),
                                                              ifelse(input$mapfeatures1 == "Chum Salmon",
                                                              paste('Change in',
                                                                    br(),
                                                             input$mapfeatures1),
                                                             paste("Change in",
                                                                   input$mapfeatures1)
                                                             )), 
                                               opacity = .7)
  )
   
   
   eagl_prism <- reactive({subset(proj_df(), project_source %in% c("EAGL", "PRISM", "HWS"))})
   chum_points <- reactive({subset(proj_df(), project_source %in% c("River Mouths"))})
   
   observe({
   
     if(is.null(input$projects)){
       leafletProxy("hucmap") %>%
         clearMarkers()
     } else {
       leafletProxy("hucmap") %>%
         clearMarkers() %>%
         addCircleMarkers(data = eagl_prism(),
                    ~lon, ~lat,
                    radius = 3,
                    fillOpacity = 1,
                    fillColor = eagl_prism()$color,
                    stroke = FALSE,
                    popup = projectpopup()) %>%
       addMarkers(data = chum_points(),
                  ~lon, ~lat,
                  icon = ~fishIcon,
                  popup = river_popup) 
    }
   
   })

   
   fishIcon <- makeIcon(iconUrl = "./data/down-arrow.png",
                        iconWidth = 12, iconHeight = 12,
                        iconAnchorX = 5, iconAnchorY = 5)

   
# variable that defines the text for the popup
   content2 <- reactive({
     paste(sep = "",
           # HUC
           "<b>HUC: </b>",
           print(shapefile()$Name)
           )
    
   })
   
   projectpopup <- reactive({
     paste("<style> div.leaflet-popup-content
           {width: auto !important;
           }</style>",
           "<b>Project: </b>",
           br(),
           print(proj_df()$name))
   })
   
   
   
   
   observeEvent(input$hucmap_shape_click, {
     clickdata$clickedShape <- input$hucmap_shape_click$id
   })
   
   observeEvent(input$hucmap_click, {
     clickdata$clickedShape <- NULL
   })
   
   observeEvent(input$mapfeatures1, {
     clickdata$clickedShape <- NULL
   })
   
   observeEvent(input$huclevel, {
     clickdata$clickedShape <- NULL
   })
   
   observeEvent(input$button, {
     clickdata$clickedShape <- NULL
   })
   
   # click_text <- renderPrint({
   #   print(clickdata$clickedShape)
   # })
   # 
   clicked_sub <- reactive({
     subset(mid_df(), HUC_Name == clickdata$clickedShape)
   })
   
  
   clicked_mid <- reactive({subset(clicked_sub(), year > "2002")})
   clicked_feat <- reactive({subset(clicked_mid(), result_type %in% input$mapfeatures1)})
   
   
   
   clicked_year <- reactive({if(input$mapfeatures1 == "Chum Salmon"){
     aggregate(measurement ~ year, clicked_feat(), "sum")} else{
       aggregate(measurement ~ year, clicked_feat(), "mean") }
     })

### end HUC code ###  
  
  

### investment code
  
   #TO CLEAN
   
  only_inv <- subset(df1, result_type == "Investment")
  group_inv <- aggregate(measurement ~ HUC_id, only_inv, "sum")
  avg_df <- aggregate(measurement ~ HUC_id, only_inv, "mean")
  count_inv <- aggregate(name ~ HUC_id, only_inv, function(x){NROW(x)})
  inv_subset <- merge(only_inv, group_inv, by = "HUC_id")
  inv_subset <- merge(inv_subset, count_inv, by = "HUC_id")
  inv_subset <- merge(inv_subset, avg_df, by = "HUC_id")
  inv_subset <- subset(inv_subset, !is.na(inv_subset$measurement.y))
  new_inv <- subset(inv_subset, select = c("HUC_id", "HUC_level", "measurement.y", "name.x", "HUC_Name", "measurement"))
  new_inv <- rename(new_inv, totalinv = measurement.y, projectcount = name.x, avginv = measurement)
  new_inv <- new_inv[!duplicated(new_inv$HUC_id),] 
  
  
  shape_inv <- reactive({
    if("10" %in% input$huclevel){
      inv_10 <- sp::merge(x = huc10_df, y = new_inv, by.x = "HUC10", by.y = "HUC_id", all.x=TRUE, duplicateGeoms = TRUE)
    } else{
      inv_12 <- sp::merge(x = huc12_df, y = new_inv, by.x = "HUC12", by.y = "HUC_id", all.x=TRUE, duplicateGeoms = TRUE)
    }
  })
    
  # for the HUC map (investment tab)
  var_inv <- reactive({shape_inv()$totalinv})
  invcolor <- reactive({colorQuantile("Blues", shape_inv()$totalinv, n = 3)})
  
  # for the investment bar chart
  
  inv_year_mid <- reactive({
      if ("PRISM" %in% input$projects | "EAGL" %in% input$projects) {
        subset(only_inv, project_source %in% input$projects)
      } else {
        only_inv}
    })
  inv_middle <- reactive({subset(inv_year_mid(), HUC_level == "10")})
  inv_by_year <- reactive({aggregate(measurement ~ year, inv_middle(), "sum")})
  
  clicked_inv_huc <- reactive({subset(inv_year_mid(), HUC_Name == clickdata$clickedShape)})
  clicked_inv <- reactive({aggregate(measurement ~ year, clicked_inv_huc(), "sum")})
  
  
  # for the variables
  mid_year_sub <- reactive({subset(reactive_subset(), year > "2002")})
  year_sub <- reactive({subset(mid_year_sub(), HUC_level %in% input$huclevel)})
  measure_year <- reactive({if(input$mapfeatures1 == "Chum Salmon"){
    aggregate(measurement ~ year, year_sub(), "sum")} else {
      aggregate(measurement ~ year, year_sub(), "mean")
    }})
  
  # sub_cd <- subset(df1, !is.na(df$cohensd_huc_mean))
  # sub_cd1 <- subset(sub_cd, !is.na(df$coloreffect))
  # sub_cd2 <- reactive({subset(sub_cd1, result_type %in% input$mapfeatures1)})
  # mid_cd <- reactive({subset(sub_cd2(), HUC_level %in% input$huclevel)})
  # cd_sub <- reactive({aggregate(cohensd_huc_mean ~ HUC_Name, mid_cd(), "mean")})
  # highlight_bar <- reactive({subset(cd_sub(), HUC_Name == input$hucmap_shape_click$id)})
  
  
  #plot_df <- reactive({merge(inv_by_year, measure_year(), by = "year")})
  
  # observeEvent(input$button,{
  #   updateCheckboxGroupInput(session, "projects", selected = character(0))
  # })
  # 
  observeEvent(input$button,{
    clickdata$clickedShape <- NULL
  })
  
  
  empty_df <- data.frame()

  
  output$inv_plot <- renderPlot({
      totalbar <- if(is.null(clickdata$clickedShape)) {
        # ALL HUC BAR
        ggplot() +
                  geom_col(data = if(nrow(clicked_inv_huc()) == 0){
                    inv_by_year()} else{ 
                      clicked_inv()},
                           aes(x = year,
                               y = measurement/1000000),
                           fill = "#136ca6",
                    width = .8) +
                   scale_x_continuous(breaks = pretty_breaks(20),
                          limits = c(2003, 2016)) +
      scale_y_continuous(breaks = pretty_breaks(6)) +
      labs(y = "Total Investment ($M)",
           title = if("PRISM" %in% input$projects & "EAGL" %in% input$projects){"Total Investment"}
           else if("PRISM" %in% input$projects | "EAGL" %in% input$projects)
           {paste(sep = "", "Total Investment", " (", input$projects, " Data)")}
           else{"Total Investment"},
           subtitle = "All HUCs") +
      theme(plot.title = element_text(hjust = .5, size = 14, face = "bold"),
            plot.subtitle = element_text(hjust = .5, size = 12, face = "bold", color = "#136ca6"),
            axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
            axis.text.y = element_text(size = 12),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 14))
      }else if(nrow(clicked_inv_huc()) == 0){
        ggplot(empty_df) +
          xlim(0, 100) +
          ylim(0, 10) +
          geom_text(aes(x = 50,
                        y = 5),
                    label = "Data pending",
                    size = 6,
                    fontface = "italic") +
          labs(
            title = if("PRISM" %in% input$projects & "EAGL" %in% input$projects){"Total Investment"}
            else if("PRISM" %in% input$projects | "EAGL" %in% input$projects)
            {paste(sep = "", "Total Investment", " (", input$projects, " Data)")}
            else{"Total Investment"},
            subtitle = paste(input$hucmap_shape_click$id)
          ) +
          theme(plot.title = element_text(hjust = .5, size = 14, face = "bold"),
                plot.subtitle = element_text(hjust = .5, size = 12),
                axis.text.x = element_blank(),
                axis.text.y = element_blank(),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.ticks = element_blank())
      } else {
        ggplot() +
          geom_col(data = clicked_inv(),
            aes(x = year,
                y = measurement/1000000),
            fill = "#3182bd",
            width = .8) +
          geom_vline(xintercept = clicked_feat()$medianyr, color = "red", linetype = "dashed") +
          scale_x_continuous(breaks = pretty_breaks(20),
                             limits = c(2003, 2016)) +
          scale_y_continuous(breaks = pretty_breaks(6)) +
          labs(y = "Total Investment ($M)",
               title = if("PRISM" %in% input$projects & "EAGL" %in% input$projects){"Total Investment"}
               else if("PRISM" %in% input$projects | "EAGL" %in% input$projects)
               {paste(sep = "", "Total Investment", " (", input$projects, " Data)")}
               else{"Total Investment"},
               subtitle = paste(input$hucmap_shape_click$id),
               caption = "Dashed line denotes median investment year") +
          theme(plot.title = element_text(hjust = .5, size = 14, face = "bold"),
                plot.subtitle = element_text(hjust = .5, size = 12),
                plot.caption = element_text(face = "italic"),
                axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
                axis.text.y = element_text(size = 12),
                axis.title.x = element_blank(),
                axis.title.y = element_text(size = 14))
      }
        
        
      totalline <- if(is.null(clickdata$clickedShape)){
        # OVERALL LINE GRAPH
        ggplot(data = measure_year(),
          aes(x = year,
              y = if(input$mapfeatures1 == "Investment"){measurement/1000000} else {measurement})) +
        geom_point() +
        geom_line() +
        scale_x_continuous(breaks = pretty_breaks(nrow(measure_year())),
                           limits = c(2003, 2016)) +
        scale_y_continuous(breaks = pretty_breaks(6), labels = comma) +
        labs(y = if(input$mapfeatures1 == "Chum Salmon"){
               "Chum Salmon (Number)"
             } else if(input$mapfeatures1 == "Investment"){
               paste("Average", input$mapfeatures1, "($M)")} else{
               paste(sep = "", "Average ", input$mapfeatures1, " (", reactive_subset()$unit,")")
             },
             title = if(input$mapfeatures1 == "Chum Salmon"){ 
               "Adult Chum Salmon"} else if(input$mapfeatures1 == "Investment"){
                 paste("Average Investment Per Project")
               } else {
                 paste("Average", input$mapfeatures1)
               },
             subtitle = "All HUCs"
                      ) +
        theme(plot.title = element_text(hjust = .5, size = 14, face = "bold"),
              plot.subtitle = element_text(hjust = .5, size = 12, face = "bold", color = "#136ca6"),
              axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
              axis.text.y = element_text(size = 12),
              axis.title.x = element_blank(),
              axis.title.y = element_text(size = 14))
      }else if(nrow(clicked_feat()) == 0){
        ggplot(empty_df) +
          xlim(0, 100) +
          ylim(0, 10) +
          geom_text(aes(x = 50,
                        y = 5),
                    label = "Data pending",
                    size = 6,
                    fontface = "italic") +
          labs(
          title = if(input$mapfeatures1 == "Chum Salmon"){ 
            "Adult Chum Salmon"} else if(input$mapfeatures1 == "Investment"){
              paste("Average Investment Per Project")
            } else {
              paste("Average", input$mapfeatures1)
            },
          subtitle = paste(input$hucmap_shape_click$id)
          ) +
          theme(plot.title = element_text(hjust = .5, size = 14, face = "bold"),
                plot.subtitle = element_text(hjust = .5, size = 12),
                axis.text.x = element_blank(),
                axis.text.y = element_blank(),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.ticks = element_blank())
      } else{
        # HUC-SPECIFIC LINE GRAPH
        ggplot(data = clicked_year(),
          aes(x = year,
              y = if(input$mapfeatures1 == "Investment"){measurement/1000000} else {measurement})) +
          geom_vline(xintercept = clicked_feat()$medianyr, color = "red", linetype = "dashed") +
          geom_point() +
          geom_line() +
          # geom_label(aes(x = clicked_feat()$medianyr, y = 0), label = "TEXT") +
          scale_x_continuous(breaks = pretty_breaks(nrow(measure_year())),
                             limits = c(2003, 2016)) +
          scale_y_continuous(breaks = pretty_breaks(6), labels = comma) +
          labs(y = if(input$mapfeatures1 == "Chum Salmon"){
            "Chum Salmon (Number)"
          } else if(input$mapfeatures1 == "Investment"){
            paste("Average", input$mapfeatures1, "(millions of dollars)")} else{
              paste(sep = "", "Average ", input$mapfeatures1, " (", reactive_subset()$unit,")")
            },
          title = if(input$mapfeatures1 == "Chum Salmon"){ 
            "Adult Chum Salmon"} else if(input$mapfeatures1 == "Investment"){
              paste("Average Investment Per Project")
            } else {
              paste("Average", input$mapfeatures1)
            },
          subtitle = paste(input$hucmap_shape_click$id)
          ) +
          theme(plot.title = element_text(hjust = .5, size = 14, face = "bold"),
                plot.subtitle = element_text(hjust = .5, size = 12),
                axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
                axis.text.y = element_text(size = 12),
                axis.title.x = element_blank(),
                axis.title.y = element_text(size = 14))
      }
      
      
            grid::grid.draw(gridExtra:::rbind_gtable(ggplotGrob(totalline),ggplotGrob(totalbar)))
      
      
  })
  
  

  

  
  
  

  
  output$contactinfo <- output$datasources <- renderUI({
    str1 <- paste("This is a project in progress developed for a University of Washington Information School capstone project
                  by Tim Blankemeyer, Emma Clarke, and Katrina Gertz. Any questions or comments can be directed to Leska Fore 
                  from the Puget Sound Partnership at leska.fore@psp.wa.gov.")

    
    HTML(paste(str1[1],  sep = "<p/>"))  
  })
  
  
## cohensd ##
  
  sub_cd <- subset(df1, !is.na(df$cohensd_huc_mean))
  sub_cd1 <- subset(sub_cd, !is.na(df$coloreffect))
  sub_cd2 <- reactive({subset(sub_cd1, result_type %in% input$mapfeatures1)})
  mid_cd <- reactive({subset(sub_cd2(), HUC_level %in% input$huclevel)})
  cd_sub <- reactive({aggregate(cohensd_huc_mean ~ HUC_Name, mid_cd(), "mean")})
  #cd_sub()$pos_neg <- reactive({ifelse(cd_sub()$cohensd_huc_mean > 0, "positive", "negative")})
  highlight_bar <- reactive({subset(cd_sub(), HUC_Name == clickdata$clickedShape)})
  

  
  pos_neg <- reactive({ifelse(cd_sub()$cohensd_huc_mean >= .2, 
                              ifelse(cd_sub()$cohensd_huc_mean >= .8,
                                     "big_improvement", 
                                     "small_improvement"),
                              ifelse(cd_sub()$cohensd_huc_mean <= -.8,
                                     "large_decline",
                                     ifelse(cd_sub()$cohensd_huc_mean > -.2,
                                            "no_change",
                                            "small_decline"))
)})
  
  hi_color <- reactive({ifelse(highlight_bar()$cohensd_huc_mean >= .2, 
                              ifelse(highlight_bar()$cohensd_huc_mean >= .8,
                                     "big_improvement", 
                                     "small_improvement"),
                              ifelse(highlight_bar()$cohensd_huc_mean <= -.8,
                                     "large_decline",
                                     ifelse(highlight_bar()$cohensd_huc_mean > -.2,
                                            "no_change",
                                            "small_decline"))
  )})
  
# } else if(cb_output() == TRUE){
#   c('#4dac26', '#b8e186', '#f7f7f7', '#f1b6da', '#d01c8b')
# } else{
#   c('#1a9641', '#a6d96a', '#d3d3d3', '#fdae61', '#d7191c')
# },

  scale_values <- reactive({if(cb_output() == FALSE){
    c(big_improvement = "#1a9641", small_improvement = "#a6d96a", no_change = "#d3d3d3", small_decline = "#fdae61", large_decline = "#d7191c")}
    else{c(big_improvement = "4dac26", small_improvement = "#b8e186", no_change = "#f7f7f7", small_decline = "#f1b6da", large_decline = "#d01c8b")}
  })
 
  
  cohens_fill <- reactive({if (cb_output() == TRUE){
    shapefile()$colorblind
  } else{
    shapefile()$coloreffect}})
  
  # scale_breaks <- reactive({if(input$mapfeatures1 == "Chum Salmon"){c("increase", "decrease")}
  #   else{c("decrease", "increase")}})
  
  output$cohensd_plot <- renderPlot({
    ggplot() +
      geom_col(data = cd_sub(),
               aes(x = HUC_Name,
               y = cohensd_huc_mean,
               fill = pos_neg()),
               alpha = .7) +
      geom_col(data = highlight_bar(),
               aes(x = HUC_Name,
                   y = cohensd_huc_mean,
                   fill = hi_color()),
               alpha = 1) +
      scale_fill_manual(values = scale_values(),
                        breaks = c("big_improvement", "small_improvement", "no_change", "small_decline", "large_decline"),
                        labels = c("large improvement", "small improvement", "no change", "small decline", "large decline"),
                        drop = FALSE) +
      guides(fill = guide_legend(title = paste(sep = "", "Change in\n", input$mapfeatures1))) +
      labs(x = "HUC Name",
           y = "Cohen's D",
           title = paste("Change Statistic By HUC", input$huclevel)) +
      scale_y_continuous(breaks = pretty_breaks(6)) + 
      scale_x_discrete(labels = function(x) str_wrap(str_replace_all(x, " ", " "), width = 9)) +
      theme(plot.title = element_text(hjust = .5, size = 14, face = "bold"),
      plot.subtitle = element_text(hjust = .5, size = 12),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12))
  })
  
  
  
  # DOWNLOAD DATA 
  # remove columns from downloaded dataset that are not relevant to the
  # selected result type (columns where every value is 'NA')
  data_to_download <- reactive({
    df1 %>% 
      dplyr::filter(result_type %in% input$result_type) %>%
      select_if(colSums(!is.na(.)) > 0)
  })
  output$downloadData <- downloadHandler(
    filename = function() { paste(input$result_type, '.csv', sep='') },
    content = function(file) {
      write.csv(data_to_download(), file)
    }
  )
  
}
# Run the application 
shinyApp(ui = ui, server = server)

