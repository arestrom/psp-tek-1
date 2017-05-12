library(shiny)
library(leaflet)
library(shinythemes)
library(tidyverse)
library(stringr)
library(scales)
library(gtable)
df <- readRDS("./data/all-dfs.rds")

ui <- navbarPage(theme = shinytheme("sandstone"),
                 "Opening Up the Data",
                 # first tab panel for HUCs
                 tabPanel("Map View",
                          sidebarLayout(
                            sidebarPanel(
                              # select features to view effects
                              radioButtons(inputId = "mapfeatures1",
                                           label = "Select variable:",
                                           c("Chum Salmon", "Turbidity", "TSS", "Investment")),
                              # select HUC level
                              radioButtons(inputId = "huclevel",
                                           label = "HUC level:",
                                           c(10, 12)),
                              radioButtons(inputId = "projects",
                                                 label = "Show investment sites from:",
                                                 c("PRISM", "EAGL", "Both"),
                                           selected = "Both"),
                              checkboxInput(inputId = "colorblind",
                                           label = "colorblind friendly",
                                           value = FALSE),
                              actionButton("button", "Reset graphs"),
                              br(),
                              br(),
                              "USER GUIDE GOES HERE",
                              
                              width = 2
                            ),
                            # map output
                            mainPanel(
                              fluidRow(
                              splitLayout(cellWidths = c("52%", "1%", "47%"),
                              leafletOutput("hucmap",
                                                    height = 600
                                                    # width = 550
                                            ),
                              br(),
                            #plotOutput("measure_plot"),

                            plotOutput("inv_plot",
                                        height = 600)),
                            br(),
                            splitLayout(cellWidths = c("34%", "1%", "65%"),
                                        wellPanel("DYNAMIC TEXT GOES HERE"),
                                        br(),
                                        plotOutput("cohensd_plot",
                                                   height = 350)))
                            
                              
                            #renderText("click_text"))
                          )
                          ))
                 
                 
)


server <- function(input, output, session) {
  
### map code ###
  # read in the shapefiles
  huc10_df <- readRDS("./data/huc10.rds")
  huc12_df <- readRDS("./data/huc12.rds")
  
  # reactive df for those features in the input
  
  df1 <- df[df$HUC_id %in% huc10_df$HUC10 | df$HUC_id %in% huc12_df$HUC12, ]
  df1 <- subset(df1, !is.na(df1$measurement))


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

river_mouths <- subset(df, result_type == "Chum Salmon")
river_popup <- reactive({
  paste("Mouth of",
        river_mouths$name)
})

cb_output <- reactive({input$colorblind})

fishIcon <- makeIcon(iconUrl = "./data/salmon3.png",
                     iconWidth = 25, iconHeight = 25,
                     iconAnchorX = 10, iconAnchorY = 10)

# filter project points
proj_df <- reactive({
  if (input$projects == "Both") {
    inv_subset
  } else {
  subset(inv_subset, project_source %in% input$projects)}
})

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
                                         addProviderTiles("Stamen.Terrain") %>%
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

                                     addCircleMarkers(data = proj_df(),
                                                      ~lon, ~lat,
                                                      fillColor = ifelse(proj_df()$project_source == "PRISM","#696969", "#A9A9A9"),
                                                      stroke = FALSE,
                                                      fillOpacity = .7,
                                                      radius = 4,
                                                      popup = projectpopup()) %>%
                                     addCircleMarkers(data = river_mouths,
                                                      ~lon, ~lat,
                                                      fillColor = "black",
                                                      stroke = FALSE,
                                                      fillOpacity = 1,
                                                      radius = 4,
                                                      popup = river_popup()
                                     ) %>%
                                     addLegend(colors = if(input$mapfeatures1 == "Investment"){
                                                          c('#deebf7', '#9ecae1', '#3182bd')
                                                        } else if(cb_output() == TRUE){
                                                          c('#4d9221', '#a1d76a', '#e6f5d0', '#fde0ef', '#e9a3c9', '#c51b7d')
                                                        } else{
                                                          c('#1a9850', '#91cf60', '#d9ef8b', '#fee08b', '#fc8d59', '#d73027')
                                                        },
                                               labels = if(input$mapfeatures1 == "Investment"){
                                                          c("small", "medium", "large")
                                                        } else if(input$mapfeatures1 == "Chum Salmon") { 
                                                          c('large increase', 'medium increase', 'small increase', 
                                                            'small decrease', 'medium decrease', 'large decrease')
                                                        } else{
                                                          c('large decrease', 'medium decrease', 'small decrease',
                                                            'small increase', 'medium increase', 'large increase')
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
                                               opacity = .7) %>%
                                     addLegend(colors = c("black", "#696969", "#A9A9A9"),
                                               labels = c("Salmon Count Sites", "PRISM Projects", "EAGL Projects"),
                                               
                                               opacity = 1,
                                               position = 'bottomright')
  )
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
           print(proj_df()$name.x))
   })
   
   
   observeEvent(input$hucmap_shape_click, {
     clickdata$clickedShape <- input$hucmap_shape_click$id
   })
   
   observeEvent(input$hucmap_shape_click, {
     clickdata$clickedShape <- NULL
   })
   
   click_text <- renderPrint({
     print(clickdata$clickedShape)
   })
   
   clicked_sub <- reactive({
     subset(df, HUC_Name == input$hucmap_shape_click$id)
   })
   
   
   
   
   # 
   clicked_mid <- reactive({subset(clicked_sub(), year > "2002")})
   clicked_feat <- reactive({subset(clicked_mid(), result_type %in% input$mapfeatures1)})
   clicked_year <- reactive({if(input$mapfeatures1 == "Chum Salmon"){
     aggregate(measurement ~ year, clicked_feat(), "sum")} else{
       aggregate(measurement ~ year, clicked_feat(), "mean") }
     })
   # 
   # output$clickedplot <- renderPrint({
   #   ggplot(data = clicked_sub(),
   #          x = year,
   #          y = measurement) +
   #     geom_col()
   # })
  
### end HUC code ###  
  
  

### investment code
  
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
    if(10 %in% input$huclevel){
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
      if (input$projects == "Both") {
        only_inv
      } else {
        subset(only_inv, project_source %in% input$projects)}
    })
  inv_middle <- reactive({subset(inv_year_mid(), HUC_level == "10")})
  inv_by_year <- reactive({aggregate(measurement ~ year, inv_middle(), "sum")})
  
  clicked_inv_huc <- reactive({subset(inv_year_mid(), HUC_Name == input$hucmap_shape_click$id)})
  clicked_inv <- reactive({aggregate(measurement ~ year, clicked_inv_huc(), "sum")})
  
  
  # for the variables
  mid_year_sub <- reactive({subset(reactive_subset(), year > "2002")})
  year_sub <- reactive({subset(mid_year_sub(), HUC_level == "10")})
  measure_year <- reactive({if(input$mapfeatures1 == "Chum Salmon"){
    aggregate(measurement ~ year, year_sub(), "sum")} else {
      aggregate(measurement ~ year, year_sub(), "mean")
    }})
  
  #plot_df <- reactive({merge(inv_by_year, measure_year(), by = "year")})
  
  output$inv_plot <- renderPlot({
      totalbar <- ggplot() +
                  geom_col(data = if(nrow(clicked_inv_huc()) > 0){
                    clicked_inv()} else{ 
                      inv_by_year()},
                           aes(x = year,
                               y = measurement/1000000),
                           fill = "#3182bd",
                    width = .8) +
                   scale_x_continuous(breaks = pretty_breaks(20),
                          limits = c(2003, 2016)) +
      scale_y_continuous(breaks = pretty_breaks(6)) +
      labs(x = "Year",
           y = "Total Investment (millions of dollars)",
           title = if(input$projects == "Both"){"Total Investment By Year"}
           else {paste(sep = "", "Total Investment By Year", " (", input$projects, " Data)")},
           subtitle = if(nrow(clicked_inv_huc()) > 0){
             paste(input$hucmap_shape_click$id)} else{
               "All HUCs"
             }) +
      theme(plot.title = element_text(hjust = .5, size = 14, face = "bold"),
            plot.subtitle = element_text(hjust = .5, size = 12),
            axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
            axis.text.y = element_text(size = 12),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14))
      
      totalline <- ggplot(data = if(nrow(clicked_sub()) > 0) {
        clicked_year()} else{
          measure_year()
        },
                         aes(x = year,
                             y = if(input$mapfeatures1 == "Investment"){measurement/1000000} else {measurement})) +
        geom_point() +
        geom_line() +
        scale_x_continuous(breaks = pretty_breaks(nrow(measure_year())),
                           limits = c(2003, 2016)) +
        scale_y_continuous(breaks = pretty_breaks(6)) +
        labs(x = "Year",
             y = if(input$mapfeatures1 == "Chum Salmon"){
               "Chum Salmon Count"
             } else if(input$mapfeatures1 == "Investment"){
               paste("Average", input$mapfeatures1, "(millions of dollars)")} else{
               paste(sep = "", "Average ", input$mapfeatures1, " (", reactive_subset()$unit,")")
             },
             title = if(input$mapfeatures1 == "Chum Salmon"){ 
               "Total Chum Salmon By Year"} else{
                 paste("Average", input$mapfeatures1, "By Year")
               },
             subtitle = ifelse(nrow(clicked_sub()) > 0, paste(input$hucmap_shape_click$id), 
                      "All HUCs")
                      ) +
        theme(plot.title = element_text(hjust = .5, size = 14, face = "bold"),
              plot.subtitle = element_text(hjust = .5, size = 12),
              axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
              axis.text.y = element_text(size = 12),
              axis.title.x = element_text(size = 14),
              axis.title.y = element_text(size = 14))
      
      
      
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
  highlight_bar <- reactive({subset(cd_sub(), HUC_Name == input$hucmap_shape_click$id)})
  
  
  output$cohensd_plot <- renderPlot({
    ggplot() +
      geom_col(data = cd_sub(),
               aes(x = HUC_Name,
               y = cohensd_huc_mean),
               fill = "#3182bd") +
      geom_col(data = highlight_bar(),
               aes(x = HUC_Name,
                   y = cohensd_huc_mean),
                   fill = "gold") +
      labs(x = "HUC Name",
           y = "Cohen's D",
           title = paste("Cohen's D Value By HUC", input$huclevel)) +
      scale_y_continuous(breaks = pretty_breaks(6)) + 
      scale_x_discrete(labels = function(x) str_wrap(str_replace_all(x, " ", " "), width = 9)) +
      theme(plot.title = element_text(hjust = .5, size = 14, face = "bold"),
      plot.subtitle = element_text(hjust = .5, size = 12),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14))
  })
  
}
# Run the application 
shinyApp(ui = ui, server = server)

