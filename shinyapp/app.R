#===============================================================================================
# Shinyapps.io link: https://ejclarke.shinyapps.io/capstone/
#
#===============================================================================================


library(shiny)
library(leaflet)
library(shinythemes)
library(tidyverse)
library(stringr)
library(scales)
library(gtable)
library(grid)
library(gridExtra)
library(gridBase)

df <- readRDS("./data/all-dfs.rds")
# rename "Chum Salmon" to "Summer Chum"
df$result_type[df$result_type == "Chum Salmon"] <- "Summer Chum"


ui <- navbarPage(theme = shinytheme("sandstone"),
                 "Effectiveness and Evaluation Tool",
                 # first tab panel for HUCs
                 tabPanel("Map View",
                          sidebarLayout(
                            sidebarPanel(
                              #img(src = "eet-banner.png", width = "100%"),
                              HTML("<img src = 'eet-banner.png' width = '100%'/>"),
                              # select features to view effects
                              br(),
                              br(),
                              radioButtons(inputId = "mapfeatures1",
                                           label = "Select variable:",
                                           c("Summer Chum", "Turbidity", "TSS", "Investment")),
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
                                          choices = c("Summer Chum", "Turbidity", "TSS", "Investment")),
                              downloadButton('downloadData', 'Download', style='padding:4px; font-size:80%;'),
                              # br(),
                              # br(),
                              # "USER GUIDE GOES HERE",
                              br(),
                              br(),
                              HTML("<img src = 'psp-logo.png' width = '100%'/>"),
                              br(),
                              br(),
                              HTML("<img src = 'gsro-logo.png' width = '100%'/>"),
                              br(),
                              br(),
                              HTML("<img src = 'doe-logo.png' width = '100%'/>"),
                              br(),
                              br(),
                              HTML("<img src = 'ischool-logo.png' width = '100%'/>"),
                              br(),
                              br(),
                              HTML("<img src = 'odl-logo.png' width = '100%'/>"),


                              width = 2
                            ),
                            # map output
                            mainPanel(
                              # hide errors
                              tags$style(type="text/css",
                                         ".shiny-output-error { visibility: hidden; }",
                                         ".shiny-output-error:before { visibility: hidden; }"
                              ),
                              fluidRow(
                              splitLayout(cellWidths = c("52%", "1%", "47%"),
                              leafletOutput("hucmap",
                                                    height = 600

                                            ),
                              br(),
                            plotOutput("inv_plot",
                                        height = 600)),
                            br(),
                            splitLayout(cellWidths = c("70%", "30%"),
                                        plotOutput("cohensd_plot",
                                                   height = 350),
                                        wellPanel(
                                          uiOutput("description"))),
                            # wellPanel(uiOutput("description", width = '20%')),
                            br(),
                            br(),
                            br(),
                            width = 9


                            #renderText("click_text"))
                          ))
                          )),
                 tabPanel("About",
                          sidebarLayout(position = "right",
                  sidebarPanel(htmlOutput("contact"),
                               br(),
                               uiOutput("acronyms"),
                               br(),
                               uiOutput("caveat"),
                               width = 2),
                  mainPanel(
                  wellPanel(
                    uiOutput("about")
                  ),
                  width = 10))
                 )


)


server <- function(input, output, session) {

### map code ###
  # read in the shapefiles
  huc10_df <- readRDS("./data/huc10.rds")
  huc12_df <- readRDS("./data/huc12.rds")

  # reactive df for those features in the input

  # if you want to filter on natural/hatchery chum count, you can do that here
  df1 <- df[df$HUC_id %in% huc10_df$HUC10 | df$HUC_id %in% huc12_df$HUC12, ]
  df1 <- subset(df1, !is.null(df1$measurement))
  df1$project_source[df1$result_type == "Summer Chum"] <- "River Mouths"

  df1$color[df1$project_source == "PRISM"] <- "#454546"
  df1$color[df1$project_source == "EAGL"] <-  "#838284"
  #df1$color[df1$project_source == "HWS"] <- "#636264"


  # create checkbox
  output$checkbox <- renderUI({
    checkboxGroupInput("projects",
                       "Select sites: ",
                       choiceNames = list(HTML("<div>PRISM projects <img src = 'darklegend.png'/></div>"),
                                          HTML("<div>EAGL projects <img src = 'lightlegend.png'/></div>"),
                                          HTML("<div>Chum sites <img src = 'squarelegend.png'/></div>")),
                       choiceValues = list("PRISM",
                                           "EAGL",
                                           "River Mouths"))
  })
  # update to null on HUC level change
    observeEvent(input$huclevel, {
      updateCheckboxGroupInput(session,
                               "projects",
                         "Select sites: ",
                         choiceNames = list(HTML("<div>PRISM projects <img src = 'darklegend.png'/></div>"),
                                            HTML("<div>EAGL projects <img src = 'lightlegend.png'/></div>"),
                                            HTML("<div>Chum sites <img src = 'squarelegend.png'/></div>")),
                         choiceValues = list("PRISM",
                                             "EAGL",
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
                                                  HTML("<div>Chum sites <img src = 'squarelegend.png'/></div>")),
                               choiceValues = list("PRISM",
                                                   "EAGL",
                                                   "River Mouths"),
                               selected = NULL)
    })


# define reactive df based on mapfeatures1 input
reactive_subset <- reactive({subset(df1, result_type %in% input$mapfeatures1)})

# further subset that df to filter only the HUCs selected from huclevel
mid_df <- reactive({subset(reactive_subset(), HUC_level %in% input$huclevel)})

# remove all duplicates (returns only one row per HUC that has enough measurements in the measurement column)
huc_df <- reactive({mid_df()[!duplicated(mid_df()$HUC_id),]})

# dynamically merge the shapefile and the df for mapping
shapefile <- reactive({
  # HUC 10 dataframe
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
    #HUC 12 dataframe
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


# popup for river mouths
river_popup <- reactive({
  paste("Mouth of",
        chum_points()$name)
})

# outputs TRUE or FALSE based on
# whether or not the colorblind option is selected
cb_output <- reactive({input$colorblind})

# filter project points by result type
factor_df <- subset(df1, result_type %in% c("Summer Chum", "Investment"))
proj_df <- reactive({subset(factor_df, project_source %in% input$projects)})

# dynamically create the fill color for hucmap
# pulls from the colorblind column if the colorblind radio
# button is selected; else, pulls from coloreffect column
fill_color <- reactive({if(input$mapfeatures1 == "Investment"){
  ~invcolor()(as.numeric(var_inv()))
} else if (cb_output() == TRUE){
  shapefile()$colorblind
} else{
  shapefile()$coloreffect}})

# dynamically create the fill opacity for hucmap
# if there is a valid Cohen's D for the HUC, fill will
# be at 70% opacity; otherwise, it's invisible
fill_opacity <- reactive({if(input$mapfeatures1 == "Investment"){
  ifelse(is.na(shape_inv()$totalinv), 0, .7)} else{
    ifelse(is.na(shapefile()$coloreffect), 0, .7)}})

# initiate clickdata$clickedShape as NULL
# NOTE: this MUST be initiated before the map for
# clicking on HUCs to work
clickdata <- reactiveValues(clickedShape = NULL)

# create the HUC map
   output$hucmap <- renderLeaflet( m <-  leaflet() %>%
                                         setView(lng = -122.91, lat = 47.775, zoom = 9) %>%
                                         addProviderTiles("Stamen.Terrain",
                                                          # prevent the user from zooming out too far
                                                          options = providerTileOptions(minZoom = 8)) %>%
                                         # use the dynamically created shapefile() dataframe
                                         addPolygons(data = shapefile(),
                                                     # this is defined below
                                                     popup = popup_content(),
                                                     # color as defined above
                                                     fillColor = fill_color(),
                                                     # opacity as defined above
                                                     fillOpacity = fill_opacity(),
                                                     # create borders
                                                     color = "black",
                                                     weight = 1,
                                                     opacity = 1,
                                                     # give the layer a unique ID
                                                     # NOTE: this MUST be here for interactivity to work
                                                     layerId = ~shapefile()$Name
                                                     ) %>%
                                        # create dynamic legend based on what the map is showing
                                        addLegend(colors = if(input$mapfeatures1 == "Investment"){
                                                           # three color scale for investments
                                                           c('#deebf7', '#9ecae1', '#3182bd')
                                                         } else if(cb_output() == TRUE){
                                                           # colorblind friendly color scale
                                                           c('#4dac26', '#b8e186', '#f7f7f7', '#f1b6da', '#d01c8b')
                                                         } else{
                                                           # regular color scale
                                                           c('#1a9641', '#a6d96a', '#d3d3d3', '#fdae61', '#d7191c')
                                                         },
                                                  # investment OR measurement labels based on input
                                               labels = if(input$mapfeatures1 == "Investment"){
                                                          c("small", "medium", "large")
                                                        } else{
                                                          c('large improvement', 'small improvement', 'no change',
                                                            'small decline', 'large decline')
                                                        },
                                               position = 'topright',
                                               # create dynamic legend title based on map features
                                               title = ifelse(input$mapfeatures1 == "Investment",
                                                              paste("Total Investment"),
                                                              ifelse(input$mapfeatures1 == "Summer Chum",
                                                              paste('Change in',
                                                                    br(),
                                                             input$mapfeatures1),
                                                             paste("Change in",
                                                                   input$mapfeatures1)
                                                             )),
                                               # match legend opacity to opacity of polygons
                                               opacity = .7)
  )

   # subset projects into project source dataframes for easier plotting
   eagl_prism <- reactive({subset(proj_df(), project_source %in% c("EAGL", "PRISM"))})
   chum_points <- reactive({subset(proj_df(), project_source %in% c("River Mouths"))})

   # points
   observe({
     # clear all markers if nothing is selected in projects
     if(is.null(input$projects)){
       leafletProxy("hucmap") %>%
         clearMarkers()
     } else {
       leafletProxy("hucmap") %>%
         # you need to clear the markers first, every time
         clearMarkers() %>%
         # add EAGL and PRISM data
         addCircleMarkers(data = eagl_prism(),
                    ~lon, ~lat,
                    radius = 3,
                    fillOpacity = 1,
                    # fill based on the color column
                    fillColor = eagl_prism()$color,
                    stroke = FALSE,
                    # show project name on click
                    popup = projectpopup()) %>%
        # add fish measurement sites
        addMarkers(data = chum_points(),
                ~lon, ~lat,
                # use the icon defined below
                icon = ~fishIcon,
                # use the popup defined above
                popup = river_popup()
                )
    }

   })

   # create an icon for the fish measurement sites
   # if you want all icons to be circles, you can consolidate
   # the eagl_prism and chum_points dataframes and just use
   # addCircleMarkers - this will load faster and be easier to maintain
   fishIcon <- makeIcon(iconUrl = 'black-square.png',
                        iconWidth = 7, iconHeight = 7,
                        iconAnchorX = 5, iconAnchorY = 5)


   # variable that defines the text for the HUC popup
   popup_content <- reactive({
     paste(sep = "",
           # HUC
           "<b>HUC: </b>",
           print(shapefile()$Name)
           )

   })

   # variable that defines the text for the project popup
   projectpopup <- reactive({
     # CSS keeps text from stretching the popup and forcing a scroll bar
     paste("<style> div.leaflet-popup-content
           {width: auto !important;
           }</style>",
           "<b>Project: </b>",
           br(),
           print(proj_df()$name))
   })


   ################################################
   # BEGIN CODE TO ALLOW LINKING GRAPHS TO HUCMAP #

   # define clickdata$clickedShape as the ID from the clicked HUC
   # this ID will be whatever you define it as in the map code
   # in this case, it's the HUC name
   observeEvent(input$hucmap_shape_click, {
     clickdata$clickedShape <- input$hucmap_shape_click$id
   })

   # clear the clickedShape if the user clicks outside
   # the borders of a HUC
   observeEvent(input$hucmap_click, {
     clickdata$clickedShape <- NULL
   })

   # clear the clickedShape if the user changes the features to view
   observeEvent(input$mapfeatures1, {
     clickdata$clickedShape <- NULL
   })

   # clear the clickedShape if the user switches betwee HUC levels
   observeEvent(input$huclevel, {
     clickdata$clickedShape <- NULL
   })

   # clear the clickedShape if the user clicks the RESET button
   observeEvent(input$button, {
     clickdata$clickedShape <- NULL
   })

   # subset the dataframe that has HUCs from the select level
   # to include only that HUC that has been clicked on
   # we're matching HUC_Name to the layer ID from the hucmap
   clicked_sub <- reactive({
     subset(mid_df(), HUC_Name == clickdata$clickedShape)
   })

   # filter to include only 2003 onwards
   # this is because we only have investment data from 2003 - 2016
   # and we want the x-axes to match up
   clicked_mid <- reactive({subset(clicked_sub(), year > "2002")})

   # subset to include only the result type indicated in the radio button input
   clicked_feat <- reactive({subset(clicked_mid(), result_type %in% input$mapfeatures1)})

   # aggregate by year
   # if Summer Chum, get the total
   # if water quality or investements, get the average
   clicked_year <- reactive({if(input$mapfeatures1 == "Summer Chum"){
     aggregate(measurement ~ year, clicked_feat(), "sum")} else{
       aggregate(measurement ~ year, clicked_feat(), "mean") }
     })

   # END LINKING CODE #
   ################################################


### investment code

  # subset the original dataframe to only show Investment data
  only_inv <- subset(df1, result_type == "Investment")
  # find the total investment for each HUC
  group_inv <- aggregate(measurement ~ HUC_id, only_inv, "sum")
  # find the average investment for each HUC
  avg_df <- aggregate(measurement ~ HUC_id, only_inv, "mean")
  # count the number of projects in each HUC
  count_inv <- aggregate(name ~ HUC_id, only_inv, function(x){NROW(x)})

  # merge the above dataframes
  inv_subset <- merge(only_inv, group_inv, by = "HUC_id")
  inv_subset <- merge(inv_subset, count_inv, by = "HUC_id")
  inv_subset <- merge(inv_subset, avg_df, by = "HUC_id")
  # remove NAs
  inv_subset <- subset(inv_subset, !is.na(inv_subset$measurement.y))

  # create new dataframe with only these columns
  new_inv <- subset(inv_subset, select = c("HUC_id", "HUC_level", "measurement.y", "name.x", "HUC_Name", "measurement"))
  # rename columns so they're easier to call
  new_inv <- rename(new_inv, totalinv = measurement.y, projectcount = name.x, avginv = measurement)
  # remove all duplicates (one set of measurements per HUC)
  new_inv <- new_inv[!duplicated(new_inv$HUC_id),]

  # subset investment dataframe based on HUC level input
  shape_inv <- reactive({
    if("10" %in% input$huclevel){
      inv_10 <- sp::merge(x = huc10_df, y = new_inv, by.x = "HUC10", by.y = "HUC_id", all.x=TRUE, duplicateGeoms = TRUE)
    } else{
      inv_12 <- sp::merge(x = huc12_df, y = new_inv, by.x = "HUC12", by.y = "HUC_id", all.x=TRUE, duplicateGeoms = TRUE)
    }
  })

  # color variables for the investment map
  var_inv <- reactive({shape_inv()$totalinv})
  invcolor <- reactive({colorQuantile("Blues", shape_inv()$totalinv, n = 3)})

  # subset for the investment graphs
  # if the user has selected one of the project sources,
  # subset to only include that source; else, show all projects
  inv_year_mid <- reactive({
      if ("PRISM" %in% input$projects | "EAGL" %in% input$projects) {
        subset(only_inv, project_source %in% input$projects)
      } else {
        only_inv}
    })

  # subset only one set of HUCs
  # since this is for total, you could use HUC10 or HUC12;
  # we just want the total investements per year
  inv_middle <- reactive({subset(inv_year_mid(), HUC_level == "10")})
  # get the total per year
  inv_by_year <- reactive({aggregate(measurement ~ year, inv_middle(), "sum")})

  # subset the reactive dataframe created in line 461
  # to only include the HUC that the user clicks on
  clicked_inv_huc <- reactive({subset(inv_year_mid(), HUC_Name == clickdata$clickedShape)})
  # find the total investments per year for that HUC
  clicked_inv <- reactive({aggregate(measurement ~ year, clicked_inv_huc(), "sum")})

# map features data
  # subset the dataframe containing the selected variables
  # again, we're using 2002 because we only have investment data from 2003 onwards
  mid_year_sub <- reactive({subset(reactive_subset(), year > "2002")})
  # subset based on HUC level
  year_sub <- reactive({subset(mid_year_sub(), HUC_level %in% input$huclevel)})
  # this is for the overall line graph; it contains the total (for salmon)
  # or the average (for everything else) across all HUCs
  measure_year <- reactive({if(input$mapfeatures1 == "Summer Chum"){
    aggregate(measurement ~ year, year_sub(), "sum")} else {
      aggregate(measurement ~ year, year_sub(), "mean")
    }})

  # create an empty dataframe to be called in geom_empty() below
  # this will allow for a blank graph to show when there is no current
  # data to graph
  empty_df <- data.frame()

  # create the graphs on the right side of the page
  output$inv_plot <- renderPlot({
      # create the bar chart
      # if no HUCs have been clicked
      totalbar <- if(is.null(clickdata$clickedShape)) {
        # bar chart for all HUCs
        ggplot() +
                  # use the sum of investments across all HUC10s
                  geom_col(data = inv_by_year(),
                           aes(x = year,
                               # divide the measurement by 1 million
                               # for easier graphing
                               y = measurement/1000000),
                           fill = "#136ca6",
                    # column width (percentage of total available space per column)
                    width = .8) +
                   # DON'T CHANGE THIS IF YOU WANT THE X-AXES TO LINE UP BETWEEN GRAPHS
                   scale_x_continuous(breaks = pretty_breaks(20),
                          limits = c(2003, 2016)) +
      # breaks for the y-axis
      scale_y_continuous(breaks = pretty_breaks(6)) +
      # define labels
      labs(y = "Total Investment ($M)",
           # dynamic title based on the projects selected
           title = if("PRISM" %in% input$projects & "EAGL" %in% input$projects){"Total Investment"}
           else if("PRISM" %in% input$projects | "EAGL" %in% input$projects)
           {paste(sep = "", "Total Investment", " (", input$projects, " Data)")}
           else{"Total Investment"},
           subtitle = "All HUCs") +
      # define text size and colors
      theme(plot.title = element_text(hjust = .5, size = 14, face = "bold"),
            # keep color the same as bar fill for best results
            plot.subtitle = element_text(hjust = .5, size = 12, face = "bold", color = "#136ca6"),
            axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
            axis.text.y = element_text(size = 12),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 14))
        # if the user has clicked on a HUC BUT there's no data
      }else if(nrow(clicked_inv_huc()) == 0){
        # call the empty dataframe from before
        ggplot(empty_df) +
          # these limits are completely arbitrary
          # but create a grid in which to orient the text
          xlim(0, 10) +
          ylim(0, 10) +
          # center text in the middle of the grid
          geom_text(aes(x = 5,
                        y = 5),
                    label = "Data pending",
                    size = 6,
                    fontface = "italic") +
          # create title and subtitle based on project input and HUC ID
          labs(
            title = if("PRISM" %in% input$projects & "EAGL" %in% input$projects){"Total Investment"}
            else if("PRISM" %in% input$projects | "EAGL" %in% input$projects)
            {paste(sep = "", "Total Investment", " (", input$projects, " Data)")}
            else{"Total Investment"},
            subtitle = paste(input$hucmap_shape_click$id)
          ) +
          theme(plot.title = element_text(hjust = .5, size = 14, face = "bold"),
                plot.subtitle = element_text(hjust = .5, size = 12),
                # next few lines must be done explicitly
                # remove axis labels
                axis.text = element_blank(),
                # remove axis titles
                axis.title = element_blank(),
                # remove all grid lines
                panel.grid = element_blank(),
                # remove tick marks
                axis.ticks = element_blank())
        # if the user has clicked on a HUC AND that HUC has data
      } else {
        ggplot() +
          geom_col(data = clicked_inv(),
            aes(x = year,
                y = measurement/1000000),
            fill = "#3182bd",
            width = .8) +
          # add dashed line at median investment year
          # line type can be changed for aesthetics
          # keep this code below the geom_col or the line won't be visible
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

      # create line graph
      totalline <- if(is.null(clickdata$clickedShape)){
        # if no HUCs have been clicked
        ggplot(data = measure_year(),
          aes(x = year,
              # divide by a million for investments only
              y = if(input$mapfeatures1 == "Investment"){measurement/1000000} else {measurement})) +

        geom_point() +
        geom_line() +
        # this must stay the same as the x-axis in the bar graph for them to line up
        scale_x_continuous(breaks = pretty_breaks(nrow(measure_year())),
                           limits = c(2003, 2016)) +
        # labels function formats the thousands with a comma
        scale_y_continuous(breaks = pretty_breaks(6), labels = comma) +
        # create labels for y-axis and title
        labs(y = if(input$mapfeatures1 == "Summer Chum"){
               "Summer Chum (Number)"
             } else if(input$mapfeatures1 == "Investment"){
               paste("Average", input$mapfeatures1, "($M)")} else{
               paste(sep = "", "Average ", input$mapfeatures1, " (", reactive_subset()$unit,")")
             },
             title = if(input$mapfeatures1 == "Summer Chum"){
               "Summer Chum"} else if(input$mapfeatures1 == "Investment"){
                 paste("Average Investment Per Project")
               } else {
                 paste("Average", input$mapfeatures1)
               },
             subtitle = "All HUCs"
                      ) +
        theme(plot.title = element_text(hjust = .5, size = 14, face = "bold"),
              plot.subtitle = element_text(hjust = .5, size = 12, face = "bold", color = "#136ca6"),
              # angle years for easier viewing
              axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
              axis.text.y = element_text(size = 12),
              # remove x-axis title
              axis.title.x = element_blank(),
              axis.title.y = element_text(size = 14))
        # if the user has clicked on a HUC but there's not enough data
        # same as in the barchart
      }else if(nrow(clicked_feat()) == 0){
        ggplot(empty_df) +
          xlim(0, 10) +
          ylim(0, 10) +
          geom_text(aes(x = 5,
                        y = 5),
                    label = "Data pending",
                    size = 6,
                    fontface = "italic") +
          labs(
          title = if(input$mapfeatures1 == "Summer Chum"){
            "Summer Chum"} else if(input$mapfeatures1 == "Investment"){
              paste("Average Investment Per Project")
            } else {
              paste("Average", input$mapfeatures1)
            },
          subtitle = paste(input$hucmap_shape_click$id)
          ) +
          theme(plot.title = element_text(hjust = .5, size = 14, face = "bold"),
                plot.subtitle = element_text(hjust = .5, size = 12),
                axis.text = element_blank(),
                axis.title = element_blank(),
                panel.grid = element_blank(),
                axis.ticks = element_blank())
      } else{
        # if the user has clicked on a HUC with enough data
        ggplot(data = clicked_year(),
          aes(x = year,
              y = if(input$mapfeatures1 == "Investment"){measurement/1000000} else {measurement})) +
          # in this graph, call geom_vline first for a line that goes behind the point and line
          geom_vline(xintercept = clicked_feat()$medianyr, color = "red", linetype = "dashed") +
          geom_point() +
          geom_line() +
          # geom_label(aes(x = clicked_feat()$medianyr, y = 0), label = "TEXT") +
          scale_x_continuous(breaks = pretty_breaks(nrow(measure_year())),
                             limits = c(2003, 2016)) +
          scale_y_continuous(breaks = pretty_breaks(6), labels = comma) +
          labs(y = if(input$mapfeatures1 == "Summer Chum"){
            "Summer Chum (Number)"
          } else if(input$mapfeatures1 == "Investment"){
            paste("Average", input$mapfeatures1, "($M)")} else{
              paste(sep = "", "Average ", input$mapfeatures1, " (", reactive_subset()$unit,")")
            },
          title = if(input$mapfeatures1 == "Summer Chum"){
            "Summer Chum"} else if(input$mapfeatures1 == "Investment"){
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

            # create a grid that calls the two graphs on top of one another
            # substituting cbind for rbind will give you two graphs side by side
            # grid::grid.draw(gridExtra:::rbind_gtable(ggplotGrob(totalline),ggplotGrob(totalbar)))
            grid::grid.draw(gridExtra:::gtable_rbind(ggplotGrob(totalline),ggplotGrob(totalbar)))
  })

## COHEN'S D ##

  # subset original dataframe, removing NAs
  sub_cd <- subset(df1, !is.na(df$cohensd_huc_mean))
  # only use HUCs that have a color (this means they have a Cohen's D)
  sub_cd1 <- subset(sub_cd, !is.na(df$coloreffect))
  # subset based on map features
  sub_cd2 <- reactive({subset(sub_cd1, result_type %in% input$mapfeatures1)})
  # subset based on HUC level
  mid_cd <- reactive({subset(sub_cd2(), HUC_level %in% input$huclevel)})
  # find the mean of the Cohen's D within each HUC
  cd_sub <- reactive({aggregate(cohensd_huc_mean ~ HUC_Name, mid_cd(), "mean")})
  # subset for the highlighted bar - the HUC that's been clicked
  highlight_bar <- reactive({subset(cd_sub(), HUC_Name == clickdata$clickedShape)})

  # define improvement/decline based on the Cohen's D value
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

  # this doesn't make sense, but just assigning the variable doesn't
  # work, so leave it as an ifelse
  hi_color <- reactive({ifelse(highlight_bar()$cohensd_huc_mean >= .2,
                              "hi_color",
                              "hi_color"
  )})

  # create color values for the Cohen's D graph
  scale_values <- reactive({if(cb_output() == FALSE){
    # regular
    c(hi_color = "black", big_improvement = "#1a9641", small_improvement = "#a6d96a", no_change = "#d3d3d3", small_decline = "#fdae61", large_decline = "#d7191c")}
    # colorblind friendly
    else{c(hi_color = "black", big_improvement = "#4dac26", small_improvement = "#b8e186", no_change = "#f7f7f7", small_decline = "#f1b6da", large_decline = "#d01c8b")}
  })

  # create Cohen's D graph
  output$cohensd_plot <- renderPlot({
    ggplot() +
      # all bars
      geom_col(data = cd_sub(),
               aes(x = HUC_Name,
               y = cohensd_huc_mean,
               # fill based on definitions defined above
               fill = pos_neg()),
               # full opacity
               alpha = 1) +
      # use values defined above
      scale_fill_manual(values = scale_values(),
                        # define order (good on top)
                        breaks = c("big_improvement", "small_improvement", "no_change", "small_decline", "large_decline"),
                        # define what actually shows in the legend
                        labels = c("large improvement", "small improvement", "no change", "small decline", "large decline"),
                        # this SHOULD display every possible color in the legend, but it isn't working
                        drop = FALSE) +
      # graph the highlight bar
      # this is actually creating a second complete set of bars
      # over the top of the first
      # but we set the alpha to 0 so you can't see them
      geom_col(data = highlight_bar(),
               aes(x = HUC_Name,
                   y = cohensd_huc_mean,
                   # set "fill color" even though it'll be invisible
                   color = hi_color()),
               alpha = 0,
               # this is the width of the border and is the only thing visible
               size = 1) +
      # this defines the color of the border
      scale_color_manual(values = "black",
                         # keep this line to avoid the black showing up in the legend
                         guide = FALSE) +
      # define the title of the legend
      # the \n keeps the legend from stretching too far horizontally
      guides(fill = guide_legend(title = paste(sep = "", "Change in\n", input$mapfeatures1))) +
      labs(x = "HUC Name",
           y = "Cohen's D",
           title = paste("Change Statistic By HUC", input$huclevel)) +
      scale_y_continuous(breaks = pretty_breaks(6)) +
      # this will break the HUC names when they are longer than 9 characters
      scale_x_discrete(labels = function(x) str_wrap(str_replace_all(x, " ", " "), width = 9)) +
      theme(plot.title = element_text(hjust = .5, size = 14, face = "bold"),
      plot.subtitle = element_text(hjust = .5, size = 12),
      axis.text.x = element_text(size = 11),
      axis.text.y = element_text(size = 12),
      axis.title = element_text(size = 14),
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

  output$about <- renderUI({
    HTML("<h4><strong>About Us</strong></h4>
<p>Tim Blankemeyer, Emma Clarke, and Katrina Gertz are a group of MLIS Candidates from the <a href = 'https://ischool.uw.edu/'>University of Washington's Information School</a>, interested in leveraging open data and open source tools to help solve complex problems.</p>
         <p>For this project, we've partnered with the Puget Sound Partnership to design a scalable data cleaning and analysis pipeline as well as an interactive visualization prototype to show what's working to restore Puget Sound.</p>
         <br>
         <h4><strong>Acknowledgements</strong></h4>
         <p>We are grateful for the support we've received from our partners:
         <ul>
         <li>Nic Weber, University of Washington's <a href = 'https://odl.ischool.uw.edu/'>Open Data Literacy</a></li>
         <li>Leska Fore, <a href = 'http://www.psp.wa.gov/'>Puget Sound Partnership</a></li>
         <li>Keith Dublanica, <a href = 'http://www.rco.wa.gov/salmon_recovery/gsro.shtml'>Governor's Salmon Recovery Office</a></li>
         <li>Chantell Krider, <a href = 'https://www.southsoundspatial.com/'>South Sound Spatial</a></li>
         </ul>
         </p>
         <br>
         <h4><strong>Data Sources</strong></h4>
         <p>Investment Data:
         <ul>
         <li><a href = 'http://www.ecy.wa.gov/funding/EAGL.html'>Ecology's Administration of Grants and Loans (EAGL) Investment Projects:</a>
         <ul>
         <li>Note: Access to data requires log-in</li>
         </ul>
         </li>
         <li><a href = 'http://www.rco.wa.gov/prism_app/about_prism.shtml'>PRoject Information SysteM (PRISM) Investment Projects</a>
         <ul>
         <li>Note: Access to data requires log-in</li>
         </ul>
         </li>
         </li>
         </ul>
         </p>
         <p>Water Quality Data:
         <ul>
         <li><a href = 'http://www.ecy.wa.gov/eim/'>Washington State Department of Ecology's Environmental Information Management (EIM)</a>
         <ul>
         <li>Search for data limited to result parameters Turbidity and TSS, and WRIA's Kennedy-Goldsborough (14), Kitsap (15), Skokomish-Dosewallips (16), Quilcence-Snow (17), and Elwah-Dungeness (18).</li>
         </ul>
         </li>
         </ul>
         </p>
         <p>Hood Canal Summer Summer Chum Data:
         <ul>
         <li>Washington State Department of Fish and Wildlife
         <ul>
         <li><a href = 'https://fortress.wa.gov/dfw/score/score/species/population_details.jsp?stockId=2500'>Strait of Juan de Fuca Summer Chum</a>
         <li><a href = 'https://fortress.wa.gov/dfw/score/score/species/population_details.jsp?stockId=2300'>Hood Canal Summer Chum</a>
          </li>
         </ul>
         </ul>
         </p>
         <br>
         <h4><b>Image Credits</b></h4>
          Triangle icon made by <a href = 'http://www.flaticon.com/authors/daniel-bruce'>Daniel Bruce</a> from <a href = 'www.flaticon.com'>www.flaticon.com</a>
          <p>
         Circle Icons made by <a href = 'http://www.flaticon.com/authors/madebyoliver'>madebyoliver</a> from <a href = 'www.flaticon.com'>www.flaticon.com</a>

         ")
  })

  output$contact <- renderUI({
    HTML("<h4><strong>Contact</strong></h4>
<p>Any questions or comments can be directed to Leska Fore from the Puget Sound Partnership at leska.fore@psp.wa.gov</p>")
  })

  output$acronyms <- renderUI({
    HTML("<h4><strong>Acronyms</strong></h4>
         <p><b>TSS:</b> Total Suspended Solids
         <br><b>HUC: </b>Hydrologic Unit Code")
  })

  output$caveat <- renderUI({
    HTML("<h4><b>Caveat</b></h4><p>This is a prototype to show what a web based analysis tool <i>might</i> look like. The underlying data for water quality and salmon were sourced from public web sites.
    Data and results have not been vetted or approved - that is our next step.</p>")
  })

  output$description <- renderUI({
    str1 <- if(is.null(clickdata$clickedShape)){
      paste("")} else{
      paste(sep = "",
            strong("HUC Name:"),
            br(),
            clickdata$clickedShape,
            br(),
            strong("Total Investment:"),
            br(),
            if(nrow(clicked_inv_huc()) > 0){
            dollar(sum(clicked_inv()$measurement))} else {
              "Data pending"
            },
            br(),
            # strong("Median Investment Year:"),
            # br(),
            # clicked_feat()$medianyr,
            strong("Change in",
                   input$mapfeatures1,
                   ":"),
            br(),
            ifelse(clickdata$clickedShape %in% cd_sub()$HUC_Name,
            (if(clicked_feat()$coloreffect == "#1a9641"){
              "Large improvement"}
            else if (clicked_feat()$coloreffect == "#a6d96a"){
              "Small improvement"}
            else if (clicked_feat()$coloreffect == "#d3d3d3"){
              "No change"}
            else if (clicked_feat()$coloreffect == "#fdae61"){
              "Small decline"}
            else if (clicked_feat()$coloreffect == "#d7191c"){
              "Large decline"
            } else {"Not enough data to determine"}),
            "Not enough data to determine"
            ))}
    HTML(paste(str1))
  })

}
# Run the application
shinyApp(ui = ui, server = server)

