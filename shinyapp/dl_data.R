library(shiny)
library(leaflet)
library(shinythemes)
library(tidyverse)
library(stringr)
# library(scales)
# library(gtable)
df <- readRDS("./data/all-dfs.rds")

ui <- fluidPage(navbarPage(theme = shinytheme("sandstone"),
                 "Opening Up the Data",
                  tabPanel("linksETC",
                          wellPanel(
                            # fluidRow(column(3, htmlOutput("pic"))),
                            # fluidRow(column(3, tags$img(src = "https://www.rstudio.com/wp-content/uploads/2014/03/blue-125.png"))),
                            # fluidRow(column(3, tags$img(src = "./www/darklegend.png"))),
                            # fluidRow(column(3, tags$img(src = "./darklegend.png"))),
                            # fluidRow(column(3, tags$img(src = "http://localhost/shinyapp/www/darklegend.png"))),
                            # fluidRow(column(3, tags$img(src = "http://localhost/darklegend.png"))),
                            # fluidRow(column(3, imageOutput("myImage"))),
                            fluidRow(column(10, includeHTML('about.html')))
                          )
                  ),
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
                              # DOWNLOAD DATA
                              selectInput("result_type", "Choose a dataset:", 
                                          choices = c("Chum Salmon", "Turbidity", "TSS", "Investment")),
                              downloadButton('downloadData', 'Download'),
                              # adding the new div tag to the sidebar            
                              tags$div(class="header", checked=NA,
                                       tags$p("Ready to take the Shiny tutorial? If so"),
                                       tags$a(href="shiny.rstudio.com/tutorial", "Click Here!")
                              ),
                              # DOWNLOAD DATA
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
                          )
                        )
))

server <- function(input, output, session) {
  
  ### map code ###
  # read in the shapefiles
  huc10_df <- readRDS("./data/huc10.rds")
  huc12_df <- readRDS("./data/huc12.rds")
  
  # reactive df for those features in the input
  
  df1 <- df[df$HUC_id %in% huc10_df$HUC10 | df$HUC_id %in% huc12_df$HUC12, ]
  df1 <- subset(df1, !is.na(df1$measurement))
  
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
  
  output$pic <- renderUI({
    HTML("<div>PRISM projects <img src = '/images/darklegend.png'/></div>")
  })
 
  output$myImage <- renderImage({
    # When input$n is 3, filename is ./images/image3.jpeg
    filename <- normalizePath(file.path('./darklegend.png'))
    
    # Return a list containing the filename and alt text
    list(src = filename)
    
  }, deleteFile = FALSE)
   
}

# Run the application 
shinyApp(ui = ui, server = server)
