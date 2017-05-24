library(shiny)
library(leaflet)
library(shinythemes)
library(tidyverse)
library(stringr)
library(scales)

# df <- readRDS("./data/all-dfs.rds")
state <- readRDS("./data/WAstate.rds")

ui <- navbarPage(theme = shinytheme("sandstone"),
                 "Effectiveness and Evaluation Tool",
                 tabPanel("Is it scalable?",
                          fluidRow(titlePanel("Is the data processing pipeline scalable?")),
                          # map output
                          fluidRow(mainPanel(
                            p("We wanted to test the scalability of our data processing pipeline, so 
                              we downloaded the statewide NHD watershed shapefiles (HUC 8), as well as
                              the statewide water quality dataset from EIM (tubidity, NTU) and the
                              statewide investment data from Habitat Work Schedule. As you can see, 
                              the data processing pipeline can include external data sources with minor 
                              tweaking."),
                            tags$style(type = "text/css", "#WAmap {height: calc(100vh - 100px) !important;}"),
                            leafletOutput("WAmap", width="90%"),
                            width = "80%"))
                          )
                 
                 
)


server <- function(input, output, session) {
  
  huc8_df <- readRDS("./data/WAhuc8.rds")
  water8 <- state %>% filter(HUC_level == '8') %>%
    distinct(HUC_id, coloreffect)
  
  state.shapefile <- sp::merge(x = huc8_df, y = water8[ , c("HUC_id","coloreffect")],
                         by.x = "HUC8", by.y = "HUC_id", all.x=TRUE, duplicateGeoms = TRUE)
  
  state.shapefile$fill_opacity <- ifelse(is.na(state.shapefile$coloreffect), 0, .7)
  
  output$WAmap <-  renderLeaflet({leaflet() %>%
                                   setView(lng = -120.7401, lat = 48, zoom = 7) %>%
                                   addProviderTiles("Stamen.Terrain",
                                                    options = providerTileOptions(minZoom = 5)) %>%
                                   # use the shapefile() df
                                   addPolygons(data = state.shapefile,
                                               popup = paste(sep = "", "<b>HUC: </b>", state.shapefile$Name),
                                               # color according to increases/decreases as defined by coloreffect column
                                               fillColor = ~state.shapefile$coloreffect,
                                               fillOpacity = ~state.shapefile$fill_opacity,
                                               color = "black",
                                               weight = 1) %>%
                                   addLegend(colors = c('#1a9641','#a6d96a','#d3d3d3','#fdae61','#d7191c'),
                                             labels = c('large improvement', 'small improvement', 'no change',
                                                        'small decline',  'large decline'),
                                             position = 'bottomright',
                                             title = 'Turbidity')})
  
  
}
# Run the application 
shinyApp(ui = ui, server = server)

