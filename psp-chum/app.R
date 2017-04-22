#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
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
library(MazamaSpatialUtils) # for lat-long to HUC conversion

# all the data manipulation bits here
# loading in spatial data, using readOGR
prj_hydro.mp <- readOGR(dsn=path.expand("./data"), layer="WBD_PRJ_HYDRO")
#  
# read in CSV of chum measurement locations that I got
# manually from using MapShaper
chumPermCodes <- read.csv("data/hoodCanalChumSiteReachCodes.csv",
                          header=TRUE)
# build vector of Permanent IDs to filter on
wantedChumSites <-
  unlist(unique(chumPermCodes["Permanent"]))
# use wantedChumSites list to filter
chum_sites.mp <- prj_hydro.mp %>%
  filter(Permanent_ %in% wantedChumSites)

# reset the rownames
rownames(chum_sites.mp@data) <- 1:nrow(chum_sites.mp@data)
# 
# pare down chumReachCodes data
keeps <-c("River.site", "Description", "Permanent")
chumSiteInfo <- chumPermCodes[keeps]
#
# convert the projection to lat long
chumsites <- spTransform(chum_sites.mp, CRS("+init=epsg:4326"))

# select first coord pair for Duckabush to make sure the
# measurement point lands in correct HUC10 and HUC12
chumsites@lines[[6]]@Lines[[1]]@coords =
  head(chumsites@lines[[6]]@Lines[[1]]@coords, n=1)

# retain just the final coord pair for each polyline
for (i in seq_along(chumsites@lines)){
  chumsites@lines[[i]]@Lines[[1]]@coords =
    tail(chumsites@lines[[i]]@Lines[[1]]@coords, n=1)
}

# convert from SpatialLinesDF to SpatialPointsDF
ptchumsites = as(chumsites, "SpatialPointsDataFrame")

# convert to geojson using geojsonio
chumsite_json <- geojson_json(ptchumsites)

# saving out this geojson - for fault tolerance
geojson_write(chumsite_json,
              file = "chumsite.json")

# read in saved GeoJSON file as sp object
chum_to_map <- geojsonio::geojson_read("chumsite.json",
                                       what = "sp")

# convert to plain ol data frame for easy combining
chumsite_df <- as.data.frame(chum_to_map)
names(chumsite_df)[names(chumsite_df) == "coords.x1"] <- "lng"
names(chumsite_df)[names(chumsite_df) == "coords.x2"] <- "lat"
names(chumsite_df)[names(chumsite_df) == "Permanent_"] <- "Permanent"

# merge with the chumsite info
chumtwo <- merge(chumsite_df,chumSiteInfo,by="Permanent")

# retool data so names match those in chumdata
chumtwo <- chumtwo %>% 
  mutate(River.site = fct_recode(River.site,
                                 "Anderson Creek - Natural" = "1. Anderson Creek Summer Chum: Natural-Origin Spawners (Prop)",
                                 "Big Beef Creek - Natural" = "2. Big Beef Creek Summer Chum: Natural-Origin Spawners (Prop)",
                                 "Big Beef Creek - Hatch" = "3. Big Beef Creek Summer Chum: Hatch-Origin Spawners (Prop.)",
                                 "Big/Little Quilcene - Escapement" = "4. Big/Little Quilcene Summer Chum: Escapement (Proportion)",
                                 "Big Quilcene River - Hatch" = "5. Big/Little Quilcene Summer Chum: Index Hatchery Escapement",
                                 "Little Quilcene River - Hatch" = "6. Big/Little Quilcene Summer Chum: Index Hatchery Escapement",
                                 "Big Quilcene River - Natural" = "7. Big/Little Quilcene Summer Chum: Index Natural Escapement",
                                 "Little Quilcene River - Natural" = "8. Big/Little Quilcene Summer Chum: Index Natural Escapement",
                                 "Dewatto River - Hatch" = "9. Dewatto Summer Chum: Hatch-Origin Spawners (Prop.)",
                                 "Dewatto River - Natural" = "10. Dewatto Summer Chum: Natural-Origin Spawners (Prop)",
                                 "Dosewallips River - Hatch" = "11. Dosewallips Summer Chum: Hatch-Origin Spawners (Prop.)",
                                 "Dosewallips River - Natural" = "12. Dosewallips Summer Chum: Natural-Origin Spawners (Prop)",
                                 "Duckabush River - Hatch" = "13. Duckabush Summer Chum: Hatch-Origin Spawners (Prop.)",
                                 "Duckabush River - Natural" = "14. Duckabush Summer Chum: Natural-Origin Spawners (Prop)",
                                 "Finch Creek - Escapement" = "15. Finch Creek Summer Chum: Escapement (Proportion)",
                                 "Hamma Hamma River - Hatch" = "16. Hamma Hamma Summer Chum: Hatch-Origin Spawners (Prop.)",
                                 "Hamma Hamma River - Natural" = "17. Hamma Hamma Summer Chum: Natural-Origin Spawners (Prop)",
                                 "Lilliwaup Creek - Hatch" = "18. Lilliwaup Creek Summer Chum: Hatch-Origin Spawners (Prop.)",
                                 "Lilliwaup Creek - Natural" = "19. Lilliwaup Creek Summer Chum: Natural-Origin Spawners (Prop)",
                                 "Skokomish River - Hatch" = "20. Skokomish Summer Chum: Hatch-Origin Spawners (Prop.)",
                                 "Skokomish River - Natural" = "21. Skokomish Summer Chum: Natural-Origin Spawners (Prop)",
                                 "Tahuya River - Hatch" = "22. Tahuya Summer Chum: Hatch-Origin Spawners (Prop.)",
                                 "Tahuya River - Natural" = "23. Tahuya Summer Chum: Natural-Origin Spawners (Prop)",
                                 "Union River - Hatch" = "24. Union Summer Chum: Index Hatchery Escapement",
                                 "Union River - Natural" = "25. Union Summer Chum: Index Natural Escapement",
                                 "Chimacum Creek - Hatch" = "1. Chimacum Creek Summer Chum: Hatch-Origin Spawners (Prop.)",
                                 "Chumacum Creek - Natural" = "2. Chimacum Creek Summer Chum: Natural-Origin Spawners (Prop)",
                                 "Dungeness River - Natural" = "3. Dungeness Summer Chum: Natural-Origin Spawners (Prop)",
                                 "Jimmycomelately Creek - Hatchery Escapement" = "4. Jimmycomelately Creek Summer Chum: Index Hatchery Escapement",
                                 "Jimmycomelately Creek - Natural Escapement" = "5. Jimmycomelately Creek Summer Chum: Index Natural Escapement",
                                 "Jimmycomelately Creek - Natural Spawners" = "6. Jimmycomelately Creek Summer Chum: Natural-Origin Spawners (Prop)",
                                 "Snow/Salmon Creeks - Escapement" = "7. Snow/Salmon Creeks Summer Chum: Escapement (Proportion)",
                                 "Salmon Creek - Hatch" = "8. Snow/Salmon Creeks Summer Chum: Index Hatchery Escapement",
                                 "Snow Creek - Hatch" = "9. Snow/Salmon Creeks Summer Chum: Index Hatchery Escapement",
                                 "Salmon Creek - Natural" = "10. Snow/Salmon Creeks Summer Chum: Index Natural Escapement",
                                 "Snow Creek - Natural" = "11. Snow/Salmon Creeks Summer Chum: Index Natural Escapement"))

# because we have points at the same lat/long, I'm going to jitter that data only for the visualization

chumtwo$jitterlng <- as.numeric(jitter(chumtwo$lng, factor = 10))
chumtwo$jitterlat <- as.numeric(jitter(chumtwo$lat, factor = 10))

############################# ADD IN HUCS ############################# 

# load saved data
# ch <- readRDS("./data/chum_huc.rds")

# following introductory vignette at
# https://cran.r-project.org/web/packages/MazamaSpatialUtils/vignettes/introduction.html

# need to install the data separately via command line (see mazama github)
setSpatialDataDir('C:/Users/tim_b/Data/Spatial')
# THIS TAKES A WHILE
loadSpatialData('WBDHU')
# only need to do the command below once (i think)
installSpatialData()

# get the HUC 12 and HUC 10 id's for each row
# THIS TAKES A WHILE
huc_ids <- chumtwo %>%
  mutate(HUC12_id = getHUC(lng, lat, SPDF = WBDHU12),
         HUC10_id = getHUC(lng, lat, SPDF = WBDHU10))

# THIS TAKES A WHILE
# get the HUC 12 Names for each HUC ID in the dataset
huc12 <- tbl_df(getHUC(chumtwo$lng,chumtwo$lat, SPDF = WBDHU12, allData=TRUE)) %>%
  # unite(coords, latitude, longitude, remove = FALSE) %>%
  rename(HUC12_id = HUC, HUC12_Name = HUCName) %>%
  select(HUC12_id, HUC12_Name)

huc12 <- distinct(huc12)

# THIS TAKES A WHILE
# get the HUC 10 Names for each HUC ID in the dataset
huc10 <- tbl_df(getHUC(chumtwo$lng,chumtwo$lat, SPDF = WBDHU10, allData=TRUE)) %>%
  rename(HUC10_id = HUC, HUC10_Name = HUCName) %>%
  select(HUC10_id, HUC10_Name)

huc10 <- distinct(huc10)

# join the HUC Names to the HUC ids for each row
chum_huc <- huc_ids %>%
  inner_join(huc12, by = 'HUC12_id') %>%
  inner_join(huc10, by = 'HUC10_id')

# because HUCing takes a while, save the output to an r data file that can be loaded in the app
saveRDS(chum_huc, "./data/chum_huc.rds")

############################# ADD IN HUCS ############################# 

# load in the yearly data from the two csv files
chumSiteData1 <- as.data.frame(read.csv("data/hoodCanalSummerChumStats.csv", header = TRUE))
chumSiteData2 <- as.data.frame(read.csv("data/straitJuanDeFucaSummerChumStats.csv", header = TRUE))

# merge the two data frames, keeping all rows
chumSiteData <- merge(chumSiteData1, chumSiteData2, by="Year", all=TRUE)

# rename all of the yearly data columns
newnames <- c("Year",
              "Anderson Creek - Natural",
              "Big Beef Creek - Natural",
              "Big Beef Creek - Hatch",
              "Big/Little Quilcene - Escapement",
              "Big Quilcene River - Hatch",
              "Little Quilcene River - Hatch",
              "Big Quilcene River - Natural",
              "Little Quilcene River - Natural",
              "Dewatto River - Hatch",
              "Dewatto River - Natural",
              "Dosewallips River - Hatch",
              "Dosewallips River - Natural",
              "Duckabush River - Hatch",
              "Duckabush River - Natural",
              "Finch Creek - Escapement",
              "Hamma Hamma River - Hatch",
              "Hamma Hamma River - Natural",
              "Lilliwaup Creek - Hatch",
              "Lilliwaup Creek - Natural",
              "Skokomish River - Hatch",
              "Skokomish River - Natural",
              "Tahuya River - Hatch",
              "Tahuya River - Natural",
              "Union River - Hatch",
              "Union River - Natural",
              "Chimacum Creek - Hatch",
              "Chumacum Creek - Natural",
              "Dungeness River - Natural",
              "Jimmycomelately Creek - Hatchery Escapement",
              "Jimmycomelately Creek - Natural Escapement",
              "Jimmycomelately Creek - Natural Spawners",
              "Snow/Salmon Creeks - Escapement",
              "Salmon Creek - Hatch",
              "Snow Creek - Hatch",
              "Salmon Creek - Natural",
              "Snow Creek - Natural")

colnames(chumSiteData) <- newnames

# tidying the data
tidychum <- gather(chumSiteData, key, value, -Year)
# making column names simple
colnames(tidychum) <- c("year", "site", "count")
# change count data to numeric from char
tidychum$count <- as.numeric(gsub(",","",tidychum$count))
# dealing with N/A values
tidychum <- na.omit(tidychum)

# saveRDS(tidychum, "./data/tidychum.rds")

chumchoices <- c("Select All", 
                 "Anderson Creek - Natural",
                 "Big Beef Creek - Natural",
                 "Big Beef Creek - Hatch",
                 "Big/Little Quilcene - Escapement",
                 "Big Quilcene River - Hatch",
                 "Little Quilcene River - Hatch",
                 "Big Quilcene River - Natural",
                 "Little Quilcene River - Natural",
                 "Dewatto River - Hatch",
                 "Dewatto River - Natural",
                 "Dosewallips River - Hatch",
                 "Dosewallips River - Natural",
                 "Duckabush River - Hatch",
                 "Duckabush River - Natural",
                 "Finch Creek - Escapement",
                 "Hamma Hamma River - Hatch",
                 "Hamma Hamma River - Natural",
                 "Lilliwaup Creek - Hatch",
                 "Lilliwaup Creek - Natural",
                 "Skokomish River - Hatch",
                 "Skokomish River - Natural",
                 "Tahuya River - Hatch",
                 "Tahuya River - Natural",
                 "Union River - Hatch",
                 "Union River - Natural",
                 "Chimacum Creek - Hatch",
                 "Chumacum Creek - Natural",
                 "Dungeness River - Natural",
                 "Jimmycomelately Creek - Hatchery Escapement",
                 "Jimmycomelately Creek - Natural Escapement",
                 "Jimmycomelately Creek - Natural Spawners",
                 "Snow/Salmon Creeks - Escapement",
                 "Salmon Creek - Hatch",
                 "Snow Creek - Hatch",
                 "Salmon Creek - Natural",
                 "Snow Creek - Natural")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Hood Canal Summer Chum Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput("chumsites",
                     "Select site(s) to display:",
                     chumchoices,
                     selected = "Select All",
                     multiple = TRUE)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         leafletOutput("chummap"),
         plotOutput("chumcounts")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  observe({
    if ("Select All" %in% input$chumsites) {
      # choose all the choices _except_ "Select All"
      selected_choices <- setdiff(chumchoices, "Select All")
      updateSelectInput(session, "chumsites", selected = selected_choices)
    }
  })
  
  chumsubset <- reactive({
    subset(chumtwo, River.site %in% input$chumsites)
  })
  
  # chumlabel <- reactive({
  #   sprintf("Chum Site: %s <br><br>Measurement: %s <br><br>Description: %s",
  #           chumsubset$GNIS_Name, 
  #           chumsubset$River.site,
  #           chumsubset$Description)
  # })
  
   output$chummap <- renderLeaflet({
     leaflet(chumsubset()) %>%
       setView(lng = -123, lat = 47.55, zoom = 9) %>%
       addProviderTiles("Esri.WorldImagery") %>% 
       addCircleMarkers(lng = ~jitterlng, 
                        lat = ~jitterlat,
                        color = "#42d4f4",
                        popup = ~River.site)
   })
   
   countsubset <- reactive({
     subset(tidychum, site %in% input$chumsites)
   })
   
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

