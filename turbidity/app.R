library(shiny)
library(leaflet)
library(plotly)
library(shinythemes)
library(ggmap)
library(tidyverse)
library(stringr)
library(scales)
locations <- read.csv('./data/EIMLocationDetails.csv', header = TRUE)


# Define UI for application that draws a histogram
ui <- navbarPage(theme = shinytheme("sandstone"),
  "Opening Up the Data",
  
  tabPanel("Water Quality Data",
  # water quality data
   sidebarLayout(
      sidebarPanel(
         radioButtons(inputId = "mapfeatures",
                      label = "Select water quality feature:",
                      c("Turbidity", "TSS")),
         checkboxGroupInput("wriaselect",
                            "WRIAs",
                            c("Elwah-Dungeness", 
                              "Kennedy-Goldsborough", 
                              "Kitsap", 
                              "Quilcence-Snow",
                              "Skokomish-Dosewallips"),
                            selected = unique(locations$Watershed_WRIA))
      , width = 2),
      mainPanel(
         leafletOutput("watermap",
                       height = 675,
                       width = 1200)

      ))),
  tabPanel("Project Data",
           sidebarLayout(
             sidebarPanel(
               #selectizeInput("projectinput",
                #              label = "Select Project(s)",
                 #             choices = unique(all_projects$name),
                  #            multiple = TRUE),
               checkboxGroupInput("wriaselect1",
                                  "WRIAs",
                                  c("Elwah-Dungeness", 
                                    "Kennedy-Goldsborough", 
                                    "Kitsap", 
                                    "Quilcence-Snow",
                                    "Skokomish-Dosewallips"),
                                  selected = unique(locations$Watershed_WRIA)),
               width = 2
             ),
             mainPanel(leafletOutput("projectmap",
                                     height = 675,
                                     width = 1200))
           ))
   #)

  )

###########PROJECT DATA###############

#################### EAGL DATA #################### 
eagl_df <- read.csv('./data/eagl.csv', header = TRUE) %>%
  select(Funding.Fiscal.Year, WRIA, Project.Title, 
         Funding.Provided, Latitude, Longitude) %>%
  rename(year = Funding.Fiscal.Year, name = Project.Title, WRIA_ID = WRIA,
         cost = Funding.Provided, lat = Latitude, lon = Longitude) %>%
  # remove missing coordinates
  filter(!lat %in% c('#N/A', '0'),
         !lon %in% c('#N/A', '0')) %>%
  # convert dollar strings ($50,000.00) to numeric values (50000) 
  mutate(cost_sm = str_sub(cost, 2, -4),
         cost = as.numeric(gsub(",", "", cost_sm)),
         lat = as.numeric(levels(lat))[lat],
         lon = as.numeric(levels(lon))[lon]) %>%
  select(-cost_sm)

# label wria number with wria name
eagl_df <- eagl_df %>% 
  mutate(WRIA_Name = ifelse(WRIA_ID == 15, "Kitsap",
                            ifelse(WRIA_ID == 14, "Kennedy-Goldsborough",
                                   ifelse(WRIA_ID == 16, "Skokomish-Dosewallips",
                                          ifelse(WRIA_ID == 17, "Quilcence-Snow",
                                                 ifelse(WRIA_ID == 18, "Elwah-Dungeness",
                                                        NA))))))

#################### PRISM DATA #################### 
hc_df <- read.csv('./data/locs.csv', header = TRUE)
f_df <- read.csv('./data/fund.csv', header = TRUE)

mdf <- merge(x = hc_df, y = f_df, by = "ProjectNumber", all.x = TRUE) %>%
  select(ProjectNumber, ProjectYear, ProjectName.x, HUC, WRIA, 
         PrimaryProgramAmount, ProjectLatitude, ProjectLongitude) %>%
  rename(id = ProjectNumber, year = ProjectYear, name = ProjectName.x, 
         cost = PrimaryProgramAmount, lat = ProjectLatitude, lon = ProjectLongitude) %>%
  # create wria number and wria name columns from combo string
  separate(WRIA, into = c("WRIA_Name", "WRIA_ID"), sep = " \\(", convert = TRUE) %>%
  mutate(WRIA_ID = as.numeric(str_sub(WRIA_ID, 1, -2))) %>%
  filter(!is.na(lat), !is.na(cost))


#################### MERGE #################### 

all_projects <- bind_rows(mdf, eagl_df)

all_projects <- within(all_projects, cost_quantile <- as.integer(cut(cost, quantile(cost, probs=0:5/5), include.lowest=TRUE)))

all_projects$marker_size <- 
  cut( 
    # input data
    all_projects$cost_quantile, 
    # cut points
    c( 0, 1, 2, 3, 4, 6) , 
    # label values (character strings work too)
    labels = c(5,7,8,9,11) ,
    # interval closed on the right?
    right = FALSE
  )

server <- function(input, output, session) {
  
  # load the turbidity data 
  turbidity <- read.csv('./data/EIMResults.csv', header = TRUE)
  
  # print out information on the variables it contains
  summary(turbidity)
  
  # create a data frame tbl for easy printing
  turb.tib <- tbl_df(turbidity)
  turb.tib
  
  # select only variables of interest, rename lengthy variable names
  # convert dates to date format
  # obtain log(x+1) since there are some negative values if use log(x)
  turb <- turb.tib %>%
    select(Study_ID, Study_Name, Location_ID, Location_Name, 
           Field_Collection_Start_Date, Field_Collection_End_Date,
           Result_Value, Calculated_Latitude_Decimal_Degrees_NAD83HARN,
           Calculated_Longitude_Decimal_Degrees_NAD83HARN) %>%
    rename(start_date = Field_Collection_Start_Date, end_date = Field_Collection_End_Date,
           turbidity_NTU = Result_Value,
           lat = Calculated_Latitude_Decimal_Degrees_NAD83HARN,
           lon = Calculated_Longitude_Decimal_Degrees_NAD83HARN) %>%
    mutate(start_date = as.Date(start_date, format = "%m/%d/%Y"),
           end_date = as.Date(end_date, format = "%m/%d/%Y"),
           logturb = log10(turbidity_NTU+1))
  # %>%
  # # mutate(duration = end_date - start_date) %>%
  # spread(key = Result_Parameter_Name, value = Result_Value)
  dim(turb)
  #distinct(turb, Result_Parameter_Name)
  turb
  
  # load location data file
  head(locations)
  
  # select only ID and WRIA
  locs <- tbl_df(locations) %>%
    select(Location_ID, Watershed_WRIA)
  # unique(locs$Watershed_WRIA)
  # join wria to main data file
  turb_wria <- turb %>%
    left_join(locs, by = "Location_ID")
  # head(turb_wria)
  # View(turb_wria)
  
  # label wria name with wria number
  turb_nums <- turb_wria %>% 
    mutate(WRIA_ID = ifelse(Watershed_WRIA == "Kitsap", 15, 
                            ifelse(Watershed_WRIA == "Kennedy-Goldsborough", 14,
                                   ifelse(Watershed_WRIA == "Skokomish-Dosewallips", 16,
                                          ifelse(Watershed_WRIA == "Quilcence-Snow", 17,
                                                 ifelse(Watershed_WRIA == "Elwah-Dungeness", 18,
                                                        NA))))))

  # load the tss data 
  tss <- read.csv('./data/TSSEIMResults.csv', header = TRUE)
  # select only variables of interest, rename lengthy variable names
  tss2 <- tbl_df(tss) %>%
    select(Study_ID, Study_Name, Location_ID, 
           Field_Collection_Start_Date, Field_Collection_Start_Date_Time,
           Result_Value, Calculated_Latitude_Decimal_Degrees_NAD83HARN,
           Calculated_Longitude_Decimal_Degrees_NAD83HARN) %>%
    rename(start_date = Field_Collection_Start_Date, dt = Field_Collection_Start_Date_Time,
           tss_mgL = Result_Value,
           lat = Calculated_Latitude_Decimal_Degrees_NAD83HARN,
           lon = Calculated_Longitude_Decimal_Degrees_NAD83HARN) %>%
    unite(PK, Study_ID, Location_ID, dt, remove = FALSE) %>%
    mutate(start_date = as.Date(start_date, format = "%m/%d/%Y"),
           logTSS = log10(tss_mgL))
  
  turb2 <- turb.tib %>%
    select(Study_ID, Study_Name, Location_ID, 
           Field_Collection_Start_Date, Field_Collection_Start_Date_Time,
           Result_Value, Calculated_Latitude_Decimal_Degrees_NAD83HARN,
           Calculated_Longitude_Decimal_Degrees_NAD83HARN) %>%
    rename(start_date = Field_Collection_Start_Date, dt = Field_Collection_Start_Date_Time,
           turbidity_NTU = Result_Value,
           lat = Calculated_Latitude_Decimal_Degrees_NAD83HARN,
           lon = Calculated_Longitude_Decimal_Degrees_NAD83HARN) %>%
    unite(PK, Study_ID, Location_ID, dt, remove = FALSE) %>%
    mutate(start_date = as.Date(start_date, format = "%m/%d/%Y"),
           logTurbidity = log10(turbidity_NTU+1))
  
  dim(turb2)
  turb2 <- unique(turb2)
  tss2 <- unique(tss2)
  sm.tss2 <- select(tss2, PK, tss_mgL, logTSS)
  
  turb_tss1 <- turb2 %>%
    inner_join(sm.tss2, by = "PK") %>%
    left_join(locs, by = "Location_ID")
  
  turb_final <- subset(turb_tss1, select = -c(tss_mgL, logTSS))
  tss_final <- subset(turb_tss1, select = -c(turbidity_NTU, logTurbidity))
  
  turb_final <- plyr::rename(turb_final, c("turbidity_NTU" = "measurement", "logTurbidity" = "logMeasurement"))
  tss_final <- plyr::rename(tss_final, c("tss_mgL" = "measurement", "logTSS" = "logMeasurement"))
  
  turb_final <- mutate(turb_final, "result_type" = "Turbidity")
  tss_final <- mutate(tss_final, "result_type" = "TSS")
  turb_final <- mutate(turb_final, "unit" = "NTU")
  tss_final <- mutate(tss_final, "unit" = "mg/L")
  
  turb_tss <- rbind(turb_final, tss_final)
  

  features_df <- reactive({subset(turb_tss, result_type %in% input$mapfeatures)})    
  reactive_df <- reactive({subset(features_df(), Watershed_WRIA %in% input$wriaselect)})
 
  output$watermap <- renderLeaflet( m <- leaflet(data = reactive_df()) %>% 
                                   setView(lng = -122.996823, lat = 47.5642594, zoom = 9) %>%
    addTiles() %>%
    addProviderTiles("Stamen.Terrain", group = "Terrain") %>%
    addCircleMarkers(~lon, ~lat, 
                     popup = content1(),
                     radius = 6,
                     color = ~qpal()(measurement),
                     stroke = FALSE, fillOpacity = 0.5,
                     group = "Water Quality") %>%
    addLegend(title = paste("Water Quality",br(), "Measurement Quantiles"), pal = qpal(), values = reactive_df()$measurement, opacity = 1, labels = c("turbidity", "TSS"))
  )
  
  content1 <- reactive({
                    paste(sep = "",
                    "<b>Project Name: </b>",
                    reactive_df()$Study_Name,
                    br(),
                    "<b>Date: </b>",
                    reactive_df()$start_date,
                    br(),
                    strong(reactive_df()$result_type),
                    "<b>: </b>",
                    reactive_df()$measurement,
                    " ",
                    reactive_df()$unit
                    )})
  
  
  qpal <- reactive({colorQuantile("Reds", reactive_df()$measurement, n = 5)})
  
  projects_reactive <- reactive({subset(all_projects, WRIA_Name %in% input$wriaselect1)})    
  
  output$projectmap <- renderLeaflet( m <- leaflet(data = projects_reactive()) %>% 
                                      setView(lng = -122.996823, lat = 47.5642594, zoom = 9) %>%
                                      addTiles() %>%
                                      addProviderTiles("Stamen.Terrain", group = "Terrain") %>%
                                      addCircleMarkers(~lon, ~lat, 
                                                       popup = content2(),
                                                       radius = projects_reactive()$marker_size,
                                                       color = ~quantpal()(cost),
                                                       stroke = FALSE, fillOpacity = 0.7,
                                                       group = "Projects") %>%
                                      addLegend(title = "Project Cost Quantiles", 
                                                pal = quantpal(), 
                                                values = projects_reactive()$cost, 
                                                opacity = 1)
  )

  quantpal <- reactive({colorQuantile("PuRd", projects_reactive()$cost, n = 5)})
  
  content2 <- reactive({
    paste(sep = "",
          "<b>Project Name: </b>",
          projects_reactive()$name,
          br(),
          "<b>Fiscal Year Funded: </b>",
          projects_reactive()$year,
          br(),
          "<b>Funding Amount: </b>",
          dollar(projects_reactive()$cost)

    )})

}
# Run the application 
shinyApp(ui = ui, server = server)

