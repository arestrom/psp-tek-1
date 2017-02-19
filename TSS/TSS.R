library(tidyverse)
library(ggmap)
library(leaflet)

# load the tss data 
tss <- read.csv('./data/EIMResults.csv', header = TRUE)

# # create a data frame tbl for easy printing
# tss.tib <- tbl_df(tss)
# tss.tib

# select only variables of interest, rename lengthy variable names
tss <- tbl_df(tss) %>%
  select(Study_ID, Study_Name, Location_ID, Location_Name, 
         Field_Collection_Start_Date, Field_Collection_End_Date,
         Result_Value, Calculated_Latitude_Decimal_Degrees_NAD83HARN,
          Calculated_Longitude_Decimal_Degrees_NAD83HARN) %>%
  rename(start_date = Field_Collection_Start_Date, end_date = Field_Collection_End_Date,
         tss_mgL = Result_Value,
         lat = Calculated_Latitude_Decimal_Degrees_NAD83HARN,
         lon = Calculated_Longitude_Decimal_Degrees_NAD83HARN) %>%
  mutate(start_date = as.Date(start_date, format = "%m/%d/%Y"),
         end_date = as.Date(end_date, format = "%m/%d/%Y"))
  # %>%
  # # mutate(duration = end_date - start_date) %>%
  # spread(key = Result_Parameter_Name, value = Result_Value)
dim(tss)
# distinct(tss, Result_Parameter_Name)
# print out information on the variables it contains
summary(tss)
tss
# load location data file
locations <- read.csv('./data/EIMLocationDetails.csv', header = TRUE)
head(locations)

# select only ID and WRIA
locs <- tbl_df(locations) %>%
  select(Location_ID, Watershed_WRIA)
# unique(locs$Watershed_WRIA)
# join wria to main data file
tss_wria <- tss %>%
  left_join(locs, by = "Location_ID")
head(tss_wria)
View(tss_wria)

# label wria name with wria number
tss_nums <- tss_wria %>% 
  mutate(WRIA_ID = ifelse(Watershed_WRIA == "Kitsap", 15, 
                     ifelse(Watershed_WRIA == "Kennedy-Goldsborough", 14,
                            ifelse(Watershed_WRIA == "Skokomish-Dosewallips", 16,
                                   ifelse(Watershed_WRIA == "Quilcence-Snow", 17,
                                          ifelse(Watershed_WRIA == "Elwah-Dungeness", 18,
                                                 NA))))))
# sm <- tss %>%
#   filter(Watershed_WRIA == "Skokomish-Dosewallips")
# dim(sm)
# 
# tss_nums %>%
#   group_by(Study_ID, Location_ID, start_date)

################################ mapping ################################################ 
# tss_nums$loc=paste(tss_nums$lat, tss_nums$lon, sep=":") ## create a lat:long location variable
# Geo <- gvisGeoMap(tss_nums, locationvar='loc', numvar="tss_NTU", 
#                   options=list(height=400, dataMode='markers'))
# plot(Geo)
# 
# G1 <- gvisGeoChart(tss_nums, locationvar='loc', numvar='tss_NTU') 
# 
# plot(G1)


m <- leaflet(data = tss_nums) %>% 
  setView(lng = -122.996823, lat = 47.5642594, zoom = 9)

waterIcon <- makeIcon(
  iconUrl = 'icons/water-15.svg',
  iconWidth = 25, iconHeight = 25
)

content1 <- paste(sep = ": ",
                  "<b>tss NTU</b>",
                  tss_nums$tss_NTU
)

content2 <- paste(sep = ": ",
                  "<b>Project Name</b>",
                  tss_nums$Study_Name
)
content3 <- paste(sep = ": ",
                  "<b>Date</b>",
                  tss_nums$start_date
)

content_items = c(content1,content2,content3)
# full_content <- paste(sep = "<br>", content2,content1)
# full_content <- paste(collapse = "<br>", content_items)
full_content <- sprintf("Project Name: %s <br>Date: %s <br> tss NTU: %s", 
                        tss_nums$Study_Name, tss_nums$start_date, tss_nums$tss_NTU)

m %>%
  addTiles() %>%
  addMarkers(~lon, ~lat, popup = full_content,
             icon = waterIcon,
             clusterOptions = markerClusterOptions())

qpal <- colorQuantile("BuPu", tss_nums$tss_NTU, n = 5)
m %>%
  addProviderTiles("Stamen.Terrain", group = "Terrain") %>%
  addCircleMarkers(~lon, ~lat, popup = full_content,
                   radius = 10,
                   color = ~qpal(tss_NTU),
                   stroke = FALSE, fillOpacity = 0.5,
                   group = "Water Quality") %>%
  addLegend(pal = qpal, values = ~tss_NTU, opacity = 1) %>%
  addLayersControl(
    baseGroups = "Terrain",
    overlayGroups = "Water Quality",
    options = layersControlOptions(collapsed = FALSE)
  )


# # Show first 20 rows from the `quakes` dataset
# leaflet(data = quakes[1:20,]) %>% 
#   addTiles() %>%
#   addMarkers(~long, ~lat, popup = ~as.character(mag))
################################ mapping ################################################ 


################################ EDA plots ################################################ 
# plot a histogram of tss values
tss %>%
  filter(0 < tss_NTU & tss_NTU < 100) %>%
  ggplot(aes(tss_NTU)) +
  geom_histogram(binwidth = 0.5)

# plot mean tss by study 
tss %>%
  group_by(Study_ID) %>%
  summarise(m.tss = mean(tss_NTU)) %>%
  ggplot(aes(x = reorder(Study_ID, m.tss), y = m.tss)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# plot mean tss by location
tss %>%
  group_by(Location_ID) %>%
  summarise(m.tss = mean(tss_NTU)) %>%
  ggplot(aes(x = reorder(Location_ID, m.tss), y = m.tss)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# plot mean tss by location
tss %>%
  group_by(Study_ID) %>%
  summarise(m.tss = mean(tss_NTU),
            last_day = max(end_date),
            first_day = min(start_date),
            change = ) %>%
  ggplot(aes(x = Study_ID, y = m.tss)) +
  geom_bar(stat="identity", fill = Location_ID) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# plot mean tss by Watershed_WRIA
tss_nums %>%
  group_by(Watershed_WRIA) %>%
  summarise(m.tss = mean(tss_NTU)) %>%
  ggplot(aes(x = reorder(Watershed_WRIA, m.tss), y = m.tss)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
################################ EDA plots ################################################ 

### parameter names: ###
# Study_Name <fctr>, Location_ID <fctr>,
#   Study_Specific_Location_ID <fctr>, Location_Name <fctr>,
#   Field_Collection_Type <fctr>, Field_Collector <fctr>,
#   Field_Collection_Start_Date <fctr>,
#   Field_Collection_Start_Time <fctr>,
#   Field_Collection_Start_Date_Time <fctr>,
#   Field_Collection_End_Date <fctr>,
# Result_Parameter_Name <fctr>,
#   Result_Parameter_CAS_Number <lgl>, Lab_Analysis_Date <fctr>,
#   Lab_Analysis_Date_Accuracy <fctr>, Lab_Analysis_Time <fctr>,
#   Result_Value <dbl>, Result_Value_Units <fctr>,
# Calculated_Latitude_Decimal_Degrees_NAD83HARN <dbl>,
#   Calculated_Longitude_Decimal_Degrees_NAD83HARN <dbl>,