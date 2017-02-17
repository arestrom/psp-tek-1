library(tidyverse)
library(ggmap)
library(leaflet)

library(sp)  # classes for spatial data
library(raster)  # grids, rasters
library(rasterVis)  # raster visualisation
library(maptools)
library(rgeos)
library(googleVis)
# load the turbidity data 
turbidity <- read.csv('EIMResults.csv', header = TRUE)

# print out information on the variables it contains
summary(turbidity)

# create a data frame tbl for easy printing
turb.tib <- tbl_df(turbidity)
turb.tib

# select only variables of interest, rename lengthy variable names
turb <- turb.tib %>%
  select(Study_ID, Study_Name, Location_ID, Location_Name, 
         Field_Collection_Start_Date, Field_Collection_End_Date,
         Result_Value, Calculated_Latitude_Decimal_Degrees_NAD83HARN,
          Calculated_Longitude_Decimal_Degrees_NAD83HARN) %>%
  rename(start_date = Field_Collection_Start_Date, end_date = Field_Collection_End_Date,
         turbidity_NTU = Result_Value,
         lat = Calculated_Latitude_Decimal_Degrees_NAD83HARN,
         lon = Calculated_Longitude_Decimal_Degrees_NAD83HARN)
  # %>%
  # # mutate(duration = end_date - start_date) %>%
  # spread(key = Result_Parameter_Name, value = Result_Value)
dim(turb)
distinct(turb, Result_Parameter_Name)
turb

# plot a histogram of turbidity values
turb %>%
  filter(0 < turbidity_NTU & turbidity_NTU < 100) %>%
  ggplot(aes(turbidity_NTU)) +
    geom_histogram(binwidth = 0.5)

# plot mean turbidity by study 
turb %>%
  group_by(Study_ID) %>%
  summarise(m.turb = mean(turbidity_NTU)) %>%
  ggplot(aes(x = reorder(Study_ID, m.turb), y = m.turb)) +
    geom_bar(stat="identity") +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))

# plot mean turbidity by location
turb %>%
  group_by(Location_ID) %>%
  summarise(m.turb = mean(turbidity_NTU)) %>%
  ggplot(aes(x = reorder(Location_ID, m.turb), y = m.turb)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# plot mean turbidity by location
turb %>%
  group_by(Study_ID) %>%
  summarise(m.turb = mean(turbidity_NTU),
            last_day = max(end_date),
            first_day = min(start_date),
            change = ) %>%
  ggplot(aes(x = Study_ID, y = m.turb)) +
  geom_bar(stat="identity", fill = Location_ID) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


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

# load location data file
locations <- read.csv('EIMLocationDetails.csv', header = TRUE)
head(locations)

# select only ID and WRIA
locs <- tbl_df(locations) %>%
  select(Location_ID, Watershed_WRIA)
# unique(locs$Watershed_WRIA)
turb_wria <- turb %>%
  left_join(locs, by = "Location_ID")
head(turb_wria)
View(turb_wria)


turb_nums <- turb_wria %>% 
  mutate(WRIA_ID = ifelse(Watershed_WRIA == "Kitsap", 15, 
                     ifelse(Watershed_WRIA == "Kennedy-Goldsborough", 14,
                            ifelse(Watershed_WRIA == "Skokomish-Dosewallips", 16,
                                   ifelse(Watershed_WRIA == "Quilcence-Snow", 17,
                                          ifelse(Watershed_WRIA == "Elwah-Dungeness", 18,
                                                 NA))))))
                            
# ifelse(<condition>, <yes>, 
#        ifelse(<condition>, <yes>, 
#               ifelse(<condition>, <yes>, <no>)
#        )
# )

# plot mean turbidity by Watershed_WRIA
turb_nums %>%
  group_by(Watershed_WRIA) %>%
  summarise(m.turb = mean(turbidity_NTU)) %>%
  ggplot(aes(x = reorder(Watershed_WRIA, m.turb), y = m.turb)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# sm <- turb %>%
#   filter(Watershed_WRIA == "Skokomish-Dosewallips")
# dim(sm)

# turb_nums$loc=paste(turb_nums$lat, turb_nums$lon, sep=":") ## create a lat:long location variable
# Geo <- gvisGeoMap(turb_nums, locationvar='loc', numvar="turbidity_NTU", 
#                   options=list(height=400, dataMode='markers'))
# plot(Geo)
# 
# G1 <- gvisGeoChart(turb_nums, locationvar='loc', numvar='turbidity_NTU') 
# 
# plot(G1)


m <- leaflet(data = turb_nums[1:20,]) %>% 
  setView(lng = -122.996823, lat = 47.5642594, zoom = 9)

waterIcon <- makeIcon(
  iconUrl = 'icons/water-15.svg',
  iconWidth = 25, iconHeight = 25
)

m %>% 
  addTiles() %>%
  addMarkers(~lon, ~lat, popup = ~as.character(turbidity_NTU),
             icon = waterIcon,
             clusterOptions = markerClusterOptions())


# # Show first 20 rows from the `quakes` dataset
# leaflet(data = quakes[1:20,]) %>% 
#   addTiles() %>%
#   addMarkers(~long, ~lat, popup = ~as.character(mag))
