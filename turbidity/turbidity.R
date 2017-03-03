library(tidyverse)
library(ggmap)
library(leaflet)

# library(sp)  # classes for spatial data
# library(raster)  # grids, rasters
# library(rasterVis)  # raster visualisation
# library(maptools)
# library(rgeos)
# library(googleVis)

# NOTE: http://apps.leg.wa.gov/WAC/default.aspx?dispo=true&cite=173-201A-200
  #Turbidity shall not exceed:
  # 5 NTU over background when the background is 50 NTU or less; or
  # A 10 percent increase in turbidity when the background turbidity is more than 50 NTU.

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
distinct(turb, Result_Parameter_Name)
turb

# load location data file
locations <- read.csv('./data/EIMLocationDetails.csv', header = TRUE)
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
# sm <- turb %>%
#   filter(Watershed_WRIA == "Skokomish-Dosewallips")
# dim(sm)
# 
# turb_nums %>%
#   group_by(Study_ID, Location_ID, start_date)

# load the tss data 
tss <- read.csv('../TSS/data/EIMResults.csv', header = TRUE)
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

# merge tss and turbidity (only rows with BOTH measures)
turb_tss <- turb2 %>%
  inner_join(sm.tss2, by = "PK") %>%
  left_join(locs, by = "Location_ID")

################################ mapping ################################################ 
# turb_nums$loc=paste(turb_nums$lat, turb_nums$lon, sep=":") ## create a lat:long location variable
# Geo <- gvisGeoMap(turb_nums, locationvar='loc', numvar="turbidity_NTU", 
#                   options=list(height=400, dataMode='markers'))
# plot(Geo)
# 
# G1 <- gvisGeoChart(turb_nums, locationvar='loc', numvar='turbidity_NTU') 
# 
# plot(G1)


m <- leaflet(data = turb_nums) %>% 
  setView(lng = -122.996823, lat = 47.5642594, zoom = 9)

waterIcon <- makeIcon(
  iconUrl = 'icons/water-15.svg',
  iconWidth = 25, iconHeight = 25
)

content1 <- paste(sep = ": ",
                  "<b>Turbidity NTU</b>",
                  turb_nums$turbidity_NTU
)

content2 <- paste(sep = ": ",
                  "<b>Project Name</b>",
                  turb_nums$Study_Name
)
content3 <- paste(sep = ": ",
                  "<b>Date</b>",
                  turb_nums$start_date
)

content_items = c(content1,content2,content3)
# full_content <- paste(sep = "<br>", content2,content1)
# full_content <- paste(collapse = "<br>", content_items)
full_content <- sprintf("Project Name: %s <br>Date: %s <br> Turbidity NTU: %s", 
                        turb_nums$Study_Name, turb_nums$start_date, turb_nums$turbidity_NTU)

m %>%
  addTiles() %>%
  addMarkers(~lon, ~lat, popup = full_content,
             icon = waterIcon,
             clusterOptions = markerClusterOptions())

qpal <- colorQuantile("BuPu", turb_nums$turbidity_NTU, n = 5)
m %>%
  addProviderTiles("Stamen.Terrain", group = "Terrain") %>%
  addCircleMarkers(~lon, ~lat, popup = full_content,
                   radius = 6,
                   color = ~qpal(turbidity_NTU),
                   stroke = FALSE, fillOpacity = 0.5,
                   group = "Water Quality") %>%
  addLegend(pal = qpal, values = ~turbidity_NTU, opacity = 1) %>%
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

# plot mean turbidity by Watershed_WRIA
turb_nums %>%
  group_by(Watershed_WRIA) %>%
  summarise(m.turb = mean(turbidity_NTU)) %>%
  ggplot(aes(x = reorder(Watershed_WRIA, m.turb), y = m.turb)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

ggplot(data = turb_wria, mapping = aes(x = start_date, y = logturb)) + 
  geom_point() + 
  geom_smooth() +
  # geom_hline(yintercept = 1.90309, color = "red", show.legend = TRUE) +
  facet_wrap(~ Watershed_WRIA) +
  xlab("Year") +
  ylab("Log Turbidity (NTU)") +
  ggtitle("Turbidity Trends Over Time")

# funding project data is from 2007-2015 ONLY
turb_wria %>%
  filter(format(start_date,"%Y") >= 2004) %>%
  ggplot(mapping = aes(x = start_date, y = logturb)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ Watershed_WRIA) +
  xlab("Year") +
  ylab("Log Turbidity (NTU)") +
  ggtitle("Turbidity Trends Over Time (2004-2016)")

# funding project data is from 2007-2015 ONLY
# only want longer length projects for meta-analysis
filtered.turb <- turb_wria %>%
  mutate(year = format(start_date,"%Y")) %>%
  filter(year >= 2004) %>%
  group_by(Study_ID) %>%
  filter(n_distinct(year) >= 6)

filtered.turb %>%
  ggplot(mapping = aes(x = start_date, y = logturb)) +
  geom_point(mapping = aes(color = Study_ID)) +
  geom_smooth() +
  # facet_wrap(~ Watershed_WRIA) +
  xlab("Year") +
  ylab("Log Turbidity (NTU)") +
  ggtitle("Turbidity Trends Over Time (2004-2016)")

turb_tss %>%
  filter(format(start_date,"%Y") >= 1995) %>%
  gather(logTSS,logTurbidity, key = 'param', value = 'result') %>%
  ggplot(mapping = aes(x = start_date, y = result)) +
  geom_point(mapping = aes(color = param)) +
  geom_smooth(mapping = aes(x = start_date, y = result, linetype = param)) +
  facet_grid(param~ Watershed_WRIA) +
  xlab("Year") +
  ylab("") +
  ggtitle("Turbidity vs TSS") +
  scale_x_date(date_breaks = '3 years', date_labels = '%Y')
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