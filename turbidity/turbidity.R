library(tidyverse)
library(leaflet)
library(MazamaSpatialUtils)

# NOTE: http://apps.leg.wa.gov/WAC/default.aspx?dispo=true&cite=173-201A-200
  #Turbidity shall not exceed:
  # 5 NTU over background when the background is 50 NTU or less; or
  # A 10 percent increase in turbidity when the background turbidity is more than 50 NTU.

# load location data file
# select only ID and WRIA
turbidity_locations <- read.csv('./data/EIMLocationDetails.csv', header = TRUE) %>%
  select(Location_ID, Watershed_WRIA)
# unique(locs$Watershed_WRIA)

# create a turbidity data frame 
# for merging with the tss data
# select only variables of interest, rename lengthy variable names
# convert dates to date format
# obtain log(x+1) since there are some negative values if use log(x) (for plots)
turbidity <- read.csv('./data/EIMResults.csv', header = TRUE) %>%
  select(Study_ID, Study_Name, Location_ID, 
         Field_Collection_Start_Date, Field_Collection_Start_Date_Time,
         Result_Value, Calculated_Latitude_Decimal_Degrees_NAD83HARN,
         Calculated_Longitude_Decimal_Degrees_NAD83HARN) %>%
  rename(start_date = Field_Collection_Start_Date, 
         measurement = Result_Value, 
         lat = Calculated_Latitude_Decimal_Degrees_NAD83HARN,
         lon = Calculated_Longitude_Decimal_Degrees_NAD83HARN) %>%
  mutate(start_date = as.Date(start_date, format = "%m/%d/%Y"),
         logMeasurement = log10(measurement+1),
         result_type = 'Turbidity',
         unit = 'NTU') %>%
  # join wria to main data file
  # label wria name with wria number
  left_join(turbidity_locations, by = "Location_ID") %>% 
  mutate(WRIA_ID = ifelse(Watershed_WRIA == "Kitsap", 15, 
                          ifelse(Watershed_WRIA == "Kennedy-Goldsborough", 14,
                                 ifelse(Watershed_WRIA == "Skokomish-Dosewallips", 16,
                                        ifelse(Watershed_WRIA == "Quilcence-Snow", 17,
                                               ifelse(Watershed_WRIA == "Elwah-Dungeness", 18,
                                                      NA))))))



# load tss location data file
# select only ID and WRIA
tss_locations <- read.csv('../polygons_TSS/data/EIMLocationDetails.csv', header = TRUE) %>%
  select(Location_ID, Watershed_WRIA)

# load the tss data 
# select only variables of interest, rename lengthy variable names
tss <- read.csv('../polygons_TSS/data/EIMResults.csv', header = TRUE) %>%
  select(Study_ID, Study_Name, Location_ID, 
         Field_Collection_Start_Date, Field_Collection_Start_Date_Time,
         Result_Value, Calculated_Latitude_Decimal_Degrees_NAD83HARN,
         Calculated_Longitude_Decimal_Degrees_NAD83HARN) %>%
  rename(start_date = Field_Collection_Start_Date,
         measurement = Result_Value,
         lat = Calculated_Latitude_Decimal_Degrees_NAD83HARN,
         lon = Calculated_Longitude_Decimal_Degrees_NAD83HARN) %>%
  mutate(start_date = as.Date(start_date, format = "%m/%d/%Y"),
         logMeasurement = log10(measurement+1),
         result_type = 'TSS',
         unit = 'mg/L') %>%
  left_join(tss_locations, by = "Location_ID") %>% 
  mutate(WRIA_ID = ifelse(Watershed_WRIA == "Kitsap", 15, 
                          ifelse(Watershed_WRIA == "Kennedy-Goldsborough", 14,
                                 ifelse(Watershed_WRIA == "Skokomish-Dosewallips", 16,
                                        ifelse(Watershed_WRIA == "Quilcence-Snow", 17,
                                               ifelse(Watershed_WRIA == "Elwah-Dungeness", 18,
                                                      NA))))))


# add both dataframes together
TURB_TSS <- rbind(turbidity,tss) 

# add unique ID column
TURB_TSS$ID <- 1:nrow(TURB_TSS)

# # then clean up data and 
# TURB_TSS <- TURB_TSS %>%
#   spread(measurement_type, measurement_value) %>%
#   mutate(logTSS = log10(tss_mgL+1),
#          logTurb = log10(turbidity_NTU+1))


################################ HUCs ################################################ 
# following introductory vignette at 
# https://cran.r-project.org/web/packages/MazamaSpatialUtils/vignettes/introduction.html

# need to install the data separately via command line (see mazama github)
setSpatialDataDir('~/Data/Spatial')
loadSpatialData('WBDHU')
# only need to do the command below once (i think)
# installSpatialData()

# testing country stuff
# loadSpatialData('NaturalEarthAdm1')
# country <- tbl_df(getCountry(TURB_TSS$lon,TURB_TSS$lat, allData=TRUE))

# get the HUC 12 and HUC 10 id's for each row
huc_ids <- TURB_TSS %>%
  mutate(HUC12_id = getHUC(lon, lat, SPDF = WBDHU12),
         HUC10_id = getHUC(lon, lat, SPDF = WBDHU10))

# get the HUC 12 Names for each HUC ID in the dataset
huc12 <- tbl_df(getHUC(TURB_TSS$lon,TURB_TSS$lat, SPDF = WBDHU12, allData=TRUE)) %>%
  # unite(coords, latitude, longitude, remove = FALSE) %>%
  rename(HUC12_id = HUC, HUC12_Name = HUCName) %>%
  select(HUC12_id, HUC12_Name)

huc12 <- distinct(huc12)

# get the HUC 10 Names for each HUC ID in the dataset
huc10 <- tbl_df(getHUC(TURB_TSS$lon,TURB_TSS$lat, SPDF = WBDHU10, allData=TRUE)) %>%
  rename(HUC10_id = HUC, HUC10_Name = HUCName) %>%
  select(HUC10_id, HUC10_Name)

huc10 <- distinct(huc10)

# join the HUC Names to the HUC ids for each row
water_huc <- huc_ids %>%
  inner_join(huc12, by = 'HUC12_id') %>%
  inner_join(huc10, by = 'HUC10_id')

saveRDS(water_huc, "./data/water_huc.rds")
# whmod2 <- readRDS("./data/water_huc.rds")
################################ HUCs ################################################ 


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
turbidity %>%
  filter(0 < measurement & measurement < 100) %>%
  ggplot(aes(measurement)) +
  geom_histogram(binwidth = 0.5)

# plot mean turbidity by study 
turbidity %>%
  group_by(Study_ID) %>%
  summarise(m.turbidity = mean(measurement)) %>%
  ggplot(aes(x = reorder(Study_ID, m.turbidity), y = m.turbidity)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# plot mean turbidity by location
turbidity %>%
  group_by(Location_ID) %>%
  summarise(m.turbidity = mean(measurement)) %>%
  ggplot(aes(x = reorder(Location_ID, m.turbidity), y = m.turbidity)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# plot mean turbidity by Watershed_WRIA
turbidity %>%
  group_by(Watershed_WRIA) %>%
  summarise(m.turb = mean(measurement)) %>%
  ggplot(aes(x = reorder(Watershed_WRIA, m.turb), y = m.turb)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# plot log turbidity in each wria over time
ggplot(data = turbidity, mapping = aes(x = start_date, y = logMeasurement)) + 
  geom_point() + 
  geom_smooth() +
  # geom_hline(yintercept = 1.90309, color = "red", show.legend = TRUE) +
  facet_wrap(~ Watershed_WRIA) +
  xlab("Year") +
  ylab("Log Turbidity (NTU)") +
  ggtitle("Turbidity Trends Over Time")

# funding project data is from 2007-2015 ONLY
turbidity %>%
  filter(format(start_date,"%Y") >= 2004) %>%
  ggplot(mapping = aes(x = start_date, y = logMeasurement)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ Watershed_WRIA) +
  xlab("Year") +
  ylab("Log Turbidity (NTU)") +
  ggtitle("Turbidity Trends Over Time (2004-2016)")

# funding project data is from 2007-2015 ONLY
# only want longer length projects for meta-analysis
filtered.turb <- turbidity %>%
  mutate(year = format(start_date,"%Y")) %>%
  filter(year >= 2004) %>%
  group_by(Study_ID) %>%
  filter(n_distinct(year) >= 6)

filtered.turb %>%
  ggplot(mapping = aes(x = start_date, y = logMeasurement)) +
  geom_point(mapping = aes(color = Study_ID)) +
  geom_smooth() +
  # facet_wrap(~ Watershed_WRIA) +
  xlab("Year") +
  ylab("Log Turbidity (NTU)") +
  ggtitle("Turbidity Trends Over Time (2004-2016)")

# plot turbidity vs tss over time
water_huc %>%
  ggplot(mapping = aes(x = start_date, y = logMeasurement)) +
  geom_point(mapping = aes(color = result_type)) +
  geom_smooth(mapping = aes(x = start_date, y = logMeasurement, linetype = result_type)) +
  facet_grid(result_type~ Watershed_WRIA) +
  xlab("Year") +
  ylab("") +
  ggtitle("Turbidity vs TSS") +
  scale_x_date(date_breaks = '10 years', date_labels = '%Y')
################################ EDA plots ################################################ 
