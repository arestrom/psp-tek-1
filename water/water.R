library(tidyverse)
library(MazamaSpatialUtils)

# NOTE: http://apps.leg.wa.gov/WAC/default.aspx?dispo=true&cite=173-201A-200
#Turbidity shall not exceed:
# 5 NTU over background when the background is 50 NTU or less; or
# A 10 percent increase in turbidity when the background turbidity is more than 50 NTU.

# load location data file
# select only ID and WRIA
turbidity_locations <- read.csv('./data/turbidity/EIMLocationDetails.csv', header = TRUE) %>%
  select(Location_ID, Watershed_WRIA)
# unique(locs$Watershed_WRIA)

# create a turbidity data frame 
# for merging with the tss data
# select only variables of interest, rename lengthy variable names
# convert dates to date format
# obtain log(x+1) since there are some negative values if use log(x) (for plots)
turbidity <- read.csv('./data/turbidity/EIMResults.csv', header = TRUE) %>%
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
tss_locations <- read.csv('./data/tss/EIMLocationDetails.csv', header = TRUE) %>%
  select(Location_ID, Watershed_WRIA)

# load the tss data 
# select only variables of interest, rename lengthy variable names
tss <- read.csv('./data/tss/EIMResults.csv', header = TRUE) %>%
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
