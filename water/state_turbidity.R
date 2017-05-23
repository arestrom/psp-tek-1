library(tidyverse)
library(MazamaSpatialUtils)
source('addHUC.R')
# NOTE: http://apps.leg.wa.gov/WAC/default.aspx?dispo=true&cite=173-201A-200
#Turbidity shall not exceed:
# 5 NTU over background when the background is 50 NTU or less; or
# A 10 percent increase in turbidity when the background turbidity is more than 50 NTU.

# create a turbidity data frame 
# for merging with the tss data
# select only variables of interest, rename lengthy variable names
# convert dates to date format
turbidity <- read.csv('./data/turbidity/state_EIMResults.csv', header = TRUE) %>%
  select(Study_ID, Study_Name, Location_ID, 
         Field_Collection_Start_Date, Field_Collection_Start_Date_Time,
         Result_Value, Calculated_Latitude_Decimal_Degrees_NAD83HARN,
         Calculated_Longitude_Decimal_Degrees_NAD83HARN) %>%
  rename(start_date = Field_Collection_Start_Date, 
         measurement = Result_Value, 
         lat = Calculated_Latitude_Decimal_Degrees_NAD83HARN,
         lon = Calculated_Longitude_Decimal_Degrees_NAD83HARN) %>%
  mutate(start_date = as.Date(start_date, format = "%m/%d/%Y"),
         result_type = 'Turbidity',
         unit = 'NTU',
         year = format(start_date,"%Y")) %>%
  filter(!is.na(measurement))

# add unique ID column
turbidity$ID <- 1:nrow(turbidity)


################################ HUCs ################################################ 

huc8 <- addHUC(turbidity, 'HUC8_id', 'HUC8_Name', WBDHU8) 

huc12 <- addHUC(turbidity, 'HUC12_id', 'HUC12_Name', WBDHU12) %>%
  select(HUC12_id, HUC12_Name, ID)

state_hucs <- left_join(huc8, huc12, by = 'ID')

saveRDS(state_hucs, "./data/state_turbidity_huc.rds")
# whmod2 <- readRDS("./data/water_huc.rds")
################################ HUCs ################################################ 
