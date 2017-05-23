library(tidyverse)
source('addHUC.R')

# NOTE: http://apps.leg.wa.gov/WAC/default.aspx?dispo=true&cite=173-201A-200
#Turbidity shall not exceed:
# 5 NTU over background when the background is 50 NTU or less; or
# A 10 percent increase in turbidity when the background turbidity is more than 50 NTU.

# create a turbidity data frame 
# for merging with the tss data
# select only variables of interest, rename lengthy variable names
# convert dates to date format
# obtain log(x+1) since there are some negative values if use log(x) (for plots)
turbidity <- read.csv('./data/turbidity/EIMResults.csv', header = TRUE) %>%
  select(Study_ID, Study_Name, Location_ID, 
         Field_Collection_Start_Date,
         Result_Value, Calculated_Latitude_Decimal_Degrees_NAD83HARN,
         Calculated_Longitude_Decimal_Degrees_NAD83HARN) %>%
  rename(start_date = Field_Collection_Start_Date, 
         measurement = Result_Value, 
         lat = Calculated_Latitude_Decimal_Degrees_NAD83HARN,
         lon = Calculated_Longitude_Decimal_Degrees_NAD83HARN) %>%
  mutate(start_date = as.Date(start_date, format = "%m/%d/%Y"),
         logMeasurement = log10(measurement+1),
         result_type = 'Turbidity',
         unit = 'NTU',
         year = format(start_date,"%Y")) 

# load the tss data 
# select only variables of interest, rename lengthy variable names
tss <- read.csv('./data/tss/EIMResults.csv', header = TRUE) %>%
  select(Study_ID, Study_Name, Location_ID, 
         Field_Collection_Start_Date,
         Result_Value, Calculated_Latitude_Decimal_Degrees_NAD83HARN,
         Calculated_Longitude_Decimal_Degrees_NAD83HARN) %>%
  rename(start_date = Field_Collection_Start_Date,
         measurement = Result_Value,
         lat = Calculated_Latitude_Decimal_Degrees_NAD83HARN,
         lon = Calculated_Longitude_Decimal_Degrees_NAD83HARN) %>%
  mutate(start_date = as.Date(start_date, format = "%m/%d/%Y"),
         logMeasurement = log10(measurement+1),
         result_type = 'TSS',
         unit = 'mg/L',
         year = format(start_date,"%Y"))


# add both dataframes together
TURB_TSS <- rbind(turbidity,tss) 

# add unique ID column
TURB_TSS$ID <- 1:nrow(TURB_TSS)

################################ HUCs ################################################ 

# add HUC Names and HUC IDs to the water dataframe by lat/lon (using the addHUC function)
# do this for each desired HUC (here, for huc 10 and huc 12)
# then combine these dataframes to have each huc Name and ID for each measurement
huc10 <- addHUC(TURB_TSS, 'HUC10_id', 'HUC10_Name', WBDHU10) 

huc12 <- addHUC(TURB_TSS, 'HUC12_id', 'HUC12_Name', WBDHU12) %>%
  select(HUC12_id, HUC12_Name, ID)

# join the HUC Names to the HUC ids for each row
water_huc <- left_join(huc10, huc12, by = 'ID')

saveRDS(water_huc, "./data/water_huc.rds")
# whmod2 <- readRDS("./data/water_huc.rds")
################################ HUCs ################################################ 
