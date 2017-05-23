library(tidyverse)
library(MazamaSpatialUtils)

# following introductory vignette at 
# https://cran.r-project.org/web/packages/MazamaSpatialUtils/vignettes/introduction.html

# need to install the data separately via command line (see mazama github)
setSpatialDataDir('~/Data/Spatial')
loadSpatialData('WBDHU')
# only need to do the command below once (i think)
# installSpatialData()

addHUC <- function(df, HUCNum_id, HUCNum_Name, SPDF) {
  # generate a subset of distinct lat and lon pairs from the large dataset
  df.sm <- distinct(df, lat,lon)
  
  # get the HUC id's for each lat/lon pair
  huc_ids <- df.sm %>%
    mutate(HUCid = getHUC(lon, lat, SPDF = SPDF)) %>%
    rename_(.dots = setNames('HUCid', paste(HUCNum_id)))
  
  # get the HUC Names for each HUC ID in the dataset
  # join the HUC Names to the HUC ids for each row
  huc_name_distinct <- getHUC(df.sm$lon, df.sm$lat, SPDF = SPDF, allData=TRUE) %>%
    rename_(.dots = setNames('HUC', paste(HUCNum_id))) %>%
    rename_(.dots = setNames('HUCName', paste(HUCNum_Name))) %>%
    # select HUC name and id columns
    select(2:3) %>%
    distinct()%>%
    right_join(huc_ids, by = HUCNum_id) %>%
    right_join(df, by = c('lat', 'lon'))
  
  return(huc_name_distinct)
  
}