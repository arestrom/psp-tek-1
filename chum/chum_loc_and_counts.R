#
# DESCRIPTION:
# This script loads and prepares Hood Canal ESU chum salmon geospatial
# and yearly measurement data for use in project data analysis pipeline
# and visualization prototype.

# INPUTS:
# - (2) CSV files with yearly chum salmon measurement data
# - (1) Shapefiles with stream polylines and associated metadata
# - (1) Manually generated CSV file of desired "mouth of the river"
#   stream reach & permanent codes to locate measurement sites

# OUTPUTS:
# - (2) RDS files - one for geospatial data, one for measurement data

# WHAT THE SCRIPT DOES:
# - reads geospatial shapefiles into a SpatialLinesDataFrame
#
# - reads CSV file of desired site data and builds filtering vector
#
# - uses that vector to filter the spatial data
#
# - transforms the spatial data coordinate system to lat-long
#   (Leaflet requires lat-long coordinates)
#
# - converts polyline data to point data by coordinate selection 
#   within the polyline coordinates and conversion of data structure 
#   to a SpatialPointsDataFrame
#
# - cleans up and merges the measurement site geospatial data 
#   with descriptive data about each site
#
# - uses MazamaSpatialUtils to assign HUC-10 and HUC-12 IDs, names
#   to each measurement row
#
# - reads in (2) CSV files of yearly measurement data,
#   and then merges them and tidies the resulting data frame
#
# - outputs geospatial data and measurement data as (2) RDS files


## ----load libraries------------------------------------------------------
library(tidyverse) # tons of data manipulating utilities
library(rgdal) # to import WBD_PRJ_HYDRO shapefiles into an R dataframe
library(forcats) # for factor revalue with tidyverse
library(MazamaSpatialUtils) # for lat-long to HUC unit assignment
library(spdplyr) # to manipulate attributes of a spatial dataframe

## ----load full spatial data----------------------------------------------

# load in geospatial shapefiles as SpatialLinesDataFrame
prj_hydro.mp <- readOGR(dsn=path.expand("./data"), 
                        layer="WBD_PRJ_HYDRO")

## ----filter spatial data to just chum reaches----------------------------
# read in CSV of the chum measurement locations (Permanent IDs)
# that I derived manually from using http://mapshaper.org/
# to locate the reach at the mouth of each river with chum salmon data
# (NOTE: This manual step eventually can be replaced by SWIFD API call)
chum_permanent_codes <- read.csv("data/hoodCanalChumSiteReachCodes.csv",
                                 header=TRUE)

# pare down chum_permanent_codes data (this will be used later)
keeps <-c("River.site", "Description", "Permanent")
chum_sites_info <- chum_permanent_codes[keeps]

# next, build a vector of Permanent IDs as filter for spatial data
wanted_chum_sites <-
  unlist(unique(chum_sites_info["Permanent"]))

# then, use that vector list to filter the spatial data (using spdplyr)
chum_sites.mp <- prj_hydro.mp %>%
  filter(Permanent_ %in% wanted_chum_sites)

# reset the rownames of the SpatialLinesDF "Data" slot
rownames(chum_sites.mp@data) <- 1:nrow(chum_sites.mp@data)

## ----convert data to lat long and isolate measurement points------------
# first, we convert the data projection from UTM to lat long
chumsites <- spTransform(chum_sites.mp, CRS("+init=epsg:4326"))

# now, we'll isolate a measurement POINT on each remaining polyline
# based on domain experts, this is a good-enough approximation of the
# measurement sites for our HUC-10 and HUC-12 aggregation levels

# All streams:
# select and retain just the first coordinate pair for each polyline
# this makes sure measurement points land in correct HUC-10, HUC-12 units
for (i in seq_along(chumsites@lines)){
  chumsites@lines[[i]]@Lines[[1]]@coords =
    head(chumsites@lines[[i]]@Lines[[1]]@coords, n=1)
}

# finally, we convert from a SpatialLinesDF to a SpatialPointsDF
point_chumsites = as(chumsites, "SpatialPointsDataFrame")

## ----merge chum sites and chum measurement data--------------------------
# convert to standard data frame for easy merging, tidy data
chumsites_df <- as.data.frame(point_chumsites) %>%
  rename(lon = coords.x1,
         lat = coords.x2,
         Permanent = Permanent_)

# merge with the chum sites info
all_chum <- merge(chumsites_df, chum_sites_info, by="Permanent")

# tidy data so names will match those in the chum measurement data
all_chum <- all_chum %>%
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
                                 "Chimacum Creek - Natural" = "2. Chimacum Creek Summer Chum: Natural-Origin Spawners (Prop)",
                                 "Dungeness River - Natural" = "3. Dungeness Summer Chum: Natural-Origin Spawners (Prop)",
                                 "Jimmycomelately Creek - Hatchery Escapement" = "4. Jimmycomelately Creek Summer Chum: Index Hatchery Escapement",
                                 "Jimmycomelately Creek - Natural Escapement" = "5. Jimmycomelately Creek Summer Chum: Index Natural Escapement",
                                 "Jimmycomelately Creek - Natural Spawners" = "6. Jimmycomelately Creek Summer Chum: Natural-Origin Spawners (Prop)",
                                 "Snow/Salmon Creeks - Escapement" = "7. Snow/Salmon Creeks Summer Chum: Escapement (Proportion)",
                                 "Salmon Creek - Hatch" = "8. Snow/Salmon Creeks Summer Chum: Index Hatchery Escapement",
                                 "Snow Creek - Hatch" = "9. Snow/Salmon Creeks Summer Chum: Index Hatchery Escapement",
                                 "Salmon Creek - Natural" = "10. Snow/Salmon Creeks Summer Chum: Index Natural Escapement",
                                 "Snow Creek - Natural" = "11. Snow/Salmon Creeks Summer Chum: Index Natural Escapement")) %>%
  rename(site = River.site) %>%
  separate(site, into = c("sitename", "project_cat"), sep = " - ",
           remove = FALSE)

# because we have points at the same lat/long, I'm going to jitter 
# that data only for the visualization 
# (NOTE: This is no longer being used for visualization)

all_chum$jitterlng <- as.numeric(jitter(all_chum$lon, factor = 10))
all_chum$jitterlat <- as.numeric(jitter(all_chum$lat, factor = 10))


##--------Assign HUC-10 and HUC-12 units to spatial data------------------ 

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
huc_ids <- all_chum %>%
  mutate(HUC12_id = getHUC(lon, lat, SPDF = WBDHU12),
         HUC10_id = getHUC(lon, lat, SPDF = WBDHU10))

# THIS TAKES A WHILE
# get the HUC 12 Names for each HUC ID in the dataset
huc12 <- tbl_df(getHUC(all_chum$lon, all_chum$lat, SPDF = WBDHU12, allData=TRUE)) %>%
  # unite(coords, latitude, longitude, remove = FALSE) %>%
  rename(HUC12_id = HUC, HUC12_Name = HUCName) %>%
  select(HUC12_id, HUC12_Name)

huc12 <- distinct(huc12)

# THIS TAKES A WHILE
# get the HUC 10 Names for each HUC ID in the dataset
huc10 <- tbl_df(getHUC(all_chum$lon, all_chum$lat, SPDF = WBDHU10, allData=TRUE)) %>%
  rename(HUC10_id = HUC, HUC10_Name = HUCName) %>%
  select(HUC10_id, HUC10_Name)

huc10 <- distinct(huc10)

# join the HUC Names to the HUC ids for each row
chum_huc <- huc_ids %>%
  inner_join(huc12, by = 'HUC12_id') %>%
  inner_join(huc10, by = 'HUC10_id')

# because HUCing takes a while, save the output to an r data file 
# that can be loaded in the Shiny app
saveRDS(chum_huc, "./data/chum_huc.rds")

## ----load and tidy chum salmon measurement data-------------------------
# load in yearly chum data from Hood Canal and Strait of Juan de Fuca
chum_measure_hood <- as.data.frame(read.csv("data/hoodCanalSummerChumStats.csv", header = TRUE))
chum_measure_fuca <- as.data.frame(read.csv("data/straitJuanDeFucaSummerChumStats.csv", header = TRUE))

# merge the two data frames, keeping all rows
chum_measure_data <- merge(chum_measure_hood, chum_measure_fuca, by="Year", all=TRUE)

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
              "Chimacum Creek - Natural",
              "Dungeness River - Natural",
              "Jimmycomelately Creek - Hatchery Escapement",
              "Jimmycomelately Creek - Natural Escapement",
              "Jimmycomelately Creek - Natural Spawners",
              "Snow/Salmon Creeks - Escapement",
              "Salmon Creek - Hatch",
              "Snow Creek - Hatch",
              "Salmon Creek - Natural",
              "Snow Creek - Natural")

colnames(chum_measure_data) <- newnames

# tidying the data, start with gather
tidy_chum_data <- gather(chum_measure_data, key, value, -Year)
# making column names simple
colnames(tidy_chum_data) <- c("year", "site", "count")
# change count data to numeric from char
tidy_chum_data$count <- as.numeric(gsub(",","",tidy_chum_data$count))
# dealing with N/A values and tidying categorical variable
tidy_chum_data <- na.omit(tidy_chum_data) %>%
  separate(site, into = c("sitename", "project_cat"), sep = " - ",
           remove = FALSE)

# save out the tidied chum data as RDS file
saveRDS(tidy_chum_data, "./data/tidychum.rds")
