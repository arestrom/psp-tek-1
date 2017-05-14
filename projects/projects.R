library(tidyverse)
library(stringr)
library(MazamaSpatialUtils)

#################### EAGL DATA #################### 
eagl_df <- read.csv('./data/EAGL_Projects.csv', header = TRUE) %>%
  select(Funding.Fiscal.Year, General.Project.Category, Project.Title, 
         Funding.Provided, Latitude, Longitude, Funding.Number) %>%
  rename(year = Funding.Fiscal.Year, name = Project.Title,
         cost = Funding.Provided, lat = Latitude, lon = Longitude,
         project_cat = General.Project.Category,
         Study_ID = Funding.Number) %>%
  # remove missing coordinates
  filter(!lat %in% c('#N/A', '0'),
         !lon %in% c('#N/A', '0')) %>%
  # convert dollar strings ($50,000.00) to numeric values (50000) 
  mutate(cost_sm = str_sub(cost, 2, -4),
         cost = as.numeric(gsub(",", "", cost_sm)),
         lat = as.numeric(levels(lat))[lat],
         lon = as.numeric(levels(lon))[lon],
         project_source = 'EAGL') %>%
  select(-cost_sm)

#################### PRISM DATA #################### 
# read in hood canal prism locations spreadsheet, correct typo project numbers, remove excess cols
location <- read.csv('./data/HoodCanal_LocationData.csv', header = TRUE) %>%
  mutate(ProjectNumber = as.character(ProjectNumber)) %>%
  mutate(ProjectNumber = replace(ProjectNumber, ProjectNumber == 'Jul-15', '07-1915')) %>%
  mutate(ProjectNumber = replace(ProjectNumber, ProjectNumber == 'Jul-16', '07-1916')) %>%
  mutate(ProjectNumber = replace(ProjectNumber, ProjectNumber == 'Jul-21', '07-2021')) %>%
  mutate(ProjectNumber = replace(ProjectNumber, ProjectNumber == 'Jul-25', '07-1925')) %>%
  mutate(ProjectNumber = replace(ProjectNumber, ProjectNumber == 'Aug-57', '08-2157')) %>%
  mutate(ProjectNumber = replace(ProjectNumber, ProjectNumber == 'Aug-90', '08-1990')) %>%
  select(-ProjectType, -ProjectName)

# read in Hood Canal funding csv file, convert ProjectNumber to character type
funding <- read.csv('./data/HoodCanal_FundingInfo.csv', header = TRUE) %>%
  mutate(ProjectNumber = as.character(ProjectNumber))

# merge funding and location prism dataframe, filter out NAs in lat/lon cols
all_prism <- left_join(x = location, y = funding, by = "ProjectNumber") %>%
  select(ProjectNumber, ProjectYear, ProjectName, 
         ProjectTotalAmount, ProjectLatitude, ProjectLongitude, ProjectType) %>%
  rename(Study_ID = ProjectNumber, year = ProjectYear, name = ProjectName, 
         cost = ProjectTotalAmount, lat = ProjectLatitude, lon = ProjectLongitude,
         project_cat = ProjectType) %>%
  mutate(project_source = 'PRISM') %>%
  filter(!is.na(lat), !is.na(cost))

#################### HWS DATA #################### 

# load habitat work schedule data
# clean and standardize data
# filter out extra project years
hws <- read.csv('./data/hws_hcc.csv', header = TRUE) %>%
  setNames(tolower(names(.))) %>%
  rename(Study_ID = id) %>%
  mutate(project_source = 'HWS',
         year = as.numeric(format(as.Date(start, format = "%Y-%m-%d"), '%Y'))) %>%
  filter(!lat == 0,
         year <= 2015,
         year > 2000) %>%
  select(name, Study_ID, cost, lat, lon, project_source, year)

#################### MERGE #################### 
# combine dataframes from both investment sources
all_projects <- bind_rows(all_prism, eagl_df) %>%
  bind_rows(hws)

#################### MERGE #################### 

################################ HUCs ################################################ 
# following introductory vignette at 
# https://cran.r-project.org/web/packages/MazamaSpatialUtils/vignettes/introduction.html

# need to install the data separately via command line (see mazama github)
setSpatialDataDir('~/Data/Spatial')
loadSpatialData('WBDHU')
# only need to do the command below once (i think)
# installSpatialData()

# get the HUC 12 and HUC 10 id's for each row
huc_ids <- all_projects %>%
  mutate(HUC12_id = getHUC(lon, lat, SPDF = WBDHU12),
         HUC10_id = getHUC(lon, lat, SPDF = WBDHU10))

# get the HUC 12 Names for each HUC ID in the dataset
huc12 <- tbl_df(getHUC(all_projects$lon,all_projects$lat, SPDF = WBDHU12, allData=TRUE)) %>%
  # unite(coords, latitude, longitude, remove = FALSE) %>%
  rename(HUC12_id = HUC, HUC12_Name = HUCName) %>%
  select(HUC12_id, HUC12_Name)

huc12 <- distinct(huc12)

# get the HUC 10 Names for each HUC ID in the dataset
huc10 <- tbl_df(getHUC(all_projects$lon,all_projects$lat, SPDF = WBDHU10, allData=TRUE)) %>%
  rename(HUC10_id = HUC, HUC10_Name = HUCName) %>%
  select(HUC10_id, HUC10_Name)

huc10 <- distinct(huc10)

# join the HUC Names to the HUC ids for each row
project_huc <- huc_ids %>%
  inner_join(huc12, by = 'HUC12_id') %>%
  inner_join(huc10, by = 'HUC10_id')

saveRDS(project_huc, "./data/project_huc.rds")

################################ HUCs ################################################ 