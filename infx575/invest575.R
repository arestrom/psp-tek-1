library(tidyverse)
library(stringr)
library(MazamaSpatialUtils)

#################### EAGL DATA #################### 
eagl_df <- read.csv('../projects/data/eagl.csv', header = TRUE) %>%
  rename(year = Funding.Fiscal.Year, name = Project.Title,
         cost = Funding.Provided, lat = Latitude, lon = Longitude,
         project_cat = General.Project.Category, Study_ID = Funding.Number, 
         CurrentStatus = Status...I.Inactive.Closed..A.Active.,
         LegDist = Leg.Dist, CongDist = Cong.Dist) %>%
  # remove missing coordinates
  filter(!lat %in% c('#N/A', '0'),
         !lon %in% c('#N/A', '0')) %>%
  # convert dollar strings ($50,000.00) to numeric values (50000) 
  mutate(cost_sm = str_sub(cost, 2, -4),
         cost = as.numeric(gsub(",", "", cost_sm)),
         lat = as.numeric(levels(lat))[lat],
         lon = as.numeric(levels(lon))[lon],
         project_source = 'EAGL',
         County = tolower(County)) %>%
  mutate_each(funs(as.character), contains("SRF.CBR.Project..Needs")) %>%
  select(-cost_sm) %>%
  gather(key = ProjectCatNames, value = FullProjectType,
         SRF.CBR.Project..Needs..Category.1, 
         SRF.CBR.Project..Needs..Category.2, 
         SRF.CBR.Project..Needs..Category.3, na.rm = TRUE)

#################### PRISM DATA #################### 
# read in hood canal prism locations spreadsheet, correct project numbers
location <- read.csv('./data/HoodCanal_LocationData.csv', header = TRUE) %>%
  mutate(ProjectNumber = as.character(ProjectNumber)) %>%
  mutate(ProjectNumber = replace(ProjectNumber, ProjectNumber == 'Jul-15', '07-1915')) %>%
  mutate(ProjectNumber = replace(ProjectNumber, ProjectNumber == 'Jul-16', '07-1916')) %>%
  mutate(ProjectNumber = replace(ProjectNumber, ProjectNumber == 'Jul-21', '07-2021')) %>%
  mutate(ProjectNumber = replace(ProjectNumber, ProjectNumber == 'Jul-25', '07-1925')) %>%
  mutate(ProjectNumber = replace(ProjectNumber, ProjectNumber == 'Aug-57', '08-2157')) %>%
  mutate(ProjectNumber = replace(ProjectNumber, ProjectNumber == 'Aug-90', '08-1990')) %>%
  select(-ProjectType, -ProjectName)

funding <- read.csv('./data/HoodCanal_FundingInfo.csv', header = TRUE) %>%
  mutate(ProjectNumber = as.character(ProjectNumber)) %>%
  mutate_each(funs(as.numeric), PrimaryProgramAmount, RCOAmount:FedLE, 
              TotalFedFund, TotalStateFund) %>%
  gather(key = AmountAttribute, value = Amount, PrimaryProgramAmount,
         RCOAmount, SponsorMatchAmount, PSARAmount:FedLE, TotalFedFund, 
         TotalStateFund, na.rm = TRUE) %>%
  select(-SnapshotLink)

costbd <- read.csv('./data/HoodCanal_CostBreakDown.csv', header = TRUE) %>%
  select(-ProjectType, -ProjectName, -CurrentStatus) %>%
  mutate_each(funs(as.character))

metrics <- read.csv('./data/HoodCanal_MetricData.csv', header = TRUE) %>%
  select(-ProjectType, -ProjectName, -CurrentStatus) %>%
  mutate_each(funs(as.character), contains("Metric")) %>%
  gather(key = ActionMetricAttribute, value = ActionMetric,
         Action_MetricLevel, ActionSummary_MetricLevel, 
         MetricCategory:MetricDetail3,
         na.rm = TRUE)

scope <- read.csv('./data/HoodCanal_ProjectScope.csv', header = TRUE) %>%
  select(-ProjectName, -ProjectNumberCC) %>%
  rename(FullProjectType = ProjectType)

all_PRISM <- left_join(x = location, y = funding, by = "ProjectNumber") %>%
  left_join(costbd, by = "ProjectNumber") %>%
  left_join(scope, by = "ProjectNumber") %>%
  right_join(metrics, by = "ProjectNumber") %>%
  rename(Study_ID = ProjectNumber, year = ProjectYear, name = ProjectName, 
         cost = ProjectTotalAmount, lat = ProjectLatitude, lon = ProjectLongitude,
         project_cat = ProjectType) %>%
  mutate(project_source = 'PRISM',
         County = tolower(County)) %>%
  filter(!is.na(lat), !is.na(cost)) 


#################### MERGE #################### 

all_projects <- bind_rows(mdf, eagl_df)

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