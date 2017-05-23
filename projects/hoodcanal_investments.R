library(tidyverse)
library(stringr)
source('../water/addHUC.R')

#################### EAGL DATA #################### 
# read in the eagl data, clean and standardize data
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

#################### MERGE #################### 
# combine dataframes from both investment sources
all_projects <- bind_rows(all_prism, eagl_df)

# add unique ID column
all_projects$ID <- 1:nrow(all_projects)

#################### MERGE #################### 

################################ HUCs ################################################ 

# add HUC Names and HUC IDs to the projects dataframe by lat/lon (using the addHUC function)
# do this for each desired HUC (here, for huc 10 and huc 12)
# then combine these dataframes to have each huc Name and ID for each project
huc10 <- addHUC(all_projects, 'HUC10_id', 'HUC10_Name', WBDHU10) 

huc12 <- addHUC(all_projects, 'HUC12_id', 'HUC12_Name', WBDHU12) %>%
  select(HUC12_id, HUC12_Name, ID)

# join the HUC Names to the HUC ids for each row
project_huc <- left_join(huc10, huc12, by = 'ID')

saveRDS(project_huc, "./data/project_huc.rds")

################################ HUCs ################################################ 