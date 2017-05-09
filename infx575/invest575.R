library(tidyverse)
library(stringr)
library(MazamaSpatialUtils)

#################### EAGL DATA #################### 
eagl_df <- read.csv('./data/EAGL_Projects.csv', header = TRUE) %>%
  rename(year = Funding.Fiscal.Year, name = Project.Title,
         cost = Funding.Provided, lat = Latitude, lon = Longitude,
         project_cat = General.Project.Category, Study_ID = Funding.Number, 
         CurrentStatus = Status...I.Inactive.Closed..A.Active.,
         LegDist = Leg.Dist, CongDist = Cong.Dist, description = Project.Description,
         sponsor = Fund.Source) %>%
  # remove missing coordinates
  filter(!lat %in% c('#N/A', '0'),
         !lon %in% c('#N/A', '0')) %>%
  mutate_each(funs(as.character), contains("SRF.CBR.Project..Needs")) %>%
  # convert dollar strings ($50,000.00) to numeric values (50000) 
  mutate(cost_sm = str_sub(cost, 2, -4),
         cost = as.numeric(gsub(",", "", cost_sm)),
         lat = as.numeric(levels(lat))[lat],
         lon = as.numeric(levels(lon))[lon],
         project_source = 'EAGL',
         County = tolower(County),
         FullProjectType = ifelse(SRF.CBR.Project..Needs..Category.2 == '', 
                                  SRF.CBR.Project..Needs..Category.1,
                                  paste(SRF.CBR.Project..Needs..Category.1,
                                        SRF.CBR.Project..Needs..Category.2, sep = ' / ')),
         CurrentStatus = ifelse(CurrentStatus == 'I', 'Inactive', 
                                ifelse(CurrentStatus == 'A', 'Active', NA))) %>%
  select(Study_ID, project_source, name, year, cost, lat, lon, project_cat, 
         CurrentStatus, LegDist, CongDist, County, FullProjectType, sponsor, description)

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
  select(-ProjectType, -ProjectName, -ProjectNumberCC, -HUC, -WRIA)

funding <- read.csv('./data/HoodCanal_FundingInfo.csv', header = TRUE) %>%
  rename(sponsor = PrimarySponsorName) %>%
  mutate(ProjectNumber = as.character(ProjectNumber)) %>%
  mutate_each(funs(as.numeric), PrimaryProgramAmount, RCOAmount:FedLE, 
              TotalFedFund, TotalStateFund) %>%
  # gather(key = AmountAttribute, value = Amount, PrimaryProgramAmount,
  #        RCOAmount, SponsorMatchAmount, PSARAmount:FedLE, TotalFedFund, 
  #        TotalStateFund, na.rm = TRUE) %>%
  # select(-SnapshotLink)
    select(ProjectType, ProjectNumber, ProjectName, sponsor, ProjectTotalAmount)

costbd <- read.csv('./data/HoodCanal_CostBreakDown.csv', header = TRUE) %>%
  select(-ProjectType, -ProjectName, -CurrentStatus) %>%
  mutate_each(funs(as.character))

# http://stackoverflow.com/questions/13673894/suppress-nas-in-paste/31508774#31508774
paste5 <- function(..., sep = " ", collapse = NULL, na.rm = F) {
  if (na.rm == F)
    paste(..., sep = sep, collapse = collapse)
  else
    if (na.rm == T) {
      paste.na <- function(x, sep) {
        x <- gsub("^\\s+|\\s+$", "", x)
        ret <- paste(x[!is.na(x) & !(x %in% "")], collapse = sep)
        is.na(ret) <- ret == ""
        return(ret)
      }
      df <- data.frame(..., stringsAsFactors = F)
      ret <- apply(df, 1, FUN = function(x) paste.na(x, sep))
      
      if (is.null(collapse))
        ret
      else {
        paste.na(ret, sep = collapse)
      }
    }
}

metrics <- read.csv('./data/HoodCanal_MetricData.csv', header = TRUE) %>%
  select(-ProjectType, -ProjectName, -CurrentStatus,
         -ProgramName, -(ProposedAnswer:UnitTypeName)) %>%
  distinct(ProjectNumber, MetricCategory, .keep_all = TRUE) %>%
  mutate_each(funs(as.character), contains("Metric")) %>%
  mutate(description = paste5(MetricCategory, MetricDetail1, MetricDetail2, 
                    MetricDetail3, sep = '; ', na.rm = T)) %>%
  select(-(MetricCategory:ActionSummary_MetricLevel)) %>%
  group_by(ProjectNumber) %>%
  summarise(description = paste(description,collapse=' / '))
  # gather(key = ActionMetricAttribute, value = ActionMetric,
  #        Action_MetricLevel, ActionSummary_MetricLevel, 
  #        MetricCategory:MetricDetail3,
  #        na.rm = TRUE)

scope <- read.csv('./data/HoodCanal_ProjectScope.csv', header = TRUE) %>%
  select(-ProjectName, -ProjectNumberCC, -ActionCategory, -ProjectDetail) %>%
  rename(FullProjectType = ProjectType)

all_PRISM <- left_join(x = location, y = funding, by = "ProjectNumber") %>%
  # left_join(costbd, by = "ProjectNumber") %>%
  left_join(scope, by = "ProjectNumber") %>%
  left_join(metrics, by = "ProjectNumber") %>%
  rename(Study_ID = ProjectNumber, year = ProjectYear, name = ProjectName, 
         cost = ProjectTotalAmount, lat = ProjectLatitude, lon = ProjectLongitude,
         project_cat = ProjectType) %>%
  mutate(project_source = 'PRISM',
         County = tolower(County)) %>%
  filter(!is.na(lat), !is.na(cost)) 


#################### MERGE #################### 

all_projects <- bind_rows(all_PRISM, eagl_df)

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