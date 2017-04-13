library(tidyverse)
library(stringr)
library(MazamaSpatialUtils)

#################### EAGL DATA #################### 
eagl_df <- read.csv('./data/eagl.csv', header = TRUE) %>%
  select(Funding.Fiscal.Year, WRIA, Project.Title, 
         Funding.Provided, Latitude, Longitude) %>%
  rename(year = Funding.Fiscal.Year, name = Project.Title, WRIA_ID = WRIA,
         cost = Funding.Provided, lat = Latitude, lon = Longitude) %>%
  # remove missing coordinates
  filter(!lat %in% c('#N/A', '0'),
         !lon %in% c('#N/A', '0')) %>%
  # convert dollar strings ($50,000.00) to numeric values (50000) 
  mutate(cost_sm = str_sub(cost, 2, -4),
         cost = as.numeric(gsub(",", "", cost_sm)),
         lat = as.numeric(levels(lat))[lat],
         lon = as.numeric(levels(lon))[lon]) %>%
  select(-cost_sm)

# label wria number with wria name
eagl_df <- eagl_df %>% 
  mutate(WRIA_Name = ifelse(WRIA_ID == 15, "Kitsap",
                          ifelse(WRIA_ID == 14, "Kennedy-Goldsborough",
                                 ifelse(WRIA_ID == 16, "Skokomish-Dosewallips",
                                        ifelse(WRIA_ID == 17, "Quilcence-Snow",
                                               ifelse(WRIA_ID == 18, "Elwah-Dungeness",
                                                      NA))))))

#################### PRISM DATA #################### 
hc_df <- read.csv('./data/locs.csv', header = TRUE)
f_df <- read.csv('./data/fund.csv', header = TRUE)

mdf <- merge(x = hc_df, y = f_df, by = "ProjectNumber", all.x = TRUE) %>%
  select(ProjectNumber, ProjectYear, ProjectName.x, HUC, WRIA, 
         PrimaryProgramAmount, ProjectLatitude, ProjectLongitude) %>%
  rename(id = ProjectNumber, year = ProjectYear, name = ProjectName.x, 
         cost = PrimaryProgramAmount, lat = ProjectLatitude, lon = ProjectLongitude) %>%
  # create wria number and wria name columns from combo string
  separate(WRIA, into = c("WRIA_Name", "WRIA_ID"), sep = " \\(", convert = TRUE) %>%
  mutate(WRIA_ID = as.numeric(str_sub(WRIA_ID, 1, -2))) %>%
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

# testing country stuff
# loadSpatialData('NaturalEarthAdm1')
# country <- tbl_df(getCountry(all_projects$lon,all_projects$lat, allData=TRUE))

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
ph <- readRDS("./data/project_huc.rds")
################################ HUCs ################################################ 
