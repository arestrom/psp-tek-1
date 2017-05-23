library(tidyverse)
source('../water/addHUC.R')

#################### HWS DATA #################### 
hws <- read.csv('./data/state_hws.csv', header = TRUE) %>%
  setNames(tolower(names(.))) %>%
  rename(Study_ID = id, CurrentStatus = status, sponsor = organization) %>%
  mutate(project_source = 'HWS',
         year = as.numeric(format(as.Date(start, format = "%Y-%m-%d"), '%Y')),
         CurrentStatus = as.character(CurrentStatus),
         CurrentStatus = ifelse(CurrentStatus == 'Dormant' | CurrentStatus == 'Proposed' | CurrentStatus == 'Conceptual',
                                'Inactive', CurrentStatus),
         description = paste(name, sponsor, CurrentStatus, sep = '; ')) %>%
  filter(!lat == 0) %>%
  select(name:cost, sponsor:lon, project_source:description) %>%   
  filter(year <= 2015) %>%
  filter(year > 2000)

# add unique ID column
hws$ID <- 1:nrow(hws)
  
  
################################ HUCs ################################################ 

huc8 <- addHUC(hws, 'HUC8_id', 'HUC8_Name', WBDHU8) 

huc12 <- addHUC(hws, 'HUC12_id', 'HUC12_Name', WBDHU12) %>%
  select(HUC12_id, HUC12_Name, ID)

state_hucs <- left_join(huc8, huc12, by = 'ID')

saveRDS(state_hucs, "./data/state_project_huc.rds")

################################ HUCs ################################################ 