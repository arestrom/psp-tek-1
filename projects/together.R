library(tidyverse)
# library(stringr)
# library(MazamaSpatialUtils)
library(lsr) 


ph <- readRDS("./data/project_huc.rds") 

chum_locations <- readRDS("../psp-chum/data/chum_huc.rds") %>%
  rename (lon = lng, site = River.site) %>%
  select(-(Permanent:Line.NR))
# print out distinct HUC names (chum are the smallest group, limits all other measurements)
unique(chum$HUC10_Name)
# [1] "Little Quillcene River-Frontal Hood Canal"
# [2] "Jefferson Creek-Hamma Hamma River"        
# [3] "Tahuya River-Frontal Hood Canal"          
# [4] "Hood Canal"                               
# [5] "Lilliwaup Creek-Frontal Hood Canal"       
# [6] "Skokomish River-Frontal Hood Canal" 

unique(chum$HUC12_Name)
# [1] "Spencer Creek-Frontal Dabob Bay"   
# [2] "Hamma Hamma River"                 
# [3] "Tahuya River"                      
# [4] "Hood Canal"                        
# [5] "Big Beef Creek-Frontal Hood Canal" 
# [6] "Tarboo Creek-Frontal Dabob Bay"    
# [7] "Dewatto River"                     
# [8] "Finch Creek-Frontal Hood Canal"    
# [9] "Skokomish River-Frontal Hood Canal"

chum_counts <- readRDS("../psp-chum/data/tidychum.rds") %>%
  left_join(chum_locations)

ph10 <- readRDS("./data/project_huc.rds") %>%
  filter(HUC10_Name %in% unique(chum_counts$HUC10_Name))  %>%
  group_by(HUC10_Name) %>%
  mutate(meanyr = mean(as.numeric(year)),
         medianyr = median(as.numeric(year)))

huc10_med <- ph10 %>%
  select(HUC10_Name, medianyr) %>%
  group_by(HUC10_Name, medianyr) %>%
  summarise()

# sum10<- ph10 %>%
#   group_by(HUC10_Name) %>%
#   summarise(meanyr = mean(year),
#             medianyr = median(year),
#             maxyr = max(year),
#             minyr = min(year),
#             numyrs = n_distinct(year))

ph12 <- readRDS("./data/project_huc.rds") %>%
  filter(HUC12_Name %in% unique(chum_counts$HUC12_Name)) %>%
  group_by(HUC12_Name) %>%
  mutate(meanyr = mean(as.numeric(year)),
         medianyr = median(as.numeric(year)))

huc12_med <- ph12 %>%
  select(HUC12_Name, medianyr) %>%
  group_by(HUC12_Name, medianyr) %>%
  summarise()
# 
# sum12<- ph12 %>%
#   group_by(HUC12_Name) %>%
#   summarise(meanyr = mean(year),
#             medianyr = median(year),
#             maxyr = max(year),
#             minyr = min(year),
#             numyrs = n_distinct(year))

wa10 <- readRDS("../turbidity/data/water_huc.rds") %>%
  filter(HUC10_Name %in% unique(chum_counts$HUC10_Name)) %>%
  mutate(year = format(start_date,"%Y")) %>%
  left_join(huc10_med, by = "HUC10_Name") %>%
  group_by(HUC10_Name) %>%
  mutate(TimePeriod = ifelse(year > medianyr, 'after',
                             ifelse(year < medianyr, 'before',
                                    'during')))
         
wa12 <- readRDS("../turbidity/data/water_huc.rds") %>%
 filter(HUC12_Name %in% unique(chum_counts$HUC12_Name)) %>%
 mutate(year = format(start_date,"%Y")) %>%
 left_join(huc12_med, by = "HUC12_Name") %>%
 group_by(HUC12_Name) %>%
 mutate(TimePeriod = ifelse(year > medianyr, 'after',
                            ifelse(year < medianyr, 'before',
                                   'during')))

# wa10 %>%
#   filter(TimePeriod == 'before', TimePeriod == 'after') %>%
#   group_by(HUC10_Name, TimePeriod) %>%
#   mutate(cohensd = cohensD())


chum10 <- chum_counts %>%
  left_join(huc10_med, by = "HUC10_Name") %>%
  group_by(HUC10_Name) %>%
  mutate(TimePeriod = ifelse(year > medianyr, 'after',
                             ifelse(year < medianyr, 'before',
                                    'during'))) %>%
  filter(!is.na(TimePeriod), !(TimePeriod == 'during')) %>%
  spread(TimePeriod, count) %>%
  mutate(cohensd = cohensD(before, after)) %>%
  gather('TimePeriod', 'count', `before`, `after`)

chum12 <- chum_counts %>%
  left_join(huc12_med, by = "HUC12_Name") %>%
  group_by(HUC12_Name) %>%
  mutate(TimePeriod = ifelse(year > medianyr, 'after',
                             ifelse(year < medianyr, 'before',
                                    'during'))) %>%
  filter(!is.na(TimePeriod), !(TimePeriod == 'during')) %>%
  spread(TimePeriod, count) %>%
  mutate(cohensd = cohensD(before, after)) %>%
  gather('TimePeriod', 'count', `before`, `after`)

