library(tidyverse)
# library(stringr)
# library(MazamaSpatialUtils)
library(lsr) #cohensD function package
library(leaflet)

# read in the project-huc dataframe
ph <- readRDS("./data/project_huc.rds") 

# read in the chum-huc dataframe, tidy data
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

# read in the chum-counts dataframe, join with location/huc data
chum_counts <- readRDS("../psp-chum/data/tidychum.rds") %>%
  left_join(chum_locations)

# create a HUC-10 project dataframe, 
# filtered to only include projects in fish HUC10s
# calculate the median project implementation year for each HUC10
ph10 <- readRDS("./data/project_huc.rds") %>%
  filter(HUC10_Name %in% unique(chum_counts$HUC10_Name))  %>%
  group_by(HUC10_Name) %>%
  mutate(meanyr = mean(as.numeric(year)),
         medianyr = median(as.numeric(year)))

# create a HUC10-median year entity to merge with outcome dataframes below
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

# create a HUC-12 project dataframe, 
# filtered to only include projects in fish HUC12s
# calculate the median project implementation year for each HUC12
ph12 <- readRDS("./data/project_huc.rds") %>%
  filter(HUC12_Name %in% unique(chum_counts$HUC12_Name)) %>%
  group_by(HUC12_Name) %>%
  mutate(meanyr = mean(as.numeric(year)),
         medianyr = median(as.numeric(year)))

# create a HUC12-median year entity to merge with outcome dataframes below
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

# read in the water quality dataframe, 
# filtered to only include measurements in fish HUC12s
# add median project year for each HUC
# group by measurement type (TSS or turbidity), then by HUC 12
# categorize each row as before, during or after the median project year in each HUC
water12 <- readRDS("../turbidity/data/water_huc.rds") %>%
  filter(HUC12_Name %in% unique(chum_counts$HUC12_Name)) %>%
  mutate(year = format(start_date,"%Y")) %>%
  left_join(huc12_med, by = "HUC12_Name") %>%
  group_by(result_type, HUC12_Name) %>%
  mutate(TimePeriod = ifelse(year > medianyr, 'after',
                             ifelse(year < medianyr, 'before',
                                    'during')))

# read in the water quality dataframe, 
# filtered to only include measurements in fish HUC10s
# add median project year for each HUC
# group by measurement type (TSS or turbidity), then by HUC 10
# categorize each row as before, during or after the median project year in each HUC
water10 <- readRDS("../turbidity/data/water_huc.rds") %>%
  filter(HUC10_Name %in% unique(chum_counts$HUC10_Name)) %>%
  mutate(year = format(start_date,"%Y")) %>%
  left_join(huc10_med, by = "HUC10_Name") %>%
  group_by(result_type, HUC10_Name) %>%
  mutate(TimePeriod = ifelse(year > medianyr, 'after',
                             ifelse(year < medianyr, 'before',
                                    'during')))

# create a dataframe with the mean water quality measurements before/after projects
# obtain the mean water quality measurement for each HUC for each time period
bwa10 <- water10 %>%
  filter(TimePeriod == 'before') %>%
  spread(TimePeriod, measurement) %>%
  summarise(meanbefore = mean(before))

awa10 <- water10 %>%
  filter(TimePeriod == 'after') %>%
  spread(TimePeriod, measurement) %>%
  summarise(meanafter = mean(after))


# calculate the cohensD for each measurement in each HUC (add new column)
# then, tidy the output dataframe
wa10 <- water10 %>%
  filter(!is.na(TimePeriod), !(TimePeriod == 'during')) %>%
  spread(TimePeriod, measurement) %>%
  mutate(cohensd = cohensD(before, after)) %>%
  gather('TimePeriod', 'measurement', `before`, `after`) %>%
  filter(!is.na(measurement)) %>%
  mutate(effectsize = ifelse(cohensd < 0.5, 'small',
                             ifelse(cohensd >= 0.8, 'large',
                                    'medium'))) %>%
  left_join(bwa10, by = "HUC10_Name") %>%
  left_join(awa10, by = "HUC10_Name") %>%
  mutate(status = ifelse(meanbefore > meanafter, 'worse',
                         ifelse(meanbefore < meanafter, 'improving',
                                'no change')),
         color = ifelse(status == 'worse', 'red',
                        ifelse(status == 'improving', 'green',
                               'black')))



# create a dataframe with the mean water quality measurements before/after projects
# obtain the mean water quality measurement for each HUC for each time period
bwa12 <- water12 %>%
  filter(TimePeriod == 'before') %>%
  spread(TimePeriod, measurement) %>%
  summarise(meanbefore = mean(before))

awa12 <- water12 %>%
  filter(TimePeriod == 'after') %>%
  spread(TimePeriod, measurement) %>%
  summarise(meanafter = mean(after))



# calculate the cohensD for each measurement in each HUC (add new column)
# then, tidy the output dataframe     
wa12 <- water12 %>%
  filter(!is.na(TimePeriod), !(TimePeriod == 'during')) %>%
  spread(TimePeriod, measurement) %>%
  mutate(cohensd = cohensD(before, after)) %>%
  gather('TimePeriod', 'measurement', `before`, `after`) %>%
  filter(!is.na(measurement)) %>%
  mutate(effectsize = ifelse(cohensd < 0.5, 'small',
                             ifelse(cohensd >= 0.8, 'large',
                                    'medium'))) %>%
  left_join(bwa12, by = "HUC12_Name") %>%
  left_join(awa12, by = "HUC12_Name") %>%
  mutate(status = ifelse(meanbefore > meanafter, 'worse',
                         ifelse(meanbefore < meanafter, 'improving',
                                'no change')),
         color = ifelse(status == 'worse', 'red',
                        ifelse(status == 'improving', 'green',
                               'black')))
# read in the chum count dataframe, 
# add median project year for each HUC
# group by HUC 10
# categorize each row as before, during or after the median project year in each HUC
# obtain the mean salmon count for each HUC for each time period
bchum10 <- chum_counts %>%
  left_join(huc10_med, by = "HUC10_Name") %>%
  group_by(HUC10_Name) %>%
  mutate(TimePeriod = ifelse(year > medianyr, 'after',
                             ifelse(year < medianyr, 'before',
                                    'during'))) %>%
  filter(TimePeriod == 'before') %>%
  spread(TimePeriod, count) %>%
  summarise(meanbefore = mean(before))

achum10 <- chum_counts %>%
  left_join(huc10_med, by = "HUC10_Name") %>%
  group_by(HUC10_Name) %>%
  mutate(TimePeriod = ifelse(year > medianyr, 'after',
                             ifelse(year < medianyr, 'before',
                                    'during'))) %>%
  filter(TimePeriod == 'after') %>%
  spread(TimePeriod, count) %>%
  summarise(meanafter = mean(after))

# read in the chum count dataframe, 
# add median project year for each HUC
# group by HUC 10
# categorize each row as before, during or after the median project year in each HUC
# calculate the cohensD in each HUC (add new column)
# then, tidy the output dataframe
# join with summary mean by time period, then classify each according to change in mean fish count
chum10 <- chum_counts %>%
  left_join(huc10_med, by = "HUC10_Name") %>%
  group_by(HUC10_Name) %>%
  mutate(TimePeriod = ifelse(year > medianyr, 'after',
                             ifelse(year < medianyr, 'before',
                                    'during'))) %>%
  filter(!is.na(TimePeriod), !(TimePeriod == 'during')) %>%
  spread(TimePeriod, count) %>%
  mutate(cohensd = cohensD(before, after)) %>%
  gather('TimePeriod', 'count', `before`, `after`) %>%
  filter(!is.na(count)) %>%
  mutate(effectsize = ifelse(cohensd < 0.5, 'small',
                             ifelse(cohensd >= 0.8, 'large',
                                    'medium'))) %>%
  left_join(bchum10, by = "HUC10_Name") %>%
  left_join(achum10, by = "HUC10_Name") %>%
  mutate(status = ifelse(meanbefore > meanafter, 'worse',
                         ifelse(meanbefore < meanafter, 'improving',
                                'no change')),
         color = ifelse(status == 'worse', 'red',
                        ifelse(status == 'improving', 'green',
                               'black')))

# read in the chum count dataframe, 
# add median project year for each HUC
# group by HUC 12
# categorize each row as before, during or after the median project year in each HUC
# obtain the mean salmon count for each HUC for each time period
bchum12 <- chum_counts %>%
  left_join(huc12_med, by = "HUC12_Name") %>%
  group_by(HUC12_Name) %>%
  mutate(TimePeriod = ifelse(year > medianyr, 'after',
                             ifelse(year < medianyr, 'before',
                                    'during'))) %>%
  filter(TimePeriod == 'before') %>%
  spread(TimePeriod, count) %>%
  summarise(meanbefore = mean(before))

achum12 <- chum_counts %>%
  left_join(huc12_med, by = "HUC12_Name") %>%
  group_by(HUC12_Name) %>%
  mutate(TimePeriod = ifelse(year > medianyr, 'after',
                             ifelse(year < medianyr, 'before',
                                    'during'))) %>%
  filter(TimePeriod == 'after') %>%
  spread(TimePeriod, count) %>%
  summarise(meanafter = mean(after))


# read in the chum count dataframe, 
# add median project year for each HUC
# group by HUC 12
# categorize each row as before, during or after the median project year in each HUC
# calculate the cohensD in each HUC (add new column)
# then, tidy the output dataframe
chum12 <- chum_counts %>%
  left_join(huc12_med, by = "HUC12_Name") %>%
  group_by(HUC12_Name) %>%
  mutate(TimePeriod = ifelse(year > medianyr, 'after',
                             ifelse(year < medianyr, 'before',
                                    'during'))) %>%
  filter(!is.na(TimePeriod), !(TimePeriod == 'during')) %>%
  spread(TimePeriod, count) %>%
  mutate(cohensd = cohensD(before, after)) %>%
  gather('TimePeriod', 'count', `before`, `after`) %>%
  filter(!is.na(count)) %>%
  mutate(effectsize = ifelse(cohensd < 0.5, 'small',
                             ifelse(cohensd >= 0.8, 'large',
                                    'medium'))) %>%
  left_join(bchum12, by = "HUC12_Name") %>%
  left_join(achum12, by = "HUC12_Name") %>%
  mutate(status = ifelse(meanbefore > meanafter, 'worse',
                         ifelse(meanbefore < meanafter, 'improving',
                                'no change')),
         color = ifelse(status == 'worse', 'red',
                         ifelse(status == 'improving', 'green',
                                'black')))

# cohen's D: 'small effect' = 0.2, med effect = 0.5, large effect = 0.8

m12 <- leaflet(data = chum12) %>% 
  setView(lng = -122.996823, lat = 47.5642594, zoom = 9)

m12 %>%
  addProviderTiles("Stamen.Terrain") %>%
  addCircleMarkers(~lon, ~lat, popup = chum12$status,
                   radius = 6,
                   color = chum12$color,
                   stroke = FALSE, fillOpacity = 0.5,
                   group = "Chum") %>%
  addCircleMarkers(ph12$lon, ph12$lat, popup = ph12$year,
                   radius = 6,
                   color = 'orange',
                   stroke = FALSE, fillOpacity = 0.5,
                   group = "Projects") %>%
  addCircleMarkers(wa12$lon, wa12$lat, popup = wa12$measurement,
                   radius = 6,
                   color = wa12$color,
                   stroke = FALSE, fillOpacity = 0.5,
                   group = "Water") %>%
  addLayersControl(
    baseGroups = c("Chum", "Water"),
    overlayGroups = "Projects",
    options = layersControlOptions(collapsed = FALSE)
  )

m10 <- leaflet() %>% 
  setView(lng = -122.996823, lat = 47.5642594, zoom = 9) %>%
  addProviderTiles("Stamen.Terrain") %>%
  addCircleMarkers(ph10$lon, ph10$lat, popup = ph10$year,
                   radius = 4,
                   color = 'orange',
                   stroke = FALSE, fillOpacity = 1,
                   group = "Projects") %>%
  addCircleMarkers(chum10$lon, chum10$lat, popup = chum10$status,
                   radius = 6,
                   color = chum10$color,
                   stroke = FALSE, fillOpacity = 0.5,
                   group = "Chum") %>%
  addCircleMarkers(wa10$lon, wa10$lat, popup = wa10$measurement,
                   radius = 6,
                   color = wa10$color,
                   stroke = FALSE, fillOpacity = 0.2,
                   group = "Water") %>%
  addLayersControl(
    baseGroups = c("Chum", "Water"),
    overlayGroups = "Projects",
    options = layersControlOptions(collapsed = FALSE)
  )
m10
