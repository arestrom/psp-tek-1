library(tidyverse)
# library(stringr)
# library(MazamaSpatialUtils)
library(lsr) #cohensD function package
library(leaflet)

######################## BEGIN PROJ ########################

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
unique(ph10$HUC10_Name)
# [1] "Skokomish River-Frontal Hood Canal"       
# [2] "Little Quillcene River-Frontal Hood Canal"
# [3] "Hood Canal"                               
# [4] "Tahuya River-Frontal Hood Canal"          
# [5] "Lilliwaup Creek-Frontal Hood Canal"   

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

######################## END PROJ ########################

######################## BEGIN WATER ########################

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
unique(water10$HUC10_Name)
# [1] "Tahuya River-Frontal Hood Canal"          
# [2] "Little Quillcene River-Frontal Hood Canal"
# [3] "Jefferson Creek-Hamma Hamma River"        
# [4] "Skokomish River-Frontal Hood Canal"       
# [5] "Lilliwaup Creek-Frontal Hood Canal"       
# [6] "Hood Canal" 

# create a dataframe with the mean water quality measurements before/after projects
# obtain the mean water quality measurement for each HUC for each time period
before_after <- function(type, df, value) {
  df <- if (type == 'before') {
    df %>% filter(TimePeriod == 'before') %>%
      spread_('TimePeriod', value) %>%
      summarise(meanbefore = mean(before))
  } else if (type == 'after') {
    df %>% filter(TimePeriod == 'after') %>%
      spread_('TimePeriod', value) %>%
      summarise(meanafter = mean(after))
  } 
  return(df)
}

bwa10 <- before_after('before', water10, 'measurement')

awa10 <- before_after('after', water10, 'measurement')

bwa12 <- before_after('before', water12, 'measurement')

awa12 <- before_after('after', water12, 'measurement')

# remove rows without a TimePeriod (no project in that HUC to get a median year)
# create a column of measurements for before and after project implementation in each HUC
# calculate the cohensD for each measurement in each HUC (add new column)
# then, categorize the effect size
apply_cohensD <- function(data, outcome) {
  if (outcome == 'water') { data %>%
      filter(!is.na(TimePeriod), !(TimePeriod == 'during')) %>%
      spread(TimePeriod, measurement) %>%
      mutate(cohensd = cohensD(before, after)) %>%
      filter(!cohensd == 'NaN') %>%
      gather('TimePeriod', 'measurement', `before`, `after`) %>%
      filter(!is.na(measurement)) %>%
      mutate(effectsize = ifelse(cohensd < 0.5, 'small',
                                 ifelse(cohensd >= 0.8, 'large',
                                        'medium')))
  } else if (outcome == 'chum') { data %>%
      filter(!is.na(TimePeriod), !(TimePeriod == 'during')) %>%
      spread(TimePeriod, count) %>%
      mutate(cohensd = cohensD(before, after)) %>%
      filter(!cohensd == 'NaN') %>%
      gather('TimePeriod', 'count', `before`, `after`) %>%
      filter(!is.na(count)) %>%
      mutate(effectsize = ifelse(cohensd < 0.5, 'small',
                                 ifelse(cohensd >= 0.8, 'large',
                                        'medium')))
  }
}

# categorize each measurement (higher mean TSS or tubidity is worse)
# then apply a binary color category and a diverging color scale (based on effect size and status)
add_status_colors <- function(data, outcome) {
  if (outcome == 'water') { data %>% 
                  mutate(status = ifelse(meanbefore < meanafter, 'worse',
                                  ifelse(meanbefore > meanafter, 'improving',
                                         'no change')),
                  color = ifelse(status == 'worse', '#e41a1c',
                                 ifelse(status == 'improving', '#4daf4a',
                                        'black')),
                  coloreffect = ifelse(effectsize == 'large' & status == 'worse', '#d73027',
                                       ifelse(effectsize == 'medium' & status == 'worse', '#fc8d59',
                                              ifelse(effectsize == 'small' & status == 'worse', '#fee08b',
                                                     ifelse(effectsize == 'small' & status == 'improving', '#d9ef8b',
                                                            ifelse(effectsize == 'medium' & status == 'improving', '#91cf60',
                                                                   ifelse(effectsize == 'large' & status == 'improving', '#1a9850',
                                                                          NA)))))))
  } else if (outcome == 'chum') { data %>% mutate(status = ifelse(meanbefore > meanafter, 'worse',
                                                                  ifelse(meanbefore < meanafter, 'improving',
                                                                         'no change')),
                                            color = ifelse(status == 'worse', '#e41a1c',
                                                           ifelse(status == 'improving', '#4daf4a',
                                                                  'black')),
                                            coloreffect = ifelse(effectsize == 'large' & status == 'worse', '#d73027',
                                                                 ifelse(effectsize == 'medium' & status == 'worse', '#fc8d59',
                                                                        ifelse(effectsize == 'small' & status == 'worse', '#fee08b',
                                                                               ifelse(effectsize == 'small' & status == 'improving', '#d9ef8b',
                                                                                      ifelse(effectsize == 'medium' & status == 'improving', '#91cf60',
                                                                                             ifelse(effectsize == 'large' & status == 'improving', '#1a9850',
                                                                                                    NA)))))))
      
    }
}

# remove rows without a TimePeriod (no project in that HUC to get a median year)
# create a column of measurements for before and after project implementation in each HUC
# calculate the cohensD for each measurement in each HUC (add new column)
# then, categorize the effect size
# add a column with the mean measurement value before/after project implementation
# apply function created above
# filter out before measurements
wa10 <- water10 %>%
  apply_cohensD ('water') %>%
  left_join(bwa10, by = c("result_type" = "result_type", "HUC10_Name" = "HUC10_Name")) %>%
  left_join(awa10, by = c("result_type" = "result_type", "HUC10_Name" = "HUC10_Name")) %>%
  add_status_colors('water') %>%
  filter(TimePeriod == 'after')

unique(wa10$HUC10_Name)
# [1] "Little Quillcene River-Frontal Hood Canal"
# [2] "Tahuya River-Frontal Hood Canal"          
# [3] "Skokomish River-Frontal Hood Canal" 

# remove rows without a TimePeriod (no project in that HUC to get a median year)
# create a column of measurements for before and after project implementation in each HUC
# calculate the cohensD for each measurement in each HUC (add new column)
# then, categorize the effect size
# then add a column with the mean measurement value before/after project implementation
# then categorize each measurement (higher mean TSS or tubidity is worse)
# then apply a binary color category and a diverging color scale (based on effect size and status)    
wa12 <- water12 %>%
  apply_cohensD('water') %>%
  left_join(bwa12, by = c("result_type" = "result_type", "HUC12_Name" = "HUC12_Name")) %>%
  left_join(awa12, by = c("result_type" = "result_type", "HUC12_Name" = "HUC12_Name")) %>%
  add_status_colors('water') %>%
  filter(TimePeriod == 'after')
# %>%
#   ungroup() %>%
#   filter(TimePeriod == 'after') %>%
#   group_by(result_type, HUC12_Name, Location_ID, year, effectsize, status) %>%
#   summarise(mean_measurement = mean(measurement))
# %>%
#   filter(year == max(year))
######################## END WATER ########################


######################## BEGIN CHUM ########################

chum12 <- chum_counts %>%
  left_join(huc12_med, by = "HUC12_Name") %>%
  group_by(HUC12_Name) %>%
  mutate(TimePeriod = ifelse(year > medianyr, 'after',
                             ifelse(year < medianyr, 'before',
                                    'during')))
chum10 <- chum_counts %>%
  left_join(huc10_med, by = "HUC10_Name") %>%
  group_by(HUC10_Name) %>%
  mutate(TimePeriod = ifelse(year > medianyr, 'after',
                             ifelse(year < medianyr, 'before',
                                    'during')))

# read in the chum count dataframe, 
# add median project year for each HUC
# group by HUC
# categorize each row as before, during or after the median project year in each HUC
# obtain the mean salmon count for each HUC for each time period
bchum10 <- before_after('before', chum10, 'count')

achum10 <- before_after('after', chum10, 'count')

bchum12 <- before_after('before', chum12, 'count')

achum12 <- before_after('after', chum12, 'count')

# read in the chum count dataframe, 
# add median project year for each HUC
# group by HUC 10
# categorize each row as before, during or after the median project year in each HUC
# calculate the cohensD in each HUC (add new column)
# then, tidy the output dataframe
# join with summary mean by time period, then classify each according to change in mean fish count
ch10 <- chum10 %>%
  apply_cohensD('chum') %>%
  left_join(bchum10, by = "HUC10_Name") %>%
  left_join(achum10, by = "HUC10_Name") %>%
  add_status_colors('chum') %>%
  filter(TimePeriod == 'after')

# read in the chum count dataframe, 
# add median project year for each HUC
# group by HUC 12
# categorize each row as before, during or after the median project year in each HUC
# calculate the cohensD in each HUC (add new column)
# then, tidy the output dataframe (remove NaN cohensD)
ch12 <- chum12 %>%
  apply_cohensD('chum') %>%
  left_join(bchum12, by = "HUC12_Name") %>%
  left_join(achum12, by = "HUC12_Name") %>%
  add_status_colors('chum') %>%
  filter(TimePeriod == 'after')

######################## END CHUM ########################

# c('#d73027','#fc8d59','#fee08b','#d9ef8b','#91cf60','#1a9850')
# cohen's D: 'small effect' = 0.2, med effect = 0.5, large effect = 0.8

######################## BEGIN MAP ########################

m12 <- leaflet(data = chum12) %>% 
  setView(lng = -122.996823, lat = 47.5642594, zoom = 9) %>%
  addProviderTiles("Stamen.Terrain") %>%
  addCircleMarkers(ph12$lon, ph12$lat, 
                   radius = 4,
                   color = '#984ea3',
                   stroke = FALSE, fillOpacity = 1,
                   group = "Projects") %>%
  addCircleMarkers(ch12$lon, ch12$lat, 
                   radius = 5,
                   color = ch12$coloreffect,
                   stroke = FALSE, fillOpacity = 0.8,
                   group = "Chum") %>%
  addCircleMarkers(wa12$lon, wa12$lat, 
                   radius = 5,
                   color = wa12$coloreffect,
                   stroke = FALSE, fillOpacity = 0.8,
                   group = "Water") %>%
  addLayersControl(
    baseGroups = c("Chum", "Water"),
    overlayGroups = "Projects",
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  addLegend(colors = c('#d73027','#fc8d59','#fee08b','#d9ef8b','#91cf60','#1a9850'),
            labels = c('large/worse', 'medium/worse', 'small/worse', 
                       'small/improving', 'medium/improving', 'large/improving'),
            position = 'bottomleft',
            title = 'Effect Size/Status')
m12

# wa10IMP <- filter(wa10, status == 'improving')
# greens <- colorFactor("Greens", wa10IMP$effectsize)

m10 <- leaflet() %>% 
  setView(lng = -122.996823, lat = 47.5642594, zoom = 9) %>%
  addProviderTiles("Stamen.Terrain") %>%
  addCircleMarkers(ph10$lon, ph10$lat,
                   radius = 4,
                   color = '#984ea3',
                   stroke = FALSE, fillOpacity = 1,
                   group = "Projects") %>%
  addCircleMarkers(ch10$lon, ch10$lat,
                   radius = 5,
                   color = ch10$coloreffect,
                   stroke = FALSE, fillOpacity = 0.8,
                   group = "Chum") %>%
  addCircleMarkers(wa10$lon, wa10$lat, 
                   radius = 5,
                   color = wa10$coloreffect,
                   stroke = FALSE, fillOpacity = 0.8,
                   group = "Water") %>%
  addLayersControl(
    baseGroups = c("Chum", "Water"),
    overlayGroups = "Projects",
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  addLegend(colors = c('#d73027','#fc8d59','#fee08b','#d9ef8b','#91cf60','#1a9850'),
            labels = c('large/worse', 'medium/worse', 'small/worse', 
                       'small/improving', 'medium/improving', 'large/improving'),
            position = 'bottomleft',
            title = 'Effect Size/Status')
m10

######################## END MAP ########################

