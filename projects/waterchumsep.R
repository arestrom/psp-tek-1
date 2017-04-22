library(tidyverse)
# library(stringr)
# library(MazamaSpatialUtils)
library(lsr) #cohensD function package
library(leaflet)

######################## BEGIN CHUM PROJ ########################

# read in the project-huc dataframe
ph <- readRDS("./data/project_huc.rds")

# ph$id <- 1:nrow(ph)

# read in the chum-huc dataframe, tidy data
chum_locations <- readRDS("../psp-chum/data/chum_huc.rds") %>%
  rename (lon = lng, site = River.site) %>%
  select(-(Permanent:Line.NR))

# read in the chum-counts dataframe, join with location/huc data
chum_counts <- readRDS("../psp-chum/data/tidychum.rds") %>%
  left_join(chum_locations)

# create a HUC-10 project dataframe, 
# filtered to only include projects in fish HUC10s
# calculate the median project implementation year for each HUC10
ph10 <- ph %>%
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
ph12 <- ph %>%
  filter(HUC12_Name %in% unique(chum_counts$HUC12_Name)) %>%
  group_by(HUC12_Name) %>%
  mutate(meanyr = mean(as.numeric(year)),
         medianyr = median(as.numeric(year)))

# create a HUC12-median year entity to merge with outcome dataframes below
huc12_med <- ph12 %>%
  select(HUC12_Name, medianyr) %>%
  group_by(HUC12_Name, medianyr) %>%
  summarise()

######################## END CHUM PROJ ########################

######################## BEGIN WATER PROJ ########################

# read in water quality rdata
water <- readRDS("../turbidity/data/water_huc.rds")

# create a HUC-10 project dataframe, 
# filtered to only include projects in water HUC10s
# calculate the median project implementation year for each HUC10
wa_ph10 <- ph %>%
  filter(HUC10_Name %in% unique(water$HUC10_Name))  %>%
  group_by(HUC10_Name) %>%
  mutate(meanyr = mean(as.numeric(year)),
         medianyr = median(as.numeric(year)))

# create a HUC10-median year entity to merge with outcome dataframes below
wa_huc10_med <- wa_ph10 %>%
  select(HUC10_Name, medianyr) %>%
  group_by(HUC10_Name, medianyr) %>%
  summarise()


# create a HUC-12 project dataframe, 
# filtered to only include projects in fish HUC12s
# calculate the median project implementation year for each HUC12
wa_ph12 <- ph %>%
  filter(HUC12_Name %in% unique(water$HUC12_Name)) %>%
  group_by(HUC12_Name) %>%
  mutate(meanyr = mean(as.numeric(year)),
         medianyr = median(as.numeric(year)))


# create a HUC12-median year entity to merge with outcome dataframes below
wa_huc12_med <- wa_ph12 %>%
  select(HUC12_Name, medianyr) %>%
  group_by(HUC12_Name, medianyr) %>%
  summarise()

######################## END WATER PROJ ########################

######################## BEGIN FUNCTION TOWN ########################

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

# remove rows without a TimePeriod (no project in that HUC to get a median year)
# create a column of measurements for before and after project implementation in each HUC
# calculate the cohensD for each measurement in each HUC (add new column)
# then, categorize the effect size
apply_cohensD <- function(data, outcome) {
  if (outcome == 'water') { data %>%
      # filter(!is.na(TimePeriod), !(TimePeriod == 'during')) %>%
      # filter(!is.na(TimePeriod)) %>%
      spread(TimePeriod, measurement) %>%
      mutate(cohensd = cohensD(before, after)) %>%
      # filter(!cohensd == 'NaN') %>%
      gather('TimePeriod', 'measurement', `before`, `after`, `during`) %>%
      # filter(!is.na(measurement)) %>%
      mutate(effectsize = ifelse(cohensd < 0.5, 'small',
                                 ifelse(cohensd >= 0.8, 'large',
                                        'medium')))
  } else if (outcome == 'chum') { data %>%
      # filter(!is.na(TimePeriod), !(TimePeriod == 'during')) %>%
      # filter(!is.na(TimePeriod)) %>%
      spread(TimePeriod, count) %>%
      mutate(cohensd = cohensD(before, after)) %>%
      # filter(!cohensd == 'NaN') %>%
      gather('TimePeriod', 'count', `before`, `after`, `during`) %>%
      # filter(!is.na(count)) %>%
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
                                                                     NA)))))),
             colorblind = ifelse(effectsize == 'large' & status == 'worse', '#d73027',
                                  ifelse(effectsize == 'medium' & status == 'worse', '#e9a3c9',
                                         ifelse(effectsize == 'small' & status == 'worse', '#fde0ef',
                                                ifelse(effectsize == 'small' & status == 'improving', '#e6f5d0',
                                                       ifelse(effectsize == 'medium' & status == 'improving', '#a1d76a',
                                                              ifelse(effectsize == 'large' & status == 'improving', '#4d9221',
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
                                                                                                          NA)))))),
                                                  colorblind = ifelse(effectsize == 'large' & status == 'worse', '#d73027',
                                                                      ifelse(effectsize == 'medium' & status == 'worse', '#e9a3c9',
                                                                             ifelse(effectsize == 'small' & status == 'worse', '#fde0ef',
                                                                                    ifelse(effectsize == 'small' & status == 'improving', '#e6f5d0',
                                                                                           ifelse(effectsize == 'medium' & status == 'improving', '#a1d76a',
                                                                                                  ifelse(effectsize == 'large' & status == 'improving', '#4d9221',
                                                                                                         NA)))))))
    
  }
}

######################## END FUNCTION TOWN ########################

######################## BEGIN WATER ########################

# read in the water quality dataframe, 
# add median project year for each HUC
# group by measurement type (TSS or turbidity), then by HUC 12
# categorize each row as before, during or after the median project year in each HUC
water12 <- water %>%
  filter(HUC12_Name %in% unique(wa_ph12$HUC12_Name)) %>%
  mutate(year = format(start_date,"%Y")) %>%
  left_join(wa_huc12_med, by = "HUC12_Name") %>%
  group_by(result_type, HUC12_Name) %>%
  mutate(TimePeriod = ifelse(year > medianyr, 'after',
                             ifelse(year < medianyr, 'before',
                                    'during')))

# read in the water quality dataframe, 
# add median project year for each HUC
# group by measurement type (TSS or turbidity), then by HUC 10
# categorize each row as before, during or after the median project year in each HUC
water10 <- water %>%
  filter(HUC10_Name %in% unique(wa_ph10$HUC10_Name)) %>%
  mutate(year = format(start_date,"%Y")) %>%
  left_join(wa_huc10_med, by = "HUC10_Name") %>%
  group_by(result_type, HUC10_Name) %>%
  mutate(TimePeriod = ifelse(year > medianyr, 'after',
                             ifelse(year < medianyr, 'before',
                                    'during')))

bwa10 <- before_after('before', water10, 'measurement')

awa10 <- before_after('after', water10, 'measurement')

bwa12 <- before_after('before', water12, 'measurement')

awa12 <- before_after('after', water12, 'measurement')

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
  add_status_colors('water') 
# %>%
#   filter(TimePeriod == 'after')

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
  add_status_colors('water') 
# %>%
#   filter(TimePeriod == 'after')

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
  add_status_colors('chum') 
# %>%
#   filter(TimePeriod == 'after')

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
  add_status_colors('chum') 
# %>%
#   filter(TimePeriod == 'after')

######################## END CHUM ########################

# c('#d73027','#fc8d59','#fee08b','#d9ef8b','#91cf60','#1a9850')
# cohen's D: 'small effect' = 0.2, med effect = 0.5, large effect = 0.8

ch12_formerge <- ch12 %>%
  select(year, lon, lat, Description, HUC12_id, HUC12_Name, 
         medianyr, cohensd:colorblind) %>%
  rename(description = Description, measurement = count,
         HUC_id = HUC12_id, HUC_Name = HUC12_Name) %>%
  mutate(result_type = 'Chum Salmon', unit = '', HUC_level = '12',
         project_cat = NA) 

chum_formerge <- ch10 %>%
  select(year, lon, lat, Description, HUC10_id, HUC10_Name, 
         medianyr, cohensd:colorblind) %>%
  rename(description = Description, measurement = count,
         HUC_id = HUC10_id, HUC_Name = HUC10_Name) %>%
  mutate(result_type = 'Chum Salmon', unit = '', HUC_level = '10',
         project_cat = NA) %>%
  rbind(ch12_formerge)

wa12_formerge <- wa12 %>%
  select(lon, lat, Study_Name, result_type, measurement, unit,
         HUC12_id, HUC12_Name, year:colorblind) %>%
  rename(description = Study_Name,
         HUC_id = HUC12_id, HUC_Name = HUC12_Name) %>%
  mutate(HUC_level = '12', year = as.numeric(year),
         project_cat = NA) 

water_formerge <- wa10 %>%
  select(lon, lat, Study_Name, result_type, measurement, unit,
         HUC10_id, HUC10_Name, year:colorblind) %>%
  rename(description = Study_Name,
         HUC_id = HUC10_id, HUC_Name = HUC10_Name) %>%
  mutate(HUC_level = '10', year = as.numeric(year),
         project_cat = NA) %>%
  rbind(wa12_formerge)

wa_ph12_formerge <- wa_ph12 %>%
  select(id, year, name, cost:HUC12_id, HUC12_Name, 
         medianyr) %>%
  rename(description = name, measurement = cost,
         HUC_id = HUC12_id, HUC_Name = HUC12_Name) %>%
  mutate(result_type = 'Investment', unit = 'dollars', HUC_level = '12',
         color = '#984ea3')

water_projects_formerge <- wa_ph10 %>%
  select(id, year, name, cost:project_cat, HUC10_id, HUC10_Name, 
         medianyr) %>%
  rename(description = name, measurement = cost,
         HUC_id = HUC10_id, HUC_Name = HUC10_Name) %>%
  mutate(result_type = 'Investment', unit = 'dollars', HUC_level = '10',
         color = '#984ea3') %>%
  rbind(wa_ph12_formerge)

ph12_formerge <- ph12 %>%
  select(id, year, name, cost:HUC12_id, HUC12_Name, 
         medianyr) %>%
  rename(description = name, measurement = cost,
         HUC_id = HUC12_id, HUC_Name = HUC12_Name) %>%
  mutate(result_type = 'Investment', unit = 'dollars', HUC_level = '12',
         color = '#984ea3')

chum_projects_formerge <- ph10 %>%
  select(id, year, name, cost:project_cat, HUC10_id, HUC10_Name, 
         medianyr) %>%
  rename(description = name, measurement = cost,
         HUC_id = HUC10_id, HUC_Name = HUC10_Name) %>%
  mutate(result_type = 'Investment', unit = 'dollars', HUC_level = '10',
         color = '#984ea3') %>%
  rbind(ph12_formerge)

all_projects_formerge <- rbind(chum_projects_formerge, water_projects_formerge) %>%
  distinct(id, HUC_id, .keep_all = TRUE) %>%
  select(-id)

# add both dataframes together
all_dfs <- rbind(chum_formerge,water_formerge) %>% rbind(all_projects_formerge)

# add unique ID column
all_dfs$id <- 1:nrow(all_dfs)

saveRDS(all_dfs, "../shinyapp/data/ALL-separate-3.rds")

######################## BEGIN DATA EXPORT ########################

# saveRDS(ch12, "../shinyapp/data/chum_huc12.rds")
# saveRDS(ch10, "../shinyapp/data/chum_huc10.rds")
# saveRDS(wa12, "../shinyapp/data/water_huc12.rds")
# saveRDS(wa10, "../shinyapp/data/water_huc10.rds")
# saveRDS(wa_ph12, "../shinyapp/data/water_project_huc12.rds")
# saveRDS(wa_ph10, "../shinyapp/data/water_project_huc10.rds")
# saveRDS(ph12, "../shinyapp/data/chum_project_huc12.rds")
# saveRDS(ph10, "../shinyapp/data/chum_project_huc10.rds")

######################## END DATA EXPORT ########################

######################## BEGIN MAP ########################

m12 <- leaflet(data = chum12) %>% 
  setView(lng = -122.996823, lat = 47.5642594, zoom = 9) %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addCircleMarkers(all_projects_formerge$lon, all_projects_formerge$lat,
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

m10 <- leaflet(data = chum10) %>% 
  setView(lng = -122.996823, lat = 47.5642594, zoom = 9) %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addCircleMarkers(all_projects_formerge$lon, all_projects_formerge$lat,
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

# colorblind: colors = c('#c51b7d','#e9a3c9','#fde0ef','#e6f5d0','#a1d76a','#4d9221')

######################## END MAP ########################

######################## SUMMARY INFO ########################

###### projects ######
# 21 HUCs, 1 NA
unique(ph$HUC10_Name)

# 46 HUCs, 1 NA
unique(ph$HUC12_Name)

unique(ph10$HUC10_Name)
# [1] "Skokomish River-Frontal Hood Canal"       
# [2] "Little Quillcene River-Frontal Hood Canal"
# [3] "Hood Canal"                               
# [4] "Tahuya River-Frontal Hood Canal"          
# [5] "Lilliwaup Creek-Frontal Hood Canal"   

# 7 HUC 12s
unique(ph12$HUC12_Name)

# 19 water-project HUC 10s
unique(wa_ph10$HUC10_Name)

# 37 water-project HUC 12s
unique(wa_ph12$HUC12_Name)

###### chum ######
# print out distinct HUC names (chum are the smallest group, limits all other measurements)
unique(chum_counts$HUC10_Name)
# [1] "Tahuya River-Frontal Hood Canal"          
# [2] "Hood Canal"                               
# [3] "Little Quillcene River-Frontal Hood Canal"
# [4] "Jefferson Creek-Hamma Hamma River"        
# [5] "Lilliwaup Creek-Frontal Hood Canal"       
# [6] "Skokomish River-Frontal Hood Canal"       
# [7] "Chimacum Creek-Frontal Port Ludlow"       
# [8] "Discovery Bay-Strait of Juan De Fuca"     
# [9] "Jimmycomelately Creek-Frontal Sequim Bay" 
# [10] "Snow Creek-Frontal Discovery Bay" 

unique(chum_counts$HUC12_Name)
# [1] "Big Beef Creek-Frontal Hood Canal"   
# [2] "Hood Canal"                          
# [3] "Tarboo Creek-Frontal Dabob Bay"      
# [4] "Dewatto River"                       
# [5] "Spencer Creek-Frontal Dabob Bay"     
# [6] "Hamma Hamma River"                   
# [7] "Finch Creek-Frontal Hood Canal"      
# [8] "Skokomish River-Frontal Hood Canal"  
# [9] "Tahuya River"                        
# [10] "Chimacum Creek"                      
# [11] "Discovery Bay-Strait of Juan De Fuca"
# [12] "Johnson Creek-Frontal Sequim Bay"    
# [13] "Eagle Creek-Frontal Discovery Bay" 

unique(chum12$HUC12_Name)
# [1] "Big Beef Creek-Frontal Hood Canal"   
# [2] "Hood Canal"                          
# [3] "Tarboo Creek-Frontal Dabob Bay"      
# [4] "Dewatto River"                       
# [5] "Spencer Creek-Frontal Dabob Bay"     
# [6] "Hamma Hamma River"                   
# [7] "Finch Creek-Frontal Hood Canal"      
# [8] "Skokomish River-Frontal Hood Canal"  
# [9] "Tahuya River"                        
# [10] "Chimacum Creek"                      
# [11] "Discovery Bay-Strait of Juan De Fuca"
# [12] "Johnson Creek-Frontal Sequim Bay"    
# [13] "Eagle Creek-Frontal Discovery Bay"

# 7 HUC 12s in ch12 (2 lacking sufficient temporal data)
unique(ch12$HUC12_Name)
# [1] "Big Beef Creek-Frontal Hood Canal"   
# [2] "Hood Canal"                          
# [3] "Tarboo Creek-Frontal Dabob Bay"      
# [4] "Dewatto River"                       
# [5] "Spencer Creek-Frontal Dabob Bay"     
# [6] "Hamma Hamma River"                   
# [7] "Finch Creek-Frontal Hood Canal"      
# [8] "Skokomish River-Frontal Hood Canal"  
# [9] "Tahuya River"                        
# [10] "Chimacum Creek"                      
# [11] "Discovery Bay-Strait of Juan De Fuca"
# [12] "Johnson Creek-Frontal Sequim Bay"    
# [13] "Eagle Creek-Frontal Discovery Bay" 

# 5 HUC 10s in ch10 (1 lacking sufficient temporal data)
unique(ch10$HUC10_Name)
# [1] "Tahuya River-Frontal Hood Canal"          
# [2] "Hood Canal"                               
# [3] "Little Quillcene River-Frontal Hood Canal"
# [4] "Jefferson Creek-Hamma Hamma River"        
# [5] "Lilliwaup Creek-Frontal Hood Canal"       
# [6] "Skokomish River-Frontal Hood Canal"       
# [7] "Chimacum Creek-Frontal Port Ludlow"       
# [8] "Discovery Bay-Strait of Juan De Fuca"     
# [9] "Jimmycomelately Creek-Frontal Sequim Bay" 
# [10] "Snow Creek-Frontal Discovery Bay"  

###### water ######
# 23 HUC 10s
unique(water$HUC10_Name)
unique(water$HUC12_Name)

# # 5 HUC 10s
# wa_pr <- filter(water, HUC10_Name %in% unique(ph10$HUC10_Name))
# unique(wa_pr$HUC10_Name)
# # 17 HUC 12s
# unique(wa_pr$HUC12_Name)

# 68 HUC 12s (water)
unique(water12$HUC12_Name)
# 23 HUC 10s (water)
unique(water10$HUC10_Name)
# 11 HUC 10s (water-project-time)
unique(wa10$HUC10_Name)
# 17 HUC 12's (water-project-time)
unique(wa12$HUC12_Name)

######################## SUMMARY INFO ########################
