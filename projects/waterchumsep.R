library(tidyverse)
library(lsr) #cohensD function package

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
  left_join(chum_locations) %>%
  separate(site, into = c("sitename", "project_cat"), sep = " - ")

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
water <- readRDS("../water/data/water_huc.rds")

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
  if (outcome == 'water') { 
    data %>%
      spread(TimePeriod, measurement) %>%
      mutate(cohensd = cohensD(before, after)) %>%
      # gather('TimePeriod', 'measurement', `before`, `after`, `during`, na.rm = TRUE) %>%
      gather('TimePeriod', 'measurement', `before`, `after`, `during`, `noProject`, na.rm = TRUE) %>%
      mutate(effectsize = ifelse(cohensd < 0.5, 'small',
                                 ifelse(cohensd >= 0.8, 'large',
                                        'medium')))
  } else if (outcome == 'chum') { 
    data %>%
      spread(TimePeriod, count) %>%
      mutate(cohensd = cohensD(before, after)) %>%
      gather('TimePeriod', 'count', `before`, `after`, `during`, `noProject`, na.rm = TRUE) %>%
      mutate(effectsize = ifelse(cohensd < 0.5, 'small',
                                 ifelse(cohensd >= 0.8, 'large',
                                        'medium')))
  }
}


# categorize each measurement (higher mean TSS or tubidity is worse)
# then apply a binary color category and a diverging color scale (based on effect size and status)
add_status_colors <- function(data, outcome) {
  if (outcome == 'water') { 
    data %>% 
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
             colorblind = ifelse(effectsize == 'large' & status == 'worse', '#c51b7d',
                                  ifelse(effectsize == 'medium' & status == 'worse', '#e9a3c9',
                                         ifelse(effectsize == 'small' & status == 'worse', '#fde0ef',
                                                ifelse(effectsize == 'small' & status == 'improving', '#e6f5d0',
                                                       ifelse(effectsize == 'medium' & status == 'improving', '#a1d76a',
                                                              ifelse(effectsize == 'large' & status == 'improving', '#4d9221',
                                                                     NA)))))))
  } else if (outcome == 'chum') { 
    data %>% 
      mutate(status = ifelse(meanbefore > meanafter, 'worse',
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
             colorblind = ifelse(effectsize == 'large' & status == 'worse', '#c51b7d',
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
  # filter(HUC12_Name %in% unique(wa_ph12$HUC12_Name)) %>%
  mutate(year = format(start_date,"%Y")) %>%
  left_join(wa_huc12_med, by = "HUC12_Name") %>%
  group_by(result_type, HUC12_Name) %>%
  mutate(TimePeriod = ifelse(year > medianyr, 'after',
                             ifelse(year < medianyr, 'before',
                                    'during'))) %>%
  mutate(TimePeriod = ifelse(is.na(TimePeriod), 'noProject', TimePeriod))

# read in the water quality dataframe, 
# add median project year for each HUC
# group by measurement type (TSS or turbidity), then by HUC 10
# categorize each row as before, during or after the median project year in each HUC
water10 <- water %>%
  # filter(HUC10_Name %in% unique(wa_ph10$HUC10_Name)) %>%
  mutate(year = format(start_date,"%Y")) %>%
  left_join(wa_huc10_med, by = "HUC10_Name") %>%
  group_by(result_type, HUC10_Name) %>%
  mutate(TimePeriod = ifelse(year > medianyr, 'after',
                             ifelse(year < medianyr, 'before',
                                    'during'))) %>%
  mutate(TimePeriod = ifelse(is.na(TimePeriod), 'noProject', TimePeriod))

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

######################## END WATER ########################


######################## BEGIN CHUM ########################

chum12 <- chum_counts %>%
  left_join(huc12_med, by = "HUC12_Name") %>%
  group_by(HUC12_Name) %>%
  mutate(TimePeriod = ifelse(year > medianyr, 'after',
                             ifelse(year < medianyr, 'before',
                                    'during'))) %>%
  mutate(TimePeriod = ifelse(is.na(TimePeriod), 'noProject', TimePeriod))

chum10 <- chum_counts %>%
  left_join(huc10_med, by = "HUC10_Name") %>%
  group_by(HUC10_Name) %>%
  mutate(TimePeriod = ifelse(year > medianyr, 'after',
                             ifelse(year < medianyr, 'before',
                                    'during'))) %>%
  mutate(TimePeriod = ifelse(is.na(TimePeriod), 'noProject', TimePeriod))

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

######################## END CHUM ########################

# c('#d73027','#fc8d59','#fee08b','#d9ef8b','#91cf60','#1a9850')
# cohen's D: 'small effect' = 0.2, med effect = 0.5, large effect = 0.8

ch12_formerge <- ch12 %>%
  select(year:project_cat, Description:jitterlat, HUC12_id, HUC12_Name, 
         medianyr, cohensd:colorblind) %>%
  rename(name = sitename, description = Description, lon = jitterlng, lat = jitterlat,
         measurement = count, HUC_id = HUC12_id, HUC_Name = HUC12_Name) %>%
  mutate(result_type = 'Chum Salmon', unit = '', HUC_level = '12',
         Study_ID = NA, full_date = NA, Location_ID = NA, logMeasurement = NA) 

chum_formerge <- ch10 %>%
  select(year:project_cat, Description:jitterlat, HUC10_id, HUC10_Name, 
         medianyr, cohensd:colorblind) %>%
  rename(name = sitename, description = Description, lon = jitterlng, lat = jitterlat,
         measurement = count, HUC_id = HUC10_id, HUC_Name = HUC10_Name) %>%
  mutate(result_type = 'Chum Salmon', unit = '', HUC_level = '10',
         Study_ID = NA, full_date = NA, Location_ID = NA, logMeasurement = NA) %>%
  rbind(ch12_formerge)

wa12_formerge <- wa12 %>%
  select(lon, lat, Study_ID, Study_Name, Location_ID, start_date,
         logMeasurement, result_type, measurement, unit,
         HUC12_id, HUC12_Name, year:colorblind) %>%
  rename(name = Study_Name, full_date = start_date,
         HUC_id = HUC12_id, HUC_Name = HUC12_Name) %>%
  mutate(HUC_level = '12', year = as.numeric(year),
         project_cat = NA, description = NA) 

water_formerge <- wa10 %>%
  select(lon, lat, Study_ID, Study_Name, Location_ID, start_date,
         logMeasurement, result_type, measurement, unit,
         HUC10_id, HUC10_Name, year:colorblind) %>%
  rename(name = Study_Name, full_date = start_date,
         HUC_id = HUC10_id, HUC_Name = HUC10_Name) %>%
  mutate(HUC_level = '10', year = as.numeric(year),
         project_cat = NA, description = NA) %>%
  rbind(wa12_formerge)

# separate projects into huc 10 and 12 dataframes, then bind them together
# format columns to match outcome data for ease with binding
p_h10 <- ph %>% select(-HUC12_id, -HUC12_Name, -WRIA_Name, -WRIA_ID) %>%
  rename(measurement = cost,
         HUC_id = HUC10_id, HUC_Name = HUC10_Name,
         Study_ID = id) %>%
  mutate(result_type = 'Investment', unit = 'dollars', HUC_level = '10')
all_projects_formerge <- ph %>% select(-HUC10_id, -HUC10_Name, -WRIA_Name, -WRIA_ID) %>%
  rename(measurement = cost,
         HUC_id = HUC12_id, HUC_Name = HUC12_Name,
         Study_ID = id) %>%
  mutate(result_type = 'Investment', unit = 'dollars', HUC_level = '12') %>%
  rbind(p_h10)

# create a list of column names that do not occur in the project df
empties <- c('description', 'full_date', 'Location_ID', 'logMeasurement', 
             'medianyr', 'cohensd', 'TimePeriod', 'effectsize', 'meanbefore', 
             'meanafter', 'status', 'color', 'coloreffect', 'colorblind')
# set all non-project columns to be NA
all_projects_formerge[,empties] <- NA

# bind all dataframes together
all_dfs <- bind_rows(chum_formerge, water_formerge, all_projects_formerge)

# add unique ID column
all_dfs$id <- 1:nrow(all_dfs)

# export the data to the shiny app directory
saveRDS(all_dfs, "../shinyapp/data/all-dfs.rds")

