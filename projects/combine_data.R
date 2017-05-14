library(tidyverse)

######################## BEGIN CHUM INVESTMENT ########################

# read in the investment-project-huc dataframe
ph <- readRDS("./data/project_huc.rds")

# ph$id <- 1:nrow(ph)

# read in the chum-huc dataframe, tidy data
chum_locations <- readRDS("../psp-chum/data/chum_huc.rds") %>%
  rename (lon = lng, site = River.site) %>%
  select(-(Permanent:Line.NR))

# read in the chum-counts dataframe, join with location/huc data
# separate out the chum site into two categorical variables for mapping
chum_counts <- readRDS("../psp-chum/data/tidychum.rds") %>%
  left_join(chum_locations) %>%
  separate(site, into = c("sitename", "project_cat"), sep = " - ",
           remove = FALSE)

# create a HUC-10 investment dataframe, 
# filtered to only include investments in fish HUC10s
# calculate the median investment implementation year for each HUC10
# create a HUC10-median year entity to merge with outcome dataframes below
huc10_med <- ph %>%
  filter(HUC10_Name %in% unique(chum_counts$HUC10_Name))  %>%
  group_by(HUC10_Name) %>%
  mutate(medianyr = median(as.numeric(year))) %>%
  select(HUC10_Name, medianyr) %>%
  group_by(HUC10_Name, medianyr) %>%
  summarise()

# create a HUC-12 investment dataframe, 
# filtered to only include investments in fish HUC12s
# calculate the median investment implementation year for each HUC12
# create a HUC12-median year entity to merge with outcome dataframes below
huc12_med <- ph %>%
  filter(HUC12_Name %in% unique(chum_counts$HUC12_Name)) %>%
  group_by(HUC12_Name) %>%
  mutate(medianyr = median(as.numeric(year))) %>%
  select(HUC12_Name, medianyr) %>%
  group_by(HUC12_Name, medianyr) %>%
  summarise()

######################## END CHUM INVESTMENT ########################

######################## BEGIN WATER INVESTMENT ########################

# read in water quality rdata
water <- readRDS("../water/data/water_huc.rds")

# create a HUC-10 investment dataframe, 
# filtered to only include investments in water HUC10s
# calculate the median investment implementation year for each HUC10
# create a HUC10-median year entity to merge with outcome dataframes below
wa_huc10_med <- ph %>%
  filter(HUC10_Name %in% unique(water$HUC10_Name))  %>%
  group_by(HUC10_Name) %>%
  mutate(medianyr = median(as.numeric(year))) %>%
  select(HUC10_Name, medianyr) %>%
  group_by(HUC10_Name, medianyr) %>%
  summarise()


# create a HUC-12 investment dataframe, 
# filtered to only include investments in fish HUC12s
# calculate the median investment implementation year for each HUC12
# create a HUC12-median year entity to merge with outcome dataframes below
wa_huc12_med <- ph %>%
  filter(HUC12_Name %in% unique(water$HUC12_Name)) %>%
  group_by(HUC12_Name) %>%
  mutate(medianyr = median(as.numeric(year))) %>%
  select(HUC12_Name, medianyr) %>%
  group_by(HUC12_Name, medianyr) %>%
  summarise()

######################## END WATER INVESTMENT ########################

######################## BEGIN FUNCTION TOWN ########################

cohensD_manual <- function(before, after) {
  lb <- length(before)
  la <- length(after)
  mb <- mean(before, na.rm = TRUE)
  ma <- mean(after, na.rm = TRUE)
  sdb <- sd(before, na.rm = TRUE)
  sda <- sd(after, na.rm = TRUE)
  step1 <- (la-1) * (sda^2)
  step2 <- (lb-1) * (sdb^2)
  var_pooled <- sqrt((step1+step2)/(la+lb-1))
  d <- (ma - mb) / var_pooled
  return(d)
}

# remove rows without a TimePeriod (no project in that HUC to get a median year)
# create a column of measurements for before and after project implementation in each HUC
# calculate the cohensD for each measurement in each HUC (add new column)
# then, categorize the effect size
apply_cohensD <- function(data, outcome, HUC) {
  if (outcome == 'water') { 
    data %>%
      group_by_('result_type', HUC) %>%
      mutate(TimePeriod = ifelse(year > medianyr, 'after',
                                 ifelse(year < medianyr, 'before',
                                        'during'))) %>%
      mutate(TimePeriod = ifelse(is.na(TimePeriod), 'noProject', TimePeriod)) %>%
      spread(TimePeriod, measurement) %>%
      mutate(cohensd_huc_mean = cohensD_manual(before, after),
             huc_mean_after = mean(after, na.rm = TRUE),
             huc_mean_before = mean(before, na.rm = TRUE)) %>%
      gather(key = 'TimePeriod', value = 'measurement', `before`, `after`, `during`, `noProject`, na.rm = TRUE) %>%
      mutate(effectsize = ifelse(abs(cohensd_huc_mean) < 0.5, 'small',
                                 ifelse(abs(cohensd_huc_mean) >= 0.8, 'large',
                                        'medium')))
  } else if (outcome == 'chum') { 
    data %>%
      group_by(site) %>%
      mutate(TimePeriod = ifelse(year > medianyr, 'after',
                                 ifelse(year < medianyr, 'before',
                                        'during'))) %>%
      mutate(TimePeriod = ifelse(is.na(TimePeriod), 'noProject', TimePeriod)) %>%
      spread(TimePeriod, count) %>%
      mutate(cohensd = cohensD_manual(before, after),
             site_mean_after = mean(after, na.rm = TRUE),
             site_mean_before = mean(before, na.rm = TRUE),
             site_sd_before = sd(before, na.rm = TRUE),
             site_sd_after = sd(after, na.rm = TRUE),
             var_pooled = sqrt((((length(after)-1)*(site_sd_after^2))+((length(before)-1)*(site_sd_before^2)))/(length(after)+length(before)-1)),
             var_cohensd = (length(before) + length(after))/(length(before) * length(after)) + (cohensd^2)/(2*(length(before) + length(after))),
             sd_cohensd = var_cohensd^0.5,
             wsubi = 1/var_cohensd,
             # correct_cd = -1*cohensd,
             wsubixd = cohensd*wsubi) %>%
      ungroup() %>%
      group_by_(HUC) %>%
      mutate(cohensd_huc_mean = (sum(wsubixd)/sum(wsubi)),
             huc_mean_after = mean(after, na.rm = TRUE),
             huc_mean_before = mean(before, na.rm = TRUE),
             sum_wsubixd = sum(wsubixd),
             sum_wsubi = sum(wsubi),
             cohensd_huc_var = 1/sum_wsubi,
             cohensd_huc_sd = cohensd_huc_var^0.5,
             plus_minus = cohensd_huc_sd*1.645,
             noProject = if (exists('noProject')) noProject else NA) %>%
      gather(key = 'TimePeriod', value = 'count', `before`, `after`, `during`, `noProject`, na.rm = TRUE) %>%
      mutate(effectsize = ifelse(abs(cohensd_huc_mean) < 0.5, 'small',
                                 ifelse(abs(cohensd_huc_mean) >= 0.8, 'large',
                                        'medium')),
             site_effectsize = ifelse(abs(cohensd) < 0.5, 'small',
                                      ifelse(abs(cohensd) >= 0.8, 'large',
                                             'medium')))
  }
}

# categorize each measurement (higher mean TSS or tubidity is worse)
# then apply a diverging color scale (based on effect size and status)
add_status_colors <- function(data, outcome) {
  if (outcome == 'water') { 
    data %>% 
      mutate(status = ifelse(cohensd_huc_mean > 0, 'worse',
                             ifelse(cohensd_huc_mean < 0, 'improving',
                                    'no change')),
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
      mutate(status = ifelse(cohensd_huc_mean < 0, 'worse',
                             ifelse(cohensd_huc_mean > 0, 'improving',
                                    'no change')),
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
# group by measurement type (TSS or turbidity), then by HUC
# categorize each row as before, during or after the median project year in each HUC
# create a column of measurements for before and after project implementation in each HUC
# calculate the cohensD for each measurement in each HUC (add new column)
# then, categorize the effect size
# add a column with the mean measurement value before/after project implementation
# add colors based on effect and direction
water10 <- water %>%
  mutate(year = format(start_date,"%Y")) %>%
  left_join(wa_huc10_med, by = "HUC10_Name") %>%
  apply_cohensD ('water', "HUC10_Name") %>%
  add_status_colors('water') 

water12 <- water %>%
  mutate(year = format(start_date,"%Y")) %>%
  left_join(wa_huc12_med, by = "HUC12_Name") %>%
  apply_cohensD ('water', "HUC12_Name") %>%
  add_status_colors('water') 

######################## END WATER ########################


######################## BEGIN CHUM ########################

# merge chum HUC 12 dataframe with median investment year dataframe
# group by chum site, then categorize each count as before/during/after investment
# or noProject if no investment in that HUC
# apply the cohen's D function to the chum dataframe to get a cohen's D for each chum site
# then join this dataframe with the before/after mean count dataframes 
# to categorize each site as "worse" or "improving" after the median investment year in each HUC 12
chum12 <- chum_counts %>%
  left_join(huc12_med, by = "HUC12_Name") %>%
  apply_cohensD('chum', HUC = 'HUC12_Name') %>%
  add_status_colors('chum') 

# merge chum HUC 10 dataframe with median investment year dataframe
# group by chum site, then categorize each count as before/during/after investment
# or noProject if no investment in that HUC
# apply the cohen's D function to the chum dataframe to get a cohen's D for each chum site
# then join this dataframe with the before/after mean count dataframes 
# to categorize each site as "worse" or "improving" after the median investment year in each HUC 10
chum10 <- chum_counts %>%
  left_join(huc10_med, by = "HUC10_Name") %>%
  apply_cohensD('chum', HUC = 'HUC10_Name') %>%
  add_status_colors('chum') 

######################## END CHUM ########################

# c('#d73027','#fc8d59','#fee08b','#d9ef8b','#91cf60','#1a9850')
# cohen's D: 'small effect' = 0.2, med effect = 0.5, large effect = 0.8

# select variables for the final merged dataframe
# rename variables to match across outcomes/investments
ch12_formerge <- chum12 %>%
  select(year:Description, HUC12_id, HUC12_Name, 
         medianyr, cohensd:colorblind) %>%
  rename(name = sitename, description = Description,
         measurement = count, HUC_id = HUC12_id, HUC_Name = HUC12_Name) %>%
  mutate(result_type = 'Chum Salmon', unit = '', HUC_level = '12',
         Study_ID = NA, full_date = NA, Location_ID = NA, logMeasurement = NA,
         project_source = NA) 

chum_formerge <- chum10 %>%
  select(year:Description, HUC10_id, HUC10_Name, 
         medianyr, cohensd:colorblind) %>%
  rename(name = sitename, description = Description,
         measurement = count, HUC_id = HUC10_id, HUC_Name = HUC10_Name) %>%
  mutate(result_type = 'Chum Salmon', unit = '', HUC_level = '10',
         Study_ID = NA, full_date = NA, Location_ID = NA, logMeasurement = NA,
         project_source = NA) %>%
  rbind(ch12_formerge)

wa12_formerge <- water12 %>%
  select(lon, lat, Study_ID, Study_Name, Location_ID, start_date,
         logMeasurement, result_type, measurement, unit,
         HUC12_id, HUC12_Name, year:colorblind) %>%
  rename(name = Study_Name, full_date = start_date,
         HUC_id = HUC12_id, HUC_Name = HUC12_Name) %>%
  mutate(HUC_level = '12', year = as.numeric(year),
         project_cat = NA, description = NA, project_source = NA) 

water_formerge <- water10 %>%
  select(lon, lat, Study_ID, Study_Name, Location_ID, start_date,
         logMeasurement, result_type, measurement, unit,
         HUC10_id, HUC10_Name, year:colorblind) %>%
  rename(name = Study_Name, full_date = start_date,
         HUC_id = HUC10_id, HUC_Name = HUC10_Name) %>%
  mutate(HUC_level = '10', year = as.numeric(year),
         project_cat = NA, description = NA, project_source = NA) %>%
  rbind(wa12_formerge)

# separate projects into huc 10 and 12 dataframes, then bind them together
# format columns to match outcome data for ease with binding
p_h10 <- ph %>% select(-HUC12_id, -HUC12_Name) %>%
  rename(measurement = cost, HUC_id = HUC10_id, HUC_Name = HUC10_Name) %>%
  mutate(result_type = 'Investment', unit = 'dollars', HUC_level = '10')
all_projects_formerge <- ph %>% select(-HUC10_id, -HUC10_Name) %>%
  rename(measurement = cost, HUC_id = HUC12_id, HUC_Name = HUC12_Name) %>%
  mutate(result_type = 'Investment', unit = 'dollars', HUC_level = '12') %>%
  rbind(p_h10)

# create a list of column names that do not occur in the project df
empties <- c('description', 'full_date', 'Location_ID', 'logMeasurement', 
             'medianyr', 'cohensd', 'TimePeriod', 'effectsize', 'mean_before', 
             'mean_after', 'status', 'color', 'coloreffect', 'colorblind')
# set all non-project columns to be NA
all_projects_formerge[,empties] <- NA

# bind all dataframes together
all_dfs <- bind_rows(chum_formerge, water_formerge, all_projects_formerge)

# add unique ID column
# all_dfs$id <- 1:nrow(all_dfs)

# export the data to the shiny app directory
saveRDS(all_dfs, "../shinyapp/data/all-dfs.rds")

