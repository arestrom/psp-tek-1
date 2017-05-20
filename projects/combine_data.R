library(tidyverse)

######################## BEGIN CHUM INVESTMENT ########################

# read in the investment-project-huc dataframe
ph <- readRDS("./data/project_huc.rds")

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

# for water quality: group dataframe by result type and then by HUC
  # categorize each measurement as before, during, or after the median investment year in that HUC
  # add a 'noProject' category for HUCs without a project
  # calculate the cohensD for each measurement in each HUC (add new column)
      # NOTE: This incorporates a sign change to make change stat consistent with chum
  # add new columns for the mean before and after median investment year in each HUC
  # reorganize the dataframe
  # then, categorize the effect size based on cohen's d standards
  # determined by Leska Fore of PSP:
     # Cohen's D value     Category
     # ----------------    -----------------
     # D ??? 0.8             Large improvement
     # 0.2 ??? D < 0.8       Small improvement
     # -0.2 < D < 0.2      No change
     # -0.8 < D ??? -0.2     Small decline
     # D ??? -0.8            Large decline

# for chum: group dataframe by chum site
  # categorize each measurement as before, during, or after the median investment year in that HUC
  # add a 'noProject' category for sites without a project
  # calculate the cohensD for each site (add new column) 
  # add new columns for the mean and sd in each site, as well as other variables for the roll-up
  # group the dataframe by HUC
  # calculate the mean cohensD for each HUC (add new column), as well as other variables
  # reorganize the dataframe
  # then, categorize the effect size based on cohen's d standards
    # determined by Leska Fore of PSP - see above

apply_cohensD <- function(data, outcome, HUC) {
  if (outcome == 'water') { 
    data %>%
      group_by_('result_type', HUC) %>%
      mutate(TimePeriod = ifelse(year > medianyr, 'after',
                                 ifelse(year < medianyr, 'before',
                                        'during'))) %>%
      mutate(TimePeriod = ifelse(is.na(TimePeriod), 'noProject', TimePeriod)) %>%
      spread(TimePeriod, measurement) %>%
      mutate(cohensd_huc_mean = -1*cohensD_manual(before, after),
             huc_mean_after = mean(after, na.rm = TRUE),
             huc_mean_before = mean(before, na.rm = TRUE)) %>%
      gather(key = 'TimePeriod', value = 'measurement', `before`, `after`, `during`, `noProject`, na.rm = TRUE) %>%
      mutate(effectsize = ifelse(abs(cohensd_huc_mean) < 0.2, 'no change',
                                 ifelse(abs(cohensd_huc_mean) >= 0.8, 'large',
                                        'small')))
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
      mutate(effectsize = ifelse(abs(cohensd_huc_mean) < 0.2, 'no change',
                                 ifelse(abs(cohensd_huc_mean) >= 0.8, 'large',
                                        'small')),
             site_effectsize = ifelse(abs(cohensd) < 0.2, 'no change',
                                      ifelse(abs(cohensd) >= 0.8, 'large',
                                             'small')))
  }
}

# Adding status colors
# Note: With sign change on water Cohen's D, there's now no need for if/else
# Apply a diverging color scale (based on effect size and status)
    # Standard colors are ColorBrewer 5-class RdYlGn; middle color replaced by lightgray (#d3d3d3)
        # Reference: http://colorbrewer2.org/?type=diverging&scheme=RdYlGn&n=5
    # colorblind colors are ColorBrewer 5-class PiYG
        # Reference: http://colorbrewer2.org/?type=diverging&scheme=PiYG&n=5
add_status_colors <- function(data) {
  colorscale = c('#d7191c','#fdae61','#d3d3d3','#a6d96a','#1a9641')
  colorscale_colorblind = c('#d01c8b','#f1b6da','#f7f7f7','#b8e186','#4dac26')
  data %>% 
      mutate(status = ifelse(cohensd_huc_mean < -0.2, 'worse',
                             ifelse(cohensd_huc_mean > 0.2, 'improving',
                                    'no change')),
             coloreffect = ifelse(effectsize == 'large' & status == 'worse', colorscale[1],
                                  ifelse(effectsize == 'small' & status == 'worse', colorscale[2],
                                         ifelse(effectsize == 'no change' & status == 'no change', colorscale[3],
                                                ifelse(effectsize == 'small' & status == 'improving', colorscale[4],
                                                       ifelse(effectsize == 'large' & status == 'improving', colorscale[5],
                                                              NA))))),
             colorblind = ifelse(effectsize == 'large' & status == 'worse', colorscale_colorblind[1],
                                 ifelse(effectsize == 'small' & status == 'worse', colorscale_colorblind[2],
                                        ifelse(effectsize == 'no change' & status == 'no change', colorscale_colorblind[3],
                                               ifelse(effectsize == 'small' & status == 'improving', colorscale_colorblind[4],
                                                      ifelse(effectsize == 'large' & status == 'improving', colorscale_colorblind[5],
                                                             NA))))))
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
  add_status_colors() 

water12 <- water %>%
  mutate(year = format(start_date,"%Y")) %>%
  left_join(wa_huc12_med, by = "HUC12_Name") %>%
  apply_cohensD ('water', "HUC12_Name") %>%
  add_status_colors() 

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
  add_status_colors() 

# merge chum HUC 10 dataframe with median investment year dataframe
# group by chum site, then categorize each count as before/during/after investment
# or noProject if no investment in that HUC
# apply the cohen's D function to the chum dataframe to get a cohen's D for each chum site
# then join this dataframe with the before/after mean count dataframes 
# to categorize each site as "worse" or "improving" after the median investment year in each HUC 10
chum10 <- chum_counts %>%
  left_join(huc10_med, by = "HUC10_Name") %>%
  apply_cohensD('chum', HUC = 'HUC10_Name') %>%
  add_status_colors() 

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

# bind all dataframes together
all_dfs <- bind_rows(chum_formerge, water_formerge, all_projects_formerge)

# export the data to the shiny app directory
saveRDS(all_dfs, "../shinyapp/data/all-dfs.rds")

