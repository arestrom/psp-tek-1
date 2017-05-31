library(tidyverse)
source('cohensD_functions.R')

######################## BEGIN CHUM INVESTMENT ########################

# read in the investment-project-huc dataframe
ph <- readRDS("./data/project_huc.rds")

# read in the chum-huc dataframe, select only columns of interest
chum_locations <- readRDS("../psp-chum/data/chum_huc.rds") %>%
  select(-(Permanent:Line.NR))

# read in the chum-counts dataframe, join with location/huc data
chum_counts <- readRDS("../psp-chum/data/tidychum.rds") %>%
  left_join(chum_locations)

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
  left_join(wa_huc10_med, by = "HUC10_Name") %>%
  apply_cohensD ('water', "HUC10_Name") %>%
  add_status_colors() 

water12 <- water %>%
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

# select variables for the final merged dataframe
# rename variables to match across outcomes/investments
ch12_formerge <- chum12 %>%
  select(-HUC10_id, -HUC10_Name) %>%
  rename(name = sitename, description = Description,
         measurement = count, HUC_id = HUC12_id, HUC_Name = HUC12_Name) %>%
  mutate(result_type = 'Chum Salmon', unit = '', HUC_level = '12') 

chum_formerge <- chum10 %>%
  select(-HUC12_id, -HUC12_Name) %>%
  rename(name = sitename, description = Description,
         measurement = count, HUC_id = HUC10_id, HUC_Name = HUC10_Name) %>%
  mutate(result_type = 'Chum Salmon', unit = '', HUC_level = '10') %>%
  rbind(ch12_formerge)

wa12_formerge <- water12 %>%
  select(-HUC10_id, -HUC10_Name) %>%
  rename(name = Study_Name, full_date = start_date,
         HUC_id = HUC12_id, HUC_Name = HUC12_Name) %>%
  mutate(HUC_level = '12', year = as.numeric(year)) 

water_formerge <- water10 %>%
  select(-HUC12_id, -HUC12_Name) %>%
  rename(name = Study_Name, full_date = start_date,
         HUC_id = HUC10_id, HUC_Name = HUC10_Name) %>%
  mutate(HUC_level = '10', year = as.numeric(year)) %>%
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
AD <- readRDS("../shinyapp/data/all-dfs.rds")
# export the data to the shiny app directory
saveRDS(all_dfs, "../shinyapp/data/all-dfs.rds")

