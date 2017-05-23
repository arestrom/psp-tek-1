library(tidyverse)
source('cohensD_functions.R')

######################## BEGIN WATER INVESTMENT ########################

# read in the investment-project-huc dataframe
ph <- readRDS("./data/state_project_huc.rds")

# read in water quality rdata
water <- readRDS("../water/data/state_turbidity_huc.rds")

# create a HUC-8 investment dataframe, 
# filtered to only include investments in water HUC8s
# calculate the median investment implementation year for each HUC8
# create a HUC8-median year entity to merge with outcome dataframes below
wa_huc8_med <- ph %>%
  filter(HUC8_Name %in% unique(water$HUC8_Name))  %>%
  group_by(HUC8_Name) %>%
  mutate(medianyr = median(as.numeric(year))) %>%
  select(HUC8_Name, medianyr) %>%
  group_by(HUC8_Name, medianyr) %>%
  summarise()

# create a HUC-12 investment dataframe, 
# filtered to only include investments in water HUC12s
# calculate the median investment implementation year for each HUC12
# create a HUC12-median year entity to merge with outcome dataframes below
wa_huc12_med <- ph %>%
  filter(HUC12_Name %in% unique(water$HUC12_Name))  %>%
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
water8 <- water %>%
  left_join(wa_huc8_med, by = "HUC8_Name") %>%
  apply_cohensD ('water', "HUC8_Name") %>%
  add_status_colors() 

water12 <- water %>%
  left_join(wa_huc12_med, by = "HUC12_Name") %>%
  apply_cohensD ('water', "HUC12_Name") %>%
  add_status_colors() 

######################## END WATER ########################

wa12_formerge <- water12 %>%
  select(-HUC8_id, -HUC8_Name) %>%
  rename(name = Study_Name, full_date = start_date,
         HUC_id = HUC12_id, HUC_Name = HUC12_Name) %>%
  mutate(HUC_level = '12', year = as.numeric(year)) 

water_formerge <- water8 %>%
  select(-HUC12_id, -HUC12_Name) %>%
  rename(name = Study_Name, full_date = start_date,
         HUC_id = HUC8_id, HUC_Name = HUC8_Name) %>%
  mutate(HUC_level = '8', year = as.numeric(year)) %>%
  rbind(wa12_formerge)

# separate projects into huc 8 and 12 dataframes, then bind them together
# format columns to match outcome data for ease with binding
p_h8 <- ph %>% select(-HUC12_id, -HUC12_Name) %>%
  rename(measurement = cost, HUC_id = HUC8_id, HUC_Name = HUC8_Name) %>%
  mutate(result_type = 'Investment', unit = 'dollars', HUC_level = '8')
all_projects_formerge <- ph %>% select(-HUC8_id, -HUC8_Name) %>%
  rename(measurement = cost, HUC_id = HUC12_id, HUC_Name = HUC12_Name) %>%
  mutate(result_type = 'Investment', unit = 'dollars', HUC_level = '12')  %>%
  rbind(p_h8)

# bind all dataframes together
all_dfs <- bind_rows(water_formerge, all_projects_formerge)

# export the data to the shiny app directory
saveRDS(all_dfs, "../shinyapp/data/WAstate.rds")

######################## BEGIN MAP ########################

library(leaflet)

all_dfs <- readRDS("../shinyapp/data/WAstate.rds")
projects <- filter(all_dfs, result_type == "Investment")
water8 <- filter(all_dfs, result_type == "Turbidity",  HUC_level == '8', TimePeriod == 'after') %>%
  distinct(HUC_id, coloreffect)
water12 <- filter(all_dfs, result_type == "Turbidity",  HUC_level == '12', TimePeriod == 'after') %>%
  distinct(HUC_id, coloreffect)

m8 <- leaflet() %>% 
  setView(lng = -120.7401, lat = 48, zoom = 7) %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addCircleMarkers(projects$lon, projects$lat,
                   radius = 4,
                   color = '#984ea3',
                   stroke = FALSE, fillOpacity = 1,
                   group = "Projects") %>%
  addCircleMarkers(water8$lon, water8$lat, 
                   radius = 5,
                   color = water8$coloreffect,
                   stroke = FALSE, fillOpacity = 0.8,
                   group = "Water") %>%
  addLayersControl(
    baseGroups = "Water",
    overlayGroups = "Projects",
    options = layersControlOptions(collapsed = FALSE)
  ) %>% 
  addLegend(colors = c('#1a9641','#a6d96a','#d3d3d3','#fdae61','#d7191c'),
            labels = c('large improvement', 'small improvement', 'no change',
                       'small decline',  'large decline'),
            position = 'bottomright',
            title = 'Turbidity')
m8

huc8_df <- readRDS("../shinyapp/data/WAhuc8.rds")
# merge the shapefile and the df for mapping
shapefile <- sp::merge(x = huc8_df, y = water8[ , c("HUC_id","coloreffect")], 
                       by.x = "HUC8", by.y = "HUC_id", all.x=TRUE, duplicateGeoms = TRUE)

shapefile$fill_opacity <- ifelse(is.na(shapefile$coloreffect), 0, .7)

m <-  leaflet() %>% 
  setView(lng = -120.7401, lat = 48, zoom = 7) %>%
  addProviderTiles("Stamen.Terrain",
                   options = providerTileOptions(minZoom = 6)) %>%
  # use the shapefile() df
  addPolygons(data = shapefile,
              # popup = paste(sep = "", "<b>HUC: </b>", shapefile$Name),
              # color according to increases/decreases as defined by coloreffect column
              fillColor = ~shapefile$coloreffect,
              fillOpacity = ~shapefile$fill_opacity,
              color = "black",
              weight = 1) %>%
  addLegend(colors = c('#1a9641','#a6d96a','#d3d3d3','#fdae61','#d7191c'),
            labels = c('large improvement', 'small improvement', 'no change',
                       'small decline',  'large decline'),
            position = 'bottomright',
            title = 'Turbidity')
m

huc12_df <- readRDS("../shinyapp/data/WAhuc12.rds")
# merge the shapefile and the df for mapping
shapefile12 <- sp::merge(x = huc12_df, y = water12[ , c("HUC_id","coloreffect")], 
                       by.x = "HUC12", by.y = "HUC_id", all.x=TRUE, duplicateGeoms = TRUE)

shapefile12$fill_opacity <- ifelse(is.na(shapefile12$coloreffect), 0, .7)

m12 <-  leaflet() %>% 
  setView(lng = -120.7401, lat = 48, zoom = 7) %>%
  addProviderTiles("Stamen.Terrain",
                   options = providerTileOptions(minZoom = 6)) %>%
  # use the shapefile12() df
  addPolygons(data = shapefile12,
              # popup = paste(sep = "", "<b>HUC: </b>", shapefile12$Name),
              # color according to increases/decreases as defined by coloreffect column
              fillColor = ~shapefile12$coloreffect,
              fillOpacity = ~shapefile12$fill_opacity,
              color = "black",
              weight = 1) %>%
  addLegend(colors = c('#1a9641','#a6d96a','#d3d3d3','#fdae61','#d7191c'),
            labels = c('large improvement', 'small improvement', 'no change',
                       'small decline',  'large decline'),
            position = 'bottomright',
            title = 'Turbidity')
m12
