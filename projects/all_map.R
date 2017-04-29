library(tidyverse)
library(leaflet)

all_dfs <- readRDS("../shinyapp/data/all-dfs.rds")
projects <- filter(all_dfs, result_type == "Investment")
# projects10 <- filter(all_dfs, result_type == "Investment", HUC_level == '10')
# projects12 <- filter(all_dfs, result_type == "Investment", HUC_level == '12')
water10 <- filter(all_dfs, result_type == "Turbidity" | result_type == "TSS",  HUC_level == '10')
water12 <- filter(all_dfs, result_type == "Turbidity" | result_type == "TSS", HUC_level == '12')
chum10 <- filter(all_dfs, result_type == "Chum Salmon", HUC_level == '10')
chum12 <- filter(all_dfs, result_type == "Chum Salmon", HUC_level == '12')
######################## BEGIN MAP ########################

m12 <- leaflet() %>% 
  setView(lng = -122.996823, lat = 47.5642594, zoom = 9) %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addCircleMarkers(projects$lon, projects$lat,
                   radius = 4,
                   color = '#984ea3',
                   stroke = FALSE, fillOpacity = 1,
                   group = "Projects") %>%
  addCircleMarkers(chum12$lon, chum12$lat,
                   radius = 5,
                   color = chum12$coloreffect,
                   stroke = FALSE, fillOpacity = 0.8,
                   group = "Chum") %>%
  addCircleMarkers(water12$lon, water12$lat, 
                   radius = 5,
                   color = water12$coloreffect,
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
  addProviderTiles("Esri.WorldImagery") %>%
  addCircleMarkers(projects$lon, projects$lat,
                   radius = 4,
                   color = '#984ea3',
                   stroke = FALSE, fillOpacity = 1,
                   group = "Projects") %>%
  addCircleMarkers(chum10$lon, chum10$lat,
                   radius = 5,
                   color = chum10$coloreffect,
                   stroke = FALSE, fillOpacity = 0.8,
                   group = "Chum") %>%
  addCircleMarkers(water10$lon, water10$lat, 
                   radius = 5,
                   color = water10$coloreffect,
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
