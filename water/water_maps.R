library(tidyverse)
library(leaflet)

water_huc <- readRDS("./data/water_huc.rds")
turbidity <- filter(water_huc, result_type == "Turbidity")
tss <- filter(water_huc, result_type == "TSS")

################################ turbidity mapping ################################### 

m <- leaflet(data = turbidity) %>% 
  setView(lng = -122.996823, lat = 47.5642594, zoom = 9)

waterIcon <- makeIcon(
  iconUrl = 'icons/water-15.svg',
  iconWidth = 25, iconHeight = 25
)

content1 <- paste(sep = ": ",
                  "<b>Turbidity NTU</b>",
                  turbidity$measurement
)

content2 <- paste(sep = ": ",
                  "<b>Project Name</b>",
                  turbidity$Study_Name
)
content3 <- paste(sep = ": ",
                  "<b>Date</b>",
                  turbidity$start_date
)

content_items = c(content1,content2,content3)
# full_content <- paste(sep = "<br>", content2,content1)
# full_content <- paste(collapse = "<br>", content_items)
full_content <- sprintf("Project Name: %s <br>Date: %s <br> Turbidity NTU: %s", 
                        turbidity$Study_Name, turbidity$start_date, turbidity$measurement)

m %>%
  addTiles() %>%
  addMarkers(~lon, ~lat, popup = full_content,
             icon = waterIcon,
             clusterOptions = markerClusterOptions())

qpal <- colorQuantile("BuPu", turbidity$measurement, n = 5)
m %>%
  addProviderTiles("Stamen.Terrain", group = "Terrain") %>%
  addCircleMarkers(~lon, ~lat, popup = full_content,
                   radius = 6,
                   color = ~qpal(measurement),
                   stroke = FALSE, fillOpacity = 0.5,
                   group = "Water Quality") %>%
  addLegend(pal = qpal, values = ~measurement, opacity = 1) %>%
  addLayersControl(
    baseGroups = "Terrain",
    overlayGroups = "Water Quality",
    options = layersControlOptions(collapsed = FALSE)
  )


# # Show first 20 rows from the `quakes` dataset
# leaflet(data = quakes[1:20,]) %>% 
#   addTiles() %>%
#   addMarkers(~long, ~lat, popup = ~as.character(mag))

################################ TSS mapping ################################################ 

m <- leaflet(data = tss) %>% 
  setView(lng = -122.996823, lat = 47.65, zoom = 9)

# waterIcon <- makeIcon(
#   iconUrl = 'icons/water-15.svg',
#   iconWidth = 25, iconHeight = 25
# )

content1 <- paste(sep = ": ",
                  "<b>TSS</b>",
                  tss$measurement
)

content2 <- paste(sep = ": ",
                  "<b>Project Name</b>",
                  tss$Study_Name
)
content3 <- paste(sep = ": ",
                  "<b>Date</b>",
                  tss$start_date
)


full_content <- sprintf("Project Name: %s <br>Date: %s <br> WRIA: %s <br> TSS mg/L: %s", 
                        tss$Study_Name, tss$start_date, 
                        tss$Watershed_WRIA, tss$measurement)

# m %>%
#   addTiles() %>%
#   addMarkers(~lon, ~lat, popup = full_content,
#              icon = waterIcon,
#              clusterOptions = markerClusterOptions())


qpal <- colorQuantile("BuPu", tss$measurement, n = 5)
m %>%
  addProviderTiles("Stamen.Terrain", group = "Terrain") %>%
  addCircleMarkers(~lon, ~lat, popup = full_content,
                   radius = 10,
                   color = ~qpal(measurement),
                   stroke = FALSE, fillOpacity = 0.5,
                   group = "Water Quality") %>%
  addLegend(pal = qpal, values = ~measurement, opacity = 1) %>%
  addLayersControl(
    baseGroups = "Terrain",
    overlayGroups = "Water Quality",
    options = layersControlOptions(collapsed = FALSE)
  )

factpal <- colorFactor("Dark2", tss$Watershed_WRIA)
m %>%
  addProviderTiles("Stamen.Terrain", group = "Terrain") %>%
  addCircleMarkers(~lon, ~lat, popup = full_content,
                   radius = 10,
                   color = ~factpal(Watershed_WRIA),
                   stroke = FALSE, fillOpacity = 0.5,
                   group = "Water Quality") %>%
  addLegend(pal = factpal, values = ~Watershed_WRIA, opacity = 1) %>%
  addLayersControl(
    baseGroups = "Terrain",
    overlayGroups = "Water Quality",
    options = layersControlOptions(collapsed = FALSE)
  )

all_safe_tss <- tss %>%
  mutate(safety = as.factor(ifelse(logMeasurement >= 1.90309, "unsafe", "safe")))

safe_tss <- filter(all_safe_tss, safety == 'safe')

unsafe_tss <- filter(all_safe_tss, safety == 'unsafe')

sfpal <- colorFactor("Set1", safe_tss$safety)

m2 <- leaflet(data = safe_tss) %>% 
  setView(lng = -122.996823, lat = 47.65, zoom = 9)

m2 %>%
  addProviderTiles("Stamen.Terrain", group = "Terrain") %>%
  addCircleMarkers(data = safe_tss, ~lon, ~lat, popup = full_content,
                   radius = 6,
                   color = "green",
                   stroke = FALSE, fillOpacity = 0.4,
                   group = "Safe TSS (TSS < 80 mg/L)") %>%
  addCircleMarkers(data = unsafe_tss, ~lon, ~lat, popup = full_content,
                   radius = 6,
                   color = "red",
                   stroke = FALSE, fillOpacity = 0.4,
                   group = "Unsafe TSS (TSS > 80 mg/L)") %>%
  addLayersControl(
    baseGroups = "Terrain",
    overlayGroups = c("Safe TSS (TSS < 80 mg/L)", "Unsafe TSS (TSS > 80 mg/L)"),
    options = layersControlOptions(collapsed = FALSE)
  )
# to add timeline slider:
# http://stackoverflow.com/questions/36554605/cant-loop-with-rs-leaflet-package-to-produce-multiple-maps/36587525#36587525
