library(leaflet)
source("shp2r.R")

# statewide HUC 8 from http://geo.wa.gov/datasets/de1373e9f5394e5284660c939c038689_0
WAstate_simplified_huc8 <- shp2r('Hydrologic_Accounting_Units_8_digit_HUC', simplify = TRUE)
saveRDS(WAstate_simplified_huc8, "../shinyapp/data/WAhuc8.rds")

WAstate_huc12 <- shp2r('WBDHU12', simplify = FALSE)
saveRDS(WAstate_huc12, "../shinyapp/data/WAhuc12.rds")

# plot(WAstate_simplified_huc8)
leaflet() %>% 
  setView(lng = -120.7401, lat = 48, zoom = 7) %>%
  addProviderTiles("Stamen.Terrain") %>%
  addPolygons(data=WAstate_simplified_huc8) 