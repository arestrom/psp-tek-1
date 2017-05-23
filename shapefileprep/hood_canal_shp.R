library(leaflet)
source("shp2r.R")

# call the shp2r function to convert each HUC-level shapefile to a SpatialPolygonsDataFrame
simplified_huc10 <- shp2r('WBDHUC10_H', simplify = TRUE)
simplified_huc12 <- shp2r('WBDHUC12_H', simplify = TRUE)

# save huc SpatialPolygonsDataFrame as rds files for use in shiny
saveRDS(simplified_huc10, "../shinyapp/data/huc10.rds")
saveRDS(simplified_huc12, "../shinyapp/data/huc12.rds")

# test mapping of SpatialPolygonsDataFrame
leaflet() %>% 
  setView(lng = -122.996823, lat = 47.65, zoom = 9) %>%
  addProviderTiles("Stamen.Terrain") %>%
  addPolygons(data=simplified_huc12) 
