library(leaflet)
library(geojsonio) # to convert GeoJson to spatial data
library(rgdal) # readOGR
library(rmapshaper)
# library(raster)

# convert shape file to Large SpatialPolygonsDataFrame
huc10_raw <- readOGR(dsn=path.expand("./data/rawshp"), layer="WBDHUC10_H")
# convert the projection to lat long
huc10 <- spTransform(huc10_raw, CRS("+init=epsg:4326"))
# plot(huc10)
# simplify the Large SpatialPolygonsDataFrame --> SpatialPolygonsDataFrame (smaller size, faster loading)
simplified_huc10 <- rmapshaper::ms_simplify(huc10)

# convert shape file to Large SpatialPolygonsDataFrame
huc12_raw <- readOGR(dsn=path.expand("./data/rawshp"), layer="WBDHUC12_H")
# convert the projection to lat long
huc12 <- spTransform(huc12_raw, CRS("+init=epsg:4326"))
# simplify the Large SpatialPolygonsDataFrame --> SpatialPolygonsDataFrame (smaller size, faster loading)
simplified_huc12 <- rmapshaper::ms_simplify(huc12)

# save huc SpatialPolygonsDataFrame as rds files for use in shiny
saveRDS(simplified_huc10, "./data/huc10.rds")
saveRDS(simplified_huc12, "./data/huc12.rds")

# h10 <- readRDS("./data/huc10.rds")

# saving out this geojson just in case 
huc10_json <- geojsonio::geojson_json(simplified_huc10)
huc12_json <- geojsonio::geojson_json(simplified_huc12)
geojson_write(huc10_json, file = "huc10.json")
geojson_write(huc12_json, file = "huc12.json")

# plot(simplified)
leaflet() %>% 
  setView(lng = -122.996823, lat = 47.65, zoom = 9) %>%
  addProviderTiles("Stamen.Terrain") %>%
  addPolygons(data=simplified_huc12) 