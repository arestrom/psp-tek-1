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


# # read in saved GeoJSON file as sp object
# chum_to_map <- geojsonio::geojson_read("chumsite.json",
#                                        what = "sp")


# extractCoords <- function(sp.df)
# {
#   results <- list()
#   for(i in 1:length(sp.df@polygons[[1]]@Polygons))
#   {
#     results[[i]] <- sp.df@polygons[[1]]@Polygons[[i]]@coords
#   }
#   results <- Reduce(rbind, results)
#   results
# }
# 
# coords <- extractCoords(huc10)
# 
# # read in saved GeoJSON file as sp object
# HWP <- geojsonio::geojson_read("../js_mapping_prototype/jsMaps/polygonMap/HWP.json",
#                                        what = "sp")
# 
# hydro <- readOGR("../js_mapping_prototype/shapefileprep/WBD_PRJ_HYDRO.shp",
#                   layer = "WBD_PRJ_HYDRO")
# huc10 <- readOGR("./data/WBDHUC10_H.shp", GDAL1_integer64_policy = TRUE,
#                  layer = "WBDHUC10_H")
# 
# huc10 <- geojsonio::geojson_read("./data/WBDHUC10_H.shp", 
#                                what = "sp")
# plot(huc10)
# 
# simplified <- rmapshaper::ms_simplify(huc10)
# plot(simplified)
# 
# adm <- getData('GADM', country='UKR', level=1)
# 
# popup <- paste0("<strong>Name: </strong>", 
#                 HWP$Project_Na)
# plot(HWP)
# leaflet() %>% 
#   setView(lng = -122.996823, lat = 47.65, zoom = 9) %>%
#   addProviderTiles("Stamen.Terrain") %>%
#   addPolygons(data=simplified) 
# # %>%
# #   addGeoJSON(HWP) %>%
# #   addPolygons(data=hydro)
# 
# get_study_poly <- function(studyid, df){
#   # extract study from dataframe
#   study <- df %>%
#     filter(Study_ID == studyid) 
#   # convert from tbl_df to dataframe
#   study <- as.data.frame(study)
#   # get just lat/lon as vector
#   coords <- cbind(study$lat,study$lon)
#   # use distal_points helper function to obtain only the n most distal points
#   dps <- distal_points(coords,10)
#   # remove duplicates and rename columns for ease of use with leaflet
#   dps.df <- as.data.frame(dps) %>%
#     rename(lat = V1, lon = V2) %>%
#     distinct(lat,lon)
#   # use chull function to make a non-intersecting polygon
#   study_poly <- dps.df[chull(dps.df),]
#   return(study_poly)
# }
# 
# # http://stackoverflow.com/questions/22152482/choose-n-most-distant-points-in-r
# distal_points <- function(xy, n){
# 
#   subset <- xy
# 
#   alldist <- as.matrix(dist(subset))
# 
#   while (nrow(subset) > n) {
#     cdists = rowSums(alldist)
#     closest <- which(cdists == min(cdists))[1]
#     subset <- subset[-closest,]
#     alldist <- alldist[-closest,-closest]
#   }
#   return(subset)
# }
# 
# m4 <- leaflet() %>% 
#   setView(lng = -122.996823, lat = 47.65, zoom = 9) %>%
#   addProviderTiles("Stamen.Terrain") %>%
#   addPolygons(data = bedi.NI.poly, lng = ~lon, lat = ~lat) %>%
#   addPolygons(data = dose.NI.poly, lng = ~lon, lat = ~lat)
# m4