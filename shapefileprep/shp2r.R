library(rgdal) # readOGR
library(rmapshaper)

shp2r <- function(layer, simplify = TRUE) {
  # convert shape file to Large SpatialPolygonsDataFrame
  shp_raw <- readOGR(dsn=path.expand("./data/rawshp"), layer=layer)
  # convert the projection to lat long
  shp_latlon <- spTransform(shp_raw, CRS("+init=epsg:4326"))
  
  if (simplify == TRUE) {
    # simplify the Large SpatialPolygonsDataFrame --> SpatialPolygonsDataFrame (smaller size, faster loading)
    simplified <- rmapshaper::ms_simplify(shp_latlon)
    return(simplified)
  } else {
    return(shp_latlon)
  }
  
}