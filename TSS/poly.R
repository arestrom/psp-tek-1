get_study_poly <- function(studyid, df){
  # extract study from dataframe
  study <- df %>%
    filter(Study_ID == studyid) 
  # convert from tbl_df to dataframe
  study <- as.data.frame(study)
  # get just lat/lon as vector
  coords <- cbind(study$lat,study$lon)
  # use distal_points helper function to obtain only the n most distal points
  dps <- distal_points(coords,10)
  # remove duplicates and rename columns for ease of use with leaflet
  dps.df <- as.data.frame(dps) %>%
    rename(lat = V1, lon = V2) %>%
    distinct(lat,lon)
  # use chull function to make a non-intersecting polygon
  study_poly <- dps.df[chull(dps.df),]
  return(study_poly)
}

# http://stackoverflow.com/questions/22152482/choose-n-most-distant-points-in-r
distal_points <- function(xy, n){

  subset <- xy

  alldist <- as.matrix(dist(subset))

  while (nrow(subset) > n) {
    cdists = rowSums(alldist)
    closest <- which(cdists == min(cdists))[1]
    subset <- subset[-closest,]
    alldist <- alldist[-closest,-closest]
  }
  return(subset)
}

m4 <- leaflet() %>% 
  setView(lng = -122.996823, lat = 47.65, zoom = 9) %>%
  addProviderTiles("Stamen.Terrain") %>%
  addPolygons(data = bedi.NI.poly, lng = ~lon, lat = ~lat) %>%
  addPolygons(data = dose.NI.poly, lng = ~lon, lat = ~lat)
m4