library(tidyverse)
library(leaflet)

water_huc <- readRDS("../water/data/water_huc.rds")
turbidity <- filter(water_huc, result_type == "Turbidity")
tss <- filter(water_huc, result_type == "TSS")

################################ create polygons ################################################ 
# http://www.nickeubank.com/wp-content/uploads/2015/10/RGIS1_SpatialDataTypes_part1_vectorData.html
# http://stackoverflow.com/questions/37573413/loading-spatialpolygonsdataframe-with-leaflet-for-r-doesnt-work
# CRSs are not a trivial topic. In short, web mapping is basically done using
# web mercator. Leaflet, however, needs non-projected latitude and longitude
# coordinates as input and internally transforms them to web mercator (bad
# design in my opinion, but we need to live with that). So for leaflet, you
# basically just need to know that they need to be in '+init=epsg:4326' (or
# '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs' to be more precise).

# creates SpatialPointsDataFrame (not a tbl df anymore!)
# coordinates(tss) <- c("lon", "lat")
# L <- tss$Study_ID == 'BEDI0004'
# tss[L,]
bedi <- tss %>%
  filter(Study_ID == 'BEDI0004') %>%
  mutate(lat = as.numeric(lat), lon = as.numeric(lon)) %>%
  select(lat,lon)
# bedi.p <- Polygon(bedi)
# bedi.polys <- Polygons(list(dose.p), "bedi")

# creates SpatialPoints
# coordinates(bedi) <- c("lon", "lat")

dose <- tss %>%
  filter(WRIA_ID == 16) %>%
  mutate(lat = as.numeric(lat), lon = as.numeric(lon)) %>%
  select(lat,lon)
# dose.p <- Polygon(dose)
# dose.polys <- Polygons(list(dose.p), "Dose")

# Make spatialy Polygons
# map <- SpatialPolygons(list(bedi.polys, dose.polys))
# is.projected(map)
# Returns `NA` if no geographic coordinate system or projection; returns
# FALSE if has geographic coordinate system but no projection.
# crs.geo <- CRS("+init=epsg:4326")
# proj4string(map) <- crs.geo  # define projection system of our data
# is.projected(map)

# head(shape)
m3 <- leaflet() %>% 
  setView(lng = -122.996823, lat = 47.65, zoom = 9) %>%
  addProviderTiles("Stamen.Terrain") %>%
  addPolygons(data = bedi, lng = ~lon, lat = ~lat)
m3
# head(dose.p)

# need to find polygon boundary (distance points) so not overplotting

bedi.c <- tss %>%
  filter(Study_ID == 'BEDI0004') 

dose.c <- tss %>%
  filter(WRIA_ID == 16)
# %>%
#   # unite(coords, lat, lon, sep = ", ") %>%
#   mutate(coords = list(lat,lon))
bedi.c <- as.data.frame(bedi.c)
dose.c <- as.data.frame(dose.c)
# bedi.c$coords <- list(bedi.c$lat, bedi.c$lon)

coords <- cbind(bedi.c$lat,bedi.c$lon)
doseco <- cbind(dose.c$lat,dose.c$lon)
# avector <- aframe[['a2']]

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
dps <- distal_points(coords,10)
# dps.p <- Polygon(dps)

dps.df <- as.data.frame(dps) %>%
  rename(lat = V1, lon = V2) %>%
  distinct(lat,lon)
# http://stackoverflow.com/questions/33718004/drawing-a-non-self-intersecting-polygon-with-r
# IMPORTANT: make non-intersecting!
bedi.NI.poly <- dps.df[chull(dps.df),]

dose.df <- as.data.frame(distal_points(doseco,10)) %>%
  rename(lat = V1, lon = V2) %>%
  distinct(lat,lon)
dose.NI.poly <- dose.df[chull(dose.df),]

m4 <- leaflet() %>% 
  setView(lng = -122.996823, lat = 47.65, zoom = 9) %>%
  addProviderTiles("Stamen.Terrain") %>%
  addPolygons(data = bedi.NI.poly, lng = ~lon, lat = ~lat) %>%
  addPolygons(data = dose.NI.poly, lng = ~lon, lat = ~lat)
m4

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

bedi <- get_study_poly('BEDI0004', tss)
m5 <- leaflet() %>% 
  setView(lng = -122.996823, lat = 47.65, zoom = 9) %>%
  addProviderTiles("Stamen.Terrain") %>%
  addPolygons(data = bedi, lng = ~lon, lat = ~lat)
m5


get_study_poly2 <- function(studyid){
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
# https://stat.ethz.ch/pipermail/r-help/2007-February/125569.html
tss <- as.data.frame(tss)
# create list of all coords
all.coords <- cbind(tss$lat,tss$lon)
# adds ALL COORDS as list to each row 
# tss$coords <- list(all.coords)


# result <- lapply(tss, cbind(tss$lat,tss$lon))
study.coords <- as.data.frame(tss) %>%
  group_by(Study_ID) %>%
  distinct(lat,lon) %>%
  do(polycoords = distal_points(cbind(.$lat,.$lon),10)) 

study.coords2 <- as.data.frame(tss) %>%
  group_by(Study_ID) %>%
  distinct(lat,lon) %>%
  do(coordslist = cbind(.$lat,.$lon))

do(study.coords2, coordslist = distal_points(study.coords2$coordslist,10))
# use distal_points helper function to obtain only the n most distal points
dps <- distal_points(coords,10)
# remove duplicates and rename columns for ease of use with leaflet
dps.df <- as.data.frame(dps) %>%
  rename(lat = V1, lon = V2) %>%
  distinct(lat,lon)
# use chull function to make a non-intersecting polygon
study_poly <- dps.df[chull(dps.df),]
