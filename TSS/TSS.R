library(tidyverse)
library(ggmap)
library(leaflet)
library(sp)

# load the tss data 
tss <- read.csv('./data/EIMResults.csv', header = TRUE)

# # create a data frame tbl for easy printing
# tss.tib <- tbl_df(tss)
# tss.tib

# select only variables of interest, rename lengthy variable names
tss <- tbl_df(tss) %>%
  select(Study_ID, Study_Name, Location_ID, Location_Name, 
         Field_Collection_Start_Date, Field_Collection_End_Date,
         Result_Value, Calculated_Latitude_Decimal_Degrees_NAD83HARN,
          Calculated_Longitude_Decimal_Degrees_NAD83HARN) %>%
  rename(start_date = Field_Collection_Start_Date, end_date = Field_Collection_End_Date,
         tss_mgL = Result_Value,
         lat = Calculated_Latitude_Decimal_Degrees_NAD83HARN,
         lon = Calculated_Longitude_Decimal_Degrees_NAD83HARN) %>%
  mutate(start_date = as.Date(start_date, format = "%m/%d/%Y"),
         end_date = as.Date(end_date, format = "%m/%d/%Y"),
         logtss = log10(tss_mgL))
  # %>%
  # # mutate(duration = end_date - start_date) %>%
  # spread(key = Result_Parameter_Name, value = Result_Value)
dim(tss)
# distinct(tss, Result_Parameter_Name)
# print out information on the variables it contains
summary(tss)
tss
# load location data file
locations_raw <- read.csv('./data/EIMLocationDetails.csv', header = TRUE)
# head(locations)

# select only ID and WRIA
locs_select <- tbl_df(locations_raw) %>%
  select(Location_ID, Watershed_WRIA)
# unique(locs$Watershed_WRIA)
# join wria to main data file
tss_wria_name <- tss %>%
  left_join(locs_select, by = "Location_ID")
# head(tss_wria)
# View(tss_wria)

# label wria name with wria number
tss_wria <- tss_wria_name %>% 
  mutate(WRIA_ID = ifelse(Watershed_WRIA == "Kitsap", 15, 
                     ifelse(Watershed_WRIA == "Kennedy-Goldsborough", 14,
                            ifelse(Watershed_WRIA == "Skokomish-Dosewallips", 16,
                                   ifelse(Watershed_WRIA == "Quilcence-Snow", 17,
                                          ifelse(Watershed_WRIA == "Elwah-Dungeness", 18,
                                                 NA))))))
# sm <- tss %>%
#   filter(Watershed_WRIA == "Skokomish-Dosewallips")
# dim(sm)
# 
# tss_wria %>%
#   group_by(Study_ID, Location_ID, start_date)

################################ mapping ################################################ 
# tss_wria$loc=paste(tss_wria$lat, tss_wria$lon, sep=":") ## create a lat:long location variable
# Geo <- gvisGeoMap(tss_wria, locationvar='loc', numvar="tss_mgL", 
#                   options=list(height=400, dataMode='markers'))
# plot(Geo)
# 
# G1 <- gvisGeoChart(tss_wria, locationvar='loc', numvar='tss_mgL') 
# 
# plot(G1)


m <- leaflet(data = tss_wria) %>% 
  setView(lng = -122.996823, lat = 47.65, zoom = 9)

waterIcon <- makeIcon(
  iconUrl = 'icons/water-15.svg',
  iconWidth = 25, iconHeight = 25
)

content1 <- paste(sep = ": ",
                  "<b>TSS</b>",
                  tss_wria$tss_mgL
)

content2 <- paste(sep = ": ",
                  "<b>Project Name</b>",
                  tss_wria$Study_Name
)
content3 <- paste(sep = ": ",
                  "<b>Date</b>",
                  tss_wria$start_date
)

# content_items = c(content1,content2,content3)
# full_content <- paste(sep = "<br>", content2,content1)
# full_content <- paste(collapse = "<br>", content_items)
full_content <- sprintf("Project Name: %s <br>Date: %s <br> WRIA: %s <br> TSS mg/L: %s", 
                        tss_wria$Study_Name, tss_wria$start_date, 
                        tss_wria$Watershed_WRIA, tss_wria$tss_mgL)

m %>%
  addTiles() %>%
  addMarkers(~lon, ~lat, popup = full_content,
             icon = waterIcon,
             clusterOptions = markerClusterOptions())


qpal <- colorQuantile("BuPu", tss_wria$tss_mgL, n = 5)
m %>%
  addProviderTiles("Stamen.Terrain", group = "Terrain") %>%
  addCircleMarkers(~lon, ~lat, popup = full_content,
                   radius = 10,
                   color = ~qpal(tss_mgL),
                   stroke = FALSE, fillOpacity = 0.5,
                   group = "Water Quality") %>%
  addLegend(pal = qpal, values = ~tss_mgL, opacity = 1) %>%
  addLayersControl(
    baseGroups = "Terrain",
    overlayGroups = "Water Quality",
    options = layersControlOptions(collapsed = FALSE)
  )

factpal <- colorFactor("Dark2", tss_wria$Watershed_WRIA)
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

all_safe_tss <- tss_wria %>%
  mutate(safety = as.factor(ifelse(logtss >= 1.90309, "unsafe", "safe")))

safe_tss <- filter(all_safe_tss, safety == 'safe')

unsafe_tss <- filter(all_safe_tss, safety == 'unsafe')

sfpal <- colorFactor("Set1", safe_tss$safety)

m2 <- leaflet(data = safe_tss) %>% 
  setView(lng = -122.996823, lat = 47.65, zoom = 9)

m2 %>%
  addProviderTiles("Stamen.Terrain", group = "Terrain") %>%
  addCircleMarkers(data = safe_tss, ~lon, ~lat, popup = full_content,
                   radius = 8,
                   color = "green",
                   stroke = FALSE, fillOpacity = 0.4,
                   group = "Safe TSS (TSS < 80 mg/L)") %>%
  addCircleMarkers(data = unsafe_tss, ~lon, ~lat, popup = full_content,
                   radius = 8,
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
################################ mapping ################################################ 

distinct(tss, Study_ID)
# 51 distinct studies
distinct(tss, Location_ID)
# 442 distinct locations

by_study <- group_by(tss_wria, Study_ID)

distinct(by_study, Location_ID)  

################################ EDA plots ################################################ 
# plot a histogram of tss values
tss %>%
  filter(0 < tss_mgL & tss_mgL < 100) %>%
  ggplot(aes(tss_mgL)) +
  geom_histogram(binwidth = 0.5)

# plot mean tss by study 
tss %>%
  group_by(Study_ID) %>%
  summarise(m.tss = mean(tss_mgL)) %>%
  ggplot(aes(x = reorder(Study_ID, m.tss), y = m.tss)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# plot mean tss by location
tss %>%
  group_by(Location_ID) %>%
  summarise(m.tss = mean(tss_mgL)) %>%
  ggplot(aes(x = reorder(Location_ID, m.tss), y = m.tss)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# plot mean tss by location
tss %>%
  group_by(Study_ID) %>%
  summarise(m.tss = mean(tss_mgL),
            last_day = max(end_date),
            first_day = min(start_date),
            change = ) %>%
  ggplot(aes(x = Study_ID, y = m.tss)) +
  geom_bar(stat="identity", fill = Location_ID) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# plot mean tss by Watershed_WRIA
tss_wria %>%
  group_by(Watershed_WRIA) %>%
  summarise(m.tss = mean(tss_mgL)) %>%
  ggplot(aes(x = reorder(Watershed_WRIA, m.tss), y = m.tss)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# plot tss by date and Watershed_WRIA
# http://www.pugetsoundnearshore.org/supporting_documents/wria14_lfa.pdf
# The U.S. Fish and Wildlife Service recommends a maximum TSS level of 80
# mg/L to protect salmonid fish (Fish and Wildlife Service 1995). 
ggplot(data = tss_wria, mapping = aes(x = start_date, y = logtss)) + 
  geom_point() + 
  geom_smooth() +
  geom_hline(yintercept = 1.90309, color = "red", show.legend = TRUE) +
  facet_wrap(~ Watershed_WRIA)

# create good/bad tss categories
tss_wria %>%
  mutate(safety = ifelse(logtss >= 1.90309, "unsafe", "safe")) %>%
  ggplot(mapping = aes(x = start_date, y = logtss)) + 
  geom_point(mapping = aes(color = safety)) + 
  scale_color_brewer(palette="Dark2") +
  geom_smooth() +
  geom_hline(yintercept = 1.90309, color = "black", show.legend = TRUE) +
  facet_wrap(~ Watershed_WRIA)


ggplot(data = tss_wria) + 
  geom_point(mapping = aes(x = start_date, y = logtss)) + 
  facet_wrap(~ Study_ID)

# funding project data is from 2007-2015 ONLY
tss_wria %>%
  filter(format(start_date,"%Y") >= 2005) %>%
ggplot() +
  geom_point(mapping = aes(x = start_date, y = logtss)) +
  facet_wrap(~ Watershed_WRIA)

# funding project data is from 2007-2015 ONLY
tss_wria %>%
  filter(format(start_date,"%Y") >= 2007) %>%
  ggplot() +
  geom_point(mapping = aes(x = start_date, y = logtss)) +
  facet_wrap(~ Location_ID)

quil <- tss_wria %>%
  filter(WRIA_ID == 17) %>%
  mutate(lat = as.numeric(lat), lon = as.numeric(lon)) %>%
  select(lat,lon)
################################ EDA plots ################################################ 

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
# coordinates(tss_wria) <- c("lon", "lat")
# L <- tss_wria$Study_ID == 'BEDI0004'
# tss_wria[L,]
bedi <- tss_wria %>%
  filter(Study_ID == 'BEDI0004') %>%
  mutate(lat = as.numeric(lat), lon = as.numeric(lon)) %>%
  select(lat,lon)
# bedi.p <- Polygon(bedi)
# bedi.polys <- Polygons(list(dose.p), "bedi")

# creates SpatialPoints
# coordinates(bedi) <- c("lon", "lat")
  
dose <- tss_wria %>%
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

bedi.c <- tss_wria %>%
  filter(Study_ID == 'BEDI0004') 

dose.c <- tss_wria %>%
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

bedi <- get_study_poly('BEDI0004', tss_wria)
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
tss_wria <- as.data.frame(tss_wria)
# create list of all coords
all.coords <- cbind(tss_wria$lat,tss_wria$lon)
# adds ALL COORDS as list to each row 
# tss_wria$coords <- list(all.coords)


# result <- lapply(tss_wria, cbind(tss_wria$lat,tss_wria$lon))
study.coords <- as.data.frame(tss_wria) %>%
  group_by(Study_ID) %>%
  distinct(lat,lon) %>%
  do(polycoords = distal_points(cbind(.$lat,.$lon),10)) 

study.coords2 <- as.data.frame(tss_wria) %>%
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
