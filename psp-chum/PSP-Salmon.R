## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----libraries-----------------------------------------------------------
library(tidyverse) # because it's the tidyverse!
library(rgdal) # to import HYDRO shapefiles into R
library(geojsonio) # to convert spatial data to GeoJson
library(spdplyr) # to manipulate attributes of spatial data
library(magrittr) # for lovely magrittr piping
library(leaflet) # for mapping
library(ggplot2) # for plotting
library(forcats) # for factor revalue with tidyverse

## ----load spatial data---------------------------------------------------
# loading in spatial data, using readOGR - WORKS!!!
prj_hydro.mp <- readOGR(dsn=path.expand("./Hood_Canal_Prj_Area_NEW"), layer="WBD_PRJ_HYDRO")


## ----see spatial data----------------------------------------------------
# NOT RUN
# plot(prj_hydro.mp)

## ----filter spatial data-------------------------------------------------
# read in CSV of chum measurement locations that I got
# manually from using MapShaper
chumPermCodes <- read.csv("hoodCanalChumSiteReachCodes.csv",
                           header=TRUE)
# build vector of Permanent IDs to filter on
wantedChumSites <-
  unlist(unique(chumPermCodes["Permanent"]))
# use wantedChumSites list to filter
chum_sites.mp <- prj_hydro.mp %>%
  filter(Permanent_ %in% wantedChumSites)

## ----alter attributes----------------------------------------------------
# reset the rownames
rownames(chum_sites.mp@data) <- 1:nrow(chum_sites.mp@data)

# pare down chumReachCodes data
keeps <-c("River.site", "Description", "Permanent")
chumSiteInfo <- chumPermCodes[keeps]


## ----convert spatial to GeoJSON------------------------------------------
# convert the projection to lat long
chumsites <- spTransform(chum_sites.mp, CRS("+init=epsg:4326"))

# retain just the final coord pair for each polyline
for (i in seq_along(chumsites@lines)){
  chumsites@lines[[i]]@Lines[[1]]@coords =
    tail(chumsites@lines[[i]]@Lines[[1]]@coords, n=1)
}

# convert from SpatialLinesDF to SpatialPointsDF
ptchumsites = as(chumsites, "SpatialPointsDataFrame")

# convert to geojson using geojsonio
chumsite_json <- geojson_json(ptchumsites)

# saving out geojson for fault tolerance
geojson_write(chumsite_json, 
              file = "chumsite.json")

## ----map GeoJSON in leaflet----------------------------------------------
# read in GeoJSON as sp object
chum_to_map <- geojsonio::geojson_read("chumsite.json",
  what = "sp")

## ------------------------------------------------------------------------
# convert to plain ol data frame for easy combining
chumsite_df <- as.data.frame(chum_to_map)
names(chumsite_df)[names(chumsite_df) == "coords.x1"] <- "lng"
names(chumsite_df)[names(chumsite_df) == "coords.x2"] <- "lat"
names(chumsite_df)[names(chumsite_df) == "Permanent_"] <- "Permanent"
names(chumsite_df)[names(chumsite_df) == "River.site"] <- "Site"

# merge with the chumsite info
chumtwo <- merge(chumsite_df,chumSiteInfo,by="Permanent")

# retool data so names match those in chumdata
chumtwo <- chumtwo %>% 
  mutate(Site = fct_recode(Site,
                                 "Anderson Creek - Natural" = "1. Anderson Creek Summer Chum: Natural-Origin Spawners (Prop)",
                                 "Big Beef Creek - Natural" = "2. Big Beef Creek Summer Chum: Natural-Origin Spawners (Prop)",
                                 "Big Beef Creek - Hatch" = "3. Big Beef Creek Summer Chum: Hatch-Origin Spawners (Prop.)",
                                 "Big/Little Quilcene - Escapement" = "4. Big/Little Quilcene Summer Chum: Escapement (Proportion)",
                                 "Big Quilcene River - Hatch" = "5. Big/Little Quilcene Summer Chum: Index Hatchery Escapement",
     "Little Quilcene River - Hatch" = "6. Big/Little Quilcene Summer Chum: Index Hatchery Escapement",
     "Big Quilcene River - Natural" = "7. Big/Little Quilcene Summer Chum: Index Natural Escapement",
     "Little Quilcene River - Natural" = "8. Big/Little Quilcene Summer Chum: Index Natural Escapement",
     "Dewatto River - Hatch" = "9. Dewatto Summer Chum: Hatch-Origin Spawners (Prop.)",
     "Dewatto River - Natural" = "10. Dewatto Summer Chum: Natural-Origin Spawners (Prop)",
     "Dosewallips River - Hatch" = "11. Dosewallips Summer Chum: Hatch-Origin Spawners (Prop.)",
     "Dosewallips River - Natural" = "12. Dosewallips Summer Chum: Natural-Origin Spawners (Prop)",
     "Duckabush River - Hatch" = "13. Duckabush Summer Chum: Hatch-Origin Spawners (Prop.)",
     "Duckabush River - Natural" = "14. Duckabush Summer Chum: Natural-Origin Spawners (Prop)",
     "Finch Creek - Escapement" = "15. Finch Creek Summer Chum: Escapement (Proportion)",
     "Hamma Hamma River - Hatch" = "16. Hamma Hamma Summer Chum: Hatch-Origin Spawners (Prop.)",
     "Hamma Hamma River - Natural" = "17. Hamma Hamma Summer Chum: Natural-Origin Spawners (Prop)",
     "Lilliwaup Creek - Hatch" = "18. Lilliwaup Creek Summer Chum: Hatch-Origin Spawners (Prop.)",
     "Lilliwaup Creek - Natural" = "19. Lilliwaup Creek Summer Chum: Natural-Origin Spawners (Prop)",
     "Skokomish River - Hatch" = "20. Skokomish Summer Chum: Hatch-Origin Spawners (Prop.)",
     "Skokomish River - Natural" = "21. Skokomish Summer Chum: Natural-Origin Spawners (Prop)",
     "Tahuya River - Hatch" = "22. Tahuya Summer Chum: Hatch-Origin Spawners (Prop.)",
     "Tahuya River - Natural" = "23. Tahuya Summer Chum: Natural-Origin Spawners (Prop)",
     "Union River - Hatch" = "24. Union Summer Chum: Index Hatchery Escapement",
     "Union River - Natural" = "25. Union Summer Chum: Index Natural Escapement"))

# create label for chum measurement sites
chumlabel <- sprintf("Chum Site: %s <br><br>Measurement: %s <br><br>Description: %s",
                     chumtwo$GNIS_Name, 
                     chumtwo$Site,
                     chumtwo$Description)


## ------------------------------------------------------------------------
# because we have points at the same lat/long, I'm going to jitter that data only for the visualization

chumtwo$jitterlng <- as.numeric(jitter(chumtwo$lng, factor = 10))
chumtwo$jitterlat <- as.numeric(jitter(chumtwo$lat, factor = 10))

leaflet(chumtwo) %>%
  setView(lng = -123, lat = 47.65, zoom = 9) %>%
  addProviderTiles("Esri.WorldImagery") %>% 
  addCircleMarkers(lng = ~jitterlng, 
                   lat = ~jitterlat,
                   color = "#42d4f4",
                   popup = chumlabel)

## ----data wrangle and make plots-----------------------------------------
# load in the yearly data
chumSiteData <- as.data.frame(read.csv("hoodCanalSummerChumStats.csv", header = TRUE))

# rename all of the yearly data columns
newnames <- c("Year",
              "Anderson Creek - Natural",
              "Big Beef Creek - Natural",
              "Big Beef Creek - Hatch",
              "Big/Little Quilcene - Escapement",
              "Big Quilcene River - Hatch",
              "Little Quilcene River - Hatch",
              "Big Quilcene River - Natural",
              "Little Quilcene River - Natural",
              "Dewatto River - Hatch",
              "Dewatto River - Natural",
              "Dosewallips River - Hatch",
              "Dosewallips River - Natural",
              "Duckabush River - Hatch",
              "Duckabush River - Natural",
              "Finch Creek - Escapement",
              "Hamma Hamma River - Hatch",
              "Hamma Hamma River - Natural",
              "Lilliwaup Creek - Hatch",
              "Lilliwaup Creek - Natural",
              "Skokomish River - Hatch",
              "Skokomish River - Natural",
              "Tahuya River - Hatch",
              "Tahuya River - Natural",
              "Union River - Hatch",
              "Union River - Natural")

colnames(chumSiteData) <- newnames

# tidying the data
tidychum <- gather(chumSiteData, key, value, -Year)
# making column names simple
colnames(tidychum) <- c("year", "site", "count")
# change count data to numeric from char
tidychum$count <- as.numeric(gsub(",","",tidychum$count))
# dealing with N/A values
tidychum <- na.omit(tidychum)
# plot all the lines
ggplot(tidychum,
       aes(x=year,
           y=count,
           color=site)) +
  geom_line()


