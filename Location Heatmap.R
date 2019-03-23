# Load packages for generating interactive heat map using Google Location History data 
library(tidyverse)
library(jsonlite)
library(lubridate)
library(leaflet)
library(leaflet.extras)

# Preapare data for mapping 
locationdata <- fromJSON("Location History.json")

myData <- locationdata$locations %>%
  select(latitudeE7, longitudeE7, `timestampMs`, velocity)

myDataClean <- myData %>%
  mutate(lat = latitudeE7 / 1E7, lon = longitudeE7 / 1E7) %>%
  mutate(timestampMs = as.numeric(timestampMs)) %>%
  mutate(Date = as.POSIXct(timestampMs / 1000, origin = "1970-01-01"))

myDataClean <- myDataClean %>%
  mutate(image = case_when(velocity > 10 ~ "drive",
                           TRUE ~ "stand")) %>%
  mutate(image = factor(image, levels = c("drive", "stand")))

newIcons <- iconList(
  stand = makeIcon("stand.png", "stand.png", 36, 36),
  drive = makeIcon("drive.png", "drive.png", 36, 36)
)

# Generating interactive heatmap using location data
myGoogleDataMap <- myDataClean %>%
  leaflet() %>%
  addTiles(group = "OSM") %>%
  addProviderTiles("CartoDB", group = "Carto") %>%
  addProviderTiles("Esri", group = "Esri") %>%
  setView(lat = 50, lng = 5, zoom = 4) %>%
  addResetMapButton() %>%
  fitBounds( ~ min(lon), ~ min(lat), ~ max(lon), ~ max(lat)) %>%
  addHeatmap(lng = ~ lon,lat = ~ lat, group = "Heat Map", blur = 20, max = 0.01, radius = 15) %>%
  addMarkers(data = head(myDataClean, 50000), ~ lon, ~ lat,icon = ~ newIcons[image],
             clusterOptions = markerClusterOptions(), 
             label = ~ format(Date, format = "%H:%M %d-%b-%Y"), group = "Points") %>%
  addLayersControl( baseGroups = c("OSM", "Carto", "Esri"),
    overlayGroups = c("Heat Map", "Points"),
    options = layersControlOptions(collapsed = FALSE)
  )

myGoogleDataMap

# Save map
library(htmlwidgets)
saveWidget(myGoogleDataMap, file = "myMap.html")



