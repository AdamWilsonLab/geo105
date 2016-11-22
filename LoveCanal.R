## Love Canal

library(leaflet)
library(ggplot2)
library(ggmap)
library(dplyr)
library(htmlwidgets)

lc=geocode("Love Canal, NY")
ub=geocode("University at Buffalo North Campus, Amherst, NY")

m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=lc$lon, lat=lc$lat, popup="The Love Canal") %>%
  addMarkers(lng=ub$lon, lat=ub$lat, popup="University at Buffalo")%>%
  setView(lng=ub$lon, lat=ub$lat+.1,10)
m  # Print the map

saveWidget(m, file="LoveCanal.html")
