#-----------------------------------------------------------------------------------------------------------------------
#' mapping program to visualize our sweet hike in patagonia! because i have infinite free time.
#' 
#' author:          catherine
#' date created:    25 oct 2018
#' 
#' credit: heavily inspired by http://mhermans.net/hiking-gpx-r-leaflet.html
#-----------------------------------------------------------------------------------------------------------------------

library(leaflet)
library(rgdal)
library(lubridate)
library(sp)
library(ggplot2)
library(tidyr)
library(colorout)
library(htmlwidgets)

write_popup <- function(popup_data, keys_of_interest = NULL) {
  if (is.null(keys_of_interest)) {
    keys_of_interest <- list("name", "company", "price")
  }

  text <- NULL
  for (key in keys_of_interest) {
    if (key %in% names(popup_data)) {
      text <- paste0(text, key, ": ", popup_data[[key]], " <br> ")
    }
  }

  if (is.null(text)) {
    text <- "no data"
  } else {
    text <- substr(text, 1, nchar(text) - 5)
  }

  return(text)
}

setwd("~/Documents/patagonia")

data_gpx <- "torres-del-paine-circuit.gpx"
# elevation <- readOGR(data_gpx, layer = "track_points", verbose = FALSE)
# head(elevation)

trace <- readOGR(data_gpx, layer = "tracks", verbose = FALSE)
head(trace)

#camps <- list(
  italiano =  list(name = "C Italiano",
                   day = "28th",
                   price = "free!",
                   lat = 51.0223439,
                   lng = 73.0424020),
  chileno =   list(name = "R Chileno",
                   day = "29th",
                   price = "$20",
                   lat = 50.9571205,
                   lng = 72.9107867),
  seron =     list(name = "C Seron",
                   day = "30th",
                   price = "$20",
                   lat = 50.8659546,
                   lng = 72.8932760),
  dickson =   list(name = "R Dickson",
                   day = "31st",
                   price = "$8",
                   lat = 50.8786012,
                   lng = 73.0776751),
  perros =    list(name = "C Perros",
                   day = "31st",
                   price = "$8",
                   lat = 50.9322899,
                   lng = 73.1344521),
  paso =      list(name = "C Paso",
                   day = "1st",
                   price = "free!",
                   lat = 50.9580205,
                   lng = 73.2000244))

camps <- read.csv("camps.csv", stringsAsFactors = FALSE)
rownames(camps) <- camps$name
camps

map <- leaflet() %>%

  # add tiles
  addProviderTiles("OpenStreetMap.Mapnik", group = "Basic") %>%
  addProviderTiles("OpenTopoMap", group = "Topographical") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%

  addLegend(position = "bottomright", opacity = 0.4,
            colors = "red",
            labels = "O-Circuit",
            title = "Torres del Paine") %>%

  # layer control
  addLayersControl(position = "bottomright",
    baseGroups = c("Basic", "Topographical", "Satellite"),
    overlayGroups = c("Hiking Route", "Campsites"),
    options = layersControlOptions(collapsed = FALSE)) %>%

  addPolylines(data = trace, color = "red", group = "Hiking Route") %>%

  addMeasure()

for (camp in camps$name) {
  map <- addAwesomeMarkers(map,
                    data = camps,
                    lng = camps[camp, "lng"],
                    lat = camps[camp, "lat"],
                    popup = write_popup(camps[camp, ]),
                    icon = makeAwesomeIcon(library = "glyphicon",
                                           icon = "tent",
                                           iconColor = "white",
                                           markerColor = "red"),
                    group = "Campsites")
}

map

saveWidget(map, file = "map.html", selfcontained = FALSE)
