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
library(colorout)
library(htmlwidgets)

write_popup <- function(popup_data, keys_of_interest = NULL) {
  if (is.null(keys_of_interest)) {
    keys_of_interest <- list("name", "company", "price", "closes")
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
    text <- substr(text, 1, nchar(text) - 5)  # remove the last <br>
  }

  return(text)
}


setwd("~/Documents/patagonia/mapping/data")

data_gpx <- "torres-del-paine-circuit.gpx"
# elevation <- readOGR(data_gpx, layer = "track_points", verbose = FALSE)
# head(elevation)

trace <- readOGR(data_gpx, layer = "tracks", verbose = FALSE)
head(trace)

camps <- read.csv("camps.csv", stringsAsFactors = FALSE, na.strings = c("", "NA"))
rownames(camps) <- camps$name
camps


html_legend <- "<span class='glyphicon glyphicon-minus' aria-hidden='true' style='color:red'></span>
                  Trail<br/>
                <span class='glyphicon glyphicon-tent' aria-hidden='true' style='color:red'></span>
                  Evening Closure<br/>
                <span class='glyphicon glyphicon-tent' aria-hidden='true' style='color:orange'></span>
                  Afernoon Closure<br/>
                <span class='glyphicon glyphicon-tent' aria-hidden='true' style='color:gray'></span>
                  Unkown Closure"


map <- leaflet() %>%
  # add tiles
  addProviderTiles("OpenStreetMap.Mapnik", group = "Basic") %>%
  addProviderTiles("OpenTopoMap", group = "Topographical") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%

  # layer control
  addLayersControl(position = "bottomright",
    baseGroups = c("Basic", "Topographical", "Satellite"),
    overlayGroups = c("Trail", "Campsites"),
    options = layersControlOptions(collapsed = FALSE)) %>%

  addPolylines(data = trace, color = "red", group = "Trail") %>%

  addMeasure() %>%

  addControl(html = html_legend, position = "bottomleft")

for (camp in camps$name) {
  if (is.na(camps[camp, "closes"])) {
    color <- "gray"
  } else if (hm(camps[camp, "closes"]) > hm("16:30")) {
    color <- "red"
  } else {
    color <- "orange"
  }

  map <- addAwesomeMarkers(map,
                    data = camps,
                    lng = camps[camp, "lng"],
                    lat = camps[camp, "lat"],
                    popup = write_popup(camps[camp, ]),
                    icon = makeAwesomeIcon(icon = "tent",
                                           markerColor = color),
                    group = "Campsites")
}

map

setwd("..")
saveWidget(map, file = "index.html", selfcontained = FALSE)
