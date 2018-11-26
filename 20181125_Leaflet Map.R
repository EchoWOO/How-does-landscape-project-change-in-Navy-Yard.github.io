library(raster)
library(tidyverse)
library(sf)
library(leaflet)
library(ggmap)
library(mapview)
library(raster)
library(magrittr)
# https://rstudio.github.io/leaflet/colors.html
# https://leaflet-extras.github.io/leaflet-providers/preview/
# https://www.r-graph-gallery.com/4-tricks-for-working-with-r-leaflet-and-shiny/

r <- raster("~/Box/Independent study/Tiff/PHL_NDVI_CALC/Landsat7Phil2018_8.tif")
NY_Bound <- st_read("~/Box/Independent study/shapefile/NavyYard/NYLeftBoundary.shp")
cr <- '+proj=longlat +datum=WGS84'
NY_Bound_pro <- st_transform(NY_Bound, cr)

pal <- colorNumeric(c("#FFFFCC", "#41B6C4", "#0C2C84"), values(r),
                    na.color = "transparent")

FILE1 <- data.frame('lat' = c(51.31, 51.52, 51.53), 
                    'lon' = c(0.06, 0.11, 0.09))
FILE2 <- data.frame('lat' = c(52.20, 52.25, 52.21), 
                    'lon' = c(0.12, 0.12, 0.12))

# map1 <- leaflet(FILE1)%>%
#   addTiles()%>%
#   addMarkers(clusterOptions = markerClusterOptions())
# 
# map2 <- leaflet(FILE2)%>%
#   addTiles()%>%
#   addMarkers(clusterOptions = markerClusterOptions())

NDVIMap <- leaflet(FILE1) %>% 
  setView(lng = -75.161147, lat = 39.956229, zoom = 11) %>% 
  addProviderTiles(providers$CartoDB.Positron, group = "Light Background") %>%
  addPolygons(data = NY_Bound_pro, color = "white", 
              fill= NA, weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              highlightOptions = highlightOptions(color = "#0d585f", weight = 2,
                                                  bringToFront = TRUE),
              # popupOptions = popupOptions(color = "#0d585f", weight = 4,
              #                                 bringToFront = TRUE),
              group = "Navy Yard") %>% 
  addRasterImage(r, colors = pal, opacity = 0.9, group = "NDVI") %>%
  addLegend(pal = pal, values = values(r),
            title = "NDVI value",
            opacity = 0.8)

BaseMap <- leaflet(FILE2) %>% 
  setView(lng = -75.161147, lat = 39.956229, zoom = 11) %>% 
  addProviderTiles(providers$Esri.WorldImagery, group = "satellite Map") %>% 
  addPolygons(data = NY_Bound_pro, color = "white", 
              fill= NA, weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              highlightOptions = highlightOptions(color = "#0d585f", weight = 2,
                                                  bringToFront = TRUE),
              group = "Navy Yard")


NDVIMap
sync(NDVIMap, BaseMap)
