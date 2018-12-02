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

r2018 <- raster("~/Box/Independent study/Tiff/PHL_NDVI_CALC/Landsat7Phil2018_8.tif")
r2015 <- raster("~/Box/Independent study/Tiff/PHL_NDVI_CALC/Landsat7Phil2015_8.tif")
r2012 <- raster("~/Box/Independent study/Tiff/PHL_NDVI_CALC/Landsat7Phil2012_8.tif")
r2009 <- raster("~/Box/Independent study/Tiff/PHL_NDVI_CALC/Landsat7Phil2009_8.tif")
r2006 <- raster("~/Box/Independent study/Tiff/PHL_NDVI_CALC/Landsat7Phil2006_8.tif")
r2003 <- raster("~/Box/Independent study/Tiff/PHL_NDVI_CALC/Landsat7Phil2003_8.tif")
r2000 <- raster("~/Box/Independent study/Tiff/PHL_NDVI_CALC/Landsat7Phil2000_8.tif")
NY_Bound <- st_read("~/Box/Independent study/shapefile/NavyYard/NYLeftBoundary.shp")
NY_Parks <- st_read("~/Box/Independent study/shapefile/NavyYard/NY_ParkBoundaries.shp")
cr <- '+proj=longlat +datum=WGS84'
NY_Bound_pro <- st_transform(NY_Bound, cr)
NY_Parks_pro <- st_transform(NY_Parks, cr)

pal <- colorNumeric(c("#FFFFCC", "#41B6C4", "#0C2C84"), values(r),
                    na.color = "transparent")

NDVIMap <- leaflet(height = 800) %>% 
  setView(lng = -75.171571, lat = 39.918365, zoom = 12) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = NY_Bound_pro, color = "white", 
              fill= NA, weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              highlightOptions = highlightOptions(color = "#0d585f", weight = 2,
                                                  bringToFront = TRUE),
              # popupOptions = popupOptions(color = "#0d585f", weight = 4,
              #                                 bringToFront = TRUE),
              group = "Navy Yard Boundary") %>% 
  addPolygons(data = NY_Parks_pro, color = "white", 
              fill= NA, weight = 0.7, smoothFactor = 0.5,
              opacity = 0.8, fillOpacity = 0.5,
              highlightOptions = highlightOptions(color = "#0d585f", weight = 1.4,
                                                  bringToFront = TRUE),
              # popupOptions = popupOptions(color = "#0d585f", weight = 4,
              #                                 bringToFront = TRUE),
              group = "Navy Yard Parks") %>% 
  addRasterImage(r2018, colors = pal, opacity = 1.0, group = "NDVI 2018", layerId = "values") %>%
  addMouseCoordinates() %>%
  addImageQuery(r, type="mousemove", layerId = "values") %>% 
  addRasterImage(r2015, colors = pal, opacity = 1.0, group = "NDVI 2015") %>%
  addRasterImage(r2012, colors = pal, opacity = 1.0, group = "NDVI 2012") %>%
  addRasterImage(r2009, colors = pal, opacity = 1.0, group = "NDVI 2009") %>%
  addRasterImage(r2009, colors = pal, opacity = 1.0, group = "NDVI 2006") %>%
  addRasterImage(r2009, colors = pal, opacity = 1.0, group = "NDVI 2003") %>%
  addRasterImage(r2009, colors = pal, opacity = 1.0, group = "NDVI 2000") %>%
  leaflet::addLayersControl(
    overlayGroups = c("Navy Yard Boundary", "Navy Yard Parks", "NDVI 2018", "NDVI 2015",
                      "NDVI 2012", "NDVI 2009", "NDVI 2006",
                      "NDVI 2003", "NDVI 2000"),  # add these layers
    options = layersControlOptions(collapsed = FALSE)  # expand on hover?
  ) %>% 
  hideGroup(c("NDVI 2015","NDVI 2012", "NDVI 2009", "NDVI 2006",
              "NDVI 2003", "NDVI 2000")) %>%  # turn these off by default
  addLegend(pal = pal, values = values(r2018),
            title = "NDVI value",
            opacity = 0.8)

BaseMap <- leaflet(height = 800) %>% 
  setView(lng = -75.171571, lat = 39.918365, zoom = 12) %>% 
  addProviderTiles(providers$Esri.WorldImagery, group = "satellite Map") %>% 
  addPolygons(data = NY_Bound_pro, color = "white", 
              fill= NA, weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              highlightOptions = highlightOptions(color = "#0d585f", weight = 2,
                                                  bringToFront = TRUE),
              group = "Navy Yard") %>% 
  addPolygons(data = NY_Parks_pro, color = "white", 
              fill= NA, weight = 0.7, smoothFactor = 0.5,
              opacity = 0.8, fillOpacity = 0.5,
              highlightOptions = highlightOptions(color = "#0d585f", weight = 1.4,
                                                  bringToFront = TRUE),
              # popupOptions = popupOptions(color = "#0d585f", weight = 4,
              #                                 bringToFront = TRUE),
              group = "Navy Yard Parks")


# NDVIMap
sync(NDVIMap, BaseMap)
