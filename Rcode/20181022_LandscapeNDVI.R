setwd("~/Box/Independent study/Tiff/NavyYard")

# ----------------------------------- load library and define plottheme ----------------------------------
library(sf)
library(tidyverse)
library(raster)
library(plyr)
library(reshape2)
library(caret)
library(wesanderson)
library(gdata)

plotTheme <- function(base_size = 24) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 18,colour = "black"),
    plot.subtitle = element_text(face="italic"),
    plot.caption = element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major.y = element_line("grey80", size = 0.1),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey80", color = "white"),
    strip.text = element_text(size=12),
    axis.title = element_text(size=8),
    axis.text = element_text(size=8),
    axis.title.y = element_text(size=12),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(colour = "black", face = "italic"),
    legend.text = element_text(colour = "black", face = "italic"),
    axis.ticks.y = element_line(color="grey70")
    # axis.text.x =  element_blank(),
    # axis.title.x = element_blank(),
    # axis.ticks.x = element_blank()
  )
}

pixpnts <- st_read("~/Box/Independent study/shapefile/NavyYard/NYpoints.shp")

pix_ltln <- st_coordinates(pixpnts,lat,lon)

pix_ltln <- as.data.frame(pix_ltln)

Pixel_ltln <- tibble::rowid_to_column(pix_ltln, "ID")

write.csv(Pixel_ltln, "PixelPoints")

# stack Feb_Mar
Landsat7NY1999_6 <- raster('Ldst7_1999_6.tif')
Landsat7NY2000_6 <- raster('Ldst7_2000_6.tif')
Landsat7NY2001_6 <- raster('Ldst7_2001_6.tif')
Landsat7NY2002_6 <- raster('Ldst7_2002_6.tif')
Landsat7NY2003_6 <- raster('Ldst7_2003_6.tif')
Landsat7NY2004_6 <- raster('Ldst7_2004_6.tif')
Landsat7NY2005_6 <- raster('Ldst7_2005_6.tif')
Landsat7NY2006_6 <- raster('Ldst7_2006_6.tif')
Landsat7NY2007_6 <- raster('Ldst7_2007_6.tif')
Landsat7NY2008_6 <- raster('Ldst7_2008_6.tif')
Landsat7NY2009_6 <- raster('Ldst7_2009_6.tif')
Landsat7NY2010_6 <- raster('Ldst7_2010_6.tif')
Landsat7NY2011_6 <- raster('Ldst7_2011_6.tif')
Landsat7NY2012_6 <- raster('Ldst7_2012_6.tif')
Landsat7NY2013_6 <- raster('Ldst7_2013_6.tif')
Landsat7NY2014_6 <- raster('Ldst7_2014_6.tif')
Landsat7NY2015_6 <- raster('Ldst7_2015_6.tif')
Landsat7NY2016_6 <- raster('Ldst7_2016_6.tif')
Landsat7NY2017_6 <- raster('Ldst7_2017_6.tif')
Landsat7NY2018_6 <- raster('Ldst7_2018_6.tif')

Ldst7stack_6 <- stack(Landsat7NY1999_6,Landsat7NY2000_6,Landsat7NY2001_6,
                      Landsat7NY2002_6,Landsat7NY2003_6,Landsat7NY2004_6,
                      Landsat7NY2005_6,Landsat7NY2006_6,Landsat7NY2007_6,
                      Landsat7NY2008_6,Landsat7NY2009_6,Landsat7NY2010_6,
                      Landsat7NY2011_6,Landsat7NY2012_6,Landsat7NY2013_6,
                      Landsat7NY2014_6,Landsat7NY2015_6,Landsat7NY2016_6,
                      Landsat7NY2017_6,Landsat7NY2018_6)

names(Ldst7stack_6) <- c("1999","2000","2001","2002","2003","2004","2005",
                         "2006","2007","2008","2009","2010","2011","2012",
                         "2013","2014","2015","2016","2017","2018")

loc <- Pixel_ltln[,c("X", "Y")]
ext_6 <- extract(Ldst7stack_6, loc)
Ldst7Points_6 <- cbind(Pixel_ltln, ext_6) %>% drop_na()

mean6 <- colMeans(Ldst7Points_6) %>% as.data.frame()
mean6_NY_ave <- mean6[4:23,]

# R graph of average NDVI change over time
ggplot (data=NULL) + 
  geom_line(mapping = aes(x = 1999:2018,y= mean6_NY_ave)) +
  scale_x_continuous()

# Density Plot
tallFormat_NDVI_NY <- gather(Ldst7Points_6, key=year, value=NDVI, X1999:X2018)
NDVIdensity_NY_6 <- ggplot(tallFormat_NDVI_NY) + geom_density(aes(x = NDVI, color = year)) +
  labs(title= "How does the Navy Yard landscape change from 1999 to 2018?",
       subtitle= "Yearly NDVI density distribution calculated from Landsat7") +
  plotTheme()
NDVIdensity_NY_6
