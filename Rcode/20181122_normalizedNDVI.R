setwd("~/Box/Independent study/shapefile/NavyYard")
library(sf)
library(tidyverse)
library(dplyr)
library(raster)
library(reshape2)
library(caret)
library(wesanderson)
library(gdata)
library(matrixStats)

# ----------------------- Plot Theme/ Datasets/ Functions -----------------------
plotTheme <- function(base_size = 24) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 18,colour = "black"),
    plot.subtitle = element_text(face="italic"),
    plot.caption = element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line(linetype = "dashed","grey80", size = 0.1),
    panel.grid.minor = element_line(linetype = "dashed","grey90", size = 0.1),
    panel.border = element_rect(linetype = "solid", fill = NA),
    strip.background = element_rect(fill = "grey80", color = "white"),
    strip.text = element_text(size=12),
    axis.title = element_text(size=8),
    axis.text = element_text(size=8),
    # axis.title.y = element_text(size=12),
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

ExportTheme <- function(base_size = 300) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 48,colour = "black"),
    plot.subtitle = element_text(face="italic"),
    plot.caption = element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line(linetype = "dashed","grey80", size = 0.1),
    panel.grid.minor = element_line(linetype = "dashed","grey90", size = 0.1),
    panel.border = element_rect(linetype = "solid", fill = NA),
    strip.background = element_rect(fill = "grey80", color = "white"),
    strip.text = element_text(size=36),
    axis.title = element_text(size=24),
    axis.text.x = element_text(size=24),
    axis.text.y = element_text(size=24),
    # axis.title.y = element_text(size=12),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(size=24, colour = "black", face = "italic"),
    legend.text = element_text(size=18, colour = "black", face = "italic"),
    legend.key = element_rect(size = 5),
    legend.key.size = unit(1.5, 'lines'),
    axis.ticks.y = element_line(color="grey70")
    # axis.text.x =  element_blank(),
    # axis.title.x = element_blank(),
    # axis.ticks.x = element_blank()
  )
}

WrapTheme <- function(base_size = 24) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 18,colour = "black"),
    plot.subtitle = element_text(face="italic"),
    plot.caption = element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major.y = element_line("grey80", size = 0.1),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "#1d4f60", color = "white"),
    strip.text = element_text(colour = 'white', size=10),
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 8),
    axis.title.y = element_text(size = 12),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(colour = "black", face = "italic"),
    legend.text = element_text(size = 6, colour = "black", face = "italic"),
    legend.position="bottom",
    legend.box = "horizontal",
    axis.ticks.y = element_line(color="grey70"),
    axis.text.x =  element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank()
  )
}

# Import dataset
NY_Park <- st_read("~/Box/Independent study/shapefile/NavyYard/NY_ParkBoundaries.shp")
NY_Bound <- st_read("~/Box/Independent study/shapefile/NavyYard/NYLeftBoundary.shp")
PhillyPoints <- st_read("~/Box/urban landscape policies & satellite imagery/Task2/SHP/PixelPoints/Landsat7_Philly_06_18.shp")
MarineParadeGrounds <- NY_Park[1,0]
LeagueIslandPark <- NY_Park[2,0]
CrescentPark <- NY_Park[3,0]
CentralGreen <- NY_Park[4,0]

# Spatially select the points in the navy yard boundary
selection <- PhillyPoints[NY_Bound,] %>% dplyr::select(pointid)
head(selection)
plot(selection)

# Spatially select the points in each of the parks
MPGpts <- selection[MarineParadeGrounds,]
LIPpts <- selection[LeagueIslandPark,]
CPpts <- selection[CrescentPark,]
CGpts <- selection[CentralGreen,]

# functions
# Max value
colMax <- function(data) sapply(data, max, na.rm = TRUE)
# Min value
colMin <- function(data) sapply(data, min, na.rm = TRUE)
# std value
colStd <- function(data) sapply(data, sd, na.rm = TRUE)
# Standardize NDVI dataset
SDdata <- function(data,colmeanVal,colsdVal){
  StandardizedData <- data
  StandardizedData[] <- NA
  for (i in 1:ncol(data)){
    for (j in 1:length(data[,i])){
      StandardizedData[j,i] <- (data[j,i]- colmeanVal[i])/colsdVal[i]
    }
  }
  return(StandardizedData)
}
# Standardize in years
# datamatrix4 <- as.matrix(dplyr::select(Ldst7Points_4, -X,-Y,-ID))
# RowMin4 <- rowMins(datamatrix4, value = FALSE, na.rm = TRUE)
# RowMax4 <- rowMaxs(datamatrix4, value = FALSE, na.rm = TRUE)
# RowSD <- rowSds(datamatrix4, na.rm=TRUE)
# RowMean <- rowMeans(datamatrix4, na.rm=TRUE)
SDYear <- function(data){
  StandardizedData <- data
  StandardizedData[] <- NA
  TempDataMatrix <- as.matrix(data)
  # RowMin <- rowMins(TempDataMatrix, value = FALSE, na.rm = TRUE)
  # RowMax <- rowMaxs(TempDataMatrix, value = FALSE, na.rm = TRUE)
  RowSD <- rowSds(TempDataMatrix, na.rm=TRUE)
  RowMean <- rowMeans(TempDataMatrix, na.rm=TRUE)
  for (i in 1:nrow(data)){
    for (j in 1:ncol(data[i,])){
      StandardizedData[i,j] <- (data[i,j]- RowMean[i])/RowSD[i]
    }
  }
  return(StandardizedData)
}

colsBlue <- c("#212A5F", "#253266", "#293B6D", "#2D4474", 
              "#314D7C", "#355583", "#395E8A", "#3D6791", 
              "#417099", "#4579A0", "#4981A7", "#4D8AAE", 
              "#5193B6", "#559CBD", "#59A5C4", "#5DADCB", 
              "#61B6D3", "#65BFDA", "#69C8E1", "#6DD1E9")

colsMint <- c("#D2FBD4", "#C5F1CE", "#B9E8C9", "#ACDEC3", 
              "#A0D5BE", "#93CBB9", "#87C2B3", "#7AB8AE", 
              "#6EAFA8", "#61A5A3", "#559C9E", "#4D9196", 
              "#46878E", "#3E7D87", "#37727F", "#2F6878", 
              "#285E70", "#205369", "#194961", "#123F5A")

colsGreen <- c("#C4E6C3", "#B5DCBB", "#A7D3B4", "#99C9AD", 
               "#8BC0A5", "#7DB69E", "#6EAD97", "#60A38F", 
               "#529A88", "#449081", "#36877A", "#338077", 
               "#307A74", "#2D7471", "#2A6E6E", "#28676B", 
               "#256168", "#225B65", "#1F5562", "#1D4F60")

colsDMint <- c("#D2FBD4", "#C2EECC", "#B3E1C5", "#A4D4BD", 
               "#95C8B6", "#86BBAE", "#76AEA7", "#67A29F", 
               "#589598", "#498890", "#3A7C89", "#357583", 
               "#316E7E", "#2C6779", "#286074", "#235A6E", 
               "#1F5369", "#1A4C64", "#16455F", "#123F5A")

labs <- c("Year 1999", "Year 2000", "Year 2001", "Year 2002",
          "Year 2003", "Year 2004", "Year 2005", "Year 2006",
          "Year 2007", "Year 2008", "Year 2009", "Year 2010",
          "Year 2011", "Year 2012", "Year 2013", "Year 2014",
          "Year 2015", "Year 2016", "Year 2017", "Year 2018")

setwd("~/Box/Independent study/Tiff/PHL_NDVI_CALC")
# -------------------- stack April and rename --------------------
Landsat7Phil1999_4 <- raster('Landsat7Phil1999_4.tif')
Landsat7Phil2000_4 <- raster('Landsat7Phil2000_4.tif')
Landsat7Phil2001_4 <- raster('Landsat7Phil2001_4.tif')
Landsat7Phil2002_4 <- raster('Landsat7Phil2002_4.tif')
Landsat7Phil2003_4 <- raster('Landsat7Phil2003_4.tif')
Landsat7Phil2004_4 <- raster('Landsat7Phil2004_4.tif')
Landsat7Phil2005_4 <- raster('Landsat7Phil2005_4.tif')
Landsat7Phil2006_4 <- raster('Landsat7Phil2006_4.tif')
Landsat7Phil2007_4 <- raster('Landsat7Phil2007_4.tif')
Landsat7Phil2008_4 <- raster('Landsat7Phil2008_4.tif')
Landsat7Phil2009_4 <- raster('Landsat7Phil2009_4.tif')
Landsat7Phil2010_4 <- raster('Landsat7Phil2010_4.tif')
Landsat7Phil2011_4 <- raster('Landsat7Phil2011_4.tif')
Landsat7Phil2012_4 <- raster('Landsat7Phil2012_4.tif')
Landsat7Phil2013_4 <- raster('Landsat7Phil2013_4.tif')
Landsat7Phil2014_4 <- raster('Landsat7Phil2014_4.tif')
Landsat7Phil2015_4 <- raster('Landsat7Phil2015_4.tif')
Landsat7Phil2016_4 <- raster('Landsat7Phil2016_4.tif')
Landsat7Phil2017_4 <- raster('Landsat7Phil2017_4.tif')
Landsat7Phil2018_4 <- raster('Landsat7Phil2018_4.tif')

NYLdst7stack_4 <- stack(Landsat7Phil1999_4,Landsat7Phil2000_4,Landsat7Phil2001_4,
                      Landsat7Phil2002_4,Landsat7Phil2003_4,Landsat7Phil2004_4,
                      Landsat7Phil2005_4,Landsat7Phil2006_4,Landsat7Phil2007_4,
                      Landsat7Phil2008_4,Landsat7Phil2009_4,Landsat7Phil2010_4,
                      Landsat7Phil2011_4,Landsat7Phil2012_4,Landsat7Phil2013_4,
                      Landsat7Phil2014_4,Landsat7Phil2015_4,Landsat7Phil2016_4,
                      Landsat7Phil2017_4,Landsat7Phil2018_4)

names(NYLdst7stack_4) <- c("1999","2000","2001","2002","2003","2004","2005",
                         "2006","2007","2008","2009","2010","2011","2012",
                         "2013","2014","2015","2016","2017","2018")
# Project Raster
sr <- "+proj=utm +zone=18 +datum=NAD83 +units=m +no_defs" 
projected_raster_4 <- projectRaster(NYLdst7stack_4, crs = sr)

pix_ltln <- st_coordinates(selection,lat,lon) %>% 
  as.data.frame() %>% 
  tibble::rowid_to_column() %>% 
  rename(ID = rowid)

st_crs(selection) 
projected_raster_4@crs

loc <- pix_ltln[,c("X", "Y")]
ext_4 <- extract(projected_raster_4, loc)

# Final combined dataset
Ldst7Points_4 <- cbind(pix_ltln, ext_4)

Data4 <- Ldst7Points_4[,4:23]
# Mean value
mean4 <- colMeans(Ldst7Points_4) %>% as.data.frame()
mean4_NY_ave <- mean4[4:23,]
# Max value
max4 <- colMax(Ldst7Points_4) %>% as.data.frame()
max4_NY_ave <- max4[4:23,]
# Min value
min4 <- colMin(Ldst7Points_4) %>% as.data.frame()
min4_NY_ave <- min4[4:23,]
# std value
sd4 <- colStd(Ldst7Points_4) %>% as.data.frame()
sd4_NY_ave <- sd4[4:23,]
# standardized NDVI every year
SDNDVI4 <- SDdata(Data4, mean4_NY_ave, sd4_NY_ave)
# standardize NDVI through years
SDYearsNDVI4 <- SDYear(Data4)

# R graph of average NDVI change over time
NDVIyearlychange4 <- ggplot (data=NULL) + 
  geom_line(mapping = aes(x = 1999:2018,y= mean4_NY_ave)) +
  scale_x_continuous(name="Years", limits=c(1999, 2018))+
  scale_y_continuous(name="NDVI average value in June", limits=c(0, 0.3)) +
  plotTheme()
NDVIyearlychange4

# Density Plot
colnames(SDNDVI4) <- c(1999:2018) 
tallFormat_NDVI_NY4 <- gather(SDNDVI4, key=year, value=NDVI, '1999':'2018')

# mean values for geom_vline
mu4 <- cbind(mean4_NY_ave, c(1999:2018)) %>% as.data.frame()
colnames(mu4) <- c("MeanNDVI", "year")
mu4$year <- as.character(mu4$year)

NDVIdensity_NY_4 <- ggplot(tallFormat_NDVI_NY4, aes(x=NDVI, color=year)) +
  geom_density(aes(x = NDVI, color = year))+
  scale_colour_manual(values = colsGreen, labels = labs)+
  geom_vline(data=mu4, aes(xintercept=MeanNDVI, color=year),
             linetype="dashed") +
  geom_text(data=mu4, aes(x = MeanNDVI, 
                         label= paste("Mean NDVI: ", round(MeanNDVI, digits = 3)), 
                         y = 1, colour= year), 
            angle=0, text = element_text(size=11)) +
  labs(title= "How does the Navy Yard NDVI change in April from 1999 to 2018?",
       subtitle= "Yearly NDVI density distribution calculated from Landsat7",
       caption = "Data Source: USGS Landsat 7") +
  xlab("Year") + 
  ylab("NDVI Density") +
  WrapTheme()

# Facet wrap
NDVIdensity_NY_4 + facet_wrap( ~ year, ncol = 5)

################ After Standardization ################ 
# R graph of average NDVI change over time after standardize
mean_SD4 <- colMeans(SDYearsNDVI4)
NDVIyearlychangeSD4 <- ggplot (data=NULL) + 
  geom_line(mapping = aes(x = 1999:2018,y= mean_SD4)) +
  scale_x_continuous(name="Years", limits=c(1999, 2018))+
  scale_y_continuous(name="NDVI average value in April", limits=c(-1, 1)) +
  plotTheme()
NDVIyearlychangeSD4

# Density Plot
colnames(SDYearsNDVI4) <- c(1999:2018) 
tallFormat_NDVI_NY_SD4 <- gather(SDYearsNDVI4, key=year, value=NDVI, '1999':'2018')

muSD4 <- cbind(mean_SD4, c(1999:2018)) %>% as.data.frame()
colnames(muSD4) <- c("MeanNDVI", "year")
muSD4$year <- as.character(muSD4$year)

NDVIdensity_NY_SD_4 <- ggplot(tallFormat_NDVI_NY_SD4, aes(x=NDVI, color=year)) +
  geom_density(aes(x = NDVI, color = year))+
  scale_colour_manual(values = colsGreen, labels = labs)+
  labs(title= "How does the Navy Yard NDVI change in April from 1999 to 2018?",
       subtitle= "Yearly NDVI density distribution calculated from Landsat7",
       caption = "Data Source: USGS Landsat 7") +
  xlab("Year") + 
  ylab("NDVI Density") +
  WrapTheme()
NDVIdensity_NY_SD_4

# Facet wrap after Standardization
NDVIdensity_NY_SD_4 + 
  geom_vline(data=muSD4, aes(xintercept=MeanNDVI, color=year),
             linetype="dashed") +
  geom_text(data=muSD4, aes(x = MeanNDVI, 
                            label= paste("Mean NDVI: ", round(MeanNDVI, digits = 3)), 
                            # Y changes the location of text
                            y = 0.7, colour= year), 
            angle=0, text = element_text(size=10)) +
  facet_wrap( ~ year, ncol = 5)

# -------------------- stack June and rename --------------------
Landsat7Phil1999_6 <- raster('Landsat7Phil1999_6.tif')
Landsat7Phil2000_6 <- raster('Landsat7Phil2000_6.tif')
Landsat7Phil2001_6 <- raster('Landsat7Phil2001_6.tif')
Landsat7Phil2002_6 <- raster('Landsat7Phil2002_6.tif')
Landsat7Phil2003_6 <- raster('Landsat7Phil2003_6.tif')
Landsat7Phil2004_6 <- raster('Landsat7Phil2004_6.tif')
Landsat7Phil2005_6 <- raster('Landsat7Phil2005_6.tif')
Landsat7Phil2006_6 <- raster('Landsat7Phil2006_6.tif')
Landsat7Phil2007_6 <- raster('Landsat7Phil2007_6.tif')
Landsat7Phil2008_6 <- raster('Landsat7Phil2008_6.tif')
Landsat7Phil2009_6 <- raster('Landsat7Phil2009_6.tif')
Landsat7Phil2010_6 <- raster('Landsat7Phil2010_6.tif')
Landsat7Phil2011_6 <- raster('Landsat7Phil2011_6.tif')
Landsat7Phil2012_6 <- raster('Landsat7Phil2012_6.tif')
Landsat7Phil2013_6 <- raster('Landsat7Phil2013_6.tif')
Landsat7Phil2014_6 <- raster('Landsat7Phil2014_6.tif')
Landsat7Phil2015_6 <- raster('Landsat7Phil2015_6.tif')
Landsat7Phil2016_6 <- raster('Landsat7Phil2016_6.tif')
Landsat7Phil2017_6 <- raster('Landsat7Phil2017_6.tif')
Landsat7Phil2018_6 <- raster('Landsat7Phil2018_6.tif')

NYLdst7stack_6 <- stack(Landsat7Phil1999_6,Landsat7Phil2000_6,Landsat7Phil2001_6,
                        Landsat7Phil2002_6,Landsat7Phil2003_6,Landsat7Phil2004_6,
                        Landsat7Phil2005_6,Landsat7Phil2006_6,Landsat7Phil2007_6,
                        Landsat7Phil2008_6,Landsat7Phil2009_6,Landsat7Phil2010_6,
                        Landsat7Phil2011_6,Landsat7Phil2012_6,Landsat7Phil2013_6,
                        Landsat7Phil2014_6,Landsat7Phil2015_6,Landsat7Phil2016_6,
                        Landsat7Phil2017_6,Landsat7Phil2018_6)

names(NYLdst7stack_6) <- c("1999","2000","2001","2002","2003","2004","2005",
                           "2006","2007","2008","2009","2010","2011","2012",
                           "2013","2014","2015","2016","2017","2018")
# Project Raster
sr <- "+proj=utm +zone=18 +datum=NAD83 +units=m +no_defs" 
projected_raster_6 <- projectRaster(NYLdst7stack_6, crs = sr)

st_crs(selection) 
projected_raster_6@crs

ext_6 <- extract(projected_raster_6, loc)

# Final combined dataset
Ldst7Points_6 <- cbind(pix_ltln, ext_6)

Data6 <- Ldst7Points_6[,4:23]
# Mean value
mean6 <- colMeans(Ldst7Points_6) %>% as.data.frame()
mean6_NY_ave <- mean6[4:23,]
# Max value
max6 <- colMax(Ldst7Points_6) %>% as.data.frame()
max6_NY_ave <- max6[4:23,]
# Min value
min6 <- colMin(Ldst7Points_6) %>% as.data.frame()
min6_NY_ave <- min6[4:23,]
# std value
sd6 <- colStd(Ldst7Points_6) %>% as.data.frame()
sd6_NY_ave <- sd6[4:23,]
# standardized NDVI 
SDNDVI6 <- SDdata(Data6, mean6_NY_ave, sd6_NY_ave)
# standardize NDVI through years
SDYearsNDVI6 <- SDYear(Data6)

# R graph of average NDVI change over time
NDVIyearlychange6 <- ggplot (data=NULL) + 
  geom_line(mapping = aes(x = 1999:2018,y= mean6_NY_ave)) +
  scale_x_continuous(name="Years", limits=c(1999, 2018))+
  scale_y_continuous(name="NDVI average value in June", limits=c(0, 0.3)) +
  plotTheme()
NDVIyearlychange6

# Density Plot
colnames(SDNDVI6) <- c(1999:2018) 
tallFormat_NDVI_NY6 <- gather(SDNDVI6, key=year, value=NDVI, '1999':'2018')

# mean values for geom_vline
mu6 <- cbind(mean6_NY_ave, c(1999:2018)) %>% as.data.frame()
colnames(mu6) <- c("MeanNDVI", "year")
mu6$year <- as.character(mu6$year)

NDVIdensity_NY_6 <- ggplot(tallFormat_NDVI_NY6, aes(x=NDVI, color=year)) +
  geom_density(aes(x = NDVI, color = year))+
  scale_colour_manual(values = colsGreen, labels = labs)+
  geom_vline(data=mu6, aes(xintercept=MeanNDVI, color=year),
             linetype="dashed") +
  geom_text(data=mu6, aes(x = MeanNDVI, 
                          label= paste("Mean NDVI: ", round(MeanNDVI, digits = 3)), 
                          y = 1, colour= year), 
            angle=0, text = element_text(size=11)) +
  labs(title= "How does the Navy Yard NDVI change in June from 1999 to 2018?",
       subtitle= "Yearly NDVI density distribution calculated from Landsat7",
       caption = "Data Source: USGS Landsat 7") +
  xlab("Year") + 
  ylab("NDVI Density") +
  WrapTheme()

# Facet wrap
NDVIdensity_NY_6 + facet_wrap( ~ year, ncol = 5)

################ After Standardization ################ 
# R graph of average NDVI change over time after standardize
mean_SD6 <- colMeans(SDYearsNDVI6)
NDVIyearlychangeSD6 <- ggplot (data=NULL) + 
  geom_line(mapping = aes(x = 1999:2018,y= mean_SD6)) +
  scale_x_continuous(name="Years", limits=c(1999, 2018))+
  scale_y_continuous(name="NDVI average value in June", limits=c(-1, 1)) +
  plotTheme()
NDVIyearlychangeSD6

# Density Plot
colnames(SDYearsNDVI6) <- c(1999:2018) 
tallFormat_NDVI_NY_SD6 <- gather(SDYearsNDVI6, key=year, value=NDVI, '1999':'2018')

muSD6 <- cbind(mean_SD6, c(1999:2018)) %>% as.data.frame()
colnames(muSD6) <- c("MeanNDVI", "year")
muSD6$year <- as.character(muSD6$year)

NDVIdensity_NY_SD_6 <- ggplot(tallFormat_NDVI_NY_SD6, aes(x=NDVI, color=year)) +
  geom_density(aes(x = NDVI, color = year))+
  scale_colour_manual(values = colsGreen, labels = labs)+
  labs(title= "How does the Navy Yard NDVI change in June from 1999 to 2018?",
       subtitle= "Yearly NDVI density distribution calculated from Landsat7",
       caption = "Data Source: USGS Landsat 7") +
  xlab("Year") + 
  ylab("NDVI Density") +
  WrapTheme()
NDVIdensity_NY_SD_6

# Facet wrap after Standardization
NDVIdensity_NY_SD_6 + 
  geom_vline(data=muSD6, aes(xintercept=MeanNDVI, color=year),
             linetype="dashed") +
  geom_text(data=muSD6, aes(x = MeanNDVI, 
                            label= paste("Mean NDVI: ", round(MeanNDVI, digits = 3)), 
                            # Y changes the location of text
                            y = 0.7, colour= year), 
            angle=0, text = element_text(size=10)) +
  facet_wrap( ~ year, ncol = 5)

# -------------------- stack Aug and rename --------------------
Landsat7Phil1999_8 <- raster('Landsat7Phil1999_8.tif')
Landsat7Phil2000_8 <- raster('Landsat7Phil2000_8.tif')
Landsat7Phil2001_8 <- raster('Landsat7Phil2001_8.tif')
Landsat7Phil2002_8 <- raster('Landsat7Phil2002_8.tif')
Landsat7Phil2003_8 <- raster('Landsat7Phil2003_8.tif')
Landsat7Phil2004_8 <- raster('Landsat7Phil2004_8.tif')
Landsat7Phil2005_8 <- raster('Landsat7Phil2005_8.tif')
Landsat7Phil2006_8 <- raster('Landsat7Phil2006_8.tif')
Landsat7Phil2007_8 <- raster('Landsat7Phil2007_8.tif')
Landsat7Phil2008_8 <- raster('Landsat7Phil2008_8.tif')
Landsat7Phil2009_8 <- raster('Landsat7Phil2009_8.tif')
Landsat7Phil2010_8 <- raster('Landsat7Phil2010_8.tif')
Landsat7Phil2011_8 <- raster('Landsat7Phil2011_8.tif')
Landsat7Phil2012_8 <- raster('Landsat7Phil2012_8.tif')
Landsat7Phil2013_8 <- raster('Landsat7Phil2013_8.tif')
Landsat7Phil2014_8 <- raster('Landsat7Phil2014_8.tif')
Landsat7Phil2015_8 <- raster('Landsat7Phil2015_8.tif')
Landsat7Phil2016_8 <- raster('Landsat7Phil2016_8.tif')
Landsat7Phil2017_8 <- raster('Landsat7Phil2017_8.tif')
Landsat7Phil2018_8 <- raster('Landsat7Phil2018_8.tif')

NYLdst7stack_8 <- stack(Landsat7Phil1999_8,Landsat7Phil2000_8,Landsat7Phil2001_8,
                        Landsat7Phil2002_8,Landsat7Phil2003_8,Landsat7Phil2004_8,
                        Landsat7Phil2005_8,Landsat7Phil2006_8,Landsat7Phil2007_8,
                        Landsat7Phil2008_8,Landsat7Phil2009_8,Landsat7Phil2010_8,
                        Landsat7Phil2011_8,Landsat7Phil2012_8,Landsat7Phil2013_8,
                        Landsat7Phil2014_8,Landsat7Phil2015_8,Landsat7Phil2016_8,
                        Landsat7Phil2017_8,Landsat7Phil2018_8)

names(NYLdst7stack_8) <- c("1999","2000","2001","2002","2003","2004","2005",
                           "2006","2007","2008","2009","2010","2011","2012",
                           "2013","2014","2015","2016","2017","2018")
# Project Raster
sr <- "+proj=utm +zone=18 +datum=NAD83 +units=m +no_defs" 
projected_raster_8 <- projectRaster(NYLdst7stack_8, crs = sr)

st_crs(selection) 
projected_raster_8@crs

ext_8 <- extract(projected_raster_8, loc)

# Final combined dataset
Ldst7Points_8 <- cbind(pix_ltln, ext_8)

Data8 <- Ldst7Points_8[,4:23]
# Mean value
mean8 <- colMeans(Ldst7Points_8) %>% as.data.frame()
mean8_NY_ave <- mean8[4:23,]
# Max value
max8 <- colMax(Ldst7Points_8) %>% as.data.frame()
max8_NY_ave <- max8[4:23,]
# Min value
min8 <- colMin(Ldst7Points_8) %>% as.data.frame()
min8_NY_ave <- min8[4:23,]
# std value
sd8 <- colStd(Ldst7Points_8) %>% as.data.frame()
sd8_NY_ave <- sd8[4:23,]
# standardized NDVI 
SDNDVI8 <- SDdata(Data8, mean8_NY_ave, sd8_NY_ave)
# standardize NDVI through years
SDYearsNDVI8 <- SDYear(Data8)

# R graph of average NDVI change over time
NDVIyearlychange8 <- ggplot (data=NULL) + 
  geom_line(mapping = aes(x = 1999:2018,y= mean8_NY_ave)) +
  scale_x_continuous(name="Years", limits=c(1999, 2018))+
  scale_y_continuous(name="NDVI average value in June", limits=c(0, 0.3)) +
  plotTheme()
NDVIyearlychange8

# Density Plot
colnames(SDNDVI8) <- c(1999:2018) 
tallFormat_NDVI_NY8 <- gather(SDNDVI8, key=year, value=NDVI, '1999':'2018')

# mean values for geom_vline
mu <- cbind(mean8_NY_ave, c(1999:2018)) %>% as.data.frame()
colnames(mu) <- c("MeanNDVI", "year")
mu$year <- as.character(mu$year)

NDVIdensity_NY_8 <- ggplot(tallFormat_NDVI_NY8, aes(x=NDVI, color=year)) +
  geom_density(aes(x = NDVI, color = year))+
  scale_colour_manual(values = colsGreen, labels = labs)+
  geom_vline(data=mu, aes(xintercept=MeanNDVI, color=year),
             linetype="dashed") +
  geom_text(data=mu, aes(x = MeanNDVI, 
                         label= paste("Mean NDVI: ", round(MeanNDVI, digits = 3)), 
                         y = 1, colour= year), 
            angle=0, text = element_text(size=11)) +
  labs(title= "How does the Navy Yard NDVI change in August from 1999 to 2018?",
       subtitle= "Yearly NDVI density distribution calculated from Landsat7",
       caption = "Data Source: USGS Landsat 7") +
  xlab("Year") + 
  ylab("NDVI Density") +
  WrapTheme()
# NDVIdensity_NY_8
# Facet wrap
NDVIdensity_NY_8 + facet_wrap( ~ year, ncol = 5)

################ After Standardization ################ 
# R graph of average NDVI change over time after standardize
mean_SD8 <- colMeans(SDYearsNDVI8)
NDVIyearlychangeSD8 <- ggplot (data=NULL) + 
  geom_line(mapping = aes(x = 1999:2018,y= mean_SD8)) +
  scale_x_continuous(name="Years", limits=c(1999, 2018))+
  scale_y_continuous(name="NDVI average value in August", limits=c(-1, 2.1)) +
  plotTheme()
NDVIyearlychangeSD8

# Density Plot
colnames(SDYearsNDVI8) <- c(1999:2018) 
tallFormat_NDVI_NY_SD8 <- gather(SDYearsNDVI8, key=year, value=NDVI, '1999':'2018')

muSD8 <- cbind(mean_SD8, c(1999:2018)) %>% as.data.frame()
colnames(muSD8) <- c("MeanNDVI", "year")
muSD8$year <- as.character(muSD8$year)

NDVIdensity_NY_SD_8 <- ggplot(tallFormat_NDVI_NY_SD8, aes(x=NDVI, color=year)) +
  geom_density(aes(x = NDVI, color = year))+
  scale_colour_manual(values = colsGreen, labels = labs)+
  labs(title= "How does the Navy Yard NDVI change in August from 1999 to 2018?",
       subtitle= "Yearly NDVI density distribution calculated from Landsat7",
       caption = "Data Source: USGS Landsat 7") +
  xlab("Year") + 
  ylab("NDVI Density") +
  WrapTheme()
NDVIdensity_NY_SD_8

# Facet wrap after Standardization
NDVIdensity_NY_SD_8 + 
  geom_vline(data=muSD8, aes(xintercept=MeanNDVI, color=year),
             linetype="dashed") +
  geom_text(data=muSD8, aes(x = MeanNDVI, 
                            label= paste("Mean NDVI: ", round(MeanNDVI, digits = 3)), 
                            # Y changes the location of text
                            y = 1, colour= year), 
            angle=0, text = element_text(size=10)) +
  facet_wrap( ~ year, ncol = 5)

# -------------------- ##### NDVI all summary plot ##### -------------------------------------
mean_alldat <- cbind(mean4_NY_ave,mean6_NY_ave,mean8_NY_ave) %>% 
  as.data.frame() %>% 
  dplyr::mutate(year = 1999:2018)
colnames(mean_alldat) <- c("4", "6", "8", "Year")

NDVIyearlychange_all <- ggplot (data=mean_alldat) + 
  geom_line(mapping = aes(x = 1999:2018,y= mean_alldat$'4',color = "April/May"), size=0.4, alpha = 0.3, linetype = "longdash", na.rm = TRUE , se= FALSE) +
  geom_line(mapping = aes(x = 1999:2018,y= mean_alldat$'6',color = "June/July"), size=0.4, alpha = 0.3, linetype = "longdash", na.rm = TRUE, se= FALSE) +
  geom_line(mapping = aes(x = 1999:2018,y= mean_alldat$'8',color = "August/September"), size=0.4, alpha = 0.3, linetype = "longdash", na.rm = TRUE, se= FALSE) +
  geom_point(mapping = aes(x = 1999:2018,y= mean_alldat$'4',color = "April/May"), size=1, alpha = 0.3) +
  geom_point(mapping = aes(x = 1999:2018,y= mean_alldat$'6',color = "June/July"), size=1, alpha = 0.3) +
  geom_point(mapping = aes(x = 1999:2018,y= mean_alldat$'8',color = "August/September"), size=1, alpha = 0.3) +
  geom_smooth(mapping = aes(x = 1999:2018,y= mean_alldat$'4',color = "April/May"), size=1, alpha = 0.3, na.rm = TRUE , se= FALSE) +
  geom_smooth(mapping = aes(x = 1999:2018,y= mean_alldat$'6',color = "June/July"), size=1, alpha = 0.3, na.rm = TRUE, se= FALSE) +
  geom_smooth(mapping = aes(x = 1999:2018,y= mean_alldat$'8',color = "August/September"), size=1, alpha = 0.3, na.rm = TRUE, se= FALSE) +
  # scale_colour_brewer(palette = "Blues") +
  scale_colour_manual(values = c("#123F5A", "#3F88A1", "#6DD1E9"), labels = c("April/May","June/July","August/September"))+
  labs(color = 'Months') +
  scale_x_continuous(name="Years", limits=c(1999, 2018))+
  scale_y_continuous(name="NDVI average value in June", limits=c(0, 0.3), breaks = c(0,0.1,0.2,0.3)) +
  scale_x_continuous("Year", labels = as.character(mean_alldat$Year), breaks = mean_alldat$Year)+
  theme(legend.position = "bottom") +
  ExportTheme()
NDVIyearlychange_all

# -------------------- MPGpts -------------------------------------
MPGpts <- selection[MarineParadeGrounds,]
LIPpts <- selection[LeagueIslandPark,]
CPpts <- selection[CrescentPark,]
CGpts <- selection[CentralGreen,]

MPGpix_ltln <- st_coordinates(MPGpts,lat,lon) %>% 
  as.data.frame() %>% 
  tibble::rowid_to_column() %>% 
  rename(ID = rowid)
MPGloc <- MPGpix_ltln[,c("X", "Y")]
MPGext_4 <- extract(projected_raster_4, MPGloc)
MPGLdst7Points_4 <- cbind(MPGpix_ltln, MPGext_4)

MPGext_6 <- extract(projected_raster_6, MPGloc)
MPGLdst7Points_6 <- cbind(MPGpix_ltln, MPGext_6)

MPGext_8 <- extract(projected_raster_8, MPGloc)
MPGLdst7Points_8 <- cbind(MPGpix_ltln, MPGext_8)

MPGData4 <- MPGLdst7Points_4[,4:23]
MPGData6 <- MPGLdst7Points_6[,4:23]
MPGData8 <- MPGLdst7Points_8[,4:23]
# Mean value
MPGmean4 <- colMeans(MPGLdst7Points_4) %>% as.data.frame()
MPGmean4_NY_ave <- MPGmean4[4:23,]
# Max value
MPGmax4 <- colMax(MPGLdst7Points_4) %>% as.data.frame()
MPGmax4_NY_ave <- MPGmax4[4:23,]
# Min value
MPGmin4 <- colMin(MPGLdst7Points_4) %>% as.data.frame()
MPGmin4_NY_ave <- MPGmin4[4:23,]
# std value
MPGsd4 <- colStd(MPGLdst7Points_4) %>% as.data.frame()
MPGsd4_NY_ave <- MPGsd4[4:23,]
# # standardized NDVI every year
# SDNDVI4 <- SDdata(Data4, mean4_NY_ave, sd4_NY_ave)
# standardize NDVI through years
# MPGSDYearsNDVI4 <- SDYear(MPGData4)

######### UNstandardized ##########
# R graph of UNstandardized average NDVI change over time
# UNStandardized Mean value
# 4_5
MPGmean4_NY_ave <- colMeans(MPGLdst7Points_4)[4:23]
MPGNDVIyearlychange4 <- ggplot (data=NULL) + 
  geom_line(mapping = aes(x = 1999:2018,y= MPGmean4_NY_ave)) +
  scale_x_continuous(name="Years", limits=c(1999, 2018))+
  scale_y_continuous(name="Marine Parade Ground NDVI average value in April", limits=c(0, 1)) +
  plotTheme()
MPGNDVIyearlychange4

# 6_7
MPGmean6_NY_ave <- colMeans(MPGLdst7Points_6)[4:23]
MPGNDVIyearlychange6 <- ggplot (data=NULL) + 
  geom_line(mapping = aes(x = 1999:2018,y= MPGmean6_NY_ave)) +
  scale_x_continuous(name="Years", limits=c(1999, 2018))+
  scale_y_continuous(name="Marine Parade Ground NDVI average value in June", limits=c(0, 1)) +
  plotTheme()
MPGNDVIyearlychange6

# 8_9
MPGmean8_NY_ave <- colMeans(MPGLdst7Points_8)[4:23]
MPGNDVIyearlychange8 <- ggplot (data=NULL) + 
  geom_line(mapping = aes(x = 1999:2018,y= MPGmean8_NY_ave)) +
  scale_x_continuous(name="Years", limits=c(1999, 2018))+
  scale_y_continuous(name="Marine Parade Ground NDVI average value in August", limits=c(0, 1)) +
  plotTheme()
MPGNDVIyearlychange8

# tall format for Density Plot
colnames(MPGData4) <- c(1999:2018) 
MPGtallFormat_NDVI_NY4 <- gather(MPGData4, key=year, value=NDVI, '1999':'2018')

colnames(MPGData6) <- c(1999:2018) 
MPGtallFormat_NDVI_NY6 <- gather(MPGData6, key=year, value=NDVI, '1999':'2018')

colnames(MPGData8) <- c(1999:2018) 
MPGtallFormat_NDVI_NY8 <- gather(MPGData8, key=year, value=NDVI, '1999':'2018')

# mean values for geom_vline
MPGmu4 <- cbind(MPGmean4_NY_ave, c(1999:2018)) %>% as.data.frame()
colnames(MPGmu4) <- c("MeanNDVI", "year")
MPGmu4$year <- as.character(MPGmu4$year)

MPGmu6 <- cbind(MPGmean6_NY_ave, c(1999:2018)) %>% as.data.frame()
colnames(MPGmu6) <- c("MeanNDVI", "year")
MPGmu6$year <- as.character(MPGmu6$year)

MPGmu8 <- cbind(MPGmean8_NY_ave, c(1999:2018)) %>% as.data.frame()
colnames(MPGmu8) <- c("MeanNDVI", "year")
MPGmu8$year <- as.character(MPGmu8$year)

# 4_5
MPGNDVIdensity_NY_4 <- ggplot(MPGtallFormat_NDVI_NY4, aes(x=NDVI, color=year)) +
  geom_density(aes(x = NDVI, color = year))+
  scale_colour_manual(values = colsGreen, labels = labs)+
  geom_vline(data=MPGmu4, aes(xintercept=MeanNDVI, color=year),
             linetype="dashed", size = 0.5, alpha = 0.8) +
  labs(title= "How does the Marine Parade Ground NDVI change in April from 1999 to 2018?",
       subtitle= "Yearly NDVI density distribution calculated from Landsat7",
       caption = "Data Source: USGS Landsat 7") +
  xlab("Year") + 
  ylab("NDVI Density") +
  WrapTheme()
MPGNDVIdensity_NY_4
# Facet wrap
MPGNDVIdensity_NY_4 + 
  geom_text(data=MPGmu4, aes(x = MeanNDVI, 
                             label= paste("Mean NDVI: \n", round(MeanNDVI, digits = 3)), 
                             y = 13, colour= year), 
            angle=0, text = element_text(size=11)) +
  facet_wrap( ~ year, ncol = 5)

# 6_7
MPGNDVIdensity_NY_6 <- ggplot(MPGtallFormat_NDVI_NY6, aes(x=NDVI, color=year)) +
  geom_density(aes(x = NDVI, color = year))+
  scale_colour_manual(values = colsGreen, labels = labs)+
  geom_vline(data=MPGmu6, aes(xintercept=MeanNDVI, color=year),
             linetype="dashed", size = 0.5, alpha = 0.8) +
  labs(title= "How does the Marine Parade Ground NDVI change in June from 1999 to 2018?",
       subtitle= "Yearly NDVI density distribution calculated from Landsat7",
       caption = "Data Source: USGS Landsat 7") +
  xlab("Year") + 
  ylab("NDVI Density") +
  WrapTheme()
MPGNDVIdensity_NY_6
# Facet wrap
MPGNDVIdensity_NY_6 + 
  geom_text(data=MPGmu6, aes(x = MeanNDVI, 
                             label= paste("Mean NDVI: \n", round(MeanNDVI, digits = 3)), 
                             y = 13, colour= year), 
            angle=0, text = element_text(size=11)) +
  facet_wrap( ~ year, ncol = 5)

# 8_9
MPGNDVIdensity_NY_8 <- ggplot(MPGtallFormat_NDVI_NY8, aes(x=NDVI, color=year)) +
  geom_density(aes(x = NDVI, color = year))+
  scale_colour_manual(values = colsGreen, labels = labs)+
  geom_vline(data=MPGmu8, aes(xintercept=MeanNDVI, color=year),
             linetype="dashed", size = 0.5, alpha = 0.8) +
  labs(title= "How does the Marine Parade Ground NDVI change in August from 1999 to 2018?",
       subtitle= "Yearly NDVI density distribution calculated from Landsat7",
       caption = "Data Source: USGS Landsat 7") +
  xlab("Year") + 
  ylab("NDVI Density") +
  WrapTheme()
MPGNDVIdensity_NY_8
# Facet wrap
MPGNDVIdensity_NY_8 + 
  geom_text(data=MPGmu8, aes(x = MeanNDVI, 
                             label= paste("Mean NDVI: \n", round(MeanNDVI, digits = 3)), 
                             y = 13, colour= year), 
            angle=0, text = element_text(size=11)) +
  facet_wrap( ~ year, ncol = 5)

# MPG All mean in one plot
MPGmean_alldat <- cbind(MPGmean4_NY_ave,MPGmean6_NY_ave,MPGmean8_NY_ave) %>% 
  as.data.frame() %>% 
  dplyr::mutate(year = 1999:2018)
colnames(MPGmean_alldat) <- c("4", "6", "8", "Year")

MPGNDVIyearlychange_all <- ggplot (data=MPGmean_alldat) + 
  geom_line(mapping = aes(x = 1999:2018,y= MPGmean_alldat$'4',color = "April/May"), size=0.4, alpha = 0.3, linetype = "longdash", na.rm = TRUE , se= FALSE) +
  geom_line(mapping = aes(x = 1999:2018,y= MPGmean_alldat$'6',color = "June/July"), size=0.4, alpha = 0.3, linetype = "longdash", na.rm = TRUE, se= FALSE) +
  geom_line(mapping = aes(x = 1999:2018,y= MPGmean_alldat$'8',color = "August/September"), size=0.4, alpha = 0.3, linetype = "longdash", na.rm = TRUE, se= FALSE) +
  geom_point(mapping = aes(x = 1999:2018,y= MPGmean_alldat$'4',color = "April/May"), size=1, alpha = 0.3) +
  geom_point(mapping = aes(x = 1999:2018,y= MPGmean_alldat$'6',color = "June/July"), size=1, alpha = 0.3) +
  geom_point(mapping = aes(x = 1999:2018,y= MPGmean_alldat$'8',color = "August/September"), size=1, alpha = 0.3) +
  geom_smooth(mapping = aes(x = 1999:2018,y= MPGmean_alldat$'4',color = "April/May"), size=1, alpha = 0.3, na.rm = TRUE , se= FALSE) +
  geom_smooth(mapping = aes(x = 1999:2018,y= MPGmean_alldat$'6',color = "June/July"), size=1, alpha = 0.3, na.rm = TRUE, se= FALSE) +
  geom_smooth(mapping = aes(x = 1999:2018,y= MPGmean_alldat$'8',color = "August/September"), size=1, alpha = 0.3, na.rm = TRUE, se= FALSE) +
  # scale_colour_brewer(palette = "Blues") +
  scale_colour_manual(values = c("#123F5A", "#3F88A1", "#6DD1E9"), labels = c("April/May","June/July","August/September"))+
  labs(color = 'Months') +
  scale_x_continuous(name="Years", limits=c(1999, 2018))+
  scale_y_continuous(name="NDVI average value in June", limits=c(0, 1), breaks = c(0,0.25,0.5,0.75,1)) +
  scale_x_continuous("Year", labels = as.character(MPGmean_alldat$Year), breaks = MPGmean_alldat$Year)+
  theme(legend.position = "bottom") +
  ExportTheme()
MPGNDVIyearlychange_all

# -------------------- LIPpts -------------------------------------
LIPpix_ltln <- st_coordinates(LIPpts,lat,lon) %>% 
  as.data.frame() %>% 
  tibble::rowid_to_column() %>% 
  rename(ID = rowid)
LIPloc <- LIPpix_ltln[,c("X", "Y")]
LIPext_4 <- extract(projected_raster_4, LIPloc)
LIPLdst7Points_4 <- cbind(LIPpix_ltln, LIPext_4)

LIPext_6 <- extract(projected_raster_6, LIPloc)
LIPLdst7Points_6 <- cbind(LIPpix_ltln, LIPext_6)

LIPext_8 <- extract(projected_raster_8, LIPloc)
LIPLdst7Points_8 <- cbind(LIPpix_ltln, LIPext_8)

LIPData4 <- LIPLdst7Points_4[,4:23]
LIPData6 <- LIPLdst7Points_6[,4:23]
LIPData8 <- LIPLdst7Points_8[,4:23]
# Mean value
LIPmean4 <- colMeans(LIPLdst7Points_4) %>% as.data.frame()
LIPmean4_NY_ave <- LIPmean4[4:23,]
# Max value
LIPmax4 <- colMax(LIPLdst7Points_4) %>% as.data.frame()
LIPmax4_NY_ave <- LIPmax4[4:23,]
# Min value
LIPmin4 <- colMin(LIPLdst7Points_4) %>% as.data.frame()
LIPmin4_NY_ave <- LIPmin4[4:23,]
# std value
LIPsd4 <- colStd(LIPLdst7Points_4) %>% as.data.frame()
LIPsd4_NY_ave <- LIPsd4[4:23,]
# # standardized NDVI every year
# SDNDVI4 <- SDdata(Data4, mean4_NY_ave, sd4_NY_ave)
# standardize NDVI through years
# LIPSDYearsNDVI4 <- SDYear(LIPData4)

######### UNstandardized ##########
# R graph of UNstandardized average NDVI change over time
# UNStandardized Mean value
# 4_5
LIPmean4_NY_ave <- colMeans(LIPLdst7Points_4)[4:23]
LIPNDVIyearlychange4 <- ggplot (data=NULL) + 
  geom_line(mapping = aes(x = 1999:2018,y= LIPmean4_NY_ave)) +
  scale_x_continuous(name="Years", limits=c(1999, 2018))+
  scale_y_continuous(name="Marine Parade Ground NDVI average value in April", limits=c(0, 1)) +
  plotTheme()
LIPNDVIyearlychange4

# 6_7
LIPmean6_NY_ave <- colMeans(LIPLdst7Points_6)[4:23]
LIPNDVIyearlychange6 <- ggplot (data=NULL) + 
  geom_line(mapping = aes(x = 1999:2018,y= LIPmean6_NY_ave)) +
  scale_x_continuous(name="Years", limits=c(1999, 2018))+
  scale_y_continuous(name="Marine Parade Ground NDVI average value in June", limits=c(0, 1)) +
  plotTheme()
LIPNDVIyearlychange6

# 8_9
LIPmean8_NY_ave <- colMeans(LIPLdst7Points_8)[4:23]
LIPNDVIyearlychange8 <- ggplot (data=NULL) + 
  geom_line(mapping = aes(x = 1999:2018,y= LIPmean8_NY_ave)) +
  scale_x_continuous(name="Years", limits=c(1999, 2018))+
  scale_y_continuous(name="Marine Parade Ground NDVI average value in August", limits=c(0, 1)) +
  plotTheme()
LIPNDVIyearlychange8

# tall format for Density Plot
colnames(LIPData4) <- c(1999:2018) 
LIPtallFormat_NDVI_NY4 <- gather(LIPData4, key=year, value=NDVI, '1999':'2018')

colnames(LIPData6) <- c(1999:2018) 
LIPtallFormat_NDVI_NY6 <- gather(LIPData6, key=year, value=NDVI, '1999':'2018')

colnames(LIPData8) <- c(1999:2018) 
LIPtallFormat_NDVI_NY8 <- gather(LIPData8, key=year, value=NDVI, '1999':'2018')

# mean values for geom_vline
LIPmu4 <- cbind(LIPmean4_NY_ave, c(1999:2018)) %>% as.data.frame()
colnames(LIPmu4) <- c("MeanNDVI", "year")
LIPmu4$year <- as.character(LIPmu4$year)

LIPmu6 <- cbind(LIPmean6_NY_ave, c(1999:2018)) %>% as.data.frame()
colnames(LIPmu6) <- c("MeanNDVI", "year")
LIPmu6$year <- as.character(LIPmu6$year)

LIPmu8 <- cbind(LIPmean8_NY_ave, c(1999:2018)) %>% as.data.frame()
colnames(LIPmu8) <- c("MeanNDVI", "year")
LIPmu8$year <- as.character(LIPmu8$year)

# 4_5
LIPNDVIdensity_NY_4 <- ggplot(LIPtallFormat_NDVI_NY4, aes(x=NDVI, color=year)) +
  geom_density(aes(x = NDVI, color = year))+
  scale_colour_manual(values = colsGreen, labels = labs)+
  geom_vline(data=LIPmu4, aes(xintercept=MeanNDVI, color=year),
             linetype="dashed", size = 0.5, alpha = 0.8) +
  labs(title= "How does the Marine Parade Ground NDVI change in April from 1999 to 2018?",
       subtitle= "Yearly NDVI density distribution calculated from Landsat7",
       caption = "Data Source: USGS Landsat 7") +
  xlab("Year") + 
  ylab("NDVI Density") +
  WrapTheme()
LIPNDVIdensity_NY_4
# Facet wrap
LIPNDVIdensity_NY_4 + 
  geom_text(data=LIPmu4, aes(x = MeanNDVI, 
                             label= paste("Mean NDVI:\n", round(MeanNDVI, digits = 3)), 
                             y = 8, colour= year), 
            angle=0, text = element_text(size=8)) +
  facet_wrap( ~ year, ncol = 5)

# 6_7
LIPNDVIdensity_NY_6 <- ggplot(LIPtallFormat_NDVI_NY6, aes(x=NDVI, color=year)) +
  geom_density(aes(x = NDVI, color = year))+
  scale_colour_manual(values = colsGreen, labels = labs)+
  geom_vline(data=LIPmu6, aes(xintercept=MeanNDVI, color=year),
             linetype="dashed", size = 0.5, alpha = 0.8) +
  labs(title= "How does the Marine Parade Ground NDVI change in June from 1999 to 2018?",
       subtitle= "Yearly NDVI density distribution calculated from Landsat7",
       caption = "Data Source: USGS Landsat 7") +
  xlab("Year") + 
  ylab("NDVI Density") +
  WrapTheme()
LIPNDVIdensity_NY_6
# Facet wrap
LIPNDVIdensity_NY_6 + 
  geom_text(data=LIPmu6, aes(x = MeanNDVI, 
                             label= paste("Mean NDVI: \n", round(MeanNDVI, digits = 3)), 
                             y = 50, colour= year), 
            angle=0, text = element_text(size=11)) +
  facet_wrap( ~ year, ncol = 5)

# 8_9
LIPNDVIdensity_NY_8 <- ggplot(LIPtallFormat_NDVI_NY8, aes(x=NDVI, color=year)) +
  geom_density(aes(x = NDVI, color = year))+
  scale_colour_manual(values = colsGreen, labels = labs)+
  geom_vline(data=LIPmu8, aes(xintercept=MeanNDVI, color=year),
             linetype="dashed", size = 0.5, alpha = 0.8) +
  labs(title= "How does the Marine Parade Ground NDVI change in August from 1999 to 2018?",
       subtitle= "Yearly NDVI density distribution calculated from Landsat7",
       caption = "Data Source: USGS Landsat 7") +
  xlab("Year") + 
  ylab("NDVI Density") +
  WrapTheme()
LIPNDVIdensity_NY_8
# Facet wrap
LIPNDVIdensity_NY_8 + 
  geom_text(data=LIPmu8, aes(x = MeanNDVI, 
                             label= paste("Mean NDVI: \n", round(MeanNDVI, digits = 3)), 
                             y = 20, colour= year), 
            angle=0, text = element_text(size=11)) +
  facet_wrap( ~ year, ncol = 5)

# LIP All mean in one plot
LIPmean_alldat <- cbind(LIPmean4_NY_ave,LIPmean6_NY_ave,LIPmean8_NY_ave) %>% 
  as.data.frame() %>% 
  dplyr::mutate(year = 1999:2018)
colnames(LIPmean_alldat) <- c("4", "6", "8", "Year")

LIPNDVIyearlychange_all <- ggplot (data=LIPmean_alldat) + 
  geom_line(mapping = aes(x = 1999:2018,y= LIPmean_alldat$'4',color = "April/May"), size=0.4, alpha = 0.3, linetype = "longdash", na.rm = TRUE , se= FALSE) +
  geom_line(mapping = aes(x = 1999:2018,y= LIPmean_alldat$'6',color = "June/July"), size=0.4, alpha = 0.3, linetype = "longdash", na.rm = TRUE, se= FALSE) +
  geom_line(mapping = aes(x = 1999:2018,y= LIPmean_alldat$'8',color = "August/September"), size=0.4, alpha = 0.3, linetype = "longdash", na.rm = TRUE, se= FALSE) +
  geom_point(mapping = aes(x = 1999:2018,y= LIPmean_alldat$'4',color = "April/May"), size=1, alpha = 0.3) +
  geom_point(mapping = aes(x = 1999:2018,y= LIPmean_alldat$'6',color = "June/July"), size=1, alpha = 0.3) +
  geom_point(mapping = aes(x = 1999:2018,y= LIPmean_alldat$'8',color = "August/September"), size=1, alpha = 0.3) +
  geom_smooth(mapping = aes(x = 1999:2018,y= LIPmean_alldat$'4',color = "April/May"), size=1, alpha = 0.3, na.rm = TRUE , se= FALSE) +
  geom_smooth(mapping = aes(x = 1999:2018,y= LIPmean_alldat$'6',color = "June/July"), size=1, alpha = 0.3, na.rm = TRUE, se= FALSE) +
  geom_smooth(mapping = aes(x = 1999:2018,y= LIPmean_alldat$'8',color = "August/September"), size=1, alpha = 0.3, na.rm = TRUE, se= FALSE) +
  # scale_colour_brewer(palette = "Blues") +
  scale_colour_manual(values = c("#123F5A", "#3F88A1", "#6DD1E9"), labels = c("April/May","June/July","August/September"))+
  labs(color = 'Months') +
  scale_x_continuous(name="Years", limits=c(1999, 2018))+
  scale_y_continuous(name="NDVI average value in June", limits=c(0, 1), breaks = c(0,0.25,0.5,0.75,1)) +
  scale_x_continuous("Year", labels = as.character(LIPmean_alldat$Year), breaks = LIPmean_alldat$Year)+
  theme(legend.position = "bottom") +
  ExportTheme()
LIPNDVIyearlychange_all

# -------------------- CPpts -------------------------------------
CPpix_ltln <- st_coordinates(CPpts,lat,lon) %>% 
  as.data.frame() %>% 
  tibble::rowid_to_column() %>% 
  rename(ID = rowid)
CPloc <- CPpix_ltln[,c("X", "Y")]
CPext_4 <- extract(projected_raster_4, CPloc)
CPLdst7Points_4 <- cbind(CPpix_ltln, CPext_4)

CPext_6 <- extract(projected_raster_6, CPloc)
CPLdst7Points_6 <- cbind(CPpix_ltln, CPext_6)

CPext_8 <- extract(projected_raster_8, CPloc)
CPLdst7Points_8 <- cbind(CPpix_ltln, CPext_8)

CPData4 <- CPLdst7Points_4[,4:23]
CPData6 <- CPLdst7Points_6[,4:23]
CPData8 <- CPLdst7Points_8[,4:23]
# Mean value
CPmean4 <- colMeans(CPLdst7Points_4) %>% as.data.frame()
CPmean4_NY_ave <- CPmean4[4:23,]
# Max value
CPmax4 <- colMax(CPLdst7Points_4) %>% as.data.frame()
CPmax4_NY_ave <- CPmax4[4:23,]
# Min value
CPmin4 <- colMin(CPLdst7Points_4) %>% as.data.frame()
CPmin4_NY_ave <- CPmin4[4:23,]
# std value
CPsd4 <- colStd(CPLdst7Points_4) %>% as.data.frame()
CPsd4_NY_ave <- CPsd4[4:23,]
# # standardized NDVI every year
# SDNDVI4 <- SDdata(Data4, mean4_NY_ave, sd4_NY_ave)
# standardize NDVI through years
# CPSDYearsNDVI4 <- SDYear(CPData4)

######### UNstandardized ##########
# R graph of UNstandardized average NDVI change over time
# UNStandardized Mean value
# 4_5
CPmean4_NY_ave <- colMeans(CPLdst7Points_4)[4:23]
CPNDVIyearlychange4 <- ggplot (data=NULL) + 
  geom_line(mapping = aes(x = 1999:2018,y= CPmean4_NY_ave)) +
  scale_x_continuous(name="Years", limits=c(1999, 2018))+
  scale_y_continuous(name="Marine Parade Ground NDVI average value in April", limits=c(0, 1)) +
  plotTheme()
CPNDVIyearlychange4

# 6_7
CPmean6_NY_ave <- colMeans(CPLdst7Points_6)[4:23]
CPNDVIyearlychange6 <- ggplot (data=NULL) + 
  geom_line(mapping = aes(x = 1999:2018,y= CPmean6_NY_ave)) +
  scale_x_continuous(name="Years", limits=c(1999, 2018))+
  scale_y_continuous(name="Marine Parade Ground NDVI average value in June", limits=c(0, 1)) +
  plotTheme()
CPNDVIyearlychange6

# 8_9
CPmean8_NY_ave <- colMeans(CPLdst7Points_8)[4:23]
CPNDVIyearlychange8 <- ggplot (data=NULL) + 
  geom_line(mapping = aes(x = 1999:2018,y= CPmean8_NY_ave)) +
  scale_x_continuous(name="Years", limits=c(1999, 2018))+
  scale_y_continuous(name="Marine Parade Ground NDVI average value in August", limits=c(0, 1)) +
  plotTheme()
CPNDVIyearlychange8

# tall format for Density Plot
colnames(CPData4) <- c(1999:2018) 
CPtallFormat_NDVI_NY4 <- gather(CPData4, key=year, value=NDVI, '1999':'2018')

colnames(CPData6) <- c(1999:2018) 
CPtallFormat_NDVI_NY6 <- gather(CPData6, key=year, value=NDVI, '1999':'2018')

colnames(CPData8) <- c(1999:2018) 
CPtallFormat_NDVI_NY8 <- gather(CPData8, key=year, value=NDVI, '1999':'2018')

# mean values for geom_vline
CPmu4 <- cbind(CPmean4_NY_ave, c(1999:2018)) %>% as.data.frame()
colnames(CPmu4) <- c("MeanNDVI", "year")
CPmu4$year <- as.character(CPmu4$year)

CPmu6 <- cbind(CPmean6_NY_ave, c(1999:2018)) %>% as.data.frame()
colnames(CPmu6) <- c("MeanNDVI", "year")
CPmu6$year <- as.character(CPmu6$year)

CPmu8 <- cbind(CPmean8_NY_ave, c(1999:2018)) %>% as.data.frame()
colnames(CPmu8) <- c("MeanNDVI", "year")
CPmu8$year <- as.character(CPmu8$year)

# 4_5
CPNDVIdensity_NY_4 <- ggplot(CPtallFormat_NDVI_NY4, aes(x=NDVI, color=year)) +
  geom_density(aes(x = NDVI, color = year))+
  scale_colour_manual(values = colsGreen, labels = labs)+
  geom_vline(data=CPmu4, aes(xintercept=MeanNDVI, color=year),
             linetype="dashed", size = 0.5, alpha = 0.8) +
  labs(title= "How does the Marine Parade Ground NDVI change in April from 1999 to 2018?",
       subtitle= "Yearly NDVI density distribution calculated from Landsat7",
       caption = "Data Source: USGS Landsat 7") +
  xlab("Year") + 
  ylab("NDVI Density") +
  WrapTheme()
CPNDVIdensity_NY_4
# Facet wrap
CPNDVIdensity_NY_4 + 
  geom_text(data=CPmu4, aes(x = MeanNDVI, 
                            label= paste("Mean NDVI:\n", round(MeanNDVI, digits = 3)), 
                            y = 12, colour= year), 
            angle=0, text = element_text(size=8)) +
  facet_wrap( ~ year, ncol = 5)

# 6_7
CPNDVIdensity_NY_6 <- ggplot(CPtallFormat_NDVI_NY6, aes(x=NDVI, color=year)) +
  geom_density(aes(x = NDVI, color = year))+
  scale_colour_manual(values = colsGreen, labels = labs)+
  geom_vline(data=CPmu6, aes(xintercept=MeanNDVI, color=year),
             linetype="dashed", size = 0.5, alpha = 0.8) +
  labs(title= "How does the Marine Parade Ground NDVI change in June from 1999 to 2018?",
       subtitle= "Yearly NDVI density distribution calculated from Landsat7",
       caption = "Data Source: USGS Landsat 7") +
  xlab("Year") + 
  ylab("NDVI Density") +
  WrapTheme()
CPNDVIdensity_NY_6
# Facet wrap
CPNDVIdensity_NY_6 + 
  geom_text(data=CPmu6, aes(x = MeanNDVI, 
                            label= paste("Mean NDVI: \n", round(MeanNDVI, digits = 3)), 
                            y = 150, colour= year), 
            angle=0, text = element_text(size=11)) +
  facet_wrap( ~ year, ncol = 5)

# 8_9
CPNDVIdensity_NY_8 <- ggplot(CPtallFormat_NDVI_NY8, aes(x=NDVI, color=year)) +
  geom_density(aes(x = NDVI, color = year))+
  scale_colour_manual(values = colsGreen, labels = labs)+
  geom_vline(data=CPmu8, aes(xintercept=MeanNDVI, color=year),
             linetype="dashed", size = 0.5, alpha = 0.8) +
  labs(title= "How does the Marine Parade Ground NDVI change in August from 1999 to 2018?",
       subtitle= "Yearly NDVI density distribution calculated from Landsat7",
       caption = "Data Source: USGS Landsat 7") +
  xlab("Year") + 
  ylab("NDVI Density") +
  WrapTheme()
CPNDVIdensity_NY_8
# Facet wrap
CPNDVIdensity_NY_8 + 
  geom_text(data=CPmu8, aes(x = MeanNDVI, 
                            label= paste("Mean NDVI: \n", round(MeanNDVI, digits = 3)), 
                            y = 20, colour= year), 
            angle=0, text = element_text(size=11)) +
  facet_wrap( ~ year, ncol = 5)

# CP All mean in one plot
CPmean_alldat <- cbind(CPmean4_NY_ave,CPmean6_NY_ave,CPmean8_NY_ave) %>% 
  as.data.frame() %>% 
  dplyr::mutate(year = 1999:2018)
colnames(CPmean_alldat) <- c("4", "6", "8", "Year")

CPNDVIyearlychange_all <- ggplot (data=CPmean_alldat) + 
  geom_line(mapping = aes(x = 1999:2018,y= CPmean_alldat$'4',color = "April/May"), size=0.4, alpha = 0.3, linetype = "longdash", na.rm = TRUE , se= FALSE) +
  geom_line(mapping = aes(x = 1999:2018,y= CPmean_alldat$'6',color = "June/July"), size=0.4, alpha = 0.3, linetype = "longdash", na.rm = TRUE, se= FALSE) +
  geom_line(mapping = aes(x = 1999:2018,y= CPmean_alldat$'8',color = "August/September"), size=0.4, alpha = 0.3, linetype = "longdash", na.rm = TRUE, se= FALSE) +
  geom_point(mapping = aes(x = 1999:2018,y= CPmean_alldat$'4',color = "April/May"), size=1, alpha = 0.3) +
  geom_point(mapping = aes(x = 1999:2018,y= CPmean_alldat$'6',color = "June/July"), size=1, alpha = 0.3) +
  geom_point(mapping = aes(x = 1999:2018,y= CPmean_alldat$'8',color = "August/September"), size=1, alpha = 0.3) +
  geom_smooth(mapping = aes(x = 1999:2018,y= CPmean_alldat$'4',color = "April/May"), size=1, alpha = 0.3, na.rm = TRUE , se= FALSE) +
  geom_smooth(mapping = aes(x = 1999:2018,y= CPmean_alldat$'6',color = "June/July"), size=1, alpha = 0.3, na.rm = TRUE, se= FALSE) +
  geom_smooth(mapping = aes(x = 1999:2018,y= CPmean_alldat$'8',color = "August/September"), size=1, alpha = 0.3, na.rm = TRUE, se= FALSE) +
  # scale_colour_brewer(palette = "Blues") +
  scale_colour_manual(values = c("#123F5A", "#3F88A1", "#6DD1E9"), labels = c("April/May","June/July","August/September"))+
  labs(color = 'Months') +
  scale_x_continuous(name="Years", limits=c(1999, 2018))+
  scale_y_continuous(name="NDVI average value in June", limits=c(0, 1), breaks = c(0,0.25,0.5,0.75,1)) +
  scale_x_continuous("Year", labels = as.character(CPmean_alldat$Year), breaks = CPmean_alldat$Year)+
  theme(legend.position = "bottom") +
  ExportTheme()
CPNDVIyearlychange_all


# -------------------- CGpts -------------------------------------
CGpix_ltln <- st_coordinates(CGpts,lat,lon) %>% 
  as.data.frame() %>% 
  tibble::rowid_to_column() %>% 
  rename(ID = rowid)
CGloc <- CGpix_ltln[,c("X", "Y")]
CGext_4 <- extract(projected_raster_4, CGloc)
CGLdst7Points_4 <- cbind(CGpix_ltln, CGext_4)

CGext_6 <- extract(projected_raster_6, CGloc)
CGLdst7Points_6 <- cbind(CGpix_ltln, CGext_6)

CGext_8 <- extract(projected_raster_8, CGloc)
CGLdst7Points_8 <- cbind(CGpix_ltln, CGext_8)

CGData4 <- CGLdst7Points_4[,4:23]
CGData6 <- CGLdst7Points_6[,4:23]
CGData8 <- CGLdst7Points_8[,4:23]
# Mean value
CGmean4 <- colMeans(CGLdst7Points_4) %>% as.data.frame()
CGmean4_NY_ave <- CGmean4[4:23,]
# Max value
CGmax4 <- colMax(CGLdst7Points_4) %>% as.data.frame()
CGmax4_NY_ave <- CGmax4[4:23,]
# Min value
CGmin4 <- colMin(CGLdst7Points_4) %>% as.data.frame()
CGmin4_NY_ave <- CGmin4[4:23,]
# std value
CGsd4 <- colStd(CGLdst7Points_4) %>% as.data.frame()
CGsd4_NY_ave <- CGsd4[4:23,]
# # standardized NDVI every year
# SDNDVI4 <- SDdata(Data4, mean4_NY_ave, sd4_NY_ave)
# standardize NDVI through years
# CGSDYearsNDVI4 <- SDYear(CGData4)

######### UNstandardized ##########
# R graph of UNstandardized average NDVI change over time
# UNStandardized Mean value
# 4_5
CGmean4_NY_ave <- colMeans(CGLdst7Points_4)[4:23]
CGNDVIyearlychange4 <- ggplot (data=NULL) + 
  geom_line(mapping = aes(x = 1999:2018,y= CGmean4_NY_ave)) +
  scale_x_continuous(name="Years", limits=c(1999, 2018))+
  scale_y_continuous(name="Marine Parade Ground NDVI average value in April", limits=c(0, 1)) +
  plotTheme()
CGNDVIyearlychange4

# 6_7
CGmean6_NY_ave <- colMeans(CGLdst7Points_6)[4:23]
CGNDVIyearlychange6 <- ggplot (data=NULL) + 
  geom_line(mapping = aes(x = 1999:2018,y= CGmean6_NY_ave)) +
  scale_x_continuous(name="Years", limits=c(1999, 2018))+
  scale_y_continuous(name="Marine Parade Ground NDVI average value in June", limits=c(0, 1)) +
  plotTheme()
CGNDVIyearlychange6

# 8_9
CGmean8_NY_ave <- colMeans(CGLdst7Points_8)[4:23]
CGNDVIyearlychange8 <- ggplot (data=NULL) + 
  geom_line(mapping = aes(x = 1999:2018,y= CGmean8_NY_ave)) +
  scale_x_continuous(name="Years", limits=c(1999, 2018))+
  scale_y_continuous(name="Marine Parade Ground NDVI average value in August", limits=c(0, 1)) +
  plotTheme()
CGNDVIyearlychange8

# tall format for Density Plot
colnames(CGData4) <- c(1999:2018) 
CGtallFormat_NDVI_NY4 <- gather(CGData4, key=year, value=NDVI, '1999':'2018')

colnames(CGData6) <- c(1999:2018) 
CGtallFormat_NDVI_NY6 <- gather(CGData6, key=year, value=NDVI, '1999':'2018')

colnames(CGData8) <- c(1999:2018) 
CGtallFormat_NDVI_NY8 <- gather(CGData8, key=year, value=NDVI, '1999':'2018')

# mean values for geom_vline
CGmu4 <- cbind(CGmean4_NY_ave, c(1999:2018)) %>% as.data.frame()
colnames(CGmu4) <- c("MeanNDVI", "year")
CGmu4$year <- as.character(CGmu4$year)

CGmu6 <- cbind(CGmean6_NY_ave, c(1999:2018)) %>% as.data.frame()
colnames(CGmu6) <- c("MeanNDVI", "year")
CGmu6$year <- as.character(CGmu6$year)

CGmu8 <- cbind(CGmean8_NY_ave, c(1999:2018)) %>% as.data.frame()
colnames(CGmu8) <- c("MeanNDVI", "year")
CGmu8$year <- as.character(CGmu8$year)

# 4_5
CGNDVIdensity_NY_4 <- ggplot(CGtallFormat_NDVI_NY4, aes(x=NDVI, color=year)) +
  geom_density(aes(x = NDVI, color = year))+
  scale_colour_manual(values = colsGreen, labels = labs)+
  geom_vline(data=CGmu4, aes(xintercept=MeanNDVI, color=year),
             linetype="dashed", size = 0.5, alpha = 0.8) +
  labs(title= "How does the Marine Parade Ground NDVI change in April from 1999 to 2018?",
       subtitle= "Yearly NDVI density distribution calculated from Landsat7",
       caption = "Data Source: USGS Landsat 7") +
  xlab("Year") + 
  ylab("NDVI Density") +
  WrapTheme()
CGNDVIdensity_NY_4
# Facet wrap
CGNDVIdensity_NY_4 + 
  geom_text(data=CGmu4, aes(x = MeanNDVI, 
                            label= paste("Mean NDVI:\n", round(MeanNDVI, digits = 3)), 
                            y = 25, colour= year), 
            angle=0, text = element_text(size=8)) +
  facet_wrap( ~ year, ncol = 5)

# 6_7
CGNDVIdensity_NY_6 <- ggplot(CGtallFormat_NDVI_NY6, aes(x=NDVI, color=year)) +
  geom_density(aes(x = NDVI, color = year))+
  scale_colour_manual(values = colsGreen, labels = labs)+
  geom_vline(data=CGmu6, aes(xintercept=MeanNDVI, color=year),
             linetype="dashed", size = 0.5, alpha = 0.8) +
  labs(title= "How does the Marine Parade Ground NDVI change in June from 1999 to 2018?",
       subtitle= "Yearly NDVI density distribution calculated from Landsat7",
       caption = "Data Source: USGS Landsat 7") +
  xlab("Year") + 
  ylab("NDVI Density") +
  WrapTheme()
CGNDVIdensity_NY_6
# Facet wrap
CGNDVIdensity_NY_6 + 
  geom_text(data=CGmu6, aes(x = MeanNDVI, 
                            label= paste("Mean NDVI: \n", round(MeanNDVI, digits = 3)), 
                            y = 30, colour= year), 
            angle=0, text = element_text(size=11)) +
  facet_wrap( ~ year, ncol = 5)

# 8_9
CGNDVIdensity_NY_8 <- ggplot(CGtallFormat_NDVI_NY8, aes(x=NDVI, color=year)) +
  geom_density(aes(x = NDVI, color = year))+
  scale_colour_manual(values = colsGreen, labels = labs)+
  geom_vline(data=CGmu8, aes(xintercept=MeanNDVI, color=year),
             linetype="dashed", size = 0.5, alpha = 0.8) +
  labs(title= "How does the Marine Parade Ground NDVI change in August from 1999 to 2018?",
       subtitle= "Yearly NDVI density distribution calculated from Landsat7",
       caption = "Data Source: USGS Landsat 7") +
  xlab("Year") + 
  ylab("NDVI Density") +
  WrapTheme()
CGNDVIdensity_NY_8
# Facet wrap
CGNDVIdensity_NY_8 + 
  geom_text(data=CGmu8, aes(x = MeanNDVI, 
                            label= paste("Mean NDVI: \n", round(MeanNDVI, digits = 3)), 
                            y = 40, colour= year), 
            angle=0, text = element_text(size=11)) +
  facet_wrap( ~ year, ncol = 5)

# CG All mean in one plot
CGmean_alldat <- cbind(CGmean4_NY_ave,CGmean6_NY_ave,CGmean8_NY_ave) %>% 
  as.data.frame() %>% 
  dplyr::mutate(year = 1999:2018)
colnames(CGmean_alldat) <- c("4", "6", "8", "Year")

CGNDVIyearlychange_all <- ggplot (data=CGmean_alldat) + 
  geom_line(mapping = aes(x = 1999:2018,y= CGmean_alldat$'4',color = "April/May"), size=0.4, alpha = 0.3, linetype = "longdash", na.rm = TRUE , se= FALSE) +
  geom_line(mapping = aes(x = 1999:2018,y= CGmean_alldat$'6',color = "June/July"), size=0.4, alpha = 0.3, linetype = "longdash", na.rm = TRUE, se= FALSE) +
  geom_line(mapping = aes(x = 1999:2018,y= CGmean_alldat$'8',color = "August/September"), size=0.4, alpha = 0.3, linetype = "longdash", na.rm = TRUE, se= FALSE) +
  geom_point(mapping = aes(x = 1999:2018,y= CGmean_alldat$'4',color = "April/May"), size=1, alpha = 0.3) +
  geom_point(mapping = aes(x = 1999:2018,y= CGmean_alldat$'6',color = "June/July"), size=1, alpha = 0.3) +
  geom_point(mapping = aes(x = 1999:2018,y= CGmean_alldat$'8',color = "August/September"), size=1, alpha = 0.3) +
  geom_smooth(mapping = aes(x = 1999:2018,y= CGmean_alldat$'4',color = "April/May"), size=1, alpha = 0.3, na.rm = TRUE , se= FALSE) +
  geom_smooth(mapping = aes(x = 1999:2018,y= CGmean_alldat$'6',color = "June/July"), size=1, alpha = 0.3, na.rm = TRUE, se= FALSE) +
  geom_smooth(mapping = aes(x = 1999:2018,y= CGmean_alldat$'8',color = "August/September"), size=1, alpha = 0.3, na.rm = TRUE, se= FALSE) +
  # scale_colour_brewer(palette = "Blues") +
  scale_colour_manual(values = c("#123F5A", "#3F88A1", "#6DD1E9"), labels = c("April/May","June/July","August/September"))+
  labs(color = 'Months') +
  scale_x_continuous(name="Years", limits=c(1999, 2018))+
  scale_y_continuous(name="NDVI average value in June", limits=c(0, 1), breaks = c(0,0.25,0.5,0.75,1)) +
  scale_x_continuous("Year", labels = as.character(CGmean_alldat$Year), breaks = CGmean_alldat$Year)+
  theme(legend.position = "bottom") +
  ExportTheme()
CGNDVIyearlychange_all


