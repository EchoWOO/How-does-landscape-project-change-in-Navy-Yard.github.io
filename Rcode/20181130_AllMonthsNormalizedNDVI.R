# Percentage of getting greener for each of the park
PerGreener <- function(data){
  return ((colMeans(data)[ncol(data)] - colMeans(data)[1])/colMeans(data)[1])
}

MPGNDVIGreener <- PerGreener(MPGData_All)
LIPNDVIGreener <- PerGreener(LIPData_All)
CPNDVIGreener <- PerGreener(CPData_All)
CGNDVIGreener <- PerGreener(CGData_All)

################# MPG #################
library(abind)
# Calculate all the mean values from April - May - Jun - Jul - Aug - Sep
arr = abind(MPGData4, MPGData6, MPGData8, along = 3)
# Mean value
MPGData_All <- rowMeans(arr, dims = 2, na.rm = TRUE)
# standardize NDVI through years
MPGSDYearsNDVI_all <- SDYear(MPGData_All)

######### UNstandardized ##########
# R graph of UNstandardized average NDVI change over time
# UNStandardized Mean value
# 4_5_6_7_8_9
# MPGmean_all_NY_ave <- colMeans(MPGData_All)
MPGNDVI_all_yearlychange <- ggplot (data=NULL) + 
  geom_point(mapping = aes(x = 1999:2018,y= colMeans(MPGData_All)), size=1, alpha = 0.5, color = "deepskyblue4") +
  geom_smooth(mapping = aes(x = 1999:2018,y= colMeans(MPGData_All)), color = "deepskyblue4", na.rm = TRUE , se= TRUE) +
  scale_x_continuous(name="Years", limits=c(1999, 2018))+
  scale_y_continuous(name="NDVI average value from April to September", limits=c(0, 1)) +
  labs(title= "How does the Marine Parade Ground NDVI change from 1999 to 2018?",
       subtitle= "yearly NDVI average value from April to September",
       caption = "Data Source: USGS Landsat 7") +
  theme(legend.position = "bottom") +
  plotTheme()
MPGNDVI_all_yearlychange

# 4_5_6_7_8_9   All summarized NDVI
# Density plot
MPGmean_all_NY_ave <- colMeans(MPGData_All) %>% as.data.frame()
MPGmean_all_NY_ave <- cbind(MPGmean_all_NY_ave, c(1999:2018)) %>% as.data.frame()
colnames(MPGmean_all_NY_ave) <- c("MeanNDVI", "year")
MPGmean_all_NY_ave$year <- as.character(MPGmean_all_NY_ave$year)

MPGtallFormat_NDVI_NY_all <- gather(MPGData_All, key=year, value=NDVI, '1999':'2018')

MPGNDVIdensity_NY_all <- ggplot(MPGtallFormat_NDVI_NY_all, aes(x=NDVI, color=year)) +
  geom_density(aes(x = NDVI, color = year))+
  scale_colour_manual(values = colsGreen, labels = labs)+
  labs(title= "How does the Marine Parade Ground NDVI change from 1999 to 2018?",
       subtitle= "Yearly NDVI density distribution calculated from Landsat7",
       caption = "Data Source: USGS Landsat 7") +
  xlab("Year") + 
  ylab("NDVI Density") +
  WrapTheme()
MPGNDVIdensity_NY_all

# Facet wrap
MPGNDVIdensity_NY_all + 
  geom_vline(data=MPGmean_all_NY_ave, aes(xintercept=MeanNDVI, color=year),
             linetype="dashed", size = 0.5, alpha = 0.8) +
  geom_text(data=MPGmean_all_NY_ave, aes(x = MeanNDVI, 
                             label= paste("Mean: \n", round(MeanNDVI, digits = 3)), 
                             y = 13, colour= year), 
            angle=0, text = element_text(size=11)) +
  facet_wrap( ~ year, ncol = 5)

############## After Standardize ###########
# MPGSDYearsNDVI_all
MPGSDmean_all_NY_ave <- colMeans(MPGSDYearsNDVI_all) %>% as.data.frame()
MPGSDmean_all_NY_ave <- cbind(MPGSDmean_all_NY_ave, c(1999:2018)) %>% as.data.frame()
colnames(MPGSDmean_all_NY_ave) <- c("MeanNDVI", "year")
MPGSDmean_all_NY_ave$year <- as.character(MPGSDmean_all_NY_ave$year)

MPGSDtallFormat_NDVI_NY_all <- gather(MPGSDYearsNDVI_all, key=year, value=NDVI, '1999':'2018')

MPGSDNDVIdensity_NY_all <- ggplot(MPGSDtallFormat_NDVI_NY_all, aes(x=NDVI, color=year)) +
  geom_density(aes(x = NDVI, color = year))+
  scale_colour_manual(values = colsGreen, labels = labs)+
  labs(title= "How does the Marine Parade Ground NDVI change from 1999 to 2018?",
       subtitle= "Standardized Yearly NDVI density distribution calculated from Landsat7",
       caption = "Data Source: USGS Landsat 7") +
  xlab("Year") + 
  ylab("NDVI Density") +
  WrapTheme()
MPGSDNDVIdensity_NY_all

# Facet wrap
MPGSDNDVIdensity_NY_all + 
  geom_vline(data=MPGSDmean_all_NY_ave, aes(xintercept=MeanNDVI, color=year),
             linetype="dashed", size = 0.5, alpha = 0.8) +
  geom_text(data=MPGSDmean_all_NY_ave, aes(x = MeanNDVI, 
                                         label= paste("Mean: \n", round(MeanNDVI, digits = 3)), 
                                         y = 13, colour= year), 
            angle=0, text = element_text(size=11)) +
  facet_wrap( ~ year, ncol = 5)


################ LIP ################
library(abind)
# Calculate all the mean values from April - May - Jun - Jul - Aug - Sep
LIParr = abind(LIPData4, LIPData6, LIPData8, along = 3)
LIParr
# Mean value
LIPData_All <- rowMeans(LIParr, dims = 2, na.rm = TRUE) %>% as.data.frame()
# standardize NDVI through years
LIPSDYearsNDVI_all <- SDYear(LIPData_All)


######### UNstandardized ##########
# R graph of UNstandardized average NDVI change over time
# UNStandardized Mean value
# 4_5_6_7_8_9
# LIPmean_all_NY_ave <- colMeans(LIPData_All)
LIPNDVI_all_yearlychange <- ggplot (data=NULL) + 
  geom_point(mapping = aes(x = 1999:2018,y= colMeans(LIPData_All)), size=1, alpha = 0.5, color = "deepskyblue4") +
  geom_smooth(mapping = aes(x = 1999:2018,y= colMeans(LIPData_All)), color = "deepskyblue4", na.rm = TRUE , se= TRUE) +
  scale_x_continuous(name="Years", limits=c(1999, 2018))+
  scale_y_continuous(name="NDVI average value from April to September", limits=c(0, 1)) +
  labs(title= "How does the League Island Park NDVI change from 1999 to 2018?",
       subtitle= "yearly NDVI average value from April to September",
       caption = "Data Source: USGS Landsat 7") +
  theme(legend.position = "bottom") +
  plotTheme()
LIPNDVI_all_yearlychange

# 4_5_6_7_8_9   All summarized NDVI
# Density plot
LIPmean_all_NY_ave <- colMeans(LIPData_All) %>% as.data.frame()
LIPmean_all_NY_ave <- cbind(LIPmean_all_NY_ave, c(1999:2018)) %>% as.data.frame()
colnames(LIPmean_all_NY_ave) <- c("MeanNDVI", "year")
LIPmean_all_NY_ave$year <- as.character(LIPmean_all_NY_ave$year)

LIPtallFormat_NDVI_NY_all <- gather(LIPData_All, key=year, value=NDVI, '1999':'2018')

LIPNDVIdensity_NY_all <- ggplot(LIPtallFormat_NDVI_NY_all, aes(x=NDVI, color=year)) +
  geom_density(aes(x = NDVI, color = year))+
  scale_colour_manual(values = colsGreen, labels = labs)+
  labs(title= "How does the League Island Park NDVI change from 1999 to 2018?",
       subtitle= "Yearly NDVI density distribution calculated from Landsat7",
       caption = "Data Source: USGS Landsat 7") +
  xlab("Year") + 
  ylab("NDVI Density") +
  WrapTheme()
LIPNDVIdensity_NY_all

# Facet wrap
LIPNDVIdensity_NY_all + 
  geom_vline(data=LIPmean_all_NY_ave, aes(xintercept=MeanNDVI, color=year),
             linetype="dashed", size = 0.5, alpha = 0.8) +
  geom_text(data=LIPmean_all_NY_ave, aes(x = MeanNDVI, 
                                         label= paste("Mean: \n", round(MeanNDVI, digits = 3)), 
                                         y = 13, colour= year), 
            angle=0, text = element_text(size=11)) +
  facet_wrap( ~ year, ncol = 5)

############## After Standardize ###########
# LIPSDYearsNDVI_all
LIPSDmean_all_NY_ave <- colMeans(LIPSDYearsNDVI_all) %>% as.data.frame()
LIPSDmean_all_NY_ave <- cbind(LIPSDmean_all_NY_ave, c(1999:2018)) %>% as.data.frame()
colnames(LIPSDmean_all_NY_ave) <- c("MeanNDVI", "year")
LIPSDmean_all_NY_ave$year <- as.character(LIPSDmean_all_NY_ave$year)

LIPSDtallFormat_NDVI_NY_all <- gather(LIPSDYearsNDVI_all, key=year, value=NDVI, '1999':'2018')

LIPSDNDVIdensity_NY_all <- ggplot(LIPSDtallFormat_NDVI_NY_all, aes(x=NDVI, color=year)) +
  geom_density(aes(x = NDVI, color = year))+
  scale_colour_manual(values = colsGreen, labels = labs)+
  labs(title= "How does the League Island Park NDVI change from 1999 to 2018?",
       subtitle= "Standardized Yearly NDVI density distribution calculated from Landsat7",
       caption = "Data Source: USGS Landsat 7") +
  xlab("Year") + 
  ylab("NDVI Density") +
  WrapTheme()
LIPSDNDVIdensity_NY_all

# Facet wrap
LIPSDNDVIdensity_NY_all + 
  geom_vline(data=LIPSDmean_all_NY_ave, aes(xintercept=MeanNDVI, color=year),
             linetype="dashed", size = 0.5, alpha = 0.8) +
  geom_text(data=LIPSDmean_all_NY_ave, aes(x = MeanNDVI, 
                                           label= paste("Mean: \n", round(MeanNDVI, digits = 3)), 
                                           y = 13, colour= year), 
            angle=0, text = element_text(size=11)) +
  facet_wrap( ~ year, ncol = 5)


################# CP ################# 
# Calculate all the mean values from April - May - Jun - Jul - Aug - Sep
CParr = abind(CPData4, CPData6, CPData8, along = 3)
CParr
# Mean value
CPData_All <- rowMeans(CParr, dims = 2, na.rm = TRUE) %>% as.data.frame()
# standardize NDVI through years
CPSDYearsNDVI_all <- SDYear(CPData_All)


######### UNstandardized ##########
# R graph of UNstandardized average NDVI change over time
# UNStandardized Mean value
# 4_5_6_7_8_9
# CPmean_all_NY_ave <- colMeans(CPData_All)
CPNDVI_all_yearlychange <- ggplot (data=NULL) + 
  geom_point(mapping = aes(x = 1999:2018,y= colMeans(CPData_All)), size=1, alpha = 0.5, color = "deepskyblue4") +
  geom_smooth(mapping = aes(x = 1999:2018,y= colMeans(CPData_All)), color = "deepskyblue4", na.rm = TRUE , se= TRUE) +
  scale_x_continuous(name="Years", limits=c(1999, 2018))+
  scale_y_continuous(name="NDVI average value from April to September", limits=c(0, 1)) +
  labs(title= "How does the Crescent Park NDVI change from 1999 to 2018?",
       subtitle= "yearly NDVI average value from April to September",
       caption = "Data Source: USGS Landsat 7") +
  theme(legend.position = "bottom") +
  plotTheme()
CPNDVI_all_yearlychange

# 4_5_6_7_8_9   All summarized NDVI
# Density plot
CPmean_all_NY_ave <- colMeans(CPData_All) %>% as.data.frame()
CPmean_all_NY_ave <- cbind(CPmean_all_NY_ave, c(1999:2018)) %>% as.data.frame()
colnames(CPmean_all_NY_ave) <- c("MeanNDVI", "year")
CPmean_all_NY_ave$year <- as.character(CPmean_all_NY_ave$year)

CPtallFormat_NDVI_NY_all <- gather(CPData_All, key=year, value=NDVI, '1999':'2018')

CPNDVIdensity_NY_all <- ggplot(CPtallFormat_NDVI_NY_all, aes(x=NDVI, color=year)) +
  geom_density(aes(x = NDVI, color = year))+
  scale_colour_manual(values = colsGreen, labels = labs)+
  labs(title= "How does the Crescent Park NDVI change from 1999 to 2018?",
       subtitle= "Yearly NDVI density distribution calculated from Landsat7",
       caption = "Data Source: USGS Landsat 7") +
  xlab("Year") + 
  ylab("NDVI Density") +
  WrapTheme()
CPNDVIdensity_NY_all

# Facet wrap
CPNDVIdensity_NY_all + 
  geom_vline(data=CPmean_all_NY_ave, aes(xintercept=MeanNDVI, color=year),
             linetype="dashed", size = 0.5, alpha = 0.8) +
  geom_text(data=CPmean_all_NY_ave, aes(x = MeanNDVI, 
                                        label= paste("Mean: \n", round(MeanNDVI, digits = 3)), 
                                        y = 13, colour= year), 
            angle=0, text = element_text(size=11)) +
  facet_wrap( ~ year, ncol = 5)

############## After Standardize ###########
# CPSDYearsNDVI_all
CPSDmean_all_NY_ave <- colMeans(CPSDYearsNDVI_all) %>% as.data.frame()
CPSDmean_all_NY_ave <- cbind(CPSDmean_all_NY_ave, c(1999:2018)) %>% as.data.frame()
colnames(CPSDmean_all_NY_ave) <- c("MeanNDVI", "year")
CPSDmean_all_NY_ave$year <- as.character(CPSDmean_all_NY_ave$year)

CPSDtallFormat_NDVI_NY_all <- gather(CPSDYearsNDVI_all, key=year, value=NDVI, '1999':'2018')

CPSDNDVIdensity_NY_all <- ggplot(CPSDtallFormat_NDVI_NY_all, aes(x=NDVI, color=year)) +
  geom_density(aes(x = NDVI, color = year))+
  scale_colour_manual(values = colsGreen, labels = labs)+
  labs(title= "How does the Crescent Park NDVI change from 1999 to 2018?",
       subtitle= "Standardized Yearly NDVI density distribution calculated from Landsat7",
       caption = "Data Source: USGS Landsat 7") +
  xlab("Year") + 
  ylab("NDVI Density") +
  WrapTheme()
CPSDNDVIdensity_NY_all

# Facet wrap
CPSDNDVIdensity_NY_all + 
  geom_vline(data=CPSDmean_all_NY_ave, aes(xintercept=MeanNDVI, color=year),
             linetype="dashed", size = 0.5, alpha = 0.8) +
  geom_text(data=CPSDmean_all_NY_ave, aes(x = MeanNDVI, 
                                          label= paste("Mean: \n", round(MeanNDVI, digits = 3)), 
                                          y = 13, colour= year), 
            angle=0, text = element_text(size=11)) +
  facet_wrap( ~ year, ncol = 5)


################# CG ################# 
# Calculate all the mean values from April - May - Jun - Jul - Aug - Sep
CGarr = abind(CGData4, CGData6, CGData8, along = 3)
CGarr
# Mean value
CGData_All <- rowMeans(CGarr, dims = 2, na.rm = TRUE) %>% as.data.frame()
# standardize NDVI through years
CGSDYearsNDVI_all <- SDYear(CGData_All)


######### UNstandardized ##########
# R graph of UNstandardized average NDVI change over time
# UNStandardized Mean value
# 4_5_6_7_8_9
# CGmean_all_NY_ave <- colMeans(CGData_All)
CGNDVI_all_yearlychange <- ggplot (data=NULL) + 
  geom_point(mapping = aes(x = 1999:2018,y= colMeans(CGData_All)), size=1, alpha = 0.5, color = "deepskyblue4") +
  geom_smooth(mapping = aes(x = 1999:2018,y= colMeans(CGData_All)), color = "deepskyblue4", na.rm = TRUE , se= TRUE) +
  scale_x_continuous(name="Years", limits=c(1999, 2018))+
  scale_y_continuous(name="NDVI average value from April to September", limits=c(0, 1)) +
  labs(title= "How does the Central Green NDVI change from 1999 to 2018?",
       subtitle= "yearly NDVI average value from April to September",
       caption = "Data Source: USGS Landsat 7") +
  theme(legend.position = "bottom") +
  plotTheme()
CGNDVI_all_yearlychange

# 4_5_6_7_8_9   All summarized NDVI
# Density plot
CGmean_all_NY_ave <- colMeans(CGData_All) %>% as.data.frame()
CGmean_all_NY_ave <- cbind(CGmean_all_NY_ave, c(1999:2018)) %>% as.data.frame()
colnames(CGmean_all_NY_ave) <- c("MeanNDVI", "year")
CGmean_all_NY_ave$year <- as.character(CGmean_all_NY_ave$year)

CGtallFormat_NDVI_NY_all <- gather(CGData_All, key=year, value=NDVI, '1999':'2018')

CGNDVIdensity_NY_all <- ggplot(CGtallFormat_NDVI_NY_all, aes(x=NDVI, color=year)) +
  geom_density(aes(x = NDVI, color = year))+
  scale_colour_manual(values = colsGreen, labels = labs)+
  labs(title= "How does the Central Green NDVI change from 1999 to 2018?",
       subtitle= "Yearly NDVI density distribution calculated from Landsat7",
       caption = "Data Source: USGS Landsat 7") +
  xlab("Year") + 
  ylab("NDVI Density") +
  WrapTheme()
CGNDVIdensity_NY_all

# Facet wrap
CGNDVIdensity_NY_all + 
  geom_vline(data=CGmean_all_NY_ave, aes(xintercept=MeanNDVI, color=year),
             linetype="dashed", size = 0.5, alpha = 0.8) +
  geom_text(data=CGmean_all_NY_ave, aes(x = MeanNDVI, 
                                        label= paste("Mean: \n", round(MeanNDVI, digits = 3)), 
                                        y = 13, colour= year), 
            angle=0, text = element_text(size=11)) +
  facet_wrap( ~ year, ncol = 5)

############## After Standardize ###########
# CGSDYearsNDVI_all
CGSDmean_all_NY_ave <- colMeans(CGSDYearsNDVI_all) %>% as.data.frame()
CGSDmean_all_NY_ave <- cbind(CGSDmean_all_NY_ave, c(1999:2018)) %>% as.data.frame()
colnames(CGSDmean_all_NY_ave) <- c("MeanNDVI", "year")
CGSDmean_all_NY_ave$year <- as.character(CGSDmean_all_NY_ave$year)

CGSDtallFormat_NDVI_NY_all <- gather(CGSDYearsNDVI_all, key=year, value=NDVI, '1999':'2018')

CGSDNDVIdensity_NY_all <- ggplot(CGSDtallFormat_NDVI_NY_all, aes(x=NDVI, color=year)) +
  geom_density(aes(x = NDVI, color = year))+
  scale_colour_manual(values = colsGreen, labels = labs)+
  labs(title= "How does the Central Green NDVI change from 1999 to 2018?",
       subtitle= "Standardized Yearly NDVI density distribution calculated from Landsat7",
       caption = "Data Source: USGS Landsat 7") +
  xlab("Year") + 
  ylab("NDVI Density") +
  WrapTheme()
CGSDNDVIdensity_NY_all

# Facet wrap
CGSDNDVIdensity_NY_all + 
  geom_vline(data=CGSDmean_all_NY_ave, aes(xintercept=MeanNDVI, color=year),
             linetype="dashed", size = 0.5, alpha = 0.8) +
  geom_text(data=CGSDmean_all_NY_ave, aes(x = MeanNDVI, 
                                          label= paste("Mean: \n", round(MeanNDVI, digits = 3)), 
                                          y = 13, colour= year), 
            angle=0, text = element_text(size=11)) +
  facet_wrap( ~ year, ncol = 5)


################# All Navy Yard #################
library(abind)
# Calculate all the mean values from April - May - Jun - Jul - Aug - Sep
arr = abind(Data4, Data6, Data8, along = 3)
# Mean value
Data_All <- rowMeans(arr, dims = 2, na.rm = TRUE) %>% as.data.frame()
# standardize NDVI through years
SDYearsNDVI_all <- SDYear(Data_All)

######### UNstandardized ##########
# R graph of UNstandardized average NDVI change over time
# UNStandardized Mean value
# 4_5_6_7_8_9
# mean_all_NY_ave <- colMeans(Data_All)
NDVI_all_yearlychange <- ggplot (data=NULL) + 
  geom_point(mapping = aes(x = 1999:2018,y= colMeans(Data_All)), size=1, alpha = 0.5, color = "deepskyblue4") +
  geom_smooth(mapping = aes(x = 1999:2018,y= colMeans(Data_All)), color = "deepskyblue4", na.rm = TRUE , se= TRUE) +
  scale_x_continuous(name="Years", limits=c(1999, 2018))+
  scale_y_continuous(name="NDVI average value from April to September", limits=c(0, 1)) +
  labs(title= "How does the Navy Yard NDVI change from 1999 to 2018?",
       subtitle= "yearly NDVI average value from April to September",
       caption = "Data Source: USGS Landsat 7") +
  theme(legend.position = "bottom") +
  plotTheme()
NDVI_all_yearlychange

# 4_5_6_7_8_9   All summarized NDVI
# Density plot
mean_all_NY_ave <- colMeans(Data_All) %>% as.data.frame()
mean_all_NY_ave <- cbind(mean_all_NY_ave, c(1999:2018)) %>% as.data.frame()
colnames(mean_all_NY_ave) <- c("MeanNDVI", "year")
mean_all_NY_ave$year <- as.character(mean_all_NY_ave$year)


tallFormat_NDVI_NY_all <- gather(Data_All, key=year, value=NDVI, 'X1999':'X2018')

NDVIdensity_NY_all <- ggplot(tallFormat_NDVI_NY_all, aes(x=NDVI, color=year)) +
  geom_density(aes(x = NDVI, color = year))+
  scale_colour_manual(values = colsGreen, labels = labs)+
  labs(title= "How does the Navy Yard NDVI change from 1999 to 2018?",
       subtitle= "Yearly NDVI density distribution calculated from Landsat7",
       caption = "Data Source: USGS Landsat 7") +
  xlab("Year") + 
  ylab("NDVI Density") +
  WrapTheme()
NDVIdensity_NY_all

# Facet wrap
NDVIdensity_NY_all + 
  geom_vline(data=mean_all_NY_ave, aes(xintercept=MeanNDVI, color=year),
             linetype="dashed", size = 0.5, alpha = 0.8) +
  geom_text(data=mean_all_NY_ave, aes(x = MeanNDVI, 
                                      label= paste("Mean: \n", round(MeanNDVI, digits = 3)), 
                                      y = 13, colour= year), 
            angle=0, text = element_text(size=11)) +
  facet_wrap( ~ year, ncol = 5)

############## After Standardize ###########
# SDYearsNDVI_all
SDmean_all_NY_ave <- colMeans(SDYearsNDVI_all) %>% as.data.frame()
SDmean_all_NY_ave <- cbind(SDmean_all_NY_ave, c(1999:2018)) %>% as.data.frame()
colnames(SDmean_all_NY_ave) <- c("MeanNDVI", "year")
SDmean_all_NY_ave$year <- as.character(SDmean_all_NY_ave$year)

SDtallFormat_NDVI_NY_all <- gather(SDYearsNDVI_all, key=year, value=NDVI, 'X1999':'X2018')

SDNDVIdensity_NY_all <- ggplot(SDtallFormat_NDVI_NY_all, aes(x=NDVI, color=year)) +
  geom_density(aes(x = NDVI, color = year))+
  scale_colour_manual(values = colsGreen, labels = labs)+
  labs(title= "How does the Navy Yard NDVI change from 1999 to 2018?",
       subtitle= "Standardized Yearly NDVI density distribution calculated from Landsat7",
       caption = "Data Source: USGS Landsat 7") +
  xlab("Year") + 
  ylab("NDVI Density") +
  WrapTheme()
SDNDVIdensity_NY_all

# Facet wrap
SDNDVIdensity_NY_all + 
  geom_vline(data=SDmean_all_NY_ave, aes(xintercept=MeanNDVI, color=year),
             linetype="dashed", size = 0.5, alpha = 0.8) +
  geom_text(data=SDmean_all_NY_ave, aes(x = MeanNDVI, 
                                        label= paste("Mean: \n", round(MeanNDVI, digits = 3)), 
                                        y = 13, colour= year), 
            angle=0, text = element_text(size=11)) +
  facet_wrap( ~ year, ncol = 5)