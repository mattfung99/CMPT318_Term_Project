### Import Libraries
library(dplyr)
library(data.table)
library(tidyverse)
library(corrplot)
library(ggplot2)

### Set Directory
getwd()
setwd("--- Set Your Directory Here ---")

require(tidyverse)
rawData <- readr::read_csv("Dataset_GroupAssignment3.txt")
rawData[is.na(rawData)] <- 0

#--------------------------------------------------------------Data Cleaning-------------------------------------------------------
### Add timestamp to dataframe for graphing
dateTime <- paste(rawData$Date, rawData$Time)
rawData$timestamp <- as.POSIXlt(strptime(dateTime, format = "%d/%m/%Y %H:%M:%S"))

### Label all the weeks for dataset
dateFormat <- as.Date(rawData$Date, format = "%d/%m/%Y")
rawData$weekNum <- format(dateFormat,"%V")

### Label weekday or weekend
daysWeek <- format(as.Date(rawData$Date, format = "%d/%m/%Y"),"%u") # days of the week 1-7 (Monday-Sunday)
rawData$week <- ifelse(daysWeek %in% c("6", "7"), "weekend", "weekdays")

### Label time of the day - morning or evening
dataTime <- as.ITime(format(rawData$Time, format = "%H:%M:%S"))  #RS added to correct error when run in Linux
rawData$STime <- as.ITime(format(rawData$Time, format = "%H:%M:%S"))  #RS added to correct error when run in Linux
startTime <- as.ITime("10:30")
endTime <- as.ITime("13:30")
rawData$day <- ifelse((dataTime >= startTime & dataTime <= endTime), "morning", "evening")

#-------------------------------------------------------------Multiplot Code-------------------------------------------------------
# URL: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#----------------------------------------------------------------Question 1--------------------------------------------------------
### Subset all relevant attributes
corrRawData <- rawData[, c(3:9)]

### Construct a correlation matrix
corrMatrix <- (round(cor(corrRawData, method = "pearson"),3))
corrMatrix

# Visualize correlation matrix 
corrplot(corrMatrix, method = "color")

### Chosen observed response variables are Global_intensity, Global_active_power, and Sub_metering_1
####################################################################################################


### Graphical visualization of Global Intensity for weekdays and weekends between 10:30 - 13:30
####################################################################################################

### Create Dataframe for Week 5, Wednesday, 10:30AM - 1:30PM
dataWeek5Weekday <- filter(rawData, weekNum == "05", daysWeek == "3", day == "morning")

### Create Dataframe for Active Power, Week 5, Wednesday, 10:30AM - 1:30PM
intensityWeek5Weekday <- dataWeek5Weekday[, c("timestamp", "STime", "Global_intensity")]

### Format the time for Week 5
intensityWeek5Weekday$Time <-format(as.POSIXct(intensityWeek5Weekday$timestamp), format = "%H:%M:%S")
intensityWeek5Weekday$Time <- as.POSIXct(intensityWeek5Weekday$Time, format = "%H:%M:%S")


### Create Dataframe for Week 15, Wednesday, 10:30AM - 1:30PM
dataWeek15Weekday <- filter(rawData, weekNum == "15", daysWeek == "3", day == "morning")

### Create Dataframe for Active Power, Week 15, Wednesday, 10:30AM - 1:30PM
intensityWeek15Weekday <- dataWeek15Weekday[, c("timestamp", "STime", "Global_intensity")]

### Format the time for Week 15
intensityWeek15Weekday$Time <-format(as.POSIXct(intensityWeek15Weekday$timestamp), format = "%H:%M:%S")
intensityWeek15Weekday$Time <- as.POSIXct(intensityWeek15Weekday$Time, format = "%H:%M:%S")


### Create Dataframe for Week 35, Wednesday, 10:30AM - 1:30PM
dataWeek35Weekday <- filter(rawData, weekNum == "35", daysWeek == "3", day == "morning")

### Create Dataframe for Active Power, Week 35, Wednesday, 10:30AM - 1:30PM
intensityWeek35Weekday <- dataWeek35Weekday[, c("timestamp", "STime", "Global_intensity")]

### Format the time for Week 35
intensityWeek35Weekday$Time <-format(as.POSIXct(intensityWeek35Weekday$timestamp), format = "%H:%M:%S")
intensityWeek35Weekday$Time <- as.POSIXct(intensityWeek35Weekday$Time, format = "%H:%M:%S")


### Create Dataframe for Week 44, Wednesday, 10:30AM - 1:30PM
dataWeek44Weekday <- filter(rawData, weekNum == "44", daysWeek == "3", day == "morning")

### Create Dataframe for Active Power, Week 44, Wednesday, 10:30AM - 1:30PM
intensityWeek44Weekday <- dataWeek44Weekday[, c("timestamp", "STime", "Global_intensity")]

### Format the time for Week 44
intensityWeek44Weekday$Time <-format(as.POSIXct(intensityWeek44Weekday$timestamp), format = "%H:%M:%S")
intensityWeek44Weekday$Time <- as.POSIXct(intensityWeek44Weekday$Time, format = "%H:%M:%S")


### Plot 
p1 <- ggplot(intensityWeek5Weekday, mapping = aes(x=Time, y=Global_intensity)) + 
  geom_point(color="light blue") +
  ggtitle("Week 5 Wednesday Global Intensity") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(y="Global Intensity (in Ampere)", x = "Time (in Hours)", color ="Fit Type") + 
  theme(plot.title = element_text(hjust=0.5), legend.position = "bottom") 

### Plot 
p2 <- ggplot(intensityWeek15Weekday, mapping = aes(x=Time, y=Global_intensity)) + 
  geom_point(color="light blue") +
  ggtitle("Week 15 Wednesday Global Intensity") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(y="Global Intensity (in Ampere)", x = "Time (in Hours)", color ="Fit Type") + 
  theme(plot.title = element_text(hjust=0.5), legend.position = "bottom") 

### Plot 
p3 <- ggplot(intensityWeek35Weekday, mapping = aes(x=Time, y=Global_intensity)) + 
  geom_point(color="light blue") +
  ggtitle("Week 35 Wednesday Global Intensity") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(y="Global Intensity (in Ampere)", x = "Time (in Hours)", color ="Fit Type") + 
  theme(plot.title = element_text(hjust=0.5), legend.position = "bottom") 

### Plot 
p4 <- ggplot(intensityWeek44Weekday, mapping = aes(x=Time, y=Global_intensity)) + 
  geom_point(color="light blue") +
  ggtitle("Week 44 Wednesday Global Intensity") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(y="Global Intensity (in Ampere)", x = "Time (in Hours)", color ="Fit Type") + 
  theme(plot.title = element_text(hjust=0.5), legend.position = "bottom") 

### 4 Plots 1 Diagram
multiplot(p1, p2, p3, p4, cols = 2)


### Create Dataframe for Week 5, Saturday, 10:30AM - 1:30PM
dataWeek5Weekday <- filter(rawData, weekNum == "05", daysWeek == "6", day == "morning")

### Create Dataframe for Active Power, Week 5, Saturday, 10:30AM - 1:30PM
intensityWeek5Weekday <- dataWeek5Weekday[, c("timestamp", "STime", "Global_intensity")]

### Format the time for Week 5
intensityWeek5Weekday$Time <-format(as.POSIXct(intensityWeek5Weekday$timestamp), format = "%H:%M:%S")
intensityWeek5Weekday$Time <- as.POSIXct(intensityWeek5Weekday$Time, format = "%H:%M:%S")


### Create Dataframe for Week 15, Saturday, 10:30AM - 1:30PM
dataWeek15Weekday <- filter(rawData, weekNum == "15", daysWeek == "6", day == "morning")

### Create Dataframe for Active Power, Week 15, Saturday, 10:30AM - 1:30PM
intensityWeek15Weekday <- dataWeek15Weekday[, c("timestamp", "STime", "Global_intensity")]

### Format the time for Week 15
intensityWeek15Weekday$Time <-format(as.POSIXct(intensityWeek15Weekday$timestamp), format = "%H:%M:%S")
intensityWeek15Weekday$Time <- as.POSIXct(intensityWeek15Weekday$Time, format = "%H:%M:%S")


### Create Dataframe for Week 35, Saturday, 10:30AM - 1:30PM
dataWeek35Weekday <- filter(rawData, weekNum == "35", daysWeek == "6", day == "morning")

### Create Dataframe for Active Power, Week 35, Saturday, 10:30AM - 1:30PM
intensityWeek35Weekday <- dataWeek35Weekday[, c("timestamp", "STime", "Global_intensity")]

### Format the time for Week 35
intensityWeek35Weekday$Time <-format(as.POSIXct(intensityWeek35Weekday$timestamp), format = "%H:%M:%S")
intensityWeek35Weekday$Time <- as.POSIXct(intensityWeek35Weekday$Time, format = "%H:%M:%S")


### Create Dataframe for Week 44, Saturday, 10:30AM - 1:30PM
dataWeek44Weekday <- filter(rawData, weekNum == "44", daysWeek == "6", day == "morning")

### Create Dataframe for Active Power, Week 44, Saturday, 10:30AM - 1:30PM
intensityWeek44Weekday <- dataWeek44Weekday[, c("timestamp", "STime", "Global_intensity")]

### Format the time for Week 44
intensityWeek44Weekday$Time <-format(as.POSIXct(intensityWeek44Weekday$timestamp), format = "%H:%M:%S")
intensityWeek44Weekday$Time <- as.POSIXct(intensityWeek44Weekday$Time, format = "%H:%M:%S")


### Plot 
p5 <- ggplot(intensityWeek5Weekday, mapping = aes(x=Time, y=Global_intensity)) + 
  geom_point(color="light blue") +
  ggtitle("Week 5 Saturday Global Intensity") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(y="Global Intensity (in Ampere)", x = "Time (in Hours)", color ="Fit Type") + 
  theme(plot.title = element_text(hjust=0.5), legend.position = "bottom") 

### Plot 
p6 <- ggplot(intensityWeek15Weekday, mapping = aes(x=Time, y=Global_intensity)) + 
  geom_point(color="light blue") +
  ggtitle("Week 15 Saturday Global Intensity") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(y="Global Intensity (in Ampere)", x = "Time (in Hours)", color ="Fit Type") + 
  theme(plot.title = element_text(hjust=0.5), legend.position = "bottom") 

### Plot 
p7 <- ggplot(intensityWeek35Weekday, mapping = aes(x=Time, y=Global_intensity)) + 
  geom_point(color="light blue") +
  ggtitle("Week 35 Saturday Global Intensity") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(y="Global Intensity (in Ampere)", x = "Time (in Hours)", color ="Fit Type") + 
  theme(plot.title = element_text(hjust=0.5), legend.position = "bottom") 

### Plot 
p8 <- ggplot(intensityWeek44Weekday, mapping = aes(x=Time, y=Global_intensity)) + 
  geom_point(color="light blue") +
  ggtitle("Week 44 Saturday Global Intensity") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(y="Global Intensity (in Ampere)", x = "Time (in Hours)", color ="Fit Type") + 
  theme(plot.title = element_text(hjust=0.5), legend.position = "bottom") 

### 4 Plots 1 Diagram
multiplot(p5, p6, p7, p8, cols = 2)


### Graphical visualization of Global Active Power for weekdays and weekends between 10:30 - 13:30
####################################################################################################

### Create Dataframe for Week 5, Wednesday, 10:30AM - 1:30PM
dataWeek5Weekday <- filter(rawData, weekNum == "05", daysWeek == "3", day == "morning")

### Create Dataframe for Active Power, Week 5, Wednesday, 10:30AM - 1:30PM
activePowerWeek5Weekday <- dataWeek5Weekday[, c("timestamp", "STime", "Global_active_power")]

### Format the time for Week 5
activePowerWeek5Weekday$Time <-format(as.POSIXct(activePowerWeek5Weekday$timestamp), format = "%H:%M:%S")
activePowerWeek5Weekday$Time <- as.POSIXct(activePowerWeek5Weekday$Time, format = "%H:%M:%S")


### Create Dataframe for Week 15, Wednesday, 10:30AM - 1:30PM
dataWeek15Weekday <- filter(rawData, weekNum == "15", daysWeek == "3", day == "morning")

### Create Dataframe for Active Power, Week 15, Wednesday, 10:30AM - 1:30PM
activePowerWeek15Weekday <- dataWeek15Weekday[, c("timestamp", "STime", "Global_active_power")]

### Format the time for Week 15
activePowerWeek15Weekday$Time <-format(as.POSIXct(activePowerWeek15Weekday$timestamp), format = "%H:%M:%S")
activePowerWeek15Weekday$Time <- as.POSIXct(activePowerWeek15Weekday$Time, format = "%H:%M:%S")


### Create Dataframe for Week 35, Wednesday, 10:30AM - 1:30PM
dataWeek35Weekday <- filter(rawData, weekNum == "35", daysWeek == "3", day == "morning")

### Create Dataframe for Active Power, Week 35, Wednesday, 10:30AM - 1:30PM
activePowerWeek35Weekday <- dataWeek35Weekday[, c("timestamp", "STime", "Global_active_power")]

### Format the time for Week 35
activePowerWeek35Weekday$Time <-format(as.POSIXct(activePowerWeek35Weekday$timestamp), format = "%H:%M:%S")
activePowerWeek35Weekday$Time <- as.POSIXct(activePowerWeek35Weekday$Time, format = "%H:%M:%S")


### Create Dataframe for Week 44, Wednesday, 10:30AM - 1:30PM
dataWeek44Weekday <- filter(rawData, weekNum == "44", daysWeek == "3", day == "morning")

### Create Dataframe for Active Power, Week 44, Wednesday, 10:30AM - 1:30PM
activePowerWeek44Weekday <- dataWeek44Weekday[, c("timestamp", "STime", "Global_active_power")]

### Format the time for Week 44
activePowerWeek44Weekday$Time <-format(as.POSIXct(activePowerWeek44Weekday$timestamp), format = "%H:%M:%S")
activePowerWeek44Weekday$Time <- as.POSIXct(activePowerWeek44Weekday$Time, format = "%H:%M:%S")


### Plot 
p9 <- ggplot(activePowerWeek5Weekday, mapping = aes(x=Time, y=Global_active_power)) + 
  geom_point(color="light blue") +
  ggtitle("Week 5 Wednesday Global Active Power") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(y="Global Active Power (in Kilowatt)", x = "Time (in Hours)", color ="Fit Type") + 
  theme(plot.title = element_text(hjust=0.5), legend.position = "bottom") 

### Plot 
p10 <- ggplot(activePowerWeek15Weekday, mapping = aes(x=Time, y=Global_active_power)) + 
  geom_point(color="light blue") +
  ggtitle("Week 15 Wednesday Global Active Power") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(y="Global Active Power (in Kilowatt)", x = "Time (in Hours)", color ="Fit Type") + 
  theme(plot.title = element_text(hjust=0.5), legend.position = "bottom") 

### Plot 
p11 <- ggplot(activePowerWeek35Weekday, mapping = aes(x=Time, y=Global_active_power)) + 
  geom_point(color="light blue") +
  ggtitle("Week 35 Wednesday Global Active Power") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(y="Global Active Power (in Kilowatt)", x = "Time (in Hours)", color ="Fit Type") + 
  theme(plot.title = element_text(hjust=0.5), legend.position = "bottom") 

### Plot 
p12 <- ggplot(activePowerWeek44Weekday, mapping = aes(x=Time, y=Global_active_power)) + 
  geom_point(color="light blue") +
  ggtitle("Week 44 Wednesday Global Active Power") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(y="Global Active Power (in Kilowatt)", x = "Time (in Hours)", color ="Fit Type") + 
  theme(plot.title = element_text(hjust=0.5), legend.position = "bottom") 

### 4 Plots 1 Diagram
multiplot(p9, p10, p11, p12, cols = 2)


### Create Dataframe for Week 5, Saturday, 10:30AM - 1:30PM
dataWeek5Weekday <- filter(rawData, weekNum == "05", daysWeek == "6", day == "morning")

### Create Dataframe for Active Power, Week 5, Saturday, 10:30AM - 1:30PM
activePowerWeek5Weekday <- dataWeek5Weekday[, c("timestamp", "STime", "Global_active_power")]

### Format the time for Week 5
activePowerWeek5Weekday$Time <-format(as.POSIXct(activePowerWeek5Weekday$timestamp), format = "%H:%M:%S")
activePowerWeek5Weekday$Time <- as.POSIXct(activePowerWeek5Weekday$Time, format = "%H:%M:%S")


### Create Dataframe for Week 15, Saturday, 10:30AM - 1:30PM
dataWeek15Weekday <- filter(rawData, weekNum == "15", daysWeek == "6", day == "morning")

### Create Dataframe for Active Power, Week 15, Saturday, 10:30AM - 1:30PM
activePowerWeek15Weekday <- dataWeek15Weekday[, c("timestamp", "STime", "Global_active_power")]

### Format the time for Week 15
activePowerWeek15Weekday$Time <-format(as.POSIXct(activePowerWeek15Weekday$timestamp), format = "%H:%M:%S")
activePowerWeek15Weekday$Time <- as.POSIXct(activePowerWeek15Weekday$Time, format = "%H:%M:%S")


### Create Dataframe for Week 35, Saturday, 10:30AM - 1:30PM
dataWeek35Weekday <- filter(rawData, weekNum == "35", daysWeek == "6", day == "morning")

### Create Dataframe for Active Power, Week 35, Saturday, 10:30AM - 1:30PM
activePowerWeek35Weekday <- dataWeek35Weekday[, c("timestamp", "STime", "Global_active_power")]

### Format the time for Week 35
activePowerWeek35Weekday$Time <-format(as.POSIXct(activePowerWeek35Weekday$timestamp), format = "%H:%M:%S")
activePowerWeek35Weekday$Time <- as.POSIXct(activePowerWeek35Weekday$Time, format = "%H:%M:%S")


### Create Dataframe for Week 44, Saturday, 10:30AM - 1:30PM
dataWeek44Weekday <- filter(rawData, weekNum == "44", daysWeek == "6", day == "morning")

### Create Dataframe for Active Power, Week 44, Saturday, 10:30AM - 1:30PM
activePowerWeek44Weekday <- dataWeek44Weekday[, c("timestamp", "STime", "Global_active_power")]

### Format the time for Week 44
activePowerWeek44Weekday$Time <-format(as.POSIXct(activePowerWeek44Weekday$timestamp), format = "%H:%M:%S")
activePowerWeek44Weekday$Time <- as.POSIXct(activePowerWeek44Weekday$Time, format = "%H:%M:%S")


### Plot 
p13 <- ggplot(activePowerWeek5Weekday, mapping = aes(x=Time, y=Global_active_power)) + 
  geom_point(color="light blue") +
  ggtitle("Week 5 Saturday Global Active Power") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(y="Global Active Power (in Kilowatt)", x = "Time (in Hours)", color ="Fit Type") + 
  theme(plot.title = element_text(hjust=0.5), legend.position = "bottom") 

### Plot 
p14 <- ggplot(activePowerWeek15Weekday, mapping = aes(x=Time, y=Global_active_power)) + 
  geom_point(color="light blue") +
  ggtitle("Week 15 Saturday Global Active Power") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(y="Global Active Power (in Kilowatt)", x = "Time (in Hours)", color ="Fit Type") + 
  theme(plot.title = element_text(hjust=0.5), legend.position = "bottom") 

### Plot 
p15 <- ggplot(activePowerWeek35Weekday, mapping = aes(x=Time, y=Global_active_power)) + 
  geom_point(color="light blue") +
  ggtitle("Week 35 Saturday Global Active Power") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(y="Global Active Power (in Kilowatt)", x = "Time (in Hours)", color ="Fit Type") + 
  theme(plot.title = element_text(hjust=0.5), legend.position = "bottom") 

### Plot 
p16 <- ggplot(activePowerWeek44Weekday, mapping = aes(x=Time, y=Global_active_power)) + 
  geom_point(color="light blue") +
  ggtitle("Week 44 Saturday Global Active Power") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(y="Global Active Power (in Kilowatt)", x = "Time (in Hours)", color ="Fit Type") + 
  theme(plot.title = element_text(hjust=0.5), legend.position = "bottom") 

### 4 Plots 1 Diagram
multiplot(p13, p14, p15, p16, cols = 2)


### Graphical visualization of Sub Metering 1 for weekdays and weekends between 10:30 - 13:30
####################################################################################################

### Create Dataframe for Week 5, Wednesday, 10:30AM - 1:30PM
dataWeek5Weekday <- filter(rawData, weekNum == "05", daysWeek == "3", day == "morning")

### Create Dataframe for Sub Metering 1, Week 5, Wednesday, 10:30AM - 1:30PM
subMeteringOneWeek5Weekday <- dataWeek5Weekday[, c("timestamp", "STime", "Sub_metering_1")]

### Format the time for Week 5
subMeteringOneWeek5Weekday$Time <-format(as.POSIXct(subMeteringOneWeek5Weekday$timestamp), format = "%H:%M:%S")
subMeteringOneWeek5Weekday$Time <- as.POSIXct(subMeteringOneWeek5Weekday$Time, format = "%H:%M:%S")


### Create Dataframe for Week 15, Wednesday, 10:30AM - 1:30PM
dataWeek15Weekday <- filter(rawData, weekNum == "15", daysWeek == "3", day == "morning")

### Create Dataframe for Sub Metering 1, Week 15, Wednesday, 10:30AM - 1:30PM
subMeteringOneWeek15Weekday <- dataWeek15Weekday[, c("timestamp", "STime", "Sub_metering_1")]

### Format the time for Week 15
subMeteringOneWeek15Weekday$Time <-format(as.POSIXct(subMeteringOneWeek15Weekday$timestamp), format = "%H:%M:%S")
subMeteringOneWeek15Weekday$Time <- as.POSIXct(subMeteringOneWeek15Weekday$Time, format = "%H:%M:%S")


### Create Dataframe for Week 35, Wednesday, 10:30AM - 1:30PM
dataWeek35Weekday <- filter(rawData, weekNum == "35", daysWeek == "3", day == "morning")

### Create Dataframe for Sub Metering 1, Week 35, Wednesday, 10:30AM - 1:30PM
subMeteringOneWeek35Weekday <- dataWeek35Weekday[, c("timestamp", "STime", "Sub_metering_1")]

### Format the time for Week 35
subMeteringOneWeek35Weekday$Time <-format(as.POSIXct(subMeteringOneWeek35Weekday$timestamp), format = "%H:%M:%S")
subMeteringOneWeek35Weekday$Time <- as.POSIXct(subMeteringOneWeek35Weekday$Time, format = "%H:%M:%S")


### Create Dataframe for Week 44, Wednesday, 10:30AM - 1:30PM
dataWeek44Weekday <- filter(rawData, weekNum == "44", daysWeek == "3", day == "morning")

### Create Dataframe for Sub Metering 1, Week 44, Wednesday, 10:30AM - 1:30PM
subMeteringOneWeek44Weekday <- dataWeek44Weekday[, c("timestamp", "STime", "Sub_metering_1")]

### Format the time for Week 44
subMeteringOneWeek44Weekday$Time <-format(as.POSIXct(subMeteringOneWeek44Weekday$timestamp), format = "%H:%M:%S")
subMeteringOneWeek44Weekday$Time <- as.POSIXct(subMeteringOneWeek44Weekday$Time, format = "%H:%M:%S")


### Plot 
p17 <- ggplot(subMeteringOneWeek5Weekday, mapping = aes(x=Time, y=Sub_metering_1)) + 
  geom_point(color="light blue") +
  ggtitle("Week 5 Wednesday Sub Metering 1") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(y="Sub Metering 1 (in Kilowatt)", x = "Time (in Hours)", color ="Fit Type") + 
  theme(plot.title = element_text(hjust=0.5), legend.position = "bottom") 

### Plot 
p18 <- ggplot(subMeteringOneWeek15Weekday, mapping = aes(x=Time, y=Sub_metering_1)) + 
  geom_point(color="light blue") +
  ggtitle("Week 15 Wednesday Sub Metering 1") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(y="Sub Metering 1 (in Kilowatt)", x = "Time (in Hours)", color ="Fit Type") + 
  theme(plot.title = element_text(hjust=0.5), legend.position = "bottom") 

### Plot 
p19 <- ggplot(subMeteringOneWeek35Weekday, mapping = aes(x=Time, y=Sub_metering_1)) + 
  geom_point(color="light blue") +
  ggtitle("Week 35 Wednesday Sub Metering 1") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(y="Sub Metering 1 (in Kilowatt)", x = "Time (in Hours)", color ="Fit Type") + 
  theme(plot.title = element_text(hjust=0.5), legend.position = "bottom") 

### Plot 
p20 <- ggplot(subMeteringOneWeek44Weekday, mapping = aes(x=Time, y=Sub_metering_1)) + 
  geom_point(color="light blue") +
  ggtitle("Week 44 Wednesday Sub Metering 1") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(y="Sub Metering 1 (in Kilowatt)", x = "Time (in Hours)", color ="Fit Type") + 
  theme(plot.title = element_text(hjust=0.5), legend.position = "bottom") 

### 4 Plots 1 Diagram
multiplot(p17, p18, p19, p20, cols = 2)


### Create Dataframe for Week 5, Saturday, 10:30AM - 1:30PM
dataWeek5Weekday <- filter(rawData, weekNum == "05", daysWeek == "6", day == "morning")

### Create Dataframe for Sub Metering 1, Week 5, Saturday, 10:30AM - 1:30PM
subMeteringOneWeek5Weekday <- dataWeek5Weekday[, c("timestamp", "STime", "Sub_metering_1")]

### Format the time for Week 5
subMeteringOneWeek5Weekday$Time <-format(as.POSIXct(subMeteringOneWeek5Weekday$timestamp), format = "%H:%M:%S")
subMeteringOneWeek5Weekday$Time <- as.POSIXct(subMeteringOneWeek5Weekday$Time, format = "%H:%M:%S")


### Create Dataframe for Week 15, Saturday, 10:30AM - 1:30PM
dataWeek15Weekday <- filter(rawData, weekNum == "15", daysWeek == "6", day == "morning")

### Create Dataframe for Sub Metering 1, Week 15, Saturday, 10:30AM - 1:30PM
subMeteringOneWeek15Weekday <- dataWeek15Weekday[, c("timestamp", "STime", "Sub_metering_1")]

### Format the time for Week 15
subMeteringOneWeek15Weekday$Time <-format(as.POSIXct(subMeteringOneWeek15Weekday$timestamp), format = "%H:%M:%S")
subMeteringOneWeek15Weekday$Time <- as.POSIXct(subMeteringOneWeek15Weekday$Time, format = "%H:%M:%S")


### Create Dataframe for Week 35, Saturday, 10:30AM - 1:30PM
dataWeek35Weekday <- filter(rawData, weekNum == "35", daysWeek == "6", day == "morning")

### Create Dataframe for Sub Metering 1, Week 35, Saturday, 10:30AM - 1:30PM
subMeteringOneWeek35Weekday <- dataWeek35Weekday[, c("timestamp", "STime", "Sub_metering_1")]

### Format the time for Week 35
subMeteringOneWeek35Weekday$Time <-format(as.POSIXct(subMeteringOneWeek35Weekday$timestamp), format = "%H:%M:%S")
subMeteringOneWeek35Weekday$Time <- as.POSIXct(subMeteringOneWeek35Weekday$Time, format = "%H:%M:%S")


### Create Dataframe for Week 44, Saturday, 10:30AM - 1:30PM
dataWeek44Weekday <- filter(rawData, weekNum == "44", daysWeek == "6", day == "morning")

### Create Dataframe for Sub Metering 1, Week 44, Saturday, 10:30AM - 1:30PM
subMeteringOneWeek44Weekday <- dataWeek44Weekday[, c("timestamp", "STime", "Sub_metering_1")]

### Format the time for Week 44
subMeteringOneWeek44Weekday$Time <-format(as.POSIXct(subMeteringOneWeek44Weekday$timestamp), format = "%H:%M:%S")
subMeteringOneWeek44Weekday$Time <- as.POSIXct(subMeteringOneWeek44Weekday$Time, format = "%H:%M:%S")


### Plot 
p21 <- ggplot(subMeteringOneWeek5Weekday, mapping = aes(x=Time, y=Sub_metering_1)) + 
  geom_point(color="light blue") +
  ggtitle("Week 5 Saturday Sub Metering 1") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(y="Sub Metering 1 (in Kilowatt)", x = "Time (in Hours)", color ="Fit Type") + 
  theme(plot.title = element_text(hjust=0.5), legend.position = "bottom") 

### Plot 
p22 <- ggplot(subMeteringOneWeek15Weekday, mapping = aes(x=Time, y=Sub_metering_1)) + 
  geom_point(color="light blue") +
  ggtitle("Week 15 Saturday Sub Metering 1") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(y="Sub Metering 1 (in Kilowatt)", x = "Time (in Hours)", color ="Fit Type") + 
  theme(plot.title = element_text(hjust=0.5), legend.position = "bottom") 

### Plot 
p23 <- ggplot(subMeteringOneWeek35Weekday, mapping = aes(x=Time, y=Sub_metering_1)) + 
  geom_point(color="light blue") +
  ggtitle("Week 35 Saturday Sub Metering 1") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(y="Sub Metering 1 (in Kilowatt)", x = "Time (in Hours)", color ="Fit Type") + 
  theme(plot.title = element_text(hjust=0.5), legend.position = "bottom") 

### Plot 
p24 <- ggplot(subMeteringOneWeek44Weekday, mapping = aes(x=Time, y=Sub_metering_1)) + 
  geom_point(color="light blue") +
  ggtitle("Week 44 Saturday Sub Metering 1") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(y="Sub Metering 1 (in Kilowatt)", x = "Time (in Hours)", color ="Fit Type") + 
  theme(plot.title = element_text(hjust=0.5), legend.position = "bottom") 

### 4 Plots 1 Diagram
multiplot(p21, p22, p23, p24, cols = 2)

#----------------------------------------------------------------Question 2--------------------------------------------------------

#----------------------------------------------------------------Question 3--------------------------------------------------------

#----------------------------------------------------------------Question 4--------------------------------------------------------