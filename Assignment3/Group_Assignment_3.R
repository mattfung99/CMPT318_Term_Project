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

### Import Libraries
library(dplyr)
library(data.table)
library(depmixS4)


ntimesVector <- function(df){
  
  agg <- aggregate(df$Date, by = list(df$Date), FUN = length)
  #x$Group.1 <- as.Date(x$Group.1, format = "%d/%m/%Y")
  agg <- agg[order(as.Date(agg$Group.1, format = "%d/%m/%Y")),]
  
  return(agg$x)
}

############################################################### Normal Data #################################################################

# Read and remove NaNs
powerData <- read.table("Dataset_GroupAssignment3.txt", sep = ',', header = TRUE)
powerData <- na.omit(powerData[, -c(4, 5, 8, 9)])

### Create a time stamp column
dateTimeCombine <- paste(powerData$Date, powerData$Time)
powerData$timestamp <- as.POSIXlt(strptime(dateTimeCombine, format = "%d/%m/%Y %H:%M:%S"))

### Label days of the week
formatingDates <- as.Date(powerData$Date, format = "%d/%m/%Y")
#powerData$weekNumber <- format(formatingDates,"%V")
powerData$dayNumber <- as.integer(format(formatingDates,"%u"))

### Filter the time period required
STime <- as.ITime(format(powerData$Time, format = "%H:%M:%S"))  #RS added to correct error when run in Linux
startingTime <- as.ITime("10:30")
endingTime <- as.ITime("13:30")
morningTime <- cbind(powerData, STime)
morning.Data.Raw <- filter(morningTime, STime >= startingTime & STime <= endingTime)
morning.Data.Raw <- morning.Data.Raw[, -8]


### Separate train raw data and test raw data
train.data.raw <- subset(morning.Data.Raw, as.Date(morning.Data.Raw$timestamp) < as.Date("2009-1-1 00:00:00 PST"))
test.data.raw <- subset(morning.Data.Raw, as.Date(morning.Data.Raw$timestamp) >= as.Date("2009-1-1 00:00:00 PST"))

### Training Set weekdays and weekends
train.data.weekdays <- train.data.raw[train.data.raw$dayNumber < 6,]
train.weekdays <- train.data.weekdays[,-c(2,6,7)]

train.data.weekends <- train.data.raw[train.data.raw$dayNumber >= 6,]
train.weekends <- train.data.weekends[,-c(2,6,7)]

### Test Set weekdays and weekends
test.data.weekdays <- test.data.raw[test.data.raw$dayNumber < 6,]
test.weekdays <- test.data.weekdays[,-c(2,6,7)]

test.data.weekends <- test.data.raw[test.data.raw$dayNumber >= 6,]
test.weekends <- test.data.weekends[,-c(2,6,7)]


############################################################### Anomaly Data #################################################################

# Read anomaly file and remove NaNs
anomalies <- read.table("DatasetWithAnomalies_GroupAssignment3.txt", sep = ',', header = TRUE)
anomalies <- na.omit(anomalies[, -c(4, 5, 8, 9)])

### Create a time stamp column
anomaliesDateTime <- paste(anomalies$Date, anomalies$Time)
anomalies$timestamp <- as.POSIXlt(strptime(anomaliesDateTime, format = "%d/%m/%Y %H:%M:%S"))

### Label days of the week
anomaliesFormatingDates <- as.Date(anomalies$Date, format = "%d/%m/%Y")
#powerData$weekNumber <- format(formatingDates,"%V")
anomalies$dayNumber <- as.integer(format(anomaliesFormatingDates,"%u"))

### Filter the time period required
ASTime <- as.ITime(format(anomalies$Time, format = "%H:%M:%S"))  #RS added to correct error when run in Linux
AstartingTime <- as.ITime("10:30")
AendingTime <- as.ITime("13:30")
anomaliesMorningTime <- cbind(anomalies, ASTime)
anomalies.morning.Data.Raw <- filter(anomaliesMorningTime, ASTime >= AstartingTime & ASTime <= AendingTime)
anomalies.morning.Data.Raw <- anomalies.morning.Data.Raw[, -8]


### Anomalies weekdays and weekends
anomalies.data.weekdays <- anomalies.morning.Data.Raw[anomalies.morning.Data.Raw$dayNumber < 6,]
anomalies.data.weekdays <- anomalies.data.weekdays[,-c(2,6,7)]

anomalies.data.weekends <- anomalies.morning.Data.Raw[anomalies.morning.Data.Raw$dayNumber >= 6,]
anomalies.data.weekends <- anomalies.data.weekends[,-c(2,6,7)]

########################################################### States Selection ####################################################################
set.seed(298224)
#set.seed(2)

minState <- 10
maxState <- 20
numStates <- minState:maxState

# Create containers for state BICs and logLik
weekdays.vector = c("States", "BICs", "logLik")
weekdays.models = array(0, dim = c(maxState-minState+1, length(weekdays.vector)))
colnames(weekdays.models) = weekdays.vector

weekends.vector = c("States", "BICs", "logLik")
weekends.models = array(0, dim = c(maxState-minState+1, length(weekends.vector)))
colnames(weekends.models) = weekends.vector


for (i in numStates)
{
  print(paste0(i))
  
  weekdays.model = depmix(list(train.weekdays$Global_intensity ~ 1, train.weekdays$Global_active_power ~ 1, train.weekdays$Sub_metering_1  ~ 1),
                          nstates = i, family = list(gaussian(), gaussian(), poisson()), data = train.weekdays, ntimes = ntimesVector(train.weekdays))
  fit.weekdays.model <- fit(weekdays.model)
  
  # Extract states, BICs, and Log likelihood
  weekdays.models[i-9, "States"] <- i
  weekdays.models[i-9, "BICs"] <- BIC( fit.weekdays.model)
  weekdays.models[i-9, "logLik"] <- logLik( fit.weekdays.model)
}
weekdays.models
Weekdays.summary <- as.data.frame(weekdays.models)
Weekdays.summary <- Weekdays.summary[1:10,]


for (i in numStates)
{
  print(paste0(i))
  weekends.model = depmix(list(train.weekends$Global_intensity ~ 1, train.weekends$Global_active_power ~ 1, train.weekends$Sub_metering_1  ~ 1),
                          nstates = i, family = list(gaussian(), gaussian(), poisson()), data = train.weekends, ntimes = ntimesVector(train.weekends))
  fit.weekends.model <- fit(weekends.model)
  
  # Extract states, BICs, and Log likelihood
  weekends.models[i-9, "States"] <- i
  weekends.models[i-9, "BICs"] <- BIC( fit.weekends.model)
  weekends.models[i-9, "logLik"] <- logLik( fit.weekends.model)
}
weekends.models
weekends.summary <- as.data.frame(weekends.models)


# Weekdays Training Set plot
par(mar = c(5, 5, 3, 5))
plot(Weekdays.summary$States, Weekdays.summary$BICs, type ="l", ylab = "BICs",
     main = "BIC and Log-Likelihood for each state - Weekdays Training Set", xlab = "States", col = "blue")

par(new = TRUE)
plot(Weekdays.summary$States, Weekdays.summary$logLik, type = "l", xaxt = "n", yaxt = "n", ylab = "", xlab = "", col = "red", lty = 2)
axis(side = 4)

mtext("Log Likelihood", side = 4, line = 3)
legend("top", c("BICs", "Log-Likelihoods"), col = c("blue", "red"), lty = c(1, 2))


# Weekends Training Set plot
par(mar = c(5, 5, 3, 5))
plot(weekends.summary$States, weekends.summary$BICs, type ="l", ylab = "BICs",
     main = "BIC and Log-Likelihood for each state - weekends Training Set", xlab = "States", col = "blue")

par(new = TRUE)
plot(weekends.summary$States, weekends.summary$logLik, type = "l", xaxt = "n", yaxt = "n", ylab = "", xlab = "", col = "red", lty = 2)
axis(side = 4)
axis(1, at = seq(10, 20, by = 1), las=1)

mtext("Log Likelihood", side = 4, line = 3)
legend("top", c("BICs", "Log-Likelihoods"), col = c("blue", "red"), lty = c(1, 2))

#----------------------------------------------------------------Question 3--------------------------------------------------------

########################################################## Model Validation Weekdays ########################################################
set.seed(298224)

weekdays.train.model = depmix(list(train.weekdays$Global_intensity ~ 1, train.weekdays$Global_active_power ~ 1, train.weekdays$Sub_metering_1  ~ 1),
                              nstates = 16, family = list(gaussian(), gaussian(), poisson()), data = train.weekdays, ntimes = ntimesVector(train.weekdays))
weekdays.fitted.train.model <- fit(weekdays.train.model)


weekdays.test.model = depmix(list(test.weekdays$Global_intensity ~ 1, test.weekdays$Global_active_power ~ 1, test.weekdays$Sub_metering_1  ~ 1),
                             nstates = 16, family = list(gaussian(), gaussian(), poisson()), data = test.weekdays, ntimes = ntimesVector(test.weekdays))


weekdays.parameters <-setpars(weekdays.test.model,getpars(weekdays.train.model))
weekdays.fitted.test.model <-fit(weekdays.parameters)


(logLik(weekdays.fitted.train.model))
(logLik(weekdays.fitted.test.model) * nrow(train.weekdays)/nrow(test.weekdays))



### state = 16
#'log Lik.' -189762.2 (df=335) training model
#'log Lik.' -194487.8 (df=335) testing model

### state = 15
#'log Lik.' -153493.7 (df=299) training model
#'log Lik.' -194487.8 (df=335) testing model


########################################################## Model Validation Weekends ########################################################
set.seed(298224)

weekends.train.model = depmix(list(train.weekends$Global_intensity ~ 1, train.weekends$Global_active_power ~ 1, train.weekends$Sub_metering_1  ~ 1),
                              nstates = 14, family = list(gaussian(), gaussian(), poisson()), data = train.weekends, ntimes = ntimesVector(train.weekends))
weekends.fitted.train.model <- fit(weekends.train.model)


weekends.test.model = depmix(list(test.weekends$Global_intensity ~ 1, test.weekends$Global_active_power ~ 1, test.weekends$Sub_metering_1  ~ 1),
                             nstates = 14, family = list(gaussian(), gaussian(), poisson()), data = test.weekends, ntimes = ntimesVector(test.weekends))


weekends.parameters <-setpars(weekends.test.model,getpars(weekends.train.model))
weekends.fitted.test.model <-fit(weekends.parameters)


(logLik(weekends.fitted.train.model)) 
(logLik(weekends.fitted.test.model) * nrow(train.weekends)/nrow(test.weekends)) 

### state = 14
# 'log Lik.' -94037.76 (df=265) training model
# 'log Lik.' -90553.72 (df=265) testing model


#----------------------------------------------------------------Question 4--------------------------------------------------------

########################################################## Anomaly Detection Weekdays ##################################################################################################
set.seed(298224)

weekdays.train.model = depmix(list(train.weekdays$Global_intensity ~ 1, train.weekdays$Global_active_power ~ 1, train.weekdays$Sub_metering_1  ~ 1),
                              nstates = 16, family = list(gaussian(), gaussian(), poisson()), data = train.weekdays, ntimes = ntimesVector(train.weekdays))
weekdays.fitted.train.model <- fit(weekdays.train.model)

weekdays.anomalies.model = depmix(list(anomalies.data.weekdays$Global_intensity ~ 1, anomalies.data.weekdays$Global_active_power ~ 1, anomalies.data.weekdays$Sub_metering_1  ~ 1),
                                  nstates = 16, family = list(gaussian(), gaussian(), poisson()), data = anomalies.data.weekdays, ntimes = ntimesVector(anomalies.data.weekdays))

weekdays.anomalies.parameters <-setpars(weekdays.anomalies.model,getpars(weekdays.train.model))
weekdays.anomalies.test.model <-fit(weekdays.anomalies.parameters)

(logLik(weekdays.fitted.train.model))
(logLik(weekdays.anomalies.test.model) * nrow(train.weekdays)/nrow(anomalies.data.weekdays))

# 'log Lik.' -189762.2 (df=335) training model
# 'log Lik.' -142879.3 (df=335) anomaly

########################################################## Anomaly Detection Weekends ##################################################################################################
set.seed(1)

weekends.train.model = depmix(list(train.weekends$Global_intensity ~ 1, train.weekends$Global_active_power ~ 1, train.weekends$Sub_metering_1  ~ 1),
                              nstates = 14, family = list(gaussian(), gaussian(), poisson()), data = train.weekends, ntimes = ntimesVector(train.weekends))
weekends.fitted.train.model <- fit(weekends.train.model)


weekends.anomalies.model = depmix(list(anomalies.data.weekends$Global_intensity ~ 1, anomalies.data.weekends$Global_active_power ~ 1, anomalies.data.weekends$Sub_metering_1  ~ 1),
                                  nstates = 14, family = list(gaussian(), gaussian(), poisson()), data = anomalies.data.weekends, ntimes = ntimesVector(anomalies.data.weekends))

weekends.anomalies.parameters <-setpars(weekends.anomalies.model,getpars(weekends.train.model))
weekends.anomalies.test.model <-fit(weekends.anomalies.parameters)

(logLik(weekends.fitted.train.model)) 
(logLik(weekends.anomalies.test.model) * nrow(train.weekends)/nrow(anomalies.data.weekends))


### state = 14
# 'log Lik.' -89932.19 (df=265) training model
# 'log Lik.' -148774.8 (df=265) anomaly model


############################################################### Moving Average #########################################################################################################

### Import Libraries
library(dplyr)
library(data.table)
library(tidyverse)
library(depmixS4)
library(ggplot2)
library(pracma)
library(corrplot)

rawData <- readr::read_csv("Dataset_GroupAssignment3.txt")
rawData[is.na(rawData)]<-0

#--------------------------------------------------------------Data Cleaning-------------------------------------------------------
### Add timestamp to dataframe for graphing
dateTime <- paste(rawData$Date, rawData$Time)
rawData$timestamp <- as.POSIXlt(strptime(dateTime, format = "%d/%m/%Y %H:%M:%S"))

### Label all the weeks for 1408
dateFormat <- as.Date(rawData$Date, format = "%d/%m/%Y")
rawData$weekNum <- format(dateFormat,"%V")

### Label weekday or weekend
daysWeek <- format(as.Date(rawData$Date, format = "%d/%m/%Y"),"%u") # days of the week 1-7 (Monday-Sunday)
rawData$week <- ifelse(daysWeek %in% c("6", "7"), "weekend", "weekdays")
rawData$dayWeek <- format(as.Date(rawData$Date, format = "%d/%m/%Y"),"%u")
rawData$year <- as.integer(format(as.Date(rawData$Date, format = "%d/%m/%Y"),"%y"))
### Label time of the day - morning or evening
dataTime <- as.ITime(format(rawData$Time, format = "%H:%M:%S"))  #RS added to correct error when run in Linux
rawData$STime <- as.integer(rawData$Time)  #RS added to correct error when run in Linux
startTime <- as.ITime("10:30")
endTime <- as.ITime("13:30")
rawData$include <- ifelse((dataTime >= startTime & dataTime < endTime), 1, 0)

anomalyData <- readr::read_csv("DatasetWithAnomalies_GroupAssignment3.txt")
anomalyData[is.na(anomalyData)]<-0

### Add timestamp to dataframe for graphing
dateTime <- paste(anomalyData$Date, anomalyData$Time)
anomalyData$timestamp <- as.POSIXlt(strptime(dateTime, format = "%d/%m/%Y %H:%M:%S"))

### Label all the weeks for 1408
dateFormat <- as.Date(anomalyData$Date, format = "%d/%m/%Y")
anomalyData$weekNum <- format(dateFormat,"%V")

### Label weekday or weekend
daysWeek <- format(as.Date(anomalyData$Date, format = "%d/%m/%Y"),"%u") # days of the week 1-7 (Monday-Sunday)
anomalyData$week <- ifelse(daysWeek %in% c("6", "7"), "weekend", "weekdays")
anomalyData$dayWeek <- format(as.Date(anomalyData$Date, format = "%d/%m/%Y"),"%u")
anomalyData$year <- as.integer(format(as.Date(anomalyData$Date, format = "%d/%m/%Y"),"%y"))
### Label time of the day - morning or evening
dataTime <- as.ITime(format(anomalyData$Time, format = "%H:%M:%S"))  #RS added to correct error when run in Linux
anomalyData$STime <- as.integer(anomalyData$Time)  #RS added to correct error when run in Linux
anomalyData$include <- ifelse((dataTime >= startTime & dataTime < endTime), 1, 0)

# Filter the data to the same week (as closely as the data permits) and the same time 
#
trainDayData <- subset(rawData, rawData$dayWeek == 3 & +
                         rawData$include == 1 & rawData$year == 8 & rawData$weekNum == 49)
trainDay = trainDayData$Global_intensity
#
trainEndData <- subset(rawData, rawData$dayWeek == 6 & +
                         rawData$include == 1 & rawData$year == 8 & rawData$weekNum == 49)
trainEnd = trainEndData$Global_intensity
#
testDayData <- subset(rawData, rawData$dayWeek == 3 & +
                        rawData$include == 1 & rawData$year == 9 & rawData$weekNum == 48)
testDay = testDayData$Global_intensity
#
testEndData <- subset(rawData, rawData$dayWeek == 6 & +
                        rawData$include == 1 & rawData$year == 9 & rawData$weekNum == 48)
testEnd = testEndData$Global_intensity
#
anomDayData <- subset(anomalyData, anomalyData$dayWeek == 3 & +
                        anomalyData$include == 1 & anomalyData$year == 9 & anomalyData$weekNum == 49)
anomDay = anomDayData$Global_intensity
#
anomEndData <- subset(anomalyData, anomalyData$dayWeek == 6 & +
                        anomalyData$include == 1 & anomalyData$year == 9 & anomalyData$weekNum == 49)
anomEnd = anomEndData$Global_intensity
#
T1 = .1 * max(trainDay)
T2 = .25 * max(trainDay)
T3 = .5 * max(trainDay)

mResult <- matrix( nrow = 3, ncol = 3)
colnames(mResult) = c("Train", "Test", "Anom")
rownames(mResult) = c("T1","T2", "T3")

MAtrainDay = movavg(trainDay, 10, "e")
diff = abs(trainDay - MAtrainDay)
diff1 <-ifelse((diff > T1), 1, 0)
mResult[1,1] = tabulate(diff1)
diff2 <-ifelse((diff > T2), 1, 0)
mResult[2,1] = tabulate(diff2)
diff3 <-ifelse((diff > T3), 1, 0)
mResult[3,1] = tabulate(diff3)

MAtestDay = movavg(testDay, 10, "e")
diff = abs(testDay - MAtestDay)
diff1 <-ifelse((diff > T1), 1, 0)
mResult[1,2] = tabulate(diff1)
diff2 <-ifelse((diff > T2), 1, 0)
mResult[2,2] = tabulate(diff2)
diff3 <-ifelse((diff > T3), 1, 0)
mResult[3,2] = tabulate(diff3)

MAanomDay = movavg(anomDay, 10, "e")
diff = abs(anomDay - MAanomDay)
diff1 <-ifelse((diff > T1), 1, 0)
mResult[1,3] = tabulate(diff1)
diff2 <-ifelse((diff > T2), 1, 0)
mResult[2,3] = tabulate(diff2)
diff3 <-ifelse((diff > T3), 1, 0)
mResult[3,3] = tabulate(diff3)

cat (" Weekday Moving Average Exceptions\n")
mResult
cat("\n")

MAtrainEnd = movavg(trainEnd, 10, "e")
diff = abs(trainEnd - MAtrainEnd)
diff1 <-ifelse((diff > T1), 1, 0)
mResult[1,1] = tabulate(diff1)
diff2 <-ifelse((diff > T2), 1, 0)
mResult[2,1] = tabulate(diff2)
diff3 <-ifelse((diff > T3), 1, 0)
mResult[3,1] = tabulate(diff3)

MAtestEnd = movavg(testEnd, 10, "e")
diff = abs(testEnd - MAtestEnd)
diff1 <-ifelse((diff > T1), 1, 0)
mResult[1,2] = tabulate(diff1)
diff2 <-ifelse((diff > T2), 1, 0)
mResult[2,2] = tabulate(diff2)
diff3 <-ifelse((diff > T3), 1, 0)
mResult[3,2] = tabulate(diff3)

MAanomEnd = movavg(anomEnd, 10, "e")
diff = abs(anomEnd - MAanomEnd)
diff1 <-ifelse((diff > T1), 1, 0)
mResult[1,3] = tabulate(diff1)
diff2 <-ifelse((diff > T2), 1, 0)
mResult[2,3] = tabulate(diff2)
diff3 <-ifelse((diff > T3), 1, 0)
mResult[3,3] = tabulate(diff3)

cat (" WeekEnd Moving Average Exceptions\n")
mResult
cat("\n")

