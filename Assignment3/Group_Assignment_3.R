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

### Create Dataframe for Week 5, Wednesday, 6:00AM - 10:00PM
dataWeek5Weekday <- filter(rawData, weekNum == "05", daysWeek == "3", day == "morning")

### Create Dataframe for Active Power, Week 5, Wednesday, 6:00AM - 10:00PM
intensityWeek5Weekday <- dataWeek5Weekday[, c("timestamp", "STime", "Global_intensity")]

### Format the time for Week 5
intensityWeek5Weekday$Time <-format(as.POSIXct(intensityWeek5Weekday$timestamp), format = "%H:%M:%S")
intensityWeek5Weekday$Time <- as.POSIXct(intensityWeek5Weekday$Time, format = "%H:%M:%S")


### Create Dataframe for Week 15, Wednesday, 6:00AM - 10:00PM
dataWeek15Weekday <- filter(rawData, weekNum == "15", daysWeek == "3", day == "morning")

### Create Dataframe for Active Power, Week 15, Wednesday, 6:00AM - 10:00PM
intensityWeek15Weekday <- dataWeek15Weekday[, c("timestamp", "STime", "Global_intensity")]

### Format the time for Week 15
intensityWeek15Weekday$Time <-format(as.POSIXct(intensityWeek15Weekday$timestamp), format = "%H:%M:%S")
intensityWeek15Weekday$Time <- as.POSIXct(intensityWeek15Weekday$Time, format = "%H:%M:%S")


### Create Dataframe for Week 35, Wednesday, 6:00AM - 10:00PM
dataWeek35Weekday <- filter(rawData, weekNum == "35", daysWeek == "3", day == "morning")

### Create Dataframe for Active Power, Week 35, Wednesday, 6:00AM - 10:00PM
intensityWeek35Weekday <- dataWeek35Weekday[, c("timestamp", "STime", "Global_intensity")]

### Format the time for Week 35
intensityWeek35Weekday$Time <-format(as.POSIXct(intensityWeek35Weekday$timestamp), format = "%H:%M:%S")
intensityWeek35Weekday$Time <- as.POSIXct(intensityWeek35Weekday$Time, format = "%H:%M:%S")


### Create Dataframe for Week 44, Wednesday, 6:00AM - 10:00PM
dataWeek44Weekday <- filter(rawData, weekNum == "44", daysWeek == "3", day == "morning")

### Create Dataframe for Active Power, Week 44, Wednesday, 6:00AM - 10:00PM
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


### Create Dataframe for Week 5, Saturday, 6:00AM - 10:00PM
dataWeek5Weekday <- filter(rawData, weekNum == "05", daysWeek == "6", day == "morning")

### Create Dataframe for Active Power, Week 5, Saturday, 6:00AM - 10:00PM
intensityWeek5Weekday <- dataWeek5Weekday[, c("timestamp", "STime", "Global_intensity")]

### Format the time for Week 5
intensityWeek5Weekday$Time <-format(as.POSIXct(intensityWeek5Weekday$timestamp), format = "%H:%M:%S")
intensityWeek5Weekday$Time <- as.POSIXct(intensityWeek5Weekday$Time, format = "%H:%M:%S")


### Create Dataframe for Week 15, Saturday, 6:00AM - 10:00PM
dataWeek15Weekday <- filter(rawData, weekNum == "15", daysWeek == "6", day == "morning")

### Create Dataframe for Active Power, Week 15, Saturday, 6:00AM - 10:00PM
intensityWeek15Weekday <- dataWeek15Weekday[, c("timestamp", "STime", "Global_intensity")]

### Format the time for Week 15
intensityWeek15Weekday$Time <-format(as.POSIXct(intensityWeek15Weekday$timestamp), format = "%H:%M:%S")
intensityWeek15Weekday$Time <- as.POSIXct(intensityWeek15Weekday$Time, format = "%H:%M:%S")


### Create Dataframe for Week 35, Saturday, 6:00AM - 10:00PM
dataWeek35Weekday <- filter(rawData, weekNum == "35", daysWeek == "6", day == "morning")

### Create Dataframe for Active Power, Week 35, Saturday, 6:00AM - 10:00PM
intensityWeek35Weekday <- dataWeek35Weekday[, c("timestamp", "STime", "Global_intensity")]

### Format the time for Week 35
intensityWeek35Weekday$Time <-format(as.POSIXct(intensityWeek35Weekday$timestamp), format = "%H:%M:%S")
intensityWeek35Weekday$Time <- as.POSIXct(intensityWeek35Weekday$Time, format = "%H:%M:%S")


### Create Dataframe for Week 44, Saturday, 6:00AM - 10:00PM
dataWeek44Weekday <- filter(rawData, weekNum == "44", daysWeek == "6", day == "morning")

### Create Dataframe for Active Power, Week 44, Saturday, 6:00AM - 10:00PM
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

