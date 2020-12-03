### Import Libraries
library(data.table)
library(stats)
library(ggplot2)
library(ggbiplot) # Uses plyr, need to load plyr before dplyr or there will be conflicts
library(dplyr)

### Set Directory
#getwd()
#setwd("--- Set Your Directory Here ---")

rawData = read.table("TermProjectDataCleaned.txt", sep = ',', header = TRUE)
#rawData[is.na(rawData)] <- 0

###############################################################################################################################################
# ----------------------------------------------------------------- PART 1---------------------------------------------------------------------
###############################################################################################################################################

# --------------------------------------------------------- 1. Train and Test HMMS ------------------------------------------------------------
# 
#----------------------------------------------------------------Data Cleaning-----------------------------------------------------------------

### Add timestamp to dataframe for graphing
dateTime <- paste(rawData$Date, rawData$Time)
rawData$timestamp <- as.POSIXlt(strptime(dateTime, format = "%d/%m/%Y %H:%M:%S"))

### Label all the weeks for the rawData
dateFormat <- as.Date(rawData$Date, format = "%d/%m/%Y")
rawData$weekNum <- format(dateFormat,"%V")

### Label weekday or weekend
daysWeek <- format(as.Date(rawData$Date, format = "%d/%m/%Y"),"%u") # days of the week 1-7 (Monday-Sunday)
rawData$week <- ifelse(daysWeek %in% c("6", "7"), "weekend", "weekdays")
rawData$dayWeek <- format(as.Date(rawData$Date, format = "%d/%m/%Y"),"%u")
rawData$year <- as.integer(format(as.Date(rawData$Date, format = "%d/%m/%Y"),"%y"))

### Label time of the day - window or other
dataTime <- as.ITime(format(rawData$Time, format = "%H:%M:%S"))  # RS added to correct error when run in Linux
rawData$STime <- as.integer(rawData$Time)  # RS added to correct error when run in Linux
startTime <- as.ITime("00:00")
endTime <- as.ITime("04:00")
vSTime <- rawData$STime
vPeriod <- vector(mode="integer",length(vSTime))
i = 1
while (i <= length(vPeriod))
{
  if ((vSTime[i] > 0)&(vSTime[i] <= 240)) vPeriod[i] <- 1
  if ((vSTime[i] > 240)&(vSTime[i] <= 480)) vPeriod[i] <- 2
  if ((vSTime[i] > 480)&(vSTime[i] <= 720)) vPeriod[i] <- 3
  if ((vSTime[i] > 720)&(vSTime[i] <= 960)) vPeriod[i] <- 4
  if ((vSTime[i] > 960)&(vSTime[i] <= 1200)) vPeriod[i] <- 5
  if ((vSTime[i] > 1200)&(vSTime[i] <= 1440)) vPeriod[i] <- 6
  i = i + 1
}
rawData$dayPeriod <- vPeriod

#---------------------------------------------------------------Setup Data for PCA-------------------------------------------------------------

### Calculate Average for every minute during the selected window
calculateAverage <- function(responseVariable)
{
  
  ### Filter for the selected response variable
  responseVariableData <- filteredData[, c("timestamp", "STime", responseVariable)]
  
  ### Format the time for Week 5
  responseVariableData$Time <-format(as.POSIXct(responseVariableData$timestamp), format = "%H:%M:%S")
  responseVariableData$Time <- as.POSIXct(responseVariableData$Time, format = "%H:%M:%S")
  
  ### Group and average the time periods 
  avgResponseVariable <- responseVariableData %>%
    group_by(Time, STime) %>%
    summarise(avg_value = mean(get(responseVariable)))
  
  ### Standardize the data
  sdAVR = sd(avgResponseVariable$avg_value)
  meanAVR = mean(avgResponseVariable$avg_value)
  avgResponseVariable$avg_value <- (avgResponseVariable$avg_value - meanAVR) / sdAVR
  
  ### Return updated dataframe
  return(avgResponseVariable)
}

aPCAs <- array(0, dim = 42)
j = 1
while (j <= 7)
{
  i = 1
  while (i <= 6)
  {
    ### Create Dataframe for Thursday, 10:30AM - 1:30PM
    tempData <- subset(rawData, daysWeek == as.character(j))# & rawData$period == (i)))
    filteredData <- subset(tempData, dayPeriod == i)
    
    ### Calculate Average and Standardize for all 7 response variables
    globalIntensityData <- calculateAverage("Global_intensity")
    globalActivePowerData <- calculateAverage("Global_active_power")
    globalReactivePowerData <- calculateAverage("Global_reactive_power")
    voltageData <- calculateAverage("Voltage")
    subMetering1Data <- calculateAverage("Sub_metering_1")
    subMetering2Data <- calculateAverage("Sub_metering_2")
    subMetering3Data <- calculateAverage("Sub_metering_3")
    
    #-------------------------------------------------------------Compute and Plot PCA-------------------------------------------------------------
    
    ### Combine the average values from all 7 response variables
    ### From Left to Right: Intensity, Active Power, Reactive Power, Voltage, Sub Metering 1, Sub Metering 2, Sub Metering 3
    pcaData <- globalIntensityData[, c("avg_value")]
    pcaData <- cbind(data.frame(pcaData), globalActivePowerData[, c("avg_value")])
    pcaData <- cbind(data.frame(pcaData), globalReactivePowerData[, c("avg_value")])
    pcaData <- cbind(data.frame(pcaData), voltageData[, c("avg_value")])
    pcaData <- cbind(data.frame(pcaData), subMetering1Data[, c("avg_value")])
    pcaData <- cbind(data.frame(pcaData), subMetering2Data[, c("avg_value")])
    pcaData <- cbind(data.frame(pcaData), subMetering3Data[, c("avg_value")])
    
    ### Rename Labels
    names(pcaData)[1] <- "Global_intensity"
    names(pcaData)[2] <- "Global_active_power"
    names(pcaData)[3] <- "Global_reactive_power"
    names(pcaData)[4] <- "Voltage"
    names(pcaData)[5] <- "Sub_metering_1"
    names(pcaData)[6] <- "Sub_metering_2"
    names(pcaData)[7] <- "Sub_metering_3"
    
    ### Create PCA
    Test.pca <- prcomp(pcaData, center = TRUE, scale. = TRUE)
    str(Test.pca)
    
    ### Plot PCA
    ggbiplot(Test.pca, scale=.75, alpha = 0.1, var.scale = 1.0) +
      ggtitle("PCA for 7 Response Variables") + theme(plot.title = element_text(hjust = 0.5))
    
    ### PCA Results
    
    sink ("PCAoutput.txt",append = TRUE)
    cat("\n day ", j," period ", i, "\n")
    sink()
    sink ("PCAoutput.txt",append = TRUE)
    print(summary(Test.pca))
    sink()
    sink ("PCAoutput.txt",append = TRUE)
    print(Test.pca$rotation)
    sink()
    
    gFile <- paste0("d",j,"p",i,".png")
    plotTitle <- paste0("Results for day ",j," period ",i)
    
    plot <- ggbiplot(Test.pca, scale=.75, alpha = 0.1, var.scale = 1.0) +
      ggtitle(plotTitle) + theme(plot.title = element_text(hjust = 0.5))
    ggsave(gFile, plot = plot)
    aPCAs[((j - 1)*6) + i] = list(Test.pca)
    i = i + 1
  }
  j = j + 1 # increment
}
