### Import Libraries
library(data.table)
library(stats)
library(ggplot2)
library(ggbiplot) # Uses plyr, need to load plyr before dplyr or there will be conflicts
library(dplyr)

### Set Directory
getwd()
setwd("--- Set Your Directory Here ---")

rawData = read.table("TermProjectDataCleaned.txt", sep = ',', header = TRUE)

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
startTime <- as.ITime("16:00")
endTime <- as.ITime("19:59")
rawData$day <- ifelse((dataTime >= startTime & dataTime <= endTime), "window", "other")

### Adjust 0 values in Voltage (they are not meaningful)
rawData$Voltage[rawData$Voltage < 200.0] = 220.0  

#---------------------------------------------------------------Setup Data for PCA-------------------------------------------------------------

### Create Dataframe for Tuesday, 4:00PM - 7:59PM
filteredData <- filter(rawData, daysWeek == "2", day == "window")

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

### Calculate Average for all 7 response variables
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
ggbiplot(Test.pca, scale=.75, alpha = 0.0, var.scale = 1.0) +
  ggtitle("PCA for 7 Response Variables") + theme(plot.title = element_text(hjust = 0.5))

### PCA Results
summary(Test.pca)

### PCA Correlations
Test.pca

#-------------------------------------------------------------Corr Matrix for Window-----------------------------------------------------------

### Detach ggbiplot and plyr
detach("package:ggbiplot", unload = TRUE)
detach("package:plyr", unload = TRUE) 

### Import library
library(corrplot)

### Subset all relevant attributes
corrRawData <- filteredData[, c(3:9)]

### Construct a correlation matrix
corrMatrix <- (round(cor(corrRawData, method = "pearson"), 3))
corrMatrix

# Visualize correlation matrix 
corrplot(corrMatrix, method = "color")

#------------------------------------------------Setup Testing and Training Data For Univariate HMM--------------------------------------------

### Setup train data for Global Intensity
trainIntensityData <- filter(filteredData, year < "9")  ### Data for all Tuesday during 4:00PM - 7:59PM from 2006 - 2008
trainIntensityData <- trainIntensityData[c(1, 6)]

### Setup test data for Global Intensity
testIntensityData <- filter(filteredData, year == "9")  ### Data for all Tuesdays during 10:30AM - 1:30PM in 2009
testIntensityData <- testIntensityData[c(1, 6)]

#-------------------------------------------------------------Setup Univariate HMM-------------------------------------------------------------

### Detach corrplot
detach("package:corrplot", unload = TRUE)

### Import dplyr again to use summarize
library(dplyr)
library(depmixS4)
library(pracma)

### Create groups 
trainGroup <- group_by(trainIntensityData, Date)
trainGroupCount <- summarise(trainGroup, count = n())

testGroup <- group_by(testIntensityData, Date)
testGroupCount <- summarise(testGroup, count = n())

### Set Seed
set.seed(298224)

### Define number of states
minState <- 2
maxState <- 20
numStates <- minState:maxState

# Create a container for BICs
BIC.vector = c("States", "BICs", "logLik")
BIC.models = array(0, dim = c(maxState, length(BIC.vector)))
colnames(BIC.models) = BIC.vector

#-------------------------------------------------------------Train Univariate HMM-------------------------------------------------------------

### Iterate through states 2 - 20
for (i in numStates)
{
  model = depmix(response = trainIntensityData$Global_intensity ~ 1, nstates = i, family = gaussian(), data = trainIntensityData, ntimes = trainGroupCount$count)
  fitModel <- fit(model)
  
  # Extract states, BICs, and Log likelihood
  BIC.models[i, "States"] <- i
  BIC.models[i, "BICs"] <- BIC(fitModel)
  BIC.models[i, "logLik"] <- logLik(fitModel)
}
BIC.models

### Plot BIC VS Log-Likelihood
par(mar = c(5, 5, 3, 5))
plot(BIC.models[1:20, 2], type ="l", ylab = "BICs",
     main = "BIC and Log-Likelihood for each state", xlab = "States", col = "blue")

par(new = TRUE)
plot(BIC.models[1:20,3], type = "l", xaxt = "n", yaxt = "n", ylab = "", xlab = "", col = "red", lty = 2)

axis(side = 4)
axis(side = 1, at = seq(0, 20, by = 1))

mtext("Log Likelihood", side = 4, line = 3)
legend("top", c("BICs", "Log-Likelihoods"), col = c("blue", "red"), lty = c(1, 2))

#--------------------------------------------------------------Test Univariate HMM-------------------------------------------------------------

### Set Seed
set.seed(298224)

### Train model for ideal state
trainIntensityModel = depmix(response = trainIntensityData$Global_intensity ~ 1, nstates = 11, family = gaussian(), data = trainIntensityData, ntimes = trainGroupCount$count)

### Fit model
trainFitModel <- fit(trainIntensityModel)

### Use forwardbackward algorithm
trainIntensityObject <- forwardbackward(trainFitModel)

################################################################

### Test model for ideal state
testIntensityModel = depmix(response = testIntensityData$Global_intensity ~ 1, nstates = 11, family = gaussian(), data = testIntensityData, ntimes = testGroupCount$count)

### Run getpars to get the parameters from a fitted model
testIntensity <- setpars(testIntensityModel, getpars(trainFitModel))

### Use forwardbackward algorithm
testIntensityObject <- forwardbackward(testIntensity)

### Output
(trainIntensityObject$logLike)
(testIntensityObject$logLike * nrow(trainIntensityData) / nrow(testIntensityData))

################################################################

### Train State 11
###   LogLik: -21598.04

### Test State 11
###   Loglik: -22155.41

### Difference = 557.37

# ---------------------------------------------------------- 2. Anomaly Detection -------------------------------------------------------------
# 
#------------------------------------------------------------Read in Anomaly Data--------------------------------------------------------------

### Read In
anomalyData1 = read.table("Data1(WithAnomalies).txt", sep = ',', header = TRUE)
anomalyData2 = read.table("Data2(WithAnomalies).txt", sep = ',', header = TRUE)
anomalyData3 = read.table("Data3(WithAnomalies).txt", sep = ',', header = TRUE)

#-------------------------------------------------------------Clean Anomaly Data---------------------------------------------------------------

setAnomalyData <- function(df)
{
  ### Add timestamp to dataframe for graphing
  dateTime <- paste(df$Date, df$Time)
  df$timestamp <- as.POSIXlt(strptime(dateTime, format = "%d/%m/%Y %H:%M:%S"))
  
  ### Label all the weeks for the df
  dateFormat <- as.Date(df$Date, format = "%d/%m/%Y")
  df$weekNum <- format(dateFormat,"%V")
  
  ### Label weekday or weekend
  daysWeek <- format(as.Date(df$Date, format = "%d/%m/%Y"),"%u") # days of the week 1-7 (Monday-Sunday)
  
  ### Label time of the day - window or other
  dataTime <- as.ITime(format(df$Time, format = "%H:%M:%S"))  # RS added to correct error when run in Linux
  df$STime <- as.integer(df$Time)  # RS added to correct error when run in Linux
  startTime <- as.ITime("16:00")
  endTime <- as.ITime("19:59")
  df$day <- ifelse((dataTime >= startTime & dataTime <= endTime), "window", "other")
  
  ### Filter to get time window
  df <- filter(df, daysWeek == "2", day == "window")
  df <- df[c(1,6)]
  
  ### return dataframe
  return (df)
}

### Create dataframes for anomaly1, anomaly2, anomaly3
anomaly1IntensityData <- setAnomalyData(anomalyData1)
anomaly2IntensityData <- setAnomalyData(anomalyData2)
anomaly3IntensityData <- setAnomalyData(anomalyData3)

#-------------------------------------------------------------Anomaly Detection 1--------------------------------------------------------------

### Create Groups
a1Group <- group_by(anomaly1IntensityData, Date)
a1GroupCount <- summarise(a1Group, count = n())

### Set Seed
set.seed(298224)

### Training model for ideal state
trainIntensityModel = depmix(response = trainIntensityData$Global_intensity ~ 1, nstates = 11, family = gaussian(), data = trainIntensityData, ntimes = trainGroupCount$count)

### Fit model
trainFitModel <- fit(trainIntensityModel)

### Use forwardbackward algorithm
trainIntensityObject <- forwardbackward(trainFitModel)

################################################################

### Test model with anomaly data 1 for ideal state
a1IntensityModel = depmix(response = anomaly1IntensityData$Global_intensity ~ 1, nstates = 11, family = gaussian(), data = anomaly1IntensityData, ntimes = a1GroupCount$count)

### Run getpars to get the parameters from a fitted model
a1Intensity <- setpars(a1IntensityModel, getpars(trainFitModel))

### Use forwardbackward algorithm
a1IntensityObject <- forwardbackward(a1Intensity)

### Output
(trainIntensityObject$logLike)
(a1IntensityObject$logLike * nrow(trainIntensityData) / nrow(anomaly1IntensityData))

################################################################

### Train State 11
###   LogLik: -21598.04

### Test State 11
###   Loglik: -29227.98

### Difference = 7629.94

#-------------------------------------------------------------Anomaly Detection 2--------------------------------------------------------------

### Create Groups
a2Group <- group_by(anomaly2IntensityData, Date)
a2GroupCount <- summarise(a2Group, count = n())

### Set Seed
set.seed(298224)

### Training model for ideal state
trainIntensityModel = depmix(response = trainIntensityData$Global_intensity ~ 1, nstates = 11, family = gaussian(), data = trainIntensityData, ntimes = trainGroupCount$count)

### Fit model
trainFitModel <- fit(trainIntensityModel)

### Use forwardbackward algorithm
trainIntensityObject <- forwardbackward(trainFitModel)

################################################################

### Test model with anomaly data 1 for ideal state
a2IntensityModel = depmix(response = anomaly2IntensityData$Global_intensity ~ 1, nstates = 11, family = gaussian(), data = anomaly2IntensityData, ntimes = a2GroupCount$count)

### Run getpars to get the parameters from a fitted model
a2Intensity <- setpars(a2IntensityModel, getpars(trainFitModel))

### Use forwardbackward algorithm
a2IntensityObject <- forwardbackward(a2Intensity)

### Output
(trainIntensityObject$logLike)
(a2IntensityObject$logLike * nrow(trainIntensityData) / nrow(anomaly2IntensityData))

################################################################

### Train State 11
###   LogLik: -21598.04

### Test State 11
###   Loglik: -48798.82

### Difference = 27200.78

#-------------------------------------------------------------Anomaly Detection 3--------------------------------------------------------------

### Create Groups
a3Group <- group_by(anomaly3IntensityData, Date)
a3GroupCount <- summarise(a3Group, count = n())

### Set Seed
set.seed(298224)

### Training model for ideal state
trainIntensityModel = depmix(response = trainIntensityData$Global_intensity ~ 1, nstates = 11, family = gaussian(), data = trainIntensityData, ntimes = trainGroupCount$count)

### Fit model
trainFitModel <- fit(trainIntensityModel)

### Use forwardbackward algorithm
trainIntensityObject <- forwardbackward(trainFitModel)

################################################################

### Test model with anomaly data 1 for ideal state
a3IntensityModel = depmix(response = anomaly3IntensityData$Global_intensity ~ 1, nstates = 11, family = gaussian(), data = anomaly3IntensityData, ntimes = a3GroupCount$count)

### Run getpars to get the parameters from a fitted model
a3Intensity <- setpars(a3IntensityModel, getpars(trainFitModel))

### Use forwardbackward algorithm
a3IntensityObject <- forwardbackward(a3Intensity)

### Output
(trainIntensityObject$logLike)
(a3IntensityObject$logLike * nrow(trainIntensityData) / nrow(anomaly3IntensityData))

################################################################

### Train State 11
###   LogLik: -21598.04

### Test State 11
###   Loglik: -49608.86

### Difference = 28010.82
