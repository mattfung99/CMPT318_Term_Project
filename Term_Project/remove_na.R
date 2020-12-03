#
#    Group Assignment Term Project Part 1
#
#install.packages("devtools")
#library(devtools)
#install_github("vqv/ggbiplot")
### Import Libraries
library(dplyr)
library(data.table)
library(tidyverse)
library(depmixS4)
library(ggplot2)
library(ggbiplot)
library(pracma)
library(corrplot)

### Set Directory
#getwd()
#setwd("--- Set Your Directory Here ---")

#require(tidyverse)
rawData <- readr::read_csv("TermProjectData.txt")
badData <- rawData[rowSums(is.na(rawData)) > 0,]
unique(badData$Date)

#
#This code is painfully slow. Takes 2 - 3 hours. 
# The code could be improved but having saved the "corrected" data it doesn't need to be run again
#
cleanCol <- c("Global_active_power","Global_reactive_power","Global_intensity","Voltage", "Sub_metering_1","Sub_metering_2","Sub_metering_3")
j = 1
while (j <= length(cleanCol))
{
  cleanCol[j]
  i = 1
  repValue = mean(rawData[[cleanCol[j]]],na.rm = TRUE)
  while (i <= nrow(rawData))
  {
    if(is.na(rawData[i, cleanCol[j]]))
    {
      rawData[i, cleanCol[j]] = repValue
    }
    else
    {
      repValue = rawData[i, cleanCol[j]]
    }
    i = i + 1 # next value
  }
  j = j + 1
}

write.table(rawData, "TermProjectDataCleaned.txt", sep=",")

