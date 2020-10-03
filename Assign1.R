# Import Libraries
library(ggplot2)

###############################################

# Set the directory
getwd()
setwd("C:/Users/offic/Documents/SFU/FALL2020/CMPT318/Assignments/A1")

###############################################

# Load the data
data = read.table("Group_Assignment_1_Dataset.txt", header = TRUE, sep =',')
data$SDate = as.POSIXlt(data$Date, tz = "", "%d/%m/%Y")

###############################################

# Dataset for Week 29
datasmall <- subset(data, SDate >= '2007/07/16' & SDate < '2007/07/23')
head(datasmall)

###############################################

# Define representative time windows
#     Day hours (7:30 AM - 5:00 PM)
#   Night hours (5:00 PM - 7:30 AM)

# Create data sets for representative time windows
datasmall$date <- strptime(datasmall$date, "%d-%m-%y %H:%M:%S")
datasmall$date <- as.POSIXct(datasmall$date)
str(datasmall)

# data set for weekday day
data_weekday_day <- subset(datasmall, (data$Date >= '2007/07/16' & data$Date < '2007/07/21') & 
                                      (data$Time >= '07:30:00' & data$Time < '17:00:00'))
                             
head(data_weekday_day)

#myts <- ts(datasmall$Global_intensity, frequency = 24*60, start = 0)
#plot(myts)
