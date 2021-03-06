### Import Libraries
library(dplyr)
library(data.table)
library(tidyverse)
library(ggplot2)
library(depmixS4)
library(pracma)

### Set Directory
getwd()
setwd("--- Set Your Directory Here ---")

require(tidyverse)
rawData <- readr::read_csv("Dataset3.txt")
rawData[is.na(rawData)] <- 0

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

### Label time of the day - morning or evening
dataTime <- as.ITime(format(rawData$Time, format = "%H:%M:%S"))  #RS added to correct error when run in Linux
rawData$STime <- as.ITime(format(rawData$Time, format = "%H:%M:%S"))  #RS added to correct error when run in Linux
startTime <- as.ITime("06:00")
endTime <- as.ITime("9:00")
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

### Create Dataframe for Week 1, Monday, 6:00AM - 6:00PM
dataWeek1 <- filter(rawData, weekNum == "05", daysWeek == "1", day == "morning")

### Create Dataframe for Active Power, Week 5, Monday, 6:00AM - 6:00PM
activePowerWeek1 <- dataWeek1[, c("timestamp", "STime", "Global_active_power")]

### Format the time for Week 1
activePowerWeek1$Time <-format(as.POSIXct(activePowerWeek1$timestamp), format = "%H:%M:%S")
activePowerWeek1$Time <- as.POSIXct(activePowerWeek1$Time, format = "%H:%M:%S")

### Create Dataframe for Week 14, Monday, 6:00AM - 6:00PM
dataWeek14 <- filter(rawData, weekNum == "14", daysWeek == "1", day == "morning")

### Create Dataframe for Active Power, Week 14, Monday, 6:00AM - 6:00PM
activePowerWeek14 <- dataWeek14[, c("timestamp", "STime", "Global_active_power")]

### Format the time for Week 14
activePowerWeek14$Time <-format(as.POSIXct(activePowerWeek14$timestamp), format = "%H:%M:%S")
activePowerWeek14$Time <- as.POSIXct(activePowerWeek14$Time, format = "%H:%M:%S")

### Create Dataframe for Week 28, Monday, 6:00AM - 6:00PM
dataWeek28 <- filter(rawData, weekNum == "28", daysWeek == "1", day == "morning")

### Create Dataframe for Active Power, Week 28, Monday, 6:00AM - 6:00PM
activePowerWeek28 <- dataWeek28[, c("timestamp", "STime", "Global_active_power")]

### Format the time for Week 28
activePowerWeek28$Time <-format(as.POSIXct(activePowerWeek28$timestamp), format = "%H:%M:%S")
activePowerWeek28$Time <- as.POSIXct(activePowerWeek28$Time, format = "%H:%M:%S")

### Create Dataframe for Week 42, Monday, 6:00AM - 6:00PM
dataWeek42 <- filter(rawData, weekNum == "42", daysWeek == "1", day == "morning")

### Create Dataframe for Active Power, Week 42, Monday, 6:00AM - 6:00PM
activePowerWeek42 <- dataWeek42[, c("timestamp", "STime", "Global_active_power")]

### Format the time for Week 42
activePowerWeek42$Time <-format(as.POSIXct(activePowerWeek42$timestamp), format = "%H:%M:%S")
activePowerWeek42$Time <- as.POSIXct(activePowerWeek42$Time, format = "%H:%M:%S")

### Plot 
p1 <- ggplot(activePowerWeek1, mapping = aes(x=Time, y=Global_active_power)) + 
  geom_point(color="light blue") +
  ggtitle("Week 1 Monday Daytime Global Active Power") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(y="Global Active Power (in Kilowatt)", x = "Time (in Hours)", color ="Fit Type") + 
  theme(plot.title = element_text(hjust=0.5), legend.position = "bottom") 

p2 <- ggplot(activePowerWeek14, mapping = aes(x=Time, y=Global_active_power)) + 
  geom_point(color="light blue") +
  ggtitle("Week 14 Monday Daytime Global Active Power") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(y="Global Active Power (in Kilowatt)", x = "Time (in Hours)", color ="Fit Type") + 
  theme(plot.title = element_text(hjust=0.5), legend.position = "bottom") 

p3 <- ggplot(activePowerWeek28, mapping = aes(x=Time, y=Global_active_power)) + 
  geom_point(color="light blue") +
  ggtitle("Week 28 Monday Daytime Global Active Power") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(y="Global Active Power (in Kilowatt)", x = "Time (in Hours)", color ="Fit Type") + 
  theme(plot.title = element_text(hjust=0.5), legend.position = "bottom") 

p4 <- ggplot(activePowerWeek42, mapping = aes(x=Time, y=Global_active_power)) + 
  geom_point(color="light blue") +
  ggtitle("Week 42 Monday Daytime Global Active Power") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(y="Global Active Power (in Kilowatt)", x = "Time (in Hours)", color ="Fit Type") + 
  theme(plot.title = element_text(hjust=0.5), legend.position = "bottom") 

### 4 Plots 1 Diagram
multiplot(p1, p2, p3, p4, cols=2)

#----------------------------------------------------------------Question 2--------------------------------------------------------

powerData <- read.table("Dataset3.txt", sep = ',', header = TRUE)
powerData <- na.omit(powerData[, 1:9])

### timestamp for graphing
dateTimeCombine <- paste(powerData$Date, powerData$Time)
powerData$timestamp <- as.POSIXlt(strptime(dateTimeCombine, format = "%d/%m/%Y %H:%M:%S"))

### Label all the weeks and days of the week
formatingDates <- as.Date(powerData$Date, format = "%d/%m/%Y")
powerData$weekNumber <- format(formatingDates,"%V")
powerData$dayNumber <- as.integer(format(formatingDates,"%u"))

### Filter out all the Mondays
powerDataMonday <- filter(powerData, dayNumber == 1) # Monday

### Label time of the day - morning or evening
STime <- as.ITime(format(powerDataMonday$Time, format = "%H:%M:%S"))  #RS added to correct error when run in Linux
startingTime <- as.ITime("07:00")
endingTime <- as.ITime("09:30")
mondayMorning <- cbind(powerDataMonday, STime)
mondayMorning <- filter(mondayMorning, STime >= startingTime & STime <= endingTime)

grouping <- group_by(mondayMorning, Date)
groupCount <- summarise(grouping, count=n())

set.seed(298224)

minState <- 2
maxState <- 20
numStates <- minState:maxState

# Create a container for BICs
BIC.vector = c("States", "BICs", "logLik")
BIC.models = array(0, dim = c(maxState, length(BIC.vector)))
colnames(BIC.models) = BIC.vector

for (i in numStates)
{
  model = depmix(response = mondayMorning$Global_intensity ~ 1, nstates = i, family = gaussian(), data = mondayMorning, ntimes = groupCount$count)
  fitModel <- fit(model)
  
  # Extract states, BICs, and Log likelihood
  BIC.models[i, "States"] <- i
  BIC.models[i, "BICs"] <- BIC(fitModel)
  BIC.models[i, "logLik"] <- logLik(fitModel)
}
BIC.models

# final plot
par(mar = c(5, 5, 3, 5))
plot(BIC.models[1:20, 2], type ="l", ylab = "BICs",
     main = "BIC and Log-Likelihood for each state", xlab = "States", col = "blue")

par(new = TRUE)
plot(BIC.models[1:20,3], type = "l", xaxt = "n", yaxt = "n", ylab = "", xlab = "", col = "red", lty = 2)

axis(side = 4)
axis(side = 1, at = seq(0, 20, by = 1))

mtext("Log Likelihood", side = 4, line = 3)
legend("top", c("BICs", "Log-Likelihoods"), col = c("blue", "red"), lty = c(1, 2))
#----------------------------------------------------------------Question 3--------------------------------------------------------

OneWeek = na.omit(rawData$Global_intensity[rawData$weekNum == "11"])
plot(OneWeek)

# Set the threshold at 1/5 of the peak Intensity 
Threshold = max(OneWeek)/10
MA1Week = movavg(OneWeek, 10, "s")
diff = abs(OneWeek - MA1Week)
max(diff)
plot(diff)
diff <-ifelse((diff > Threshold), 1, 0)
cat(tCount = tabulate(diff))
plot(diff)

MA1Week = movavg(OneWeek, 10, "t")
diff = abs(OneWeek - MA1Week)
max(diff)
plot(diff)
diff <-ifelse((diff > Threshold), 1, 0)
cat(tCount = tabulate(diff))
plot(diff)

MA1Week = movavg(OneWeek, 10, "w")
diff = abs(OneWeek - MA1Week)
max(diff)
plot(diff)
diff <-ifelse((diff > Threshold), 1, 0)
cat(tCount = tabulate(diff))
plot(diff)

MA1Week = movavg(OneWeek, 10, "e")
diff = abs(OneWeek - MA1Week)
max(diff)
plot(diff)
diff <-ifelse((diff > Threshold), 1, 0)
cat(tCount = tabulate(diff))
plot(diff)