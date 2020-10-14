library(dplyr)
library(data.table)
library(psych)
library(corrplot)
library(ggplot2)

### Read in text file
rawData = read.table("Group_Assignment_1_Dataset.txt", sep = ',', header = TRUE)

#--------------------------------------------------------------Data Cleaning-------------------------------------------------------
### Add timestamp to dataframe for graphing
dateTime <- paste(rawData$Date, rawData$Time)
rawData$timestamp <- as.POSIXlt(strptime(dateTime, format = "%d/%m/%Y %H:%M:%S"))

### Label all the weeks for 2007 
dateFormat <- as.Date(rawData$Date, format = "%d/%m/%Y")
rawData$weekNum <- format(dateFormat,"%V")

### Select the appropriate week
weekSelection <- "29"
data <- filter(rawData, weekNum == weekSelection)

### Label time of the day - morning or evening
# dataTime <- as.ITime(data$Time)  //RBS
dataTime <- as.ITime(format(data$Time, format = "%H:%M:%S"))  #RS added to correct error when run in Linux
data$STime <- as.ITime(format(data$Time, format = "%H:%M:%S"))  #RS added to correct error when run in Linux
startTime <- as.ITime("07:30")
endTime <- as.ITime("17:00")
data$day <- ifelse((dataTime >= startTime & dataTime <= endTime), "morning", "evening")

### Label weekday or weekend
daysWeek <- format(as.Date(data$Date, format = "%d/%m/%Y"),"%u") # days of the week 1-7 (Monday-Sunday)
data$week <- ifelse(daysWeek %in% c("6", "7"), "weekend", "weekdays")


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

#--------------------------------------------------------------Question 1-------------------------------------------------------
summary(data) # check NA

### An aggregate function containing: Arithmetic and geometric mean, median, mode and standard deviation for features A, B and C
dataStats <- function(x,t)
{
  cat( t , ' Arithmetic Mean' , mean(x), '\n') #A Arithmetic Mean
  cat( t, ' Geometric Mean' , geometric.mean(x), '\n') #A Geometric Mean
  cat( t, ' Median ' , median(x), '\n') #A Median
  cat( t, ' Mode ', (names(table(x))[table(x)==max(table(x))]), '\n') #A Mode
  cat( t , ' Standard Deviation', sd(x), '\n') #A Standard Deviation
  cat('\n')
}

x = data$Global_active_power
t ='Feature A Global Active Power'
dataStats(x,t)

x = data$Global_reactive_power
t ='Feature B Global Reactive Power'
dataStats(x,t)

x = data$Voltage
t ='Feature C Voltage'
dataStats(x,t)

### For features A and B, compute the min and max values on weekdays and weekend days during day hours and night hours

# Weekdays day hours data set
weekdaysDayhours <- filter(data, week == "weekdays" & day == "morning")

# Weekdays night hours data set
weekdaysNighthours <- filter(data, week == "weekdays" & day == "evening")

# weekend day hours data set
weekendDayhours <- filter(data, week == "weekend" & day == "morning")

# weekend night hours data set
weekendNighthours <- filter(data, week == "weekend" & day == "evening")

# Features A and B min & max Weekdays day hours
min(weekdaysDayhours$Global_active_power)
max(weekdaysDayhours$Global_active_power)

min(weekdaysDayhours$Global_reactive_power)
max(weekdaysDayhours$Global_reactive_power)

# Features A and B min & max Weekdays night hours
min(weekdaysNighthours$Global_active_power)
max(weekdaysNighthours$Global_active_power)

min(weekdaysNighthours$Global_reactive_power)
max(weekdaysNighthours$Global_reactive_power)

# Features A and B min & max weekend day hours
min(weekendDayhours$Global_active_power)
max(weekendDayhours$Global_active_power)

min(weekendDayhours$Global_reactive_power)
max(weekendDayhours$Global_reactive_power)

# Features A and B min & max weekend night hours
min(weekendNighthours$Global_active_power)
max(weekendNighthours$Global_active_power)

min(weekendNighthours$Global_reactive_power)
max(weekendNighthours$Global_reactive_power)

#--------------------------------------------------------------Question 2-------------------------------------------------------
# Subset all relevant attributes
corrData <- data[, c(3:9)]

# Construct a correlation matrix
corrMatrix <- (round(cor(corrData, method = "pearson"),3))
corrMatrix

# Visualize correlation matrix 
# x11()
corrplot(corrMatrix, method="color")

#--------------------------------------------------------------Question 3-------------------------------------------------------

### Subset dataframe: intensity measurements on weekday morning
intensity_weekdays_day <- weekdaysDayhours[, c("timestamp", "Global_intensity", "STime")]

# Extract hour, minute, second from datatime and reset the data type as POSIXct
intensity_weekdays_day$Time <-format(as.POSIXct(intensity_weekdays_day$timestamp), format = "%H:%M:%S")
intensity_weekdays_day$Time <- as.POSIXct(intensity_weekdays_day$Time, format = "%H:%M:%S")

# Group and average the time periods 
avg_days_day <- intensity_weekdays_day %>%
  group_by(Time, STime) %>%
  summarise(avg_intensity = mean(Global_intensity))

# Fit linear model to the data
fit_linear_days_day <- lm(avg_intensity ~ Time, avg_days_day)
fit_linear_days_day
predicted <- predict(fit_linear_days_day, interval="prediction")
avg_days_day <- cbind(avg_days_day, predicted)
colnames(avg_days_day)[4]<-"fitlm"

# Fit polynomial model to the data
fit_poly_days_day <- lm(avg_intensity ~ poly(STime, degree = 3, raw = TRUE), avg_days_day)
fit_poly_days_day
predicted <- predict(fit_poly_days_day, interval="prediction")
head(predicted)
avg_days_day <- cbind(avg_days_day, predicted)
colnames(avg_days_day)[5]<-"fit3"

fit_poly_days_day <- lm(avg_intensity ~ poly(STime, degree = 10, raw = TRUE), avg_days_day)
fit_poly_days_day
predicted <- predict(fit_poly_days_day, interval="prediction")
head(predicted)
avg_days_day <- cbind(avg_days_day, predicted)
colnames(avg_days_day)[6]<-"fit10"


### subset dataframe: intensity measurements on weekday evening
intensity_weekdays_night <- weekdaysNighthours[, c("timestamp", "Global_intensity", "STime")]

# Extract hour, minute, second from datatime and reset the data type as POSIXct
intensity_weekdays_night$Time <-format(as.POSIXct(intensity_weekdays_night$timestamp), format = "%H:%M:%S")
intensity_weekdays_night$Time <- as.POSIXct(intensity_weekdays_night$Time, format = "%H:%M:%S")

# Group and average the time periods 
avg_days_night <- intensity_weekdays_night %>%
  group_by(Time, STime) %>%
  summarise(avg_intensity = mean(Global_intensity))

avg_days_night$ITime = as.integer(avg_days_night$STime)
# Restructure the dataframe to remove the gap in plot
part1_weekday_night <- avg_days_night[451:nrow(avg_days_night),]
part2_weekday_night <- avg_days_night[1:450,]
part2_weekday_night$Time <- part2_weekday_night$Time + (60*60*24)
part2_weekday_night$ITime = part2_weekday_night$ITime + 86341
avg_days_night <- rbind(part1_weekday_night, part2_weekday_night)
#avg_days_night$STime <- as.ITime(format(avg_days_night$Time, format = "%H:%M:%S"))

# plot intensity vs time
# x11()
plot(avg_days_night$Time, avg_days_night$avg_intensity)

# Fit linear model to the data
fit_linear_days_night <- lm(avg_intensity ~ Time, avg_days_night)
fit_linear_days_night

predicted <- predict(fit_linear_days_night, interval="prediction")
avg_days_night <- cbind(avg_days_night, predicted)
colnames(avg_days_night)[5]<-"fitlm"
# Fit polynomial model to the data
fit_poly_days_night <- lm(avg_intensity ~ poly(ITime, degree = 3, raw = TRUE), avg_days_night)
fit_poly_days_night

predicted <- predict(fit_poly_days_night, interval="prediction")
head(predicted)
avg_days_night <- cbind(avg_days_night, predicted)
colnames(avg_days_night)[6]<-"fit3"
fit_poly_days_night <- lm(avg_intensity ~ poly(ITime, degree = 10, raw = TRUE), avg_days_night)
fit_poly_days_night

predicted <- predict(fit_poly_days_night, interval="prediction")
head(predicted)

avg_days_night <- cbind(avg_days_night, predicted)
colnames(avg_days_night)[7]<-"fit10"


### Subset dataframe: intensity measurements on weekday morning
intensity_weekend_day <- weekendDayhours[, c("timestamp", "Global_intensity", "STime")]

# Extract hour, minute, second from datatime and reset the data type as POSIXct
intensity_weekend_day$Time <-format(as.POSIXct(intensity_weekend_day$timestamp), format = "%H:%M:%S")
intensity_weekend_day$Time <- as.POSIXct(intensity_weekend_day$Time, format = "%H:%M:%S")

# Group and average the time periods 
avg_weekend_day <- intensity_weekend_day %>%
  group_by(Time, STime) %>%
  summarise(avg_intensity = mean(Global_intensity))

# Fit linear model to the data
fit_linear_weekend_day <- lm(avg_intensity ~ Time, avg_weekend_day)
fit_linear_weekend_day
predicted <- predict(fit_linear_weekend_day, interval="prediction")
avg_weekend_day <- cbind(avg_weekend_day, predicted)
colnames(avg_weekend_day)[4]<-"fitlm"

# Fit polynomial model to the data
fit_poly_weekend_day <- lm(avg_intensity ~ poly(STime, degree = 3, raw = TRUE), avg_weekend_day)
fit_poly_weekend_day
predicted <- predict(fit_poly_weekend_day, interval="prediction")
head(predicted)
avg_weekend_day <- cbind(avg_weekend_day, predicted)
colnames(avg_weekend_day)[5]<-"fit3"

fit_poly_weekend_day <- lm(avg_intensity ~ poly(STime, degree = 10, raw = TRUE), avg_weekend_day)
fit_poly_weekend_day
predicted <- predict(fit_poly_weekend_day, interval="prediction")
head(predicted)
avg_weekend_day <- cbind(avg_weekend_day, predicted)
colnames(avg_weekend_day)[6]<-"fit10"


### subset dataframe: intensity measurements on weekday evening
intensity_weekend_night <- weekendNighthours[, c("timestamp", "Global_intensity", "STime")]

# Extract hour, minute, second from datatime and reset the data type as POSIXct
intensity_weekend_night$Time <-format(as.POSIXct(intensity_weekend_night$timestamp), format = "%H:%M:%S")
intensity_weekend_night$Time <- as.POSIXct(intensity_weekend_night$Time, format = "%H:%M:%S")

# Group and average the time periods 
avg_weekend_night <- intensity_weekend_night %>%
  group_by(Time, STime) %>%
  summarise(avg_intensity = mean(Global_intensity))

avg_weekend_night$ITime = as.integer(avg_weekend_night$STime)
# Restructure the dataframe to remove the gap in plot
part1_weekday_night <- avg_weekend_night[451:nrow(avg_weekend_night),]
part2_weekday_night <- avg_weekend_night[1:450,]
part2_weekday_night$Time <- part2_weekday_night$Time + (60*60*24)
part2_weekday_night$ITime = part2_weekday_night$ITime + 86341
avg_weekend_night <- rbind(part1_weekday_night, part2_weekday_night)
#avg_weekend_night$STime <- as.ITime(format(avg_weekend_night$Time, format = "%H:%M:%S"))

# plot intensity vs time
# x11()
plot(avg_weekend_night$Time, avg_weekend_night$avg_intensity)

# Fit linear model to the data
fit_linear_weekend_night <- lm(avg_intensity ~ Time, avg_weekend_night)
fit_linear_weekend_night

predicted <- predict(fit_linear_weekend_night, interval="prediction")
avg_weekend_night <- cbind(avg_weekend_night, predicted)
colnames(avg_weekend_night)[5]<-"fitlm"
# Fit polynomial model to the data
fit_poly_weekend_night <- lm(avg_intensity ~ poly(ITime, degree = 3, raw = TRUE), avg_weekend_night)
fit_poly_weekend_night

predicted <- predict(fit_poly_weekend_night, interval="prediction")
head(predicted)
avg_weekend_night <- cbind(avg_weekend_night, predicted)
colnames(avg_weekend_night)[6]<-"fit3"
fit_poly_weekend_night <- lm(avg_intensity ~ poly(ITime, degree = 10, raw = TRUE), avg_weekend_night)
fit_poly_weekend_night

predicted <- predict(fit_poly_weekend_night, interval="prediction")
head(predicted)

avg_weekend_night <- cbind(avg_weekend_night, predicted)
colnames(avg_weekend_night)[7]<-"fit10"


# Linear Plots
colors <- c("linear" = "black")
p1 <- ggplot(avg_days_day, mapping = aes(x=Time, y=avg_intensity)) + 
  geom_point(color="light blue") +
  geom_line(data=avg_days_day, mapping=aes(x=Time, y=avg_days_day$fitlm[,c(1)], color="linear"), size=1) +
  ggtitle("Weekday Day Hours Global Intensity") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(y="Global Intensity (in Ampere)", x = "Time (in Hours)", color ="Fit Type") + 
  theme(plot.title = element_text(hjust=0.5), legend.position = "bottom") 

colors <- c("linear" = "black")
p2 <- ggplot(avg_days_night, mapping = aes(x=Time, y=avg_intensity)) + 
  geom_point(color="light blue") +
  geom_line(data=avg_days_night, mapping=aes(x=Time, y=avg_days_night$fitlm[,c(1)], color="linear"), size=1) +
  ggtitle("Weekday Night Hours Global Intensity") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(y="Global Intensity (in Ampere)", x = "Time (in Hours)", color ="Fit Type") + 
  theme(plot.title = element_text(hjust=0.5), legend.position = "bottom") 

colors <- c("linear" = "black")
p3 <- ggplot(avg_weekend_day, mapping = aes(x=Time, y=avg_intensity)) + 
  geom_point(color="light blue") +
  geom_line(data=avg_weekend_day, mapping=aes(x=Time, y=avg_weekend_day$fitlm[,c(1)], color="linear"), size=1) +
  ggtitle("Weekend Day Hours Global Intensity") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(y="Global Intensity (in Ampere)", x = "Time (in Hours)", color ="Fit Type") + 
  theme(plot.title = element_text(hjust=0.5), legend.position = "bottom") 

colors <- c("linear" = "black")
p4 <- ggplot(avg_weekend_night, mapping = aes(x=Time, y=avg_intensity)) + 
  geom_point(color="light blue") +
  geom_line(data=avg_weekend_night, mapping=aes(x=Time, y=avg_weekend_night$fitlm[,c(1)], color="linear"), size=1) +
  ggtitle("Weekend Night Hours Global Intensity") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(y="Global Intensity (in Ampere)", x = "Time (in Hours)", color ="Fit Type") + 
  theme(plot.title = element_text(hjust=0.5), legend.position = "bottom") 


# Polynomial Plots
colors <- c("3 df"= "red", "10 df" = "blue")
p5 <- ggplot(avg_days_day, mapping = aes(x=Time, y=avg_intensity)) + 
  geom_point(color="light blue") +
  geom_line(data=avg_days_day, mapping=aes(x=Time, y=avg_days_day$fit3[,c(1)], color="3 df"), size=1) +
  geom_line(data=avg_days_day, mapping=aes(x=Time, y=avg_days_day$fit10[,c(1)], color="10 df"), size=1) +
  ggtitle("Weekday Day Hours Global Intensity") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(y="Global Intensity (in Ampere)", x = "Time (in Hours)", color ="Fit Type") + 
  theme(plot.title = element_text(hjust=0.5), legend.position = "bottom") 

colors <- c("3 df"= "red", "10 df" = "blue")
p6 <- ggplot(avg_days_night, mapping = aes(x=Time, y=avg_intensity)) + 
  geom_point(color="light blue") +
  geom_line(data=avg_days_night, mapping=aes(x=Time, y=avg_days_night$fit3[,c(1)], color="3 df"), size=1) +
  geom_line(data=avg_days_night, mapping=aes(x=Time, y=avg_days_night$fit10[,c(1)], color="10 df"), size=1) +
  labs(title = "Weekday Night Global Intensity", x = "Time (in Hours)", y ="Global Intensity (in Ampere)", color ="Fit Type") +
  theme(plot.title = element_text(hjust=0.5), legend.position = "bottom") 

colors <- c("3 df"= "red", "10 df" = "blue")
p7 <- ggplot(avg_weekend_day, mapping = aes(x=Time, y=avg_intensity)) + 
  geom_point(color="light blue") +
  geom_line(data=avg_weekend_day, mapping=aes(x=Time, y=avg_weekend_day$fit3[,c(1)], color="3 df"), size=1) +
  geom_line(data=avg_weekend_day, mapping=aes(x=Time, y=avg_weekend_day$fit10[,c(1)], color="10 df"), size=1) +
  ggtitle("Weekend Day Hours Global Intensity") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(y="Global Intensity (in Ampere)", x = "Time (in Hours)", color ="Fit Type") + 
  theme(plot.title = element_text(hjust=0.5), legend.position = "bottom") 

colors <- c("3 df"= "red", "10 df" = "blue")
p8 <- ggplot(avg_weekend_night, mapping = aes(x=Time, y=avg_intensity)) + 
  geom_point(color="light blue") +
  geom_line(data=avg_weekend_night, mapping=aes(x=Time, y=avg_weekend_night$fit3[,c(1)], color="3 df"), size=1) +
  geom_line(data=avg_weekend_night, mapping=aes(x=Time, y=avg_weekend_night$fit10[,c(1)], color="10 df"), size=1) +
  labs(title = "Weekend Night Global Intensity", x = "Time (in Hours)", y ="Global Intensity (in Ampere)", color ="Fit Type") +
  theme(plot.title = element_text(hjust=0.5), legend.position = "bottom") 

multiplot(p1, p2, p3, p4, cols=2)
multiplot(p5, p6, p7, p8, cols=2)
