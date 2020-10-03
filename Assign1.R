# Install packages needed for output
#install.packages('psych')
#install.packages("lubridate")

###############################################

# Import Libraries
library("lubridate")  # For Q3
library('psych')      # For Q1

###############################################

# Set Directory
getwd()
# setwd("C:/Users/offic/Documents/SFU/FALL2020/CMPT318/Assignments/A1")   # Matt's Directory
setwd(" -- set your directory here -- ")

###############################################

# Create dataStats function 
dataStats <- function(x, t)
{
  cat( t, ' Arithmetic Mean' , mean(x), '\n') #A Arithmetic Mean
  cat( t, ' Geometric Mean' , geometric.mean(x), '\n') #A Geometric Mean
  cat( t, ' Median ' , median(x), '\n') #A Median
  cat( t, ' Mode ', (which.max(tabulate(x * 100)) / 100), '\n')
  cat( t , ' Standard Deviation', sd(x), '\n') #A Standard Deviation
  cat('\n')
}

# Load the data
data = read.table("Group_Assignment_1_Dataset.txt", header = TRUE, sep =',')
data$SDate = as.POSIXlt(data$Date, tz="", "%d/%m/%Y")
data_week_29 <- subset(data, SDate >='2007/07/16' & SDate < '2007/07/23')

# Remove the large data frame
rm(data) 

# Create Time of Day column 1 - 1440 minutes
data_week_29$ToD <- strptime(data_week_29$Time, format = "%H:%M:%S")

# Create Day of Week column 1 = Monday - 7 = Sunday
data_week_29$DoW = format(data_week_29$SDate, "%u") 
head(data_week_29)

###############################################

x = data_week_29$Global_active_power
t ='Feature A Global Active Power'
dataStats(x, t)
x = data_week_29$Global_reactive_power
t ='Feature B Global Reactive Power'
dataStats(x, t)
x = data_week_29$Voltage
t ='Feature C Voltage'
dataStats(x, t)

#
# Question 1
#
a = data_week_29$Global_active_power
b = data_week_29$Global_reactive_power
c = data_week_29$Voltage
d = data_week_29$Global_intensity
e = data_week_29$Sub_metering_1
f = data_week_29$Sub_metering_2
g = data_week_29$Sub_metering_3
values = list(a, b, c, d, e, f, g)

###############################################

#
# Question 2
#
m = length(values)
CorMX = matrix(0.0, m, m, byrow = TRUE)
i = 1
j = 1

while (i <= m)
{
  while (j <= m)
  {
    CorMX[i,j] = cor(values[[i]], values[[j]], method = "pearson" )
    j <- j + 1
  }
  j = 1
  i <- i +1
}

rownames(CorMX) <- c('A', 'B', 'C', 'D', 'E', 'F', 'G')
colnames(CorMX) <- c('A', 'B', 'C', 'D', 'E', 'F', 'G')
round(CorMX, 3)
heatmap(CorMX)

###############################################

#
# Question 3
#

# Function for computing average of the values for each minute in a time window
# Runtime O(n)
calculate_average_minute <- function(vector_in, num_days, num_minutes)
{
  # Create temporary vector
  vector_average <- vector()
  j = 1;

  # Append all GLOBAL_INTENSITY values for Day 1
  for (i in 1:num_minutes)
  {
    vector_average[i] <- vector_in[i]
  }
  
  i = i + 1

  # Sum all GLOBAL_INTENSITY values for rest of the days
  while (j < num_days)
  {
    for (x in 1:num_minutes)
    {
      vector_average[x] = vector_average[x] + vector_in[i]
      i = i + 1
    }
    j = j + 1
  }

  # Divide each element by number of days to get average
  for (y in 1:num_minutes)
  {
    vector_average[y] = vector_average[y] / num_days 
  }
  return(vector_average)
}

# Define representative time windows
#     Day hours (7:30 AM - 4:59 PM)
#   Night hours (5:00 PM - 7:29 AM)

# Create condition for querying time by hours and minutes
query_by_time = as.numeric(hour(data_week_29$ToD)) * 60 + as.numeric(minute(data_week_29$ToD))

# Create subset dataset for weekday day hours
data_weekday_day <- subset(data_week_29, DoW < 6 & query_by_time >= 450 & query_by_time < 1020)

# Create subset dataset for weekday night hours
data_weekday_night <- subset(data_week_29, DoW < 6 & (query_by_time >= 1020 | query_by_time < 450))

# Create subset dataset for weekend day hours
data_weekend_day <- subset(data_week_29, DoW >= 6 & query_by_time >= 450 & query_by_time < 1020)

# Create subset dataset for weekend night hours
data_weekend_night <- subset(data_week_29, DoW >= 6 & (query_by_time >= 1020 | query_by_time < 450))

# Create vector to store data_weekday_day
vector_weekday_day <- data_weekday_day$Global_intensity

# Create vector to store data_weekday_night
vector_weekday_night <- data_weekday_night$Global_intensity

# Create vector to store data_weekdend_day
vector_weekend_day <- data_weekend_day$Global_intensity

# Create vector to store data_weekend_night
vector_weekend_night <- data_weekend_night$Global_intensity

# Calculate average of values per minute for weekday_day
calc_weekday_day <- vector()
calc_weekday_day = calculate_average_minute(vector_weekday_day, 5, 570)

# Calculate average of values per minute for weekday_day
calc_weekday_night <- vector()
calc_weekday_night = calculate_average_minute(vector_weekday_night, 5, 870)

# Calculate average of values per minute for weekday_day
calc_weekend_day <- vector()
calc_weekend_day = calculate_average_minute(vector_weekend_day, 2, 570)

# Calculate average of values per minute for weekday_day
calc_weekend_night <- vector()
calc_weekend_night = calculate_average_minute(vector_weekend_night, 2, 870)

# Create time series for weekday_day
ts_weekday_day <- ts(calc_weekday_day, frequency = 570, start = 0)

# Create time series for weekday_night
ts_weekday_night <- ts(calc_weekday_night, frequency = 870, start = 0)

# Create time series for weekday_day
ts_weekend_day <- ts(calc_weekend_day, frequency = 570, start = 0)

# Create time series for weekday_day
ts_weekend_night <- ts(calc_weekend_night, frequency = 870, start = 0)

# ---- Testing Only ----
#plot(ts_weekday_day) 
#plot(ts_weekday_night) 
#plot(ts_weekend_day) 
#plot(ts_weekend_night) 

# Linear Regression
# ---- TO BE DONE ---- 

###############################################