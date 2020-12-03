library(dplyr)
library(data.table)
library(depmixS4)


############################################################### Helper Functions #################################################################

ntimesVector <- function(df){
  
  agg <- aggregate(df$Date, by = list(df$Date), FUN = length)
  agg <- agg[order(as.Date(agg$Group.1, format = "%d/%m/%Y")),]
  
  return(agg$x)
}

############################################################### Normal Data #################################################################

### Read cleaned data
powerData <- read.table("TermProjectDataCleaned.txt", sep = ',', header = TRUE)

### Create a time stamp column
dateTimeCombine <- paste(powerData$Date, powerData$Time)
powerData$timestamp <- as.POSIXlt(strptime(dateTimeCombine, format = "%d/%m/%Y %H:%M:%S"))

### Label and filter days of the week
formatingDates <- as.Date(powerData$Date, format = "%d/%m/%Y")
powerData$dayNumber <- as.integer(format(formatingDates,"%u"))
tuesday <- filter(powerData, dayNumber == '2') # Tuesday

### Filter the time period required
STime <- as.ITime(format(tuesday$Time, format = "%H:%M:%S"))  #RS added to correct error when run in Linux
startingTime <- as.ITime("16:00")
endingTime <- as.ITime("19:59")
tuesdayEvening <- cbind(tuesday, STime)
colnames(tuesdayEvening)[12]<-"STime"
data.Raw <- filter(tuesdayEvening, STime >= startingTime & STime <= endingTime)

### Separate train data and test data
train.data.raw <- subset(data.Raw, as.Date(data.Raw$timestamp) < as.Date("2009-1-1 00:00:00 PST"))
test.data.raw <- subset(data.Raw, as.Date(data.Raw$timestamp) >= as.Date("2009-1-1 00:00:00 PST"))

train.data.multi <- train.data.raw[,-c(2:5,7,10:12)]
test.data.multi <- test.data.raw[,-c(2:5,7,10:12)]

############################################################### Anomaly Data 1 ###############################################################

# Read anomaly file 1
anomaly1 <- read.table("Data1(WithAnomalies).txt", sep = ',', header = TRUE)

### Create a time stamp column
anomaly1DateTime <- paste(anomaly1$Date, anomaly1$Time)
anomaly1$timestamp <- as.POSIXlt(strptime(anomaly1DateTime, format = "%d/%m/%Y %H:%M:%S"))

### Label days of the week
anomaly1FormatingDates <- as.Date(anomaly1$Date, format = "%d/%m/%Y")
anomaly1$dayNumber <- as.integer(format(anomaly1FormatingDates,"%u"))
anomaly1tuesday <- filter(anomaly1, dayNumber == '2') # Tuesday

### Filter the time period required
anomaly1STime <- as.ITime(format(anomaly1tuesday$Time, format = "%H:%M:%S"))  #RS added to correct error when run in Linux
anomaly1startingTime <- as.ITime("16:00")
anomaly1endingTime <- as.ITime("19:59")
anomaly1.tuesday.evening <- cbind(anomaly1tuesday, anomaly1STime)
colnames(anomaly1.tuesday.evening)[12]<-"anomaly1STime"
anomaly1.data.Raw <- filter(anomaly1.tuesday.evening, anomaly1STime >= anomaly1startingTime & anomaly1STime <= anomaly1endingTime)
anomaly1.data.multi <- anomaly1.data.Raw[,-c(2:5,7,10:12)]

############################################################### Anomaly Data 2 ###############################################################

# Read anomaly file 2
anomaly2 <- read.table("Data2(WithAnomalies).txt", sep = ',', header = TRUE)

### Create a time stamp column
anomaly2DateTime <- paste(anomaly2$Date, anomaly2$Time)
anomaly2$timestamp <- as.POSIXlt(strptime(anomaly2DateTime, format = "%d/%m/%Y %H:%M:%S"))

### Label days of the week
anomaly2FormatingDates <- as.Date(anomaly2$Date, format = "%d/%m/%Y")
anomaly2$dayNumber <- as.integer(format(anomaly2FormatingDates,"%u"))
anomaly2tuesday <- filter(anomaly2, dayNumber == '2') # Tuesday

### Filter the time period required
anomaly2STime <- as.ITime(format(anomaly2tuesday$Time, format = "%H:%M:%S"))  #RS added to correct error when run in Linux
anomaly2startingTime <- as.ITime("16:00")
anomaly2endingTime <- as.ITime("19:59")
anomaly2.tuesday.evening <- cbind(anomaly2tuesday, anomaly2STime)
colnames(anomaly2.tuesday.evening)[12]<-"anomaly2STime"
anomaly2.data.Raw <- filter(anomaly2.tuesday.evening, anomaly2STime >= anomaly2startingTime & anomaly2STime <= anomaly2endingTime)
anomaly2.data.multi <- anomaly2.data.Raw[,-c(2:5,7,10:12)]

############################################################### Anomaly Data 3 ###############################################################

# Read anomaly file 3
anomaly3 <- read.table("Data3(WithAnomalies).txt", sep = ',', header = TRUE)

### Create a time stamp column
anomaly3DateTime <- paste(anomaly3$Date, anomaly3$Time)
anomaly3$timestamp <- as.POSIXlt(strptime(anomaly3DateTime, format = "%d/%m/%Y %H:%M:%S"))

### Label days of the week
anomaly3FormatingDates <- as.Date(anomaly3$Date, format = "%d/%m/%Y")
anomaly3$dayNumber <- as.integer(format(anomaly3FormatingDates,"%u"))
anomaly3tuesday <- filter(anomaly3, dayNumber == '2') # Tuesday

### Filter the time period required
anomaly3STime <- as.ITime(format(anomaly3tuesday$Time, format = "%H:%M:%S"))  #RS added to correct error when run in Linux
anomaly3startingTime <- as.ITime("16:00")
anomaly3endingTime <- as.ITime("19:59")
anomaly3.tuesday.evening <- cbind(anomaly3tuesday, anomaly3STime)
colnames(anomaly3.tuesday.evening)[12]<-"anomaly3STime"
anomaly3.data.Raw <- filter(anomaly3.tuesday.evening, anomaly3STime >= anomaly3startingTime & anomaly3STime <= anomaly3endingTime)
anomaly3.data.multi <- anomaly3.data.Raw[,-c(2:5,7,10:12)]

########################################################### States Selection ####################################################################

set.seed(298224)
#set.seed(2)

minState <- 2
maxState <- 20
numStates <- minState:maxState

# Create containers for states, BICs and logLik
multi.hmm.vector = c("States", "BICs", "logLik")
multi.hmm.models = array(0, dim = c(maxState, length(multi.hmm.vector)))
colnames(multi.hmm.models) = multi.hmm.vector

for (i in numStates)
{
  print(paste0(i))
  
  train.tuesday.model = depmix(list(train.data.multi$Global_intensity ~ 1, train.data.multi$Sub_metering_2  ~ 1, train.data.multi$Sub_metering_3  ~ 1),
                 nstates = i, family = list(gaussian(), poisson(), poisson()), data = train.data.multi, ntimes = ntimesVector(train.data.multi))
  fit.tuesday.model <- fit(train.tuesday.model)
  
  # Extract states, BICs, and Log likelihood
  multi.hmm.models[i, "States"] <- i
  multi.hmm.models[i, "BICs"] <- BIC(fit.tuesday.model)
  multi.hmm.models[i, "logLik"] <- logLik(fit.tuesday.model)
}
multi.hmm.models
multi.hmm.summary <- as.data.frame(multi.hmm.models)

# Weekdays Training Set plot
par(mar = c(5, 5, 3, 5))
plot(multi.hmm.summary$States, multi.hmm.summary$BICs, type ="l", ylab = "BICs",
     main = "BIC and Log-Likelihood for each state - Tuesday 4:00 pm - 7:59 pm Training Set", xlab = "States", col = "blue")

par(new = TRUE)
plot(multi.hmm.summary$States, multi.hmm.summary$logLik, type = "l", xaxt = "n", yaxt = "n", ylab = "", xlab = "", col = "red", lty = 2)
axis(side = 4)
axis(side = 1, at = seq(0, 20, by = 1))

mtext("Log Likelihood", side = 4, line = 3)
legend("top", c("BICs", "Log-Likelihoods"), col = c("blue", "red"), lty = c(1, 2))

############################################################# Model Validation ########################################################

set.seed(298224)

train.tuesday.model = depmix(list(train.data.multi$Global_intensity ~ 1, train.data.multi$Sub_metering_2  ~ 1, train.data.multi$Sub_metering_3  ~ 1),
                             nstates = 8, family = list(gaussian(), poisson(), poisson()), data = train.data.multi, ntimes = ntimesVector(train.data.multi))
fit.tuesday.model <- fit(train.tuesday.model)
tuesday.train.object <- forwardbackward(fit.tuesday.model)


tuesday.test.model = depmix(list(test.data.multi$Global_intensity ~ 1, test.data.multi$Sub_metering_2  ~ 1, test.data.multi$Sub_metering_3  ~ 1),
                             nstates = 8, family = list(gaussian(), poisson(), poisson()), data = test.data.multi, ntimes = ntimesVector(test.data.multi))
tuesday.test <-setpars(tuesday.test.model, getpars(fit.tuesday.model))
tuesday.test.object <- forwardbackward(tuesday.test)


(tuesday.train.object$logLike) # -101210.4
(tuesday.test.object$logLike * nrow(train.data.multi)/nrow(test.data.multi)) # -96566.9
# difference: 4643.5

########################################################## Anomaly Detection 1 ##################################################################################################

set.seed(298224)

train.tuesday.model = depmix(list(train.data.multi$Global_intensity ~ 1, train.data.multi$Sub_metering_2  ~ 1, train.data.multi$Sub_metering_3  ~ 1),
                             nstates = 8, family = list(gaussian(), poisson(), poisson()), data = train.data.multi, ntimes = ntimesVector(train.data.multi))
fit.tuesday.model <- fit(train.tuesday.model)
tuesday.train.object <- forwardbackward(fit.tuesday.model)


tuesday.anomaly1.model = depmix(list(anomaly1.data.multi$Global_intensity ~ 1, anomaly1.data.multi$Sub_metering_2  ~ 1, anomaly1.data.multi$Sub_metering_3  ~ 1),
                                  nstates = 8, family = list(gaussian(), poisson(), poisson()), data = anomaly1.data.multi, ntimes = ntimesVector(anomaly1.data.multi))
tuesday.anomaly1 <-setpars(tuesday.anomaly1.model,getpars(fit.tuesday.model))
tuesday.anomaly1.object <- forwardbackward(tuesday.anomaly1)


(tuesday.train.object$logLike) # -101,210.4
(tuesday.anomaly1.object$logLike * nrow(train.data.multi)/nrow(anomaly1.data.multi)) # -111,260.7
# difference: 10,050.3

########################################################## Anomaly Detection 2 ##################################################################################################

set.seed(298224)

train.tuesday.model = depmix(list(train.data.multi$Global_intensity ~ 1, train.data.multi$Sub_metering_2  ~ 1, train.data.multi$Sub_metering_3  ~ 1),
                             nstates = 8, family = list(gaussian(), poisson(), poisson()), data = train.data.multi, ntimes = ntimesVector(train.data.multi))
fit.tuesday.model <- fit(train.tuesday.model)
tuesday.train.object <- forwardbackward(fit.tuesday.model)


tuesday.anomaly2.model = depmix(list(anomaly2.data.multi$Global_intensity ~ 1, anomaly2.data.multi$Sub_metering_2  ~ 1, anomaly2.data.multi$Sub_metering_3  ~ 1),
                                nstates = 8, family = list(gaussian(), poisson(), poisson()), data = anomaly2.data.multi, ntimes = ntimesVector(anomaly2.data.multi))
tuesday.anomaly2 <-setpars(tuesday.anomaly2.model,getpars(fit.tuesday.model))
tuesday.anomaly2.object <- forwardbackward(tuesday.anomaly2)


(tuesday.train.object$logLike) # -101,210.4
(tuesday.anomaly2.object$logLike * nrow(train.data.multi)/nrow(anomaly2.data.multi)) # -112,778.8
# difference: 11,568.4

########################################################## Anomaly Detection 3 ##################################################################################################

set.seed(298224)

train.tuesday.model = depmix(list(train.data.multi$Global_intensity ~ 1, train.data.multi$Sub_metering_2  ~ 1, train.data.multi$Sub_metering_3  ~ 1),
                             nstates = 8, family = list(gaussian(), poisson(), poisson()), data = train.data.multi, ntimes = ntimesVector(train.data.multi))
fit.tuesday.model <- fit(train.tuesday.model)
tuesday.train.object <- forwardbackward(fit.tuesday.model)


tuesday.anomaly3.model = depmix(list(anomaly3.data.multi$Global_intensity ~ 1, anomaly3.data.multi$Sub_metering_2  ~ 1, anomaly3.data.multi$Sub_metering_3  ~ 1),
                                nstates = 8, family = list(gaussian(), poisson(), poisson()), data = anomaly3.data.multi, ntimes = ntimesVector(anomaly3.data.multi))
tuesday.anomaly3 <-setpars(tuesday.anomaly3.model,getpars(fit.tuesday.model))
tuesday.anomaly3.object <- forwardbackward(tuesday.anomaly3)


(tuesday.train.object$logLike) # -101,210.4
(tuesday.anomaly3.object$logLike * nrow(train.data.multi)/nrow(anomaly3.data.multi)) # -114501.5
# difference: 13,291.1
