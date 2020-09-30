#install.packages('psych')
library('psych')
#Create dataStats function 
dataStats <- function(x,t)
{cat( t , ' Arithmetic Mean' , mean(x), '\n') #A Arithmetic Mean
  cat( t, ' Geometric Mean' , geometric.mean(x), '\n') #A Geometric Mean
  cat( t, ' Median ' , median(x), '\n') #A Median
  cat( t, ' Mode ', (which.max(tabulate(x * 100))/100), '\n')
  cat( t , ' Standard Deviation', sd(x), '\n') #A Standard Deviation
  cat('\n')
  }
data=read.table("Group_Assignment_1_Dataset.txt",header = TRUE,sep=',')
data$SDate=as.POSIXlt(data$Date,tz="","%d/%m/%Y")
datasmall<-subset(data, SDate >='2007/07/16' & SDate < '2007/07/23')
# head(datasmall)
x = datasmall$Global_active_power
t ='Feature A Global Active Power'
dataStats(x,t)
x = datasmall$Global_reactive_power
t ='Feature B Global Reactive Power'
dataStats(x,t)
x = datasmall$Voltage
t ='Feature C Voltage'
dataStats(x,t)
