#install.packages('psych')
library('psych')
#Create dataStats function 
dataStats <- function(x,t)
  {
    cat( t , ' Arithmetic Mean' , mean(x), '\n') #A Arithmetic Mean
    cat( t, ' Geometric Mean' , geometric.mean(x), '\n') #A Geometric Mean
    cat( t, ' Median ' , median(x), '\n') #A Median
    cat( t, ' Mode ', (which.max(tabulate(x * 100))/100), '\n')
    cat( t , ' Standard Deviation', sd(x), '\n') #A Standard Deviation
    cat('\n')
  }
data=read.table("Group_Assignment_1_Dataset.txt",header = TRUE,sep=',')
data$SDate=as.POSIXlt(data$Date,tz="","%d/%m/%Y")
datasmall<-subset(data, SDate >='2007/07/16' & SDate < '2007/07/23')
rm(data) #remove the large data frame
datasmall$ToD=as.numeric(datasmall$Time) #Create Time of Day column 1 - 1440 minutes
datasmall$DoW=format(datasmall$SDate,"%u") #Create Day of Week column 1 = Monday - 7 = Sunday
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
#
# 1b TBC
#
a = datasmall$Global_active_power
b = datasmall$Global_reactive_power
c = datasmall$Voltage
d = datasmall$Global_intensity
e = datasmall$Sub_metering_1
f = datasmall$Sub_metering_2
g = datasmall$Sub_metering_3
values = list(a,b,c,d,e,f,g)
m = length(values)
CorMX = matrix(0.0,m,m,byrow=TRUE)
i=1
j=1
while (i <= m)
{
  while (j <= m)
  {
    CorMX[i,j]=cor(values[[i]],values[[j]], method ="pearson" )
    j <- j + 1
  }
  j = 1
  i <- i +1
}
rownames(CorMX) <-c('A','B','C','D','E','F','G')
colnames(CorMX) <-c('A','B','C','D','E','F','G')
round(CorMX,3)
heatmap(CorMX)
#
# Question 3
#
dayW<-subset(datasmall, DoW < 6 & ToD >= 450 & ToD < 1021)  # extract the data for daytime and weekdays
