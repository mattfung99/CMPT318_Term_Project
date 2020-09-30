data=read.table("Group_Assignment_1_Dataset.txt",header = TRUE,sep=',')
data$SDate=as.POSIXlt(data$Date,tz="","%d/%m/%Y")
datasmall<-subset(data, SDate >='2007/07/16' & SDate < '2007/07/23')
head(datasmall)
