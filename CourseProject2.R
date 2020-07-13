library(ggplot2)
library(dplyr)

#loading data
file <- download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2","zippedfile.bz2")
stormdata <- read.csv("zippedfile.bz2")

#processsing data - pop
data_health <- stormdata[,c(8,23,24)]
#data_health$loss <- data_health$FATALITIES+data_health$INJURIES
data_health <- mutate(data_health,loss=FATALITIES+INJURIES)


#population loss
data_health <- aggregate(loss ~ EVTYPE, data_health,sum,na.rm=TRUE)
data_health <- arrange(data_health,-loss)

#plot population loss
h <- ggplot(data_health[1:6,],aes(x=loss,y=EVTYPE))
h <- h + geom_bar(stat="identity", fill="#FFB6C1", alpha=0.6, width=0.5)
h <- h + labs(x="Population Loss",y="Event",title="Events causing highest population loss")
h


#preprocessing data - eco
dataprop <- stormdata[,c(8,25,26)]
datacrop <- stormdata[,c(8,27,28)]

m <- c(0,0,0,1,10,10,10,10,10,10,10,10,10,10^9,10^2,10^2,10^3,10^6,10^6)
symbol <- sort(unique(stormdata$PROPDMGEXP))
multiply <- data.frame(cbind(symbol,m)) #making df of symbol and factor
multiply$m <- as.numeric(multiply$m)

#property
colnames(dataprop)[3] <- "symbol"
dataprop <- merge(dataprop,multiply,by="symbol") #match exp to symbol
dataprop <- mutate(dataprop,PROPDMG=PROPDMG*m) #multiply damage by symbol
dataprop <- dataprop[,2:3] #subsetting to get only required columns
dataprop <- aggregate(PROPDMG~EVTYPE,data=dataprop,sum) #getting total loss depending on event


#crop
colnames(datacrop)[3] <- "symbol"
datacrop <- merge(datacrop,multiply,by="symbol") #match exp to symbol
datacrop <- mutate(datacrop,CROPDMG=CROPDMG*m) #multiply damage by symbol
datacrop <- datacrop[,2:3] #subsetting to get only required columns
datacrop <- aggregate(CROPDMG~EVTYPE,data=datacrop,sum) #getting total loss depending on event

#add both
dataeco <- merge(dataprop,datacrop,by="EVTYPE") #combine both dataframes
dataeco <- mutate(dataeco,loss=CROPDMG+PROPDMG) #add property and crop damages
dataeco <- arrange(dataeco,-loss)

#plot economic loss
h <- ggplot(dataeco[1:6,],aes(x=loss,y=EVTYPE))
h <- h + geom_bar(stat="identity", fill="#FFB6C1", alpha=0.6, width=0.5)
h <- h + labs(x="Economical Loss",y="Event",title="Events causing highest economical loss")
h