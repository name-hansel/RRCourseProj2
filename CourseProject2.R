library(ggplot2)
library(dplyr)

#loading data
file <- download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2","zippedfile.bz2")
stormdata <- read.csv("zippedfile.bz2")

#processsing data
data_health <- stormdata[,c(8,23,24,39)]
data_h_f <- data_health[which(data_health$FATALITIES>0),c(1,2,4)]
data_h_i <- data_health[,c(1,3,4)]

#fatalities
data_h_f <- aggregate(FATALITIES ~ EVTYPE, data_h_f,sum,na.rm=TRUE)
data_h_f <- arrange(data_h_f,-FATALITIES)
h <- ggplot(data_h_f[1:6,],aes(x=FATALITIES,y=EVTYPE)) +
        geom_bar(stat="identity", fill="#aec56f", alpha=0.6, width=0.5) +
        coord_flip()
print(h)

