


## Install required packages for graphs, markdown, knitr etc.

installPackages <- function(pkg){ 
   new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])] 
   if (length(new.pkg)) 
     install.packages(new.pkg, dependencies = TRUE) 
   sapply(pkg, require, character.only = TRUE) 
 } 

packages <- c("knitr", "htmltools", "caTools", 'markdown', "plyr", "lattice", "data.table", "httr", "ggplot2" )
installPackages(packages)



### Loading and preprocessing the data

setwd("C:/Coursera/ReprodResearch/week1/assignment")
fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"

#create a data directory for activity file
dataDir <- "data"
if(!file.exists(dataDir)){
  dir.create(dataDir)
} 

#create a figure directory for graphs
figureDir <- "figure" 
if(!file.exists(figureDir)){
  dir.create(figureDir)
}

zip <- paste(getwd(), "/activity.zip", sep = "")
if(!file.exists(zip)){
    download.file(fileURL, zip, mode="wb")
}

dataFile <- paste(getwd(), "/data/activity.csv", sep = "")
if(!file.exists(dataFile)){
    unzip(zip, list = FALSE, overwrite = FALSE, exdir = dataDir)
}

activity <- read.table(file = dataFile, header = TRUE, sep = ",")



## What is mean total number of steps taken per day?
    
activity$daily <- as.POSIXct(with(activity, paste(date, paste(interval %/% 100, interval %% 100, sep=":"))), format="%Y-%m-%d %H:%M",tz="")

stepsAggregate <- setNames(aggregate(steps~as.Date(date), activity, sum, na.rm = TRUE), c("date","steps"))

xaxis <- seq(1, nrow(stepsAggregate), by = 6)

scaleProp <- list(x = list(rot = 45, cex = 1.0, labels = format(stepsAggregate$date, "%d-%b-%Y")[xaxis], at = xaxis))

barchart(date ~ steps, data = stepsAggregate, main = "steps per day", ylab = "steps", xlab = "date", scales = scaleProp, horizontal = F, col="green")

paste("mean:", mean(stepsAggregate$steps))
paste("median:", median(stepsAggregate$steps))



## What is the average daily activity pattern?
   
   
timeSeriesData <- aggregate(steps ~ interval, data = activity, FUN = mean)

plot(timeSeriesData, type = "l", axes = F, xlab = "Time of the day", 
    ylab = "Average across all days provided a time", main = "Average number of steps taken", 
    col = "blue")

axis(1,at=c(seq(0,2400,100),835), label = paste(c(seq(0,24,1),8),c(rep(":00",25),":40"),sep=""), pos = 0)
axis(2, at=c(seq(0,210,30),206.2), label = c(seq(0,210,30),206.2), pos = 0)

maxSteps <- which.max(timeSeriesData$steps)
segments(832, 0, 832, 206.2, col = "red", lty = "dashed")
text(835,200, "max average of steps: (832,206.2)", col = "red", adj = c(-.1, -.1))
segments(0, 206.2, 832, 206.2, col = "red", lty = "dashed")
timeSeriesData [maxSteps, ]

#which.max(timeSeriesData$steps)/12
paste(835, "is equivalent to 8.667 hours, this gives the maximum is reached at 8:40 am")



## Imputing missing values

paste("missing observations:", sum(is.na(activity$steps)))

"missing observations can be replaced with the sample mean instead of replacing them by zero (the 1st won't bias the estimates while the 2nd will)"

dataNew <- activity
dataNew[is.na(activity$steps), ]$steps <- mean(activity$steps)

dataNew$daily <- as.POSIXct(with(dataNew, paste(date, paste(interval %/% 100, interval %% 100, sep=":"))),
    			format="%Y-%m-%d %H:%M",tz="")

aggregateSteps <- setNames(aggregate(steps~as.Date(date), dataNew, sum, na.rm = TRUE), c("date","steps"))

xaxis <- seq(1, nrow(aggregateSteps), by = 6)

scaleProp1 <- list(x = list(rot = 45, cex = 1.0, labels = format(aggregateSteps$date, "%d-%b-%Y")[xaxis], at = xaxis))

barchart(date ~ steps, data = aggregateSteps, main = "steps per day", ylab = "steps", xlab = "date", scales = scaleProp1, horizontal = F)

paste("mean:", mean(aggregateSteps$steps))
paste("median:", median(aggregateSteps$steps))

#find the difference with first part of assignment as below
paste("means difference:", mean(aggregateSteps$steps)-mean(stepsAggregate$steps))
paste("medians difference:", median(aggregateSteps$steps)-median(stepsAggregate$steps))
   


## Are there differences in activity patterns between weekdays and weekends?

str(dataNew)
dataNew$date <- as.Date(dataNew$date, "%Y-%m-%d")
dataNew$day <- weekdays(dataNew$date)
dataNew$dayType <- c("weekday")

for (i in 1:nrow(dataNew)){
  if (dataNew$day[i] == "Saturday" || dataNew$day[i] == "Sunday"){
    dataNew$dayType[i] <- "weekend"
  }
}

dataNew$dayType <- as.factor(dataNew$dayType)
aggregateDayType <- aggregate(steps ~ interval+dayType, dataNew, mean)

qplot(interval, steps, data=aggregateDayType, geom=c("line"), xlab="5-min intervals", 
      ylab="steps mean", main="") + facet_wrap(~ dayType, ncol=1)
      

   