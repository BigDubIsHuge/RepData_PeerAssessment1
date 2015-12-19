---
title: "PeerAssessment1"
output: html_document
---

##What is mean total number of steps taken per day?
It is important to be aware of the total number of steps taken per day between the early of October 2012 to the end of November 2012.  
In this report, this information is illustrated through a histogram as shown below:  


```r
data <- read.csv('activity.csv',header = TRUE,stringsAsFactors = FALSE)
vectorSum <- tapply(data$steps,data$date,sum)
df <- data.frame(date=names(vectorSum),sum=vectorSum)
hist(df$sum,breaks=30,main='Total number of steps per day',
     xlab='Number of steps', 
     col='red')
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png) 

Based on the provided information, the mean and median of the total number of steps taken per day is 1.0766189 &times; 10<sup>4</sup>, and 
10765, respectively. 

##What is the average daily activity pattern?
The following figure represents the variation of the average number of steps taken (averaged over all days).  

```r
vectorMean <- tapply(data$steps,data$interval,mean,na.rm=TRUE)
intervals <- subset(data$interval,data$date=="2012-10-01")
plot(intervals,vectorMean,type = "l", 
     xlab="Time Interval",
     ylab="The average number of steps")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

```r
newdataframe <- data.frame(intervals,vectorMean)
maximumStepsInterval <- names(which(newdataframe$vectorMean==max(vectorMean)))
```

From this figure, it is noted that the 835 5-minute time interval contains the maximum number of steps.  


```r
numberNAs <- sum(is.na(data$steps))
```
Missing values are observed in the given data, and there are 2304 of them.  
In this report, all missing values are replaced with the mean value of their 5-minute interval.  
After modification, the new histogram is presented below:  


```r
for (i in 1:nrow(data)){
  if(is.na(data[i,1])){
    index <- which(newdataframe$intervals==data[i,3])
    data[i,1]<-newdataframe[index,2]
  }
} 

vectorSum <- tapply(data$steps,data$date,sum)
df <- data.frame(date=names(vectorSum),sum=vectorSum)
hist(df$sum,breaks=30,main='Total number of steps per day',
     xlab='Number of steps', 
     col='red')
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

The new mean and median of the total number of steps taken per day is 1.0766189 &times; 10<sup>4</sup>, and 1.0766189 &times; 10<sup>4</sup>, respectively. It is not surprising that the mean value stays the same as before the modification as the mean values are used to fill NAs.  
The change in median value is also not very significant.

##Are there differences in activity patterns between weekdays and weekends?
From the following figure, it seems that people tends to walk slightly more in weekends than weekdays. However, it is observed that population walks the most between 8 to 9 am on weekdays.


```r
par(mfrow=c(2,1))
WeekType <- ifelse(weekdays(as.Date(data$date)) %in% c("Sunday","Saturday"),"Weekend","Weekday")
data <- cbind(data,WeekType)
dataWeekday <- subset(data,WeekType=="Weekday")
dataWeekend <- subset(data,WeekType=="Weekend")

WeekdayMean <- tapply(dataWeekday$steps,dataWeekday$interval,mean)
Weekdayintervals <- subset(dataWeekday$interval,dataWeekday$date=="2012-10-01")

plot(Weekdayintervals,WeekdayMean,type = "l", 
     main="Weekday",
     xlab="Time Interval",
     ylab="Average steps",
     ylim=c(0,200),
     cex.axis=0.7)

WeekendMean <- tapply(dataWeekend$steps,dataWeekend$interval,mean)
plot(Weekdayintervals,WeekendMean,type = "l", 
     main="Weekend",
     xlab="Time Interval",
     ylab="Average steps",
     ylim=c(0,200),
     cex.axis=0.7)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 
