---
title: "Activity Monitoring"
author: "Chris Peck"
date: "August 30, 2016"
output: html_document
---


Read in the data set

```r
setwd("/Users/cpeck44/data/Activity Monitoring")
t<-read.csv("./activity.csv")
```


Plot a histogram of the total number of steps taken each day

```r
t1<-as.data.frame(aggregate(t$steps,by=list(t$date),FUN=sum))
names(t1)[1]<-"date"
names(t1)[2]<-"steps"
hist(t1$steps,xlab="Steps",ylab="Number of days",main="Total number of steps taken each day")
```

![plot of chunk plot1](figure/plot1-1.png)

Calculate the mean and median steps taken per day using the summary function

```r
t1<-as.data.frame(aggregate(t$steps,by=list(t$date),FUN=sum))
names(t1)[1]<-"date"
names(t1)[2]<-"steps"
summary(t1$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##      41    8841   10760   10770   13290   21190       8
```

Create a time series plot of average steps across all days by interval with NAs omitted

```r
t2<-na.omit(t)
t3<-as.data.frame(aggregate(t2$steps,by=list(t2$interval),FUN=mean))
names(t3)[1]<-"interval"
names(t3)[2]<-"steps"
plot(t3$interval,t3$steps,type="l",xlab="Interval",ylab="Average Steps",main="Average Number of Steps Taken by Interval")
```

![plot of chunk plot2](figure/plot2-1.png)

Deterimine the 5-minute interval that, on average, contains the maximum number of steps

```r
index<-which.max(t3$steps)
t3[index,]
```

```
##     interval    steps
## 104      835 206.1698
```

Count the number of rows with NAs in the data

```r
sum(is.na(t$steps))
```

```
## [1] 2304
```

Replace the missing values with the means by interval

```r
t5<-t
t5$steps[is.na(t5$steps)] <- ave(t5$steps, t5$interval, FUN=function(x)mean(x, 
  na.rm = T))[is.na(t5$steps)]
```

Plot a histogram of the number of steps taken by day on data with imputed NAs

```r
t6<-as.data.frame(aggregate(t5$steps,by=list(t5$date),FUN=sum))
names(t6)[1]<-"date"
names(t6)[2]<-"steps"
hist(t6$steps,xlab="Steps",ylab="Number of days",main="Total number of steps taken each day (imputed NAs)")
```

![plot of chunk plot3](figure/plot3-1.png)

Calculate the mean and median steps taken per day using replaced NAs

```r
summary(t6$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10770   10770   12810   21190
```

Compare these values to the original values without imputed misisng values.

The median increased from 10760 to 10770 because all days that had NAs were replaced with the mean, causing the distribution of days to be different and the median to change. The mean of 10770 remained the same since missing values were replaced with the mean.  The summary of the original data prior to imputing NAs is shown below for comparison.  The impact of imputing values had a minimal impact on the median and mean.

```r
summary(t1$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##      41    8841   10760   10770   13290   21190       8
```

Create a panel plot comparing the average number of steps taken per 5-minute
interval across weekdays and weekends.  For this analysis, we will use the dataset with the imputed NAs. First we must add a field to the data frame that contains an indicator of weekend or weekday and then summarize the steps by the indicator and interval.

```r
## fill in weekday and weekend
t5$date<-as.Date(t5$date, format = "%Y-%m-%d")
t5$day<-weekdays(t5$date)

## create a dataframe to translate days of the week into weekday or weekend
day<-c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
daytype<-c("Weekday","Weekday","Weekday","Weekday","Weekday","Weekend","Weekend")
dtdf<-data.frame(day,daytype)

## create a dataframe with weekday or weekend in a column
z<-merge(t5,dtdf,by="day")

## calculate mean steps by interval and weekday/weekend
z1<-as.data.frame(aggregate(z$steps,by=list(z$interval,z$daytype),FUN=mean))
names(z1)[1]<-"interval"
names(z1)[2]<-"Day"
names(z1)[3]<-"Steps"

## create a panel plot with mean steps by interval and weekday/weekend
library("lattice")
xyplot(Steps~interval|Day,data=z1,main="Average steps by Interval for Weekends and Weekdays (imputed NAs)",type="l",layout=c(1,2))
```

![plot of chunk plot4](figure/plot4-1.png)
