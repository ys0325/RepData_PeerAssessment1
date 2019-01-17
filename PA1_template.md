---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---
##Loading packages required for data analysis

```r
library(ggplot2)
```

## Loading and preprocessing the data

```r
Dataset<-read.csv("activity.csv")
#convert varible "date" from factor to date
Dataset$date<-as.Date(Dataset$date, "%Y-%m-%d")
```
## Making Histogram of the total number of steps taken each day

```r
#Remove NA value in dataset
Dataset1<-Dataset[complete.cases(Dataset),]
#Aggregrate total steps by date
DailySteps<-aggregate(steps~date,Dataset1, sum)
#Making histogram using plot
 plot(DailySteps$steps~DailySteps$date, type = "h", lwd = 5, xlab = "Date", ylab = "Steps", main = "Histogram of the total number of steps taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

## Calculating What mean total number of steps taken per day is:

```r
MeanSteps<-mean(DailySteps$steps)
MeanSteps
```

```
## [1] 10766.19
```

```r
MedianSteps<-median(DailySteps$steps, rm.na = TRUE)
MedianSteps
```

```
## [1] 10765
```


## What is the average daily activity pattern?

```r
#Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
AvgIntervalSteps<-aggregate(steps~interval, Dataset1, mean)
plot(AvgIntervalSteps, type = "l", lwd = 2, xlab = "Interval", ylab = "Avg Steps per Interval", 
     main = "Average number of steps taken of each interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
AvgIntervalSteps[which.max(AvgIntervalSteps$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

```r
#Interval No. 835 has the maximum numbers of steps across all the days.
```

## Imputing missing values

```r
#Calculate and report the total number of missing values in the dataset
sum(is.na(Dataset$steps))
```

```
## [1] 2304
```

```r
#Devise a strategy for filling in all of the missing values in the dataset
#The strategy chosen for missing values is replacing missing values with the mean of all existing values in 5-minute interval, since the case is a time series one and steps vary with differnt intervals
#loading package imputeTS(package"imputeTS" already installed)
library(imputeTS)
```

```
## Warning: package 'imputeTS' was built under R version 3.5.2
```

```r
#Fill in missing values in the dataset and create new dataset
NewDataset<-na.interpolation(Dataset)
#Aggregate daily steps and create histogram
DailySteps1<-aggregate(steps~date,NewDataset, sum)
plot(DailySteps1$steps~DailySteps1$date, type = "h", lwd = 5, xlab = "Date", ylab = "Steps", main = "Histogram of imputed total number of steps taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
#Calculate new mean and median
MeanSteps1<-mean(DailySteps1$steps)
MeanSteps1
```

```
## [1] 9354.23
```

```r
MedianSteps1<-median(DailySteps1$steps)
MedianSteps1
```

```
## [1] 10395
```

## Are there differences in activity patterns between weekdays and weekends?

```r
library(lattice)
#Create a new factor variable in the dataset with two levels ??? "weekday" and "weekend"
DataWithDay<-NewDataset
DataWithDay$day<-weekdays(DataWithDay$date)
DataWithDay$type[DataWithDay$day != "Saturday"& DataWithDay$day != "Sunday"] = "Weekday"
DataWithDay$type[DataWithDay$day == "Saturday"| DataWithDay$day == "Sunday"] = "Weekend"
#get average steps per interval in weekdays and weekends, respectively
DayType<-aggregate(steps~interval+type, DataWithDay, mean)
#Create the panel comparing weekdays and weekends
xyplot(steps~interval|type, DayType, type = "l", ylab = "Number of Steps", lwd = 2,layout = c(1,2))
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->
