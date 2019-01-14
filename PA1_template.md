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
Dataset<-Dataset[complete.cases(Dataset),]
#Aggregrate total steps by date
DailySteps<-aggregate(steps~date,Dataset, sum)
#Making histogram using plot
 plot(DailySteps$steps~DailySteps$date, type = "h", lwd = 5, xlab = "Date", ylab = "Steps", main = "Histogram of the total number of steps taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

## Calculating What mean total number of steps taken per day is:




## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?