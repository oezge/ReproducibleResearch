---
title: "assignment1"
author: "Oezge"
date: "14.03.2015"
output: html_document
---
  
### Loading and preprocessing the data 

- It is assumed that the file to be read is already in the working directory


```r
filepath <- paste(getwd(), 'activity.csv', sep="/")
actData <- read.csv(file = filepath, stringsAsFactors = FALSE)
actData$date <- as.Date(actData$date)
```
 
- The data is loaded in a way that anything that can be in format factor will not 
be allowed and thay are loaded in character format. In this way, the date column
is loaded  in character format. In the second line, the format of date column is
changed to date format, %y%m%d.

### What is mean total number of steps taken per day?

I use functions from dplyr package to subset the data , 
so I start with  uploading it.


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

 - First and second parts of the question are computed together :


```r
actDataGrp <- group_by(actData,date)
actDataGrp <- na.omit(actDataGrp)
sumSteps <- summarize(actDataGrp, sum(steps))
names(sumSteps) <- c('date', 'sums')
hist(sumSteps$sums, xlab = '', main = "Histogram of total 
     number of steps on each day")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

 - Computing mean and median of total number of steps taken per day :


```r
meanSumSteps <- mean(sumSteps$sums)
medianSumSteps <- median(sumSteps$sums)
```

The mean and median of sum of steps on each day is 1.0766189 &times; 10<sup>4</sup> and  
10765 respectively.

### What is the average daily activity pattern?

In order to plot, I use ggplot2 package, so first it is downloaded

```r
library(ggplot2)
```

- The first part of the question is answered below with the code chunk. The data 
is first grouped by Interval column and then the na values are ommitted. Then 
the average of steps with respect to number of dates are calculated and
new information is stored in a data frame called averagedSteps. For plotting,
ggplot is used.



```r
actDataInterval <- na.omit(group_by(actData,interval))
numberDates <- length(unique(actDataInterval))
averagedSteps <- summarize(actDataInterval, sum(steps)/numberDates)
names(averagedSteps) <- c('interval', 'averageStepNumb')
ggplot(data = averagedSteps, aes(interval, averageStepNumb)) + geom_line() +
  scale_x_continuous(breaks= round(seq(min(averagedSteps$interval),
                                       max(averagedSteps$interval), by = 200),1))
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

- From the figure it can be seen that, the maxiumum number of steps are contained
by the interval that is between 800th and 900th.

### Imputing missing values

- The first question asks the number of missing values in the data frame.


```r
numNA <- length(which(is.na(actData)))
```

The number of missing values in the original data is 2304.

- The second question asks to develop a strategy to replace NA values.

I prefer to replace all NA values with 0's.

- The third question asks to implement this strategy to a new data set


```r
actDataNoMiss <- actData
actDataNoMiss$steps[which(is.na(actData))] <- rep(0, numNA)
```

- The fourth question asks to make a histogram as in the first part and compare
the results


```r
actDataGrpNoMiss <- group_by(actDataNoMiss,date)
sumStepsNoMiss <- summarize(actDataGrpNoMiss, sum(steps))
names(sumStepsNoMiss) <- c('date', 'sums')
hist(sumStepsNoMiss$sums, xlab = '', main = "Histogram of total 
     number of steps on each day")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png) 

Computing mean and median of total number of steps taken per day :


```r
meanSumStepsNoMiss <- mean(sumStepsNoMiss$sums)
medianSumStepsNoMiss <- median(sumStepsNoMiss$sums)
```

The mean and median of sum of steps on each day is 9354.2295082 and  
1.0395 &times; 10<sup>4</sup> respectively.

If we compare two histograms, we see that they look pretty much the same. 
However, the values of medians and means are pretty different .

### Are there differences in activity patterns between weekdays and weekends?

- The first question asks  to create a new data set with two levels : weekdays
and weekend


```r
weekdays <-(weekdays(actDataNoMiss$date))
actDataNoMiss$weekdays <- as.factor(weekdays)
levels(actDataNoMiss$weekdays) <- c('weekday','weekday','weekday','weekday',
                                    'weekday','weekend','weekend')
```

- The second question asks to make a time series plot of the 5-minute interval 
and the average number of steps taken, averaged across all weekday days or 
weekend days.


```r
actDataNoMiss_wdays <- filter(actDataNoMiss, actDataNoMiss$weekdays == 'weekday')
actDataNoMiss_wend <- filter(actDataNoMiss, actDataNoMiss$weekdays == 'weekend')

AveSteps_wdays <-summarize(group_by(actDataNoMiss_wdays, interval),mean(steps))
AveSteps_wend <-summarize(group_by(actDataNoMiss_wend, interval),mean(steps))
names(AveSteps_wdays) <- c('interval','avesteps')
names(AveSteps_wend) <- c('interval','avesteps')
ggplot(data = AveSteps_wdays, aes(interval, avesteps)) + geom_line() + 
              scale_x_continuous(breaks= round(seq(min(AveSteps_wdays$interval),
                                                   max(AveSteps_wdays$interval), 
                                                   by = 200),1)) + 
                                                   ggtitle("Average Steps for weekdays")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png) 

```r
ggplot(data = AveSteps_wend, aes(interval, avesteps)) + geom_line() + 
              scale_x_continuous(breaks= round(seq(min(AveSteps_wend$interval),
                                                   max(AveSteps_wend$interval), 
                                                   by = 200),1)) +
                                                     ggtitle("Average Steps for weekend")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-2.png) 
