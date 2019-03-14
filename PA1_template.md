---
title: "Reproducible Research:  Peer Assessment 1"
output: 
        html_document:
                keep_md:  true
---



## Loading and preprocessing the data

1.  Load the data (i.e. read.csv())


```r
setwd("C:/Users/schiprt/Desktop/Tom/005 - Reproducible Research/002 - Week 2/Project")
activity <- read.csv("activity.csv")
```

2.  Process/transform the data (if necessary) into a format suitable for your analysis


```r
activity$date <- as.Date(activity$date)
```


## What is the mean total number of steps taken per day?

1.  Calculate the total number of steps taken per day


```r
steps_per_day <- aggregate(steps ~ date, activity, sum)
head(steps_per_day)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```

2.  Make a histogram of the total number of steps taken each day

```r
hist(steps_per_day$steps, main = "Number of Steps per Day", xlab = "Number of Steps", col="blue")
```

![plot of chunk hist001](./figure/hist001-1.png)

3.  Calculate and report the mean and median of the total number of steps taken per day

```r
mean_steps <- mean(steps_per_day$steps)
mean_steps
```

```
## [1] 10766.19
```

```r
median_steps <- median(steps_per_day$steps)
median_steps    
```

```
## [1] 10765
```

## What is the average daily activity pattern?

1.  Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
avg_steps_per_interval <- aggregate(steps ~ interval, activity, sum)
plot(avg_steps_per_interval$interval,avg_steps_per_interval$steps, type = "l", main = "Average Steps per Interval", xlab = "Interval", ylab = "Average Number of Steps")
```

![plot of chunk time_series](./figure/time_series-1.png)

2.  Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
max_interval <- avg_steps_per_interval[which.max(avg_steps_per_interval$steps),"interval"]
max_interval
```

```
## [1] 835
```

## Inputting missing values

1.  Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
count_nas <- sum(is.na(activity$steps))
count_nas
```

```
## [1] 2304
```

2.  Devise a strategy for filling in all of the missing values in the dataset. This example uses the mean for that 5-minute interval.


```r
mean_steps_per_interval <- aggregate(steps ~ interval, activity, mean)
for(i in 1:nrow(activity)) {
        if (is.na(activity[i,"steps"]) == TRUE) {
                interval_value <- activity[i,"interval"]
                replace_na_value <- mean_steps_per_interval[which(mean_steps_per_interval$interval == interval_value),"steps"]
                activity[i,4] <- replace_na_value
        } else {
                activity[i,4] <- activity[i,"steps"]
        }
}
colnames(activity)[4] <- "filled_steps"
head(activity)
```

```
##   steps       date interval filled_steps
## 1    NA 2012-10-01        0    1.7169811
## 2    NA 2012-10-01        5    0.3396226
## 3    NA 2012-10-01       10    0.1320755
## 4    NA 2012-10-01       15    0.1509434
## 5    NA 2012-10-01       20    0.0754717
## 6    NA 2012-10-01       25    2.0943396
```

3.  Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
activity_filled <- activity
activity_filled[,1] <- activity_filled[,4]
activity_filled <- activity_filled[,1:3]
head(activity_filled)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

4.  Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
steps_per_day_filled <- aggregate(steps ~ date, activity_filled, sum)
mean_steps_filled <- mean(steps_per_day_filled$steps)
median_steps_filled <- median(steps_per_day_filled$steps)
mean_dif <- mean_steps_filled - mean_steps
median_dif <- median_steps_filled - median_steps
hist(steps_per_day_filled$steps, main = "Number of Steps per Day", xlab = "Number of Steps", col="red")
```

![plot of chunk steps_per_day_filled](./figure/steps_per_day_filled-1.png)

The mean of the total steps after replacing the NAs with the mean for that interval is 1.0766189 &times; 10<sup>4</sup>.  This is a difference of 0.  

The median of the total steps after replacing the NAs with the mean for that interval is 1.0766189 &times; 10<sup>4</sup>.  This is a difference of 1.1886792. 

## Are there differences in activity patterns between weekdays and weekends?

1.  Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
activity_filled$day <- weekdays(activity_filled$date)
for(i in 1:nrow(activity_filled)) {
        if ((activity_filled[i,"day"]) == "Saturday" || activity_filled[i,"day"] =="Sunday") {
                activity_filled[i,5] <- "Weekend"
        } else {
                activity_filled[i,5] <- "Weekday"
        }
}
colnames(activity_filled)[5] <- "weekend_weekday"
head(activity_filled)
```

```
##       steps       date interval    day weekend_weekday
## 1 1.7169811 2012-10-01        0 Monday         Weekday
## 2 0.3396226 2012-10-01        5 Monday         Weekday
## 3 0.1320755 2012-10-01       10 Monday         Weekday
## 4 0.1509434 2012-10-01       15 Monday         Weekday
## 5 0.0754717 2012-10-01       20 Monday         Weekday
## 6 2.0943396 2012-10-01       25 Monday         Weekday
```

2.  Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
we_wd_steps_per_interval_filled <- aggregate(steps ~ interval + weekend_weekday, activity_filled, mean)
library(lattice)
```

```
## Warning: package 'lattice' was built under R version 3.4.4
```

```r
xyplot(steps ~ interval | factor(weekend_weekday), we_wd_steps_per_interval_filled, aspect = 1/2, type = "l")
```

![plot of chunk weekend_weekday_plot](./figure/weekend_weekday_plot-1.png)
