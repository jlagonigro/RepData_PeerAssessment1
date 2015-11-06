---
title: "Reproducible Research: Peer Assessment 1"
author: "John Lagonigro"
output: html_document
keep_md: true
---


## Loading and preprocessing the data

```r
library(dplyr)
activity <- read.csv("activity.csv", colClasses = c("integer","Date","integer"), header=TRUE)
```

## What is mean total number of steps taken per day?
1. Calculate the total number of steps taken per day

```r
total_steps_per_day <- activity %>%
                            filter(!is.na(steps)) %>%
                            group_by(date) %>%
                            summarize(steps = sum(steps)) %>%
                            print
```

```
## Source: local data frame [53 x 2]
## 
##          date steps
##        (date) (int)
## 1  2012-10-02   126
## 2  2012-10-03 11352
## 3  2012-10-04 12116
## 4  2012-10-05 13294
## 5  2012-10-06 15420
## 6  2012-10-07 11015
## 7  2012-10-09 12811
## 8  2012-10-10  9900
## 9  2012-10-11 10304
## 10 2012-10-12 17382
## ..        ...   ...
```

2. Make a histogram of the total number of steps taken each day

```r
hist(total_steps_per_day$steps, breaks=20, xlab = "Steps", ylab = "Frequency", main="Total Steps Taken Each Day", col="green")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

3. Calculate and report the mean and median of the total number of steps taken per day

```r
print(paste("Mean steps each day:", mean(total_steps_per_day$steps), sep=" "))
```

```
## [1] "Mean steps each day: 10766.1886792453"
```

```r
print(paste("Median steps each day:", median(total_steps_per_day$steps), sep=" "))
```

```
## [1] "Median steps each day: 10765"
```

## What is the average daily activity pattern?
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
avg_daily_activity <- activity %>%
                            filter(!is.na(steps)) %>%
                            group_by(interval) %>%
                            summarize(steps=mean(steps)) %>%
                            print
```

```
## Source: local data frame [288 x 2]
## 
##    interval     steps
##       (int)     (dbl)
## 1         0 1.7169811
## 2         5 0.3396226
## 3        10 0.1320755
## 4        15 0.1509434
## 5        20 0.0754717
## 6        25 2.0943396
## 7        30 0.5283019
## 8        35 0.8679245
## 9        40 0.0000000
## 10       45 1.4716981
## ..      ...       ...
```

```r
plot(avg_daily_activity, type = "l")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
print(paste("5-minute interval with maximum average number of steps:",avg_daily_activity[which.max(avg_daily_activity$steps), ]$interval, sep=" "))
```

```
## [1] "5-minute interval with maximum average number of steps: 835"
```


## Imputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
steps_are_NA <- activity %>%
                    filter(is.na(steps))
print(paste("Number of rows with NA:",nrow(steps_are_NA),sep=" "))
```

```
## [1] "Number of rows with NA: 2304"
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
activity_fixed_NA <- activity
for(i in 1:nrow(activity_fixed_NA)){
    if(is.na(activity_fixed_NA[i,"steps"])) {
        activity_fixed_NA[i,"steps"] <- avg_daily_activity[avg_daily_activity$interval==activity_fixed_NA[i,"interval"],"steps"]
    }
}
```

4a. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 

```r
total_fixed_steps_per_day <- activity_fixed_NA %>%
        group_by(date) %>%
        summarize(steps = sum(steps)) %>%
        print
```

```
## Source: local data frame [61 x 2]
## 
##          date    steps
##        (date)    (dbl)
## 1  2012-10-01 10766.19
## 2  2012-10-02   126.00
## 3  2012-10-03 11352.00
## 4  2012-10-04 12116.00
## 5  2012-10-05 13294.00
## 6  2012-10-06 15420.00
## 7  2012-10-07 11015.00
## 8  2012-10-08 10766.19
## 9  2012-10-09 12811.00
## 10 2012-10-10  9900.00
## ..        ...      ...
```

```r
hist(total_fixed_steps_per_day$steps, breaks=20, xlab = "Steps", ylab = "Frequency", main="Total Steps Taken Each Day with NAs fixed with Means", col="green")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png) 

4b. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
print(paste("Mean steps each day (fixed NAs):", mean(total_fixed_steps_per_day$steps), sep=" "))
```

```
## [1] "Mean steps each day (fixed NAs): 10766.1886792453"
```

```r
print(paste("Median steps each day (fixed NAs):", median(total_fixed_steps_per_day$steps), sep=" "))
```

```
## [1] "Median steps each day (fixed NAs): 10766.1886792453"
```

```r
print(paste("Change in mean when replacing NAs with mean for interval:", mean(total_fixed_steps_per_day$steps)-mean(total_steps_per_day$steps),sep=" "))
```

```
## [1] "Change in mean when replacing NAs with mean for interval: 0"
```

```r
print(paste("Change in median when replacing NAs with mean for interval:", median(total_fixed_steps_per_day$steps)-median(total_steps_per_day$steps),sep=" "))
```

```
## [1] "Change in median when replacing NAs with mean for interval: 1.1886792452824"
```

## Are there differences in activity patterns between weekdays and weekends?
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
activity_fixed_NA <- mutate(activity_fixed_NA, day=weekdays(date))
activity_fixed_NA$day <- ifelse((activity_fixed_NA$day=="Saturday" | activity_fixed_NA$day=="Sunday"),"Weekend", "Weekday")
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
library(lattice)
weekend_vs_weekday <- aggregate(steps ~ interval + day, data = activity_fixed_NA, mean)
xyplot(steps ~ interval | day, weekend_vs_weekday, type = "l", layout = c(1, 2), xlab = "Interval", ylab = "Steps", main="Total Steps for Weekend vs. Weekday")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png) 
