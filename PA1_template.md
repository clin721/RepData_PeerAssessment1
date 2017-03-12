# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
if(!file.exists('activity.csv'))
  unzip(zipfile="activity.zip")
activity = read.csv("activity.csv")
library(ggplot2)
```

## What is mean total number of steps taken per day?


```r
dailySteps = with(activity, tapply(activity$steps, activity$date, sum, na.rm=T))
hist(dailySteps, col = "blue", main = "Total Steps Taken Daily", breaks = 20)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
dailySteps.med = median(dailySteps, na.rm = TRUE)
dailySteps.mean = mean(dailySteps, na.rm = TRUE)
```

## What is the average daily activity pattern?

```r
stepIntervals = with(activity, tapply(activity$steps, activity$interval, mean, na.rm=T))
qplot(as.numeric(names(stepIntervals)), stepIntervals,
      geom = "line",
      xlab = "5-minute interval",
      ylab = "Average number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
stepIntervals.max = which.max(stepIntervals)
time = names(stepIntervals)[stepIntervals.max]
```

## Imputing missing values

1. Calculating the amount of missing values


```r
missingVal = is.na(activity$steps)
total = sum(missingVal)
```

2. Devise a strategy for filling in all of the missing values in the dataset.

There are many ways to go about replacing the missing values, each one changing the dataset in a different way. In my project, the missing values are replaced with the average values of their corresponding 5-minute intervals. 

3. Recreate the dataset with the missing values filled in.

```r
#will use the average interval value to fill in the NA values
naInterval = activity$interval[missingVal]
activity.new = activity
activity.new$steps[missingVal] = stepIntervals[as.character(naInterval)]
newDaily = with(activity, tapply(activity$steps, activity$date, sum))
```

4. Make a histogram of the total number of steps taken each day. Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? 

```r
hist(newDaily, col = "red", breaks = 20,
     main = "Daily Steps with NA Values Replaced",
     xlab = "Total Steps Taken Daily")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

```r
new.med = median(newDaily)
new.mean = mean(newDaily)
```

Both the mean and the median are higher after imputing missing values. This is because when the daily amount of steps were calculated, days with missing values ended up with zero steps taken. Filling in the missing values will change the amount of steps taken to a positive value, resulting in an increase in both the mean and median values.

## Are there differences in activity patterns between weekdays and weekends?

1. Create a factor variable to differentiate between weekday and weekend

```r
activity.new$dayWeek = ifelse(as.POSIXlt(activity.new$date)$wday %in% c(0,6), 
                              'weekend', 'weekday')

groupedData = aggregate(steps ~ interval+dayWeek, data = activity.new, mean)
```

2. Make a panel plot of the steps for every interval, averaged across weekdays and weekends.


```r
ggplot(groupedData, aes(interval, steps)) + 
         geom_line(colour = 'red') + 
         facet_grid(dayWeek ~ .) +
         xlab('Interval') + 
         ylab('Number of steps')
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->
