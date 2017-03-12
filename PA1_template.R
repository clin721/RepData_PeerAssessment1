#
library(ggplot2)
activity = read.csv("activity.csv")

#
dailySteps = with(activity, tapply(activity$steps, activity$date, sum, na.rm=T))
dailySteps.med = median(dailySteps, na.rm = TRUE)
dailySteps.mean = mean(dailySteps, na.rm = TRUE)
hist(dailySteps, col = "blue", main = "Total Steps Taken Daily", breaks = 20)

#
stepIntervals = with(activity, tapply(activity$steps, activity$interval, mean, na.rm=T))
qplot(as.numeric(names(stepIntervals)), stepIntervals,
      geom = "line",
      xlab = "5-minute interval",
      ylab = "Average number of steps")
  
stepIntervals.max = which.max(stepIntervals)
time = names(stepIntervals)[stepIntervals.max]


# time is 8:35 AM

#
missingVal = is.na(activity$steps)
total = sum(missingVal)

#will use the average interval value to fill in the NA values

naInterval = activity$interval[missingVal]
activity.new = activity
activity.new$steps[missingVal] = stepIntervals[as.character(naInterval)]
newDaily = with(activity, tapply(activity$steps, activity$date, sum))

hist(newDaily, col = "red", breaks = 20,
     main = "Daily Steps with NA Values Replaced",
     xlab = "Total Steps Taken Daily")
new.med = median(newDaily)
new.mean = mean(newDaily)

#
activity.new$dayWeek = ifelse(as.POSIXlt(activity.new$date)$wday %in% c(0,6), 
                              'weekend', 'weekday')

groupedData = aggregate(steps ~ interval+dayWeek, data = activity.new, mean)
ggplot(groupedData, aes(interval, steps)) + 
         geom_line(colour = 'red') + 
         facet_grid(dayWeek ~ .) +
         xlab('Interval') + ylab('Number of steps')

