---
title: "Reproducible Research: Peer Assessment 1"
date: "November 15, 2014"
output: html_document
keep_md: true
---

##Loading and preprocessing the data

Load the data.
```{r}
data = read.csv("activity.csv")
```

Process/transform the data (if necessary) into a suitable format for analysis.
```{r}
noNA_data = na.omit(data)
rownames(noNA_data) = 1:nrow(noNA_data)
```


##What is mean total number of steps taken per day?

Histogram of the total number of steps taken each day
```{r}
library(ggplot2)
ggplot(noNA_data, aes(date, steps))+ geom_bar(stat = "identity", colour = "steelblue", fill = "steelblue", width = 0.5) + labs(title = "Histogram of Total Number of Steps Taken Each Day", x = "Date", y = "Total number of steps")
```

Mean of the total number of steps taken per day.
```{r}
totalSteps = aggregate(noNA_data$steps, list(Date = noNA_data$date), FUN = "sum")$x
mean(totalSteps)
```

Median of the total number of steps taken per day,
```{r}
median(totalSteps)
```


##What is the average daily activity pattern?

Time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).
```{r}
avg_steps = aggregate(noNA_data$steps, list(interval = as.numeric(as.character(noNA_data$interval))), FUN = "mean")
names(avg_steps)[2] = "meanOfSteps"

ggplot(avg_steps, aes(interval, meanOfSteps)) + geom_line(color = "steelblue", size = 0.8) + labs(title = "Time Series Plot of the 5-minute Interval", x = "5-minute intervals", y = "Average Number of Steps Taken")
```

Which 5-minute interval, on average across all the days in the data set, contains the maximum number of steps?
```{r}
avg_steps[avg_steps$meanOfSteps == max(avg_steps$meanOfSteps), ]
```


##Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

Total number of missing values in the dataset.
```{r}
sum(is.na(data))
```

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

New dataset that is equal to the original dataset but with the missing data filled in.
```{r}
new_data = data
for (i in 1:nrow(new_data)) 
{
  if (is.na(new_data$steps[i])) 
  {
      new_data$steps[i]=avg_steps[which(new_data$interval[i] == avg_steps$interval), ]$meanOfSteps
  }
}

head(new_data)

sum(is.na(new_data))
```


Histogram of the total number of steps taken each day.
```{r}
ggplot(new_data, aes(date, steps)) + geom_bar(stat = "identity",colour = "steelblue",fill = "steelblue",width = 0.7) + labs(title = "Histogram of Total Number of Steps Taken Each Day (no missing data)", x = "Date", y = "Total number of steps")
```

Mean of the total number of steps taken per day.
```{r}
newTotalSteps = aggregate(new_data$steps, list(Date = new_data$date), FUN = "sum")$x
mean(newTotalSteps)
```

Median of the total number of steps taken per day.
```{r}
median(newTotalSteps)
```

Comparison of two means.
```{r}
mean(newTotalSteps) - mean(totalSteps)
```

Comparison of two medians.
```{r}
median(newTotalSteps) - median(totalSteps)
```

So, after imputing the missing data, the new mean of total steps taken per day is the same as the old mean of total steps taken per day. But the new median of total steps taken per day is greater than the old median of total steps taken per day.


##Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.
```{r}
head(new_data)
new_data$weekdays = factor(format.Date(new_data$date,"%A"))
levels(new_data$weekdays)
```

The dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
```{r}
levels(new_data$weekdays) = list(weekday = c("Monday","Tuesday","Wednesday","Thursday", "Friday"),
                                  weekend = c("Saturday", "Sunday"))
levels(new_data$weekdays)
table(new_data$weekdays)
```


Time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).
```{r}
avg_steps = aggregate(new_data$steps,list(interval = as.numeric(as.character(new_data$interval)), 
                       weekdays = new_data$weekdays), FUN = "mean")
names(avg_steps)[3] = "meanOfSteps"

library(lattice)

xyplot(avg_steps$meanOfSteps~avg_steps$interval | avg_steps$weekdays,layout = c(1, 2),type = "l", 
       xlab = "Interval", ylab = "Number of steps")
```
