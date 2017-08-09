# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

1. Load the data

```r
unzip("activity.zip")
activity_data <- read.csv(file="activity.csv",
                          header = TRUE,
                          na.strings = "NA")
```

2. Process/transform the data (if necessary) into a format suitable for your analysis

```r
activity_data$date <- as.Date(as.character(activity_data$date), "%Y-%m-%d")
```

## What is mean total number of steps taken per day?

1. Calculate the total number of steps taken per day

```r
daily_total_steps <- aggregate(steps ~ date,
                               sum,
                               data=activity_data,
                               na.rm=TRUE)
```

2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

```r
hist(daily_total_steps$steps,
     breaks = 10)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day

```r
mean(daily_total_steps$steps)
```

```
## [1] 10766.19
```

```r
median(daily_total_steps$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

1. Make a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
daily_activity_pattern <- aggregate(steps ~ interval,
                                    mean,
                                    data=activity_data,
                                    na.rm=TRUE)
plot(daily_activity_pattern$interval,
     daily_activity_pattern$steps,
     main="Average daily activity pattern",
     xlab="Interval",
     ylab="Steps",
     type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
daily_activity_pattern[which.max(daily_activity_pattern$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```


## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with 𝙽𝙰s)

```r
sum(is.na(activity_data$steps))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
The strategy implemented below performs the following to impute the data:
* Identify the missing values in the activity data
* Replaces the missing values with the value for that interval from the mean data set


```r
# This method imputes the number of steps for NA values
# This done by taking the mean number of steps from the
# corresponding interval across the data set
impute_num_steps <-function(activity_data, mean_activity_data) {

  # identify NA steps
  na_indices <- which(is.na(activity_data))

  for (i in na_indices) {
    interval <- activity_data[i, 3]
    
    # extract mean for the given interval
    mean_interval <- mean_activity_data[ mean_activity_data$interval==interval, ]
    # Column 2 of the mean_activity_data is the steps column
    mean_steps <- mean_interval[1,2]

    # Set the value of the number of steps to the mean value
    # Column 1 of activity_data is the 'steps' column 
    activity_data[i, 1] <- mean_steps
  }
  activity_data
}
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
imputed_activity_data <- impute_num_steps(activity_data, daily_activity_pattern)
```

## Are there differences in activity patterns between weekdays and weekends?

```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.4.1
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)

isWeekday <- function(test_date) {
  day_of_week <- weekdays(test_date)
  
  is_weekday <- 1
  weekend_days <- c("Saturday", "Sunday")
  if ( day_of_week %in% weekend_days) {
    is_weekday <- 2
  }
  is_weekday
}

# Create weekday column and populate
imputed_activity_data <- mutate(imputed_activity_data, weekday = 1 )
imputed_activity_data$weekday <- lapply(imputed_activity_data$date, isWeekday)

weekday_activity <- filter(imputed_activity_data, weekday==1)
daily_weekday_activity_pattern <- aggregate(steps ~ interval,
                                            mean,
                                            data=weekday_activity
                                            )
daily_weekday_activity_pattern <- mutate(daily_weekday_activity_pattern, weekday = 1 )
weekend_activity <- filter(imputed_activity_data, weekday==2)
daily_weekend_activity_pattern <- aggregate(steps ~ interval,
                                            mean,
                                            data=weekend_activity
                                            )
daily_weekend_activity_pattern <- mutate(daily_weekend_activity_pattern, weekday = 2 )

aggregated_activity <- rbind(daily_weekday_activity_pattern,
                             daily_weekend_activity_pattern)

aggregated_activity$weekday <- as.factor(aggregated_activity$weekday)
levels(aggregated_activity$weekday) <- c("weekday", "weekend")
ggplot(aggregated_activity, aes(interval, steps, weekday)) + geom_line() + facet_grid(weekday ~ .) + ggtitle("Weekday vs Weekend Daily Activity Profile")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->
