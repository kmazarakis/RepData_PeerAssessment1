# Reproducible Research: Peer Assessment 1
Kleanthis Mazarakis  


## Loading and preprocessing the data

```r
activity_data <- read.csv("activity.csv")
head(activity_data)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
str(activity_data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
activity_data$dateTime <- as.Date(activity_data$date, format = "%Y-%m-%d")
```

## What is mean total number of steps taken per day?

```r
StepsPerDay <- tapply(activity_data$steps, activity_data$dateTime, FUN = "sum")
hist(StepsPerDay)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
mean_per_day <- mean(StepsPerDay, na.rm = T)
mean_per_day
```

```
## [1] 10766.19
```

```r
median_per_day <- median(StepsPerDay, na.rm = T)
median_per_day
```

```
## [1] 10765
```
The mean steps per day was 10766 and the median was 10765

## What is the average daily activity pattern?

```r
interval_average <- tapply(activity_data$steps, activity_data$interval, mean, na.rm = T)
intervals_factor <- factor(activity_data$interval)
plot(levels(intervals_factor), interval_average, type = "l", xlab = "Intervals", ylab = "Average Steps", main = "Average Daily Activity Pattern")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

Now let's check which 5 min interval, on average, contains the maximum number of steps

```r
# Creating a data frame with the intervals and the average steps per interval and then will select the maximum row
df <- data.frame(levels(intervals_factor), interval_average)
df[df$interval_average == max(df$interval_average),]
```

```
##     levels.intervals_factor. interval_average
## 835                      835         206.1698
```
So the maximum number of steps per interval is 206 and it occured on interval 835.

## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
Total_na_rows <- nrow(activity_data)-sum(complete.cases(activity_data))
paste("The Total number of rows containing missing values is", Total_na_rows)
```

```
## [1] "The Total number of rows containing missing values is 2304"
```

```r
summary(is.na(activity_data))
```

```
##    steps            date          interval        dateTime      
##  Mode :logical   Mode :logical   Mode :logical   Mode :logical  
##  FALSE:15264     FALSE:17568     FALSE:17568     FALSE:17568    
##  TRUE :2304      NA's :0         NA's :0         NA's :0        
##  NA's :0
```

It looks like all the missing values are only on the steps column
in order to fill in the missing values, we are going to use the mean of the specific 5 minute interval


```r
for (i in 1:nrow(activity_data)) {
    if (is.na(activity_data$steps[i])) {activity_data$steps[i] <- interval_average[names(interval_average) == activity_data$interval[i]]}
}

nrow(activity_data)-sum(complete.cases(activity_data))
```

```
## [1] 0
```

```r
head(activity_data)
```

```
##       steps       date interval   dateTime
## 1 1.7169811 2012-10-01        0 2012-10-01
## 2 0.3396226 2012-10-01        5 2012-10-01
## 3 0.1320755 2012-10-01       10 2012-10-01
## 4 0.1509434 2012-10-01       15 2012-10-01
## 5 0.0754717 2012-10-01       20 2012-10-01
## 6 2.0943396 2012-10-01       25 2012-10-01
```

We can see that the missing values have been replaced successfully


4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
StepsPerDay_no_na <- tapply(activity_data$steps, activity_data$dateTime, FUN = "sum")
hist(StepsPerDay_no_na, main = "Histogram of Steps per Day with missing values imputed by the average of the interval", xlab = "Steps per Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

```r
mean(StepsPerDay_no_na)
```

```
## [1] 10766.19
```

```r
median(StepsPerDay_no_na)
```

```
## [1] 10766.19
```

From our results, we can see that imputing the missing values didn;t change our results dramatically. It seems that only the median changed slightly, from 10765 to 10766.

## Are there differences in activity patterns between weekdays and weekends?


```r
activity_data$weekday_indicator <- factor(ifelse(weekdays(activity_data$dateTime) %in% c("Saturday", "Sunday"), "weekend", "weekday"))

#Calculate average steps per interval for weekdays and weekends
StepsPerDay_weekday_indicator <- data.frame(tapply(X = activity_data$steps, INDEX = list(activity_data$interval, activity_data$weekday_indicator), FUN = mean))

#Create the two plots
StepsPerDay_weekday_indicator$intervals <- row.names(StepsPerDay_weekday_indicator)
library(reshape2)
```

```
## Warning: package 'reshape2' was built under R version 3.3.3
```

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.3.3
```

```r
dfmelt <- melt(StepsPerDay_weekday_indicator, id.vars = 3, value.name = "steps")
ggplot(dfmelt, aes(x=as.numeric(intervals), y = steps,  colour = variable)) + geom_line() + facet_grid(variable ~ .) + xlab("Intervals")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

There are some slight differences in activity patterns, since on weekdays we can see that the subject is a bit more active in the early morning hours, than at the same hours during weekends.
