---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---



## Loading and preprocessing the data


```r
activityData <- read.csv("activity.csv", stringsAsFactors = FALSE)
```


## What is mean total number of steps taken per day?


```r
totalStepsByDay <- aggregate(steps ~ date, data = activityData, sum, na.rm = TRUE)
```


```r
hist(totalStepsByDay$steps, main = "Total Steps Each Day", xlab = "Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->


```r
meanSteps <- mean(totalStepsByDay$steps)
medianSteps <- median(totalStepsByDay$steps)
```

## What is the average daily activity pattern?


```r
averageStepsByInterval <- aggregate(steps ~ interval, data = activityData, mean, na.rm = TRUE)
plot(averageStepsByInterval$interval, averageStepsByInterval$steps, type = "l", xlab = "Interval", ylab = "Average Number of Steps", main = "Average Daily Activity Pattern")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->



```r
maxStepsInterval <- averageStepsByInterval[which.max(averageStepsByInterval$steps), ]
```

## Imputing missing values


```r
missingValues <- sum(is.na(activityData$steps))
```



```r
imputedData <- activityData
for(i in 1:nrow(imputedData)) {
  if(is.na(imputedData$steps[i])) {
    imputedData$steps[i] <- averageStepsByInterval$steps[which(averageStepsByInterval$interval == imputedData$interval[i])]
  }
}
```



```r
totalStepsByDayImputed <- aggregate(steps ~ date, data = imputedData, sum)
hist(totalStepsByDayImputed$steps, main = "Total Steps Each Day (Imputed)", xlab = "Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->


```r
meanStepsImputed <- mean(totalStepsByDayImputed$steps)
medianStepsImputed <- median(totalStepsByDayImputed$steps)
```

## Are there differences in activity patterns between weekdays and weekends?


```r
imputedData$date <- as.Date(imputedData$date)
imputedData$dayType <- ifelse(weekdays(imputedData$date) %in% c("Saturday", "Sunday"), "Weekend", "Weekday")
```


```r
library(lattice)
averageStepsByDayType <- aggregate(steps ~ interval + dayType, data = imputedData, mean)
xyplot(steps ~ interval | dayType, data = averageStepsByDayType, layout = c(1, 2), type = "l", xlab = "Interval", ylab = "Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->
