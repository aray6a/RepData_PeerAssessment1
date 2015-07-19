# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

Function created and called to unzip data from 'activity.zip' file and to read data in DataFrame. Necessay libraries are loaded.


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

```r
library(ggplot2)
library(scales)

readActivityData <- function() {
  unzip("activity.zip")
  activity <- read.csv("activity.csv")
  activity$date <- as.Date( as.character( activity$date) )
  activity
}
activity = readActivityData()
```



## What is mean total number of steps taken per day?

```r
makeBarGraph <- function(activity) {
  activity %>% group_by( date ) %>% summarize( totalSteps = sum(steps))
  byday <- activity %>% group_by( date ) %>% summarize( totalSteps = sum(steps))
  byday$date <- as.Date( as.character(byday$date) )

  g <- ggplot( byday, aes( x = date, y = totalSteps ) ) + geom_bar( stat = 'identity') + scale_x_date( labels = date_format("%m/%d"), breaks = date_breaks("1 week") )
  list(g, byday)
}
meanMed <- function( activity ) {
  list( mean( activity$steps, na.rm = T ), median( activity$steps, na.rm = T) )
}
makeBarGraph( activity )
```

```
## [[1]]
```

```
## Warning in loop_apply(n, do.ply): Removed 8 rows containing missing values
## (position_stack).
```

![](PA1_template_files/figure-html/unnamed-chunk-1-1.png) 

```
## 
## [[2]]
## Source: local data frame [61 x 2]
## 
##          date totalSteps
## 1  2012-10-01         NA
## 2  2012-10-02        126
## 3  2012-10-03      11352
## 4  2012-10-04      12116
## 5  2012-10-05      13294
## 6  2012-10-06      15420
## 7  2012-10-07      11015
## 8  2012-10-08         NA
## 9  2012-10-09      12811
## 10 2012-10-10       9900
## ..        ...        ...
```

```r
meanActivity <- meanMed( activity )
```
The mean number of steps is 37.3825996.  The median number of steps is 0.

## What is the average daily activity pattern?


```r
intervalTimeSeries <- function( activity ) {
  byInterval <- activity %>% group_by( interval ) %>% summarize( totalSteps = mean(steps, na.rm = T) )
  ggplot( byInterval, aes( x = interval, y = totalSteps ) ) + geom_point() + geom_line()
}
intervalTimeSeries( activity )
```

![](PA1_template_files/figure-html/Number4-1.png) 

```r
maxStepsByInterval <- function( activity ) {
  byInterval <- activity %>% group_by( interval ) %>% summarize( totalSteps = mean(steps, na.rm = T) )
  byInterval[ byInterval$totalSteps == max(byInterval$totalSteps),]
}
maxInterval <- maxStepsByInterval( activity )
```


The interval with the maximum number of steps is 206.1698113


## Imputing missing values

```r
numberOfNAs <- function( activity ) {
  activity$countindex = 1
  sum( activity[ is.na(activity$steps), ]$countindex )
}
```
Using the average steps to fill in missing values.

```r
fillMissingSteps <- function( activity ) {
  mm <- meanMed( activity )
  activity$steps <- ifelse( is.na(activity$steps), mm[[1]], activity$steps)
  activity
}
```

```r
  nas <- numberOfNAs( activity )
  makeBarGraph( fillMissingSteps( activity ) )
```

```
## [[1]]
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```
## 
## [[2]]
## Source: local data frame [61 x 2]
## 
##          date totalSteps
## 1  2012-10-01   10766.19
## 2  2012-10-02     126.00
## 3  2012-10-03   11352.00
## 4  2012-10-04   12116.00
## 5  2012-10-05   13294.00
## 6  2012-10-06   15420.00
## 7  2012-10-07   11015.00
## 8  2012-10-08   10766.19
## 9  2012-10-09   12811.00
## 10 2012-10-10    9900.00
## ..        ...        ...
```

```r
  meanActivity <- meanMed( fillMissingSteps( activity ) )
```
The number of NAs is 2304.  The mean number of steps is 37.3825996.  The median number of steps is 0.


## Are there differences in activity patterns between weekdays and weekends?

```r
  applyWeekDay <- function( activity ) {
    activity$weekType <- weekdays( activity$date )
    activity$weekType <- ifelse (activity$weekType %in% c('Saturday', 'Sunday'), 'weekend', 'weekday')
    activity$weekType <- factor( activity$weekType)
    activity
  }

  plotByWeekType <- function( activity ) {
    ggplot( activity, aes( x = interval, y = steps, group = weekType, color = weekType)) + geom_line() + facet_wrap( ~ weekType)
  }
  activityFilled <- fillMissingSteps( activity)
  activityWeekType <- applyWeekDay( activityFilled )
  head( activityWeekType )
```

```
##     steps       date interval weekType
## 1 37.3826 2012-10-01        0  weekday
## 2 37.3826 2012-10-01        5  weekday
## 3 37.3826 2012-10-01       10  weekday
## 4 37.3826 2012-10-01       15  weekday
## 5 37.3826 2012-10-01       20  weekday
## 6 37.3826 2012-10-01       25  weekday
```

```r
  plotByWeekType( activityWeekType )
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

Yes there are differences.  More steps are taken on the weekdays.
