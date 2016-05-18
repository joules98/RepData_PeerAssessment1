This is an explanation of the document.

Loading the Data and Libraries
------------------------------

First, load the data into your working directory and have a quick look at the structure.

``` r
actDat <- read.csv("activity.csv", header=TRUE)
head(actDat)
```

    ##   steps       date interval
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20
    ## 6    NA 2012-10-01       25

``` r
str(actDat)
```

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

To prepare for the next steps, please ensure you have installed the "dplyr", "ggplot2", and "lattice" packages if you have not already done so. The next steps are to simply load theses libraries.

``` r
library(dplyr)
```

    ## Warning: package 'dplyr' was built under R version 3.1.2

    ## 
    ## Attaching package: 'dplyr'

    ## The following object is masked from 'package:stats':
    ## 
    ##     filter

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(ggplot2)
```

    ## Warning: package 'ggplot2' was built under R version 3.1.3

``` r
library(lattice)
```

    ## Warning: package 'lattice' was built under R version 3.1.3

The Mean Total Steps Taken per Day
----------------------------------

To answer the question "What is mean total number of steps taken per day?", while ignoring the missing values, we can use the following method:

``` r
by_day <- group_by(actDat, date)
StepsPerDay <- summarise(by_day,
                         count = n(),
                         TotSteps = as.numeric(sum(steps, na.rm = TRUE)),
                         DayMean = mean(steps, na.rm = TRUE),
                         DayMedian = median(steps, na.rm = TRUE))
StepsPerDay$Day<-factor(weekdays(as.Date(StepsPerDay$date)), 
                        levels = c("Monday", "Tuesday", "Wednesday", 
                                   "Thursday", "Friday", "Saturday", "Sunday"))
```

A list of the steps per day is here:

``` r
StepsPerDay
```

    ## Source: local data frame [61 x 6]
    ## 
    ##          date count TotSteps  DayMean DayMedian       Day
    ## 1  2012-10-01   288        0      NaN        NA    Monday
    ## 2  2012-10-02   288      126  0.43750         0   Tuesday
    ## 3  2012-10-03   288    11352 39.41667         0 Wednesday
    ## 4  2012-10-04   288    12116 42.06944         0  Thursday
    ## 5  2012-10-05   288    13294 46.15972         0    Friday
    ## 6  2012-10-06   288    15420 53.54167         0  Saturday
    ## 7  2012-10-07   288    11015 38.24653         0    Sunday
    ## 8  2012-10-08   288        0      NaN        NA    Monday
    ## 9  2012-10-09   288    12811 44.48264         0   Tuesday
    ## 10 2012-10-10   288     9900 34.37500         0 Wednesday
    ## ..        ...   ...      ...      ...       ...       ...

``` r
str(StepsPerDay)
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    61 obs. of  6 variables:
    ##  $ date     : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 2 3 4 5 6 7 8 9 10 ...
    ##  $ count    : int  288 288 288 288 288 288 288 288 288 288 ...
    ##  $ TotSteps : num  0 126 11352 12116 13294 ...
    ##  $ DayMean  : num  NaN 0.438 39.417 42.069 46.16 ...
    ##  $ DayMedian: num  NA 0 0 0 0 0 0 NA 0 0 ...
    ##  $ Day      : Factor w/ 7 levels "Monday","Tuesday",..: 1 2 3 4 5 6 7 1 2 3 ...
    ##  - attr(*, "drop")= logi TRUE

A historgram of the steps per day is here:

``` r
qplot(Day, 
      data = StepsPerDay, 
      geom = "bar", 
      weight = TotSteps,
      ylab = "TotalSteps",
      fill = Day)
```

![](ReproResearchAssign1_files/figure-markdown_github/unnamed-chunk-4-1.png)<!-- -->

Then, we calculate the mean and median and display the result here:

``` r
newMean<-tapply(StepsPerDay$TotSteps,StepsPerDay$Day, mean)
newMedian<-tapply(StepsPerDay$TotSteps,StepsPerDay$Day, median)
newMM <- data.frame(rbind(dayMean = round(newMean), dayMedian = round(newMedian)))
newMM
```

    ##           Monday Tuesday Wednesday Thursday Friday Saturday Sunday
    ## dayMean     7758    8950     10481     7300   9613    10968  10743
    ## dayMedian  10139    8918     11352     7047  10600    11498  11646

The calculation for steps per interval follows here:

``` r
by_interval <- group_by(actDat, interval)
StepsPerInterval <- summarise(by_interval,
                         AvgSteps = as.numeric(mean(steps, trim= 0, na.rm = TRUE)))

StepsPerInterval
```

    ## Source: local data frame [288 x 2]
    ## 
    ##    interval  AvgSteps
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

``` r
str(StepsPerInterval)
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    288 obs. of  2 variables:
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
    ##  $ AvgSteps: num  1.717 0.3396 0.1321 0.1509 0.0755 ...
    ##  - attr(*, "drop")= logi TRUE

``` r
by_interval
```

    ## Source: local data frame [17,568 x 3]
    ## Groups: interval
    ## 
    ##    steps       date interval
    ## 1     NA 2012-10-01        0
    ## 2     NA 2012-10-01        5
    ## 3     NA 2012-10-01       10
    ## 4     NA 2012-10-01       15
    ## 5     NA 2012-10-01       20
    ## 6     NA 2012-10-01       25
    ## 7     NA 2012-10-01       30
    ## 8     NA 2012-10-01       35
    ## 9     NA 2012-10-01       40
    ## 10    NA 2012-10-01       45
    ## ..   ...        ...      ...

The Average Daily Activity Pattern
----------------------------------

The plot for the time series of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis) is here:

``` r
plot(StepsPerInterval$interval, StepsPerInterval$AvgSteps, 
       type = "l")
```

![](ReproResearchAssign1_files/figure-markdown_github/unnamed-chunk-7-1.png)<!-- -->

The 5-minute interval, on average across all the days in the dataset, that contains the maximum number of steps is here:

``` r
IntervalWithMaxAvgSteps <- StepsPerInterval[which.max(StepsPerInterval$AvgSteps),]
IntervalWithMaxAvgSteps
```

    ## Source: local data frame [1 x 2]
    ## 
    ##   interval AvgSteps
    ## 1      835 206.1698

The Impact of Imputing Missing Values
-------------------------------------

The presence of missing data may introduce bias into some calculations or summaries of the data. Because there are a number of days/intervals where there are missing values (coded as NA), we will see if filling in missing values will change our result.

The total number of missing values in the dataset is:

``` r
sum(!complete.cases(actDat$steps))
```

    ## [1] 2304

Creating a new dataset by filling in the missing values with the mean for that 5-minute interval will allow us to see what impact the missing values have.

``` r
actDatImputed <- actDat
for (i in which(!complete.cases(actDatImputed$steps))) {
        findInterval <- 
                actDatImputed[i, "interval"]
        actDatImputed[i, "steps"] <- 
                StepsPerInterval[StepsPerInterval$interval %in% findInterval, "AvgSteps"]
}
```

A check to ensure there are no missing values in the new imputed dataset is here:

``` r
sum(!complete.cases(actDatImputed$steps))
```

    ## [1] 0

The Mean Total Steps per Day with the New Imputed Dataset
---------------------------------------------------------

To answer the question "What is mean total number of steps taken per day using the new imputed dataset?", we can use the following method:

``` r
by_dayImp <- group_by(actDatImputed, date)
StepsPerDayImp <- summarise(by_dayImp,
                         countImp = n(),
                         TotStepsImp = as.numeric(sum(steps, na.rm = TRUE)),
                         DayMeanImp = mean(steps, na.rm = TRUE),
                         DayMedianImp = median(steps, na.rm = TRUE))
StepsPerDayImp$Day<-factor(weekdays(as.Date(StepsPerDayImp$date)), 
                        levels = c("Monday", "Tuesday", "Wednesday", 
                                   "Thursday", "Friday", "Saturday", "Sunday"))
```

A list of the steps per day with the imputed dataset is here:

``` r
StepsPerDayImp
```

    ## Source: local data frame [61 x 6]
    ## 
    ##          date countImp TotStepsImp DayMeanImp DayMedianImp       Day
    ## 1  2012-10-01      288    10766.19   37.38260     34.11321    Monday
    ## 2  2012-10-02      288      126.00    0.43750      0.00000   Tuesday
    ## 3  2012-10-03      288    11352.00   39.41667      0.00000 Wednesday
    ## 4  2012-10-04      288    12116.00   42.06944      0.00000  Thursday
    ## 5  2012-10-05      288    13294.00   46.15972      0.00000    Friday
    ## 6  2012-10-06      288    15420.00   53.54167      0.00000  Saturday
    ## 7  2012-10-07      288    11015.00   38.24653      0.00000    Sunday
    ## 8  2012-10-08      288    10766.19   37.38260     34.11321    Monday
    ## 9  2012-10-09      288    12811.00   44.48264      0.00000   Tuesday
    ## 10 2012-10-10      288     9900.00   34.37500      0.00000 Wednesday
    ## ..        ...      ...         ...        ...          ...       ...

``` r
str(StepsPerDayImp)
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    61 obs. of  6 variables:
    ##  $ date        : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 2 3 4 5 6 7 8 9 10 ...
    ##  $ countImp    : int  288 288 288 288 288 288 288 288 288 288 ...
    ##  $ TotStepsImp : num  10766 126 11352 12116 13294 ...
    ##  $ DayMeanImp  : num  37.383 0.438 39.417 42.069 46.16 ...
    ##  $ DayMedianImp: num  34.1 0 0 0 0 ...
    ##  $ Day         : Factor w/ 7 levels "Monday","Tuesday",..: 1 2 3 4 5 6 7 1 2 3 ...
    ##  - attr(*, "drop")= logi TRUE

A historgram of the steps per day with the imputed dataset is here:

``` r
qplot(Day, 
      data = StepsPerDayImp, 
      geom = "bar", 
      weight = TotStepsImp,
      ylab = "TotalSteps",
      fill = Day)
```

![](ReproResearchAssign1_files/figure-markdown_github/unnamed-chunk-14-1.png)<!-- -->

Then, we calculate the mean and median for the imputed dataset and display the result here:

``` r
newMeanImp<-tapply(StepsPerDayImp$TotSteps,StepsPerDayImp$Day, mean)
newMedianImp<-tapply(StepsPerDayImp$TotSteps,StepsPerDayImp$Day, median)
newMMImp <- data.frame(rbind(dayMeanImp = round(newMeanImp), dayMedianImp = round(newMedianImp)))
newMMImp
```

    ##              Monday Tuesday Wednesday Thursday Friday Saturday Sunday
    ## dayMeanImp    10151    8950     11677     8496  12006    12314  12089
    ## dayMedianImp  10765    8918     11352    10056  10766    11596  11646

When we compare the daily mean for imputed data (above) to the daily mean from the original dataset, we see:

``` r
newMM
```

    ##           Monday Tuesday Wednesday Thursday Friday Saturday Sunday
    ## dayMean     7758    8950     10481     7300   9613    10968  10743
    ## dayMedian  10139    8918     11352     7047  10600    11498  11646

The calculation for steps per interval for the imputed data follows here:

``` r
by_intervalImp <- group_by(actDatImputed, interval)
StepsPerIntervalImp <- summarise(by_intervalImp,
                         AvgSteps = as.numeric(mean(steps, trim= 0, na.rm = TRUE)))

StepsPerIntervalImp
```

    ## Source: local data frame [288 x 2]
    ## 
    ##    interval  AvgSteps
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

``` r
str(StepsPerIntervalImp)
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    288 obs. of  2 variables:
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
    ##  $ AvgSteps: num  1.717 0.3396 0.1321 0.1509 0.0755 ...
    ##  - attr(*, "drop")= logi TRUE

``` r
by_intervalImp
```

    ## Source: local data frame [17,568 x 3]
    ## Groups: interval
    ## 
    ##        steps       date interval
    ## 1  1.7169811 2012-10-01        0
    ## 2  0.3396226 2012-10-01        5
    ## 3  0.1320755 2012-10-01       10
    ## 4  0.1509434 2012-10-01       15
    ## 5  0.0754717 2012-10-01       20
    ## 6  2.0943396 2012-10-01       25
    ## 7  0.5283019 2012-10-01       30
    ## 8  0.8679245 2012-10-01       35
    ## 9  0.0000000 2012-10-01       40
    ## 10 1.4716981 2012-10-01       45
    ## ..       ...        ...      ...

The Average Daily Activity Pattern with Imputed Data
----------------------------------------------------

The plot for the time series of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis) for the imputed dataset is here:

``` r
plot(StepsPerIntervalImp$interval, StepsPerIntervalImp$AvgSteps, 
       type = "l")
```

![](ReproResearchAssign1_files/figure-markdown_github/unnamed-chunk-18-1.png)<!-- -->

The 5-minute interval, on average across all the days in the dataset, that contains the maximum number of steps for the imputed dataset is here:

``` r
IntervalWithMaxAvgStepsImp <- StepsPerIntervalImp[which.max(StepsPerIntervalImp$AvgSteps),]
IntervalWithMaxAvgStepsImp
```

    ## Source: local data frame [1 x 2]
    ## 
    ##   interval AvgSteps
    ## 1      835 206.1698

Differences in Activity Patterns between Weekdays and Weekends
--------------------------------------------------------------

We can compare the activity patterns between weekdays and weekends by creating a new factor variable in the dataset with two levels - "weekday" and "weekend", indicating whether a given date is a weekday or weekend day.

``` r
StepsPerDayImp$wkDay<-factor(weekdays(as.Date(StepsPerDayImp$date)), 
                             levels = c("Monday", "Tuesday", "Wednesday", 
                                        "Thursday", "Friday"))
StepsPerDayImp$wkendDay<-factor(weekdays(as.Date(StepsPerDayImp$date)), 
                                levels = c("Saturday", "Sunday"))
StepsPerDayImp$DayType<-factor(weekdays(as.Date(StepsPerDayImp$date)),
                               levels = c("weekday", "weekend"))
StepsPerDayType <- StepsPerDayImp
for (i in which(complete.cases(StepsPerDayType$wkDay))) {
        StepsPerDayType[i, "DayType"] <- c("weekday")       
}

for (i in which(complete.cases(StepsPerDayType$wkendDay))) {
        StepsPerDayType[i, "DayType"] <- c("weekend")       
}
```

A panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken

``` r
xyplot(StepsPerIntervalImp$AvgSteps~StepsPerIntervalImp$interval|StepsPerDayType$DayType, 
       type = "l",
       main="Average Steps per Time Interval Comparison", 
       ylab="Average Steps", 
       xlab="5-Minute Interval", 
       layout=(c(1,2))
)
```

![](ReproResearchAssign1_files/figure-markdown_github/unnamed-chunk-21-1.png)<!-- -->
