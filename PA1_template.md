---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

To enable the readers to view the code used in each code chunck:


```r
knitr::opts_chunk$set(echo = TRUE)
```


The following packages will be used: 

```r
library(dplyr)
library(mice)
library(ggplot2)
```


**Set the directory to the folder "repdata_data_activity" and import the data** 



```r
activity <- read.csv("./repdata_data_activity/activity.csv", na.strings = "NA")
```

To convert the date column to class "Date":


```r
activity$date <- as.Date(activity$date, format = "%Y-%m-%d")
```


To plot a histogram of the total number of steps taken each day:

```r
steps_per_day <- activity %>%
        group_by(date) %>%
        summarise(steps_day = sum(steps, na.rm = TRUE))
        

hist(steps_per_day$steps_day, breaks = 61, xlab = "Number of steps per day", main = "", col = "lightblue")
```

[plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)


## What is mean total number of steps taken per day?

To calculate the mean and the median of steps taken per day:


```r
steps_mean <- mean(steps_per_day$steps_day, na.rm = TRUE)

steps_median <- median(steps_per_day$steps_day, na.rm = TRUE)

print(steps_mean)
```

```
## [1] 9354.23
```

```r
print(steps_median)
```

```
## [1] 10395
```


## What is the average daily activity pattern?


To plot a time series of the number of steps taken averaged by the 5-minutes interval accross all days:



```r
steps_time <- activity%>%
        group_by(interval) %>%
        summarise(mean = mean(steps, na.rm = TRUE)) 

plot(steps_time$interval, steps_time$mean, type = "l", xlab = "5-minutes Interval", ylab = "Number of steps", xlim = c(0, 2355))
```

[plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)


To calculate the 5-minute interval that, on average, contains the maximum number of steps:



```r
steps_time[which.max(steps_time$mean),]
```

```
## # A tibble: 1 x 2
##   interval  mean
##      <int> <dbl>
## 1      835  206.
```

## Imputing missing values


To design one strategy to impute missing data. First, have a look at how many points are missing: 



```r
pMiss <- function(x){sum(is.na(x))/length(x)*100}

missing <- apply(activity, 2, pMiss)

missing_sum <- sum(is.na(activity$steps))

print(missing)
```

```
##    steps     date interval 
## 13.11475  0.00000  0.00000
```

```r
print(missing_sum)
```

```
## [1] 2304
```


In total, 2304 data points are missing for the steps variable, which represent approximatedly 13%. 

To impute missing data, the approach *Multivariate Imputation by Chained Equation (MICE)* will be used. 
This approach can be carried out in R via the package MICE. 

For information on the method, the paper [mice: Multivariate Imputation by Chained Equations in R](https://www.jstatsoft.org/article/view/v045i03) and the tutorial by [R-blogguers](https://www.r-bloggers.com/2015/10/imputing-missing-data-with-r-mice-package/) are recommended. 


From the R Documentation, it is stated that "The mice package implements a method to deal with missing data. The package creates multiple imputations (replacement values) for multivariate missing data. The method is based on Fully Conditional Specification, where each incomplete variable is imputed by a separate model. The MICE algorithm can impute mixes of continuous, binary, unordered categorical and ordered categorical data".

It will be usesd 5 cycles and 30 iterations:



```r
imputed <- mice(activity, maxit = 30)
```


To check the distribution of the imputed data compared with the observed data: 



```r
densityplot(imputed)
```

[plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png)


To have a look at which range of steps the imputed data occured:



```r
stripplot(imputed, pch = 20, cex = 1.2)
```

[plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png)


To replace the missing values (NA) in the dataset by the imputed output of MICE:



```r
complete_activity <- complete(imputed, 1)
```


To create a histogram of the total number of steps taken each day after the imputation:



```r
steps_imputed <- complete_activity %>%
        group_by(date) %>%
        summarise(steps_day = sum(steps, na.rm = TRUE))
        

hist(steps_imputed$steps_day, breaks = 61, xlab = "Number of steps per day", main = "", col = "lightblue")
```

[plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-1.png)


To calculate the mean and the median of steps per day after the imputation: 



```r
imputed_mean <- mean(steps_imputed$steps_day, na.rm = TRUE)

imputed_median <- median(steps_imputed$steps_day, na.rm = TRUE)

print(imputed_mean)
```

```
## [1] 10615.46
```

```r
print(imputed_median)
```

```
## [1] 10439
```


## Are there differences in activity patterns between weekdays and weekends?



For the next analysis, the imputed data will be used. 

To create a panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends. For this, let's identify for which days of the week the data is available. 

**For the next steps, the codes are written in german as the language setting. Optional code is provided in case the language setting is english. This option is assigned with #.** 




```r
complete_activity$day <- weekdays(complete_activity$date)

complete_activity %>%
        group_by(day) %>%
        tally()
```

```
## # A tibble: 7 x 2
##   day            n
## * <chr>      <int>
## 1 Dienstag    2592
## 2 Donnerstag  2592
## 3 Freitag     2592
## 4 Mittwoch    2592
## 5 Montag      2592
## 6 Samstag     2304
## 7 Sonntag     2304
```

The data points are well distributed between weekdays and weekend.



```r
complete_activity <- complete_activity %>%
    mutate(week_day = if_else(day %in% c("Freitag", "Samstag", "Sonntag"), "Weekend", "Weekday"))

#complete_activity <- complete_activity %>%
#    mutate(week_day = if_else(day %in% c("Friday", "Saturday", "Sonday"), "Weekend", "Weekday"))
```


To create a panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends:


```r
week <- complete_activity %>%
        group_by(interval, week_day) %>%
        summarise(average_steps = mean(steps))

ggplot(week, aes(interval, average_steps)) + 
        geom_line() + 
        theme_bw() +
        facet_grid(.~ week_day) +
        labs(y = "Number of steps", x = "Interval")
```

[plot of chunk unnamed-chunk-18](figure/unnamed-chunk-18-1.png)


