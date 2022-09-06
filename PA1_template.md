---
title: "Peer-graded Assignment: Course Project 1"
subtitle: "Reproducible Research - Week 2"
author: "Alireza Haddadi"
output: 
  html_document:
    keep_md: true
---

### Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

The dataset used in this assignment consists of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

### Data

The data for this assignment can be downloaded from the course web site  
Dataset: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) [52K]  
The variables included in this dataset are:

* **steps:** Number of steps taking in a 5-minute interval (missing values are coded as NA)
 
* **date:** The date on which the measurement was taken in YYYY-MM-DD format  

* **interval:** Identifier for the 5-minute interval in which measurement was taken

## Assignment

This assignment consists on several questions as follows:

### 1.Loading and preprocessing the data

At first we need two `dplyr` & `ggplot2` libraries ,so we have:


```r
library(dplyr)
library(ggplot2)
```

**Load the data.** In the case the data is already available in the working directory, it is loaded directly. On the other hand, it is first downloaded and unzipped.


```r
file_url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
zip_file <- ".\\activity.zip"

if (!file.exists(zip_file)) {
        download.file(file_url, destfile = zip_file, mode = 'wb')
        date_download <- date() 
}
file_name <-".\\activity.csv"
if (!file.exists(file_name)) {
        unzip(zipfile = zip_file, exdir = getwd())
}
```

In order to have a more clean code we set these variables in a short way:


```r
ASPD <- "average_steps_per_date"
ASPI <- "average_steps_per_interval"
FMI <- "five_minute_interval"
ASPW <- "average_steps_per_weekday"
```

**Process/transform the data (if necessary) into a format suitable for your analysis.** Since the date column is a date, it is transform from character type to date type.


```r
Data <- read.csv(file_name, header = TRUE)
Data$date <- as.Date(Data$date, format = "%Y-%m-%d")
```

### 2.What is mean total number of steps taken per day?

**For this part of the assignment, you can ignore the missing values in the dataset.**


```r
ASPD <- Data %>% select(steps, date) %>% group_by(date) %>% summarise_all(funs(sum))
```

**Make a histogram of the total number of steps taken each day.**


```r
ggplot(ASPD, aes(steps)) + 
        geom_histogram(bins = 25, color = "darkblue",
                       fill = "lightblue", alpha = 0.4) + 
        labs(title = "Steps histogram", x = "Number of steps", y = "Count")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->


**Calculate and report the mean and median total number of steps taken per day.**

The mean total number of steps taken per day is:


```r
(meanData <- mean(ASPD$steps, na.rm = T))
```

```
## [1] 10766.19
```

The median total number of steps taken per day is:


```r
(medianData <- median(ASPD$steps, na.rm = T))
```

```
## [1] 10765
```

### 3.What is the average daily activity pattern?

**Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)**


```r
ASPI <- Data %>% select(steps, interval) %>% group_by(interval) %>%
        summarise_all(funs(mean), na.rm = T)
```

**Histogram of the total number of steps taken each day**


```r
ggplot(ASPI, aes(interval, steps)) + geom_line() +
        labs(title = "The average daily activity pattern",
             x = "The 5-minute interval", y = "Average number of steps") + theme_classic()
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

**Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?**


```r
FMI <- ASPI$interval[which.max(ASPI$steps)]
```

The 5-minute interval rsulted is: 835

### Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

**Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)**


```r
Nas <- sum(is.na(Data$steps))
```

The total number of missing values in the dataset is: 2304

**Create a new dataset that is equal to the original dataset but with the missing data filled in.**


```r
FillnaData <- Data
```

**The strategy selected for filling in all of the missing values in the dataset is the use of the mean for that 5-minute interval.**


```r
na_steps <- is.na(Data$steps)
FillnaData$steps[na_steps] <- ASPI$steps[na_steps]


ASPD2 <- FillnaData %>% select(steps, date, interval) %>% group_by(date) %>%
        summarise_all(funs(sum))
```

**Make a histogram of the total number of steps taken each day.**


```r
ggplot(ASPD2, aes(steps)) + 
        geom_histogram(bins = 25, color = "darkblue",
                       fill = "lightgreen", alpha = 0.4) + 
        labs(title = "Steps histogram", x = "Number of steps", y = "Count")
```

![](PA1_template_files/figure-html/unnamed-chunk-15-1.png)<!-- -->

**Calculate and report the mean and median total number of steps taken per day.**

The mean total number of steps taken per day is:


```r
(meanFillnaData <- mean(ASPD2$steps, na.rm = T))
```

```
## [1] 10766.19
```

The median total number of steps taken per day:


```r
(medianFillnaData <- median(ASPD2$steps, na.rm = T))
```

```
## [1] 10765.59
```

**Do these values differ from the estimates from the first part of the assignment?**

Both mean and median are almost the same values than before filling missing values. There is a slightly increase in the median after filling the NA values, becoming equal to the mean.

**What is the impact of imputing missing data on the estimates of the total daily number of steps?**

There are mire significant difference in the quartiles, with an increase in the 1st Quartile and an decrease in the 3rd Quartile . Let’s see the summary of both data sets:

The summary of the original data set is:


```r
summary(ASPD$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##      41    8841   10765   10766   13294   21194       8
```

The summary of the data set with missing values filled is:


```r
summary(ASPD2$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##      41    8860   10766   10766   13191   21194       7
```

**Are there differences in activity patterns between weekdays and weekends?**


For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

**Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.**


```r
weekdays_values = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")

date_type <- ifelse(weekdays(FillnaData$date) %in% weekdays_values,
        'weekday', 'weekend')

FillnaData$day_type <- factor(date_type)
```

Make a panel plot containing a time series plot (i.e. type = “l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was created using simulated data:


```r
ASPW <- aggregate(steps ~ interval + day_type, data = FillnaData
                  , FUN = mean, na.rm = T) 

ggplot(ASPW, aes(interval, steps, color = day_type)) +
        geom_line() +
        facet_grid(day_type ~ .) +
        labs(title = "Activity pattern by the week of the day", 
             x = "The 5-minute interval", y = "Average number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-21-1.png)<!-- -->
