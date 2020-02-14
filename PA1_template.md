---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
The data provided was inside the repository in a file called "activity.zip"

The variables in the dataset are the following:

* **steps**: Number of steps taking in a 5-minute interval (missing values are coded as NA)
* **date**: The date on which the measurement was taken in YYYY-MM-DD format
* **interval**: Identifier for the 5-minute interval in which measurement was taken

the dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

- Unzip the data file


```r
unzip(zipfile = "activity.zip", files = "activity.csv", overwrite = TRUE)
```
- Load and store the datafile in a dataframe object called "activitydata"


```r
activitydata <-read.table(file = "activity.csv", header = TRUE, sep = ",", na.strings = "NA")

str(activitydata)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```
It is important to notice that the *date* variable is stored as a *factor*, rather than a *date*. 
Let's change it to a more easy to handle format

```r
activitydata$date <- as.Date(x = activitydata$date, format = "%Y-%m-%d")
str(activitydata)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```
In order to check the consistency, let's compute the number of *NA's* in the *steps* variable

```r
sum(is.na(activitydata$steps))
```

```
## [1] 2304
```
And the fraction of *NA's* is

```r
sum(is.na(activitydata$steps))/nrow(activitydata)
```

```
## [1] 0.1311475
```

## What is mean total number of steps taken per day?
Use the library *dplyr* for data manipulation


```r
library(dplyr)
```
And then, the variable *totalsteps* is defined as follows, removing the *NA's* in the dataset using **na.rm=TRUE**  

```r
totalsteps <- activitydata %>% group_by(date) %>%  summarize(total = sum(steps, na.rm = TRUE))
head(totalsteps)
```

```
## # A tibble: 6 x 2
##   date       total
##   <date>     <int>
## 1 2012-10-01     0
## 2 2012-10-02   126
## 3 2012-10-03 11352
## 4 2012-10-04 12116
## 5 2012-10-05 13294
## 6 2012-10-06 15420
```
By using the *ggplot2* package, it is easier to create a plot using the grammar of graphics

```r
library(ggplot2)
```
And the Histogram for Total Steps per day is the following

```r
totalsteps %>% ggplot(aes(x = total)) + 
  geom_histogram(color = "white" ,fill = "orange",bins = 25) + 
  geom_vline(xintercept = mean(totalsteps$total, na.rm=TRUE), color = "blue", lwd = 1, lty = 2) +
  geom_vline(xintercept = median(totalsteps$total, na.rm = TRUE), color = "purple", lwd = 1, lty=3) +
  ggtitle("Histogram of total steps per day") +
  ylab("Count") + 
  xlab("Total steps per day") +
  theme_bw()
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

Where the dashed line is the mean and the dotted line the median, with values:

```r
mean(totalsteps$total, na.rm = TRUE)
```

```
## [1] 9354.23
```

```r
median(totalsteps$total, na.rm = TRUE)
```

```
## [1] 10395
```
## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
