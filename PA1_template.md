Reproducible Research: Peer Assessment 1
===========================================

This project is an analysis of two months of exercise data,
obtained during October and November of 2012. 

## Loading and Preprocessing Data

The data is contained in a file named "activity.csv". The following code makes sure that the "activity.csv" file 
is present in the current working directory. The file is
unzipped from "activity.zip". 


```r
if(!file.exists("activity.csv")) {
      unzip("./activity.zip")
      if(!file.exists("activity.csv")) {
           message("source data can't be downloaded")
        }
}
```

To load the file into R, use the read.csv command. 


```r
activityData <- read.csv("activity.csv", header = TRUE)
str(activityData)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

As the str(activityData) command shows, the structure of the 
activityData data frame is three columns: steps, date, interval. 

There are "NA" or missing values present in the data. 
The date column has been read in as factors. To make
processing easier, we will gather the different dates 
into a vector: 


```r
dateVector <- levels(activityData$date)
str(dateVector)
```

```
##  chr [1:61] "2012-10-01" "2012-10-02" "2012-10-03" ...
```

The "date" information has been read in as factors.
To make analysis possible we need to convert the dates
to R date format. 


```r
dateVector <- as.Date(dateVector, format = "%Y-%m-%d")
activityData$date <- as.Date(activityData$date, format = "%Y-%m-%d")
```


## Mean Steps Per Day (ignoring missing data)

The following code computes a vector containing the number of
steps per day. 


```r
total_day <- sapply(dateVector, function(i) {
             total_day <- numeric()
             x_sub <- subset(activityData, date == i)
             sum_sub <- sum(x_sub$steps, na.rm=TRUE)
             total_day <- c(total_day, sum_sub)
}
)
```

The following code plots a histogram of total day steps: 

```r
hist(total_day, breaks = 22
     , main = "Histogram of Total Steps Per Day"
     , col ="red"
     , xlab = "Total Steps Per Day")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 

And the mean and median are computed as follows: 


```r
mean_total_steps <- mean(total_day)
median_total_steps <- median(total_day)
```

* The mean total steps per day is: 9354.
* The median total steps per day is: 1.0395 &times; 10<sup>4</sup>.

## Average Daily Activity

The data contains a long stream of the number of steps taken at each five-minute interval, each day. To compute the average number of steps at each 5-minute interval, I'm putting the data into a matrix where each row is a five-minute interval, and each column is a date. Then the row averages are the five-minute averages. 


```r
stepMatrix <- matrix (nrow = 288, ncol = length(dateVector))
for (i in 1:length(dateVector)) {
        begin <- 288*(i-1) + 1
        end <- 288*i
        stepMatrix[,i] <- activityData$steps[begin:end]      
}
stepsIntervals <- 1:288
for (i in 1:288) {
        stepsIntervals[i] <- mean(stepMatrix[i,], na.rm = TRUE)
}
```

To plot the averages over the course of a day: 


```r
x_values <- activityData$interval[1:288]
plot(x_values, stepsIntervals
     , main = "Average Steps at Daily 5-Minute Intervals"
     , xlab = "Time Interval"
     , ylab = "Average Steps"
     , type = "l")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9.png) 

To determine the time at which the maximum average number of steps occurs: 


```r
maxSteps <- max(stepsIntervals)
testMax <- stepsIntervals == maxSteps
indexMax <- which(testMax)
timeMax <- activityData$interval[indexMax]
```

The time when the maximum occurs is 8:35. 

## Impute Missing Values

Some of the step values are missing, labeled as NA. 
We will replace missing values with the average values for the 5-minute interval, calculated in the previous section, and see what kind of difference that makes. 


```r
revisedStepMatrix <- stepMatrix
for (i in 1:61) {
      for (j in 1:288){
         if(is.na(revisedStepMatrix[j,i])) {
           revisedStepMatrix[j,i] <- stepsIntervals[j]
         } 
      }
}
```
We now have revised step values in the matrix revisedStepMatrix. 
We will create a revised data frame. 


```r
revisedSteps <- numeric()
adder <- numeric()
for (i in 1:61) {
              adder <- revisedStepMatrix[,i]
              revisedSteps <- c(revisedSteps, adder)           
        } 
revisedData <- activityData
revisedData$steps <- revisedSteps
```

The new dataset is called revisedSteps. 

We can look at the revised histogram:


```r
rev_total_day <- sapply(dateVector, function(i) {
             rev_total_day <- numeric()
             x_sub <- subset(revisedData, date == i)
             sum_sub <- sum(x_sub$steps, na.rm=TRUE)
             rev_total_day <- c(rev_total_day, sum_sub)
}
)
```

The following code plots a histogram of total day steps: 

```r
hist(rev_total_day, breaks = 22
     , main = "Histogram of (Revised) Total Steps Per Day"
     , col ="red"
     , xlab = "Total Steps Per Day")
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14.png) 

And the mean and median are computed as follows: 


```r
rev_mean_total_steps <- mean(rev_total_day)
rev_median_total_steps <- median(rev_total_day)
```

* The mean total steps per day is: 1.0766 &times; 10<sup>4</sup>.
* The median total steps per day is: 1.0766 &times; 10<sup>4</sup>.

Compared to previous:
* previous mean: 9354.2295
* previous median: 1.0395 &times; 10<sup>4</sup>

The differences are (revised) - unrevised: 
* mean: 1411.9592
* median: 371.1887


```r
meanNA <- format(as.character(round(mean_total_steps)), scientific = FALSE)
medianNA <- format(as.character(round(median_total_steps)), scientific = FALSE)
meanRev <- format(as.character(round(rev_mean_total_steps)), scientific = FALSE)
medianRev <- format(as.character(round(rev_median_total_steps)), scientific = FALSE)
deltaMean <- round(rev_mean_total_steps - mean_total_steps)
deltaMed <- round(rev_median_total_steps - median_total_steps)
```

The results are summarized here: 

Result | NAs Present | NAs Removed | Difference
-------|-------------|-------------|-------------
mean   |9354   |10766  |1412
median |10395 |10766|371

Filling in missing values appears to raise the mean and median. The effect is greater on the mean. 

## Activity Levels: Weekday Vs. Weekend

In order to compare the five-minute averages on weekdays vs. weekends, I will separate the data matrix stepMatrix into weekday and weekend, compute five-minute averages, and plot the averages. 

To separate stepMatrix into weekdaySteps and weekendSteps:


```r
weekdayBoolean <- c(TRUE, FALSE, FALSE, rep(TRUE, 4))
## The period Oct. 1/2010 to Nov. 30/2010 is 61 days. 
columnBoolean <- logical(length=61)
columnBoolean <- (!columnBoolean)*weekdayBoolean
```

```
## Warning: longer object length is not a multiple of shorter object length
```

```r
columnBoolean <- as.logical(columnBoolean)
## Use columnBoolean to separate stepMatrix:
weekdayMatrix <- stepMatrix[, columnBoolean]
weekendMatrix <- stepMatrix[,!columnBoolean]
```

Now compute the five-minute averages for weekday and weekend: 


```r
weekdayAvs <- numeric(length=288)
weekendAvs <- numeric(length=288)
weekdayAvs <- rowMeans(weekdayMatrix, na.rm = TRUE)
weekendAvs <- rowMeans(weekendMatrix, na.rm = TRUE)
```

Now make a dual plot of weekday averages and weekend averages for comparison: 


```r
## for the x-axis use x_values:
timeIntervals <- c(x_values, x_values)
minuteAvs <- c(weekdayAvs, weekendAvs)
timeLevels <- c(rep("Weekday", 288), rep("Weekend", 288))
plotFrame <- data.frame(timeIntervals, minuteAvs, timeLevels)
library(lattice)
xyplot(minuteAvs~timeIntervals|timeLevels, type = "l", layout=c(1,2))
```

![plot of chunk unnamed-chunk-19](figure/unnamed-chunk-19.png) 

The two plots show more spread-out activity during the weekend. 

