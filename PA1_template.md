# REPRODUCIBLE RESEARCH: Peer Assessment 1

=================================================================================

## Loading and preprocessing the data

### 1. Load the data (i.e. read.csv())




```r
library(ggplot2)

GetData <- function(){

# Create a working directory 
	if(!file.exists("./My WorkingDir")){dir.create("./My WorkingDir")}
	# dir.create("./My WorkingDir")
	fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"

# Download the files  in the working directory
	download.file(fileUrl,destfile="./My WorkingDir/Dataset.zip")
	unzip(zipfile="./My WorkingDir/Dataset.zip",exdir=getwd())
	FilePath <- getwd()


# read "activity.csv" file
	MyData <- read.csv("activity.csv")

head(MyData, 5)
names(MyData)
	
}
GetData()
```

### 2.Process/transform the data (if necessary) into a format suitable for your analysis

```r
library(dplyr)
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
knit2html("activity.csv")
```

```
## 
## 
## processing file: activity.csv
```

```
## output file: activity.txt
```

```r
MyData <- read.csv("activity.csv")
MyData$date <- as.Date(MyData$date, format = "%Y-%m-%d")	
```

## What is mean total number of steps taken per day?

###1. Calculate the total number of steps taken per day

```r
	TotalStepsDaily <- tapply(MyData$steps, MyData$date, FUN=sum, na.rm=TRUE)
```
### 2. Make a histogram of the total number of steps taken per day


```r
	library(ggplot2)
	qplot(TotalStepsDaily, binwidth=1000, xlab="Total Number of Steps Daily", ylab="Frequency")
```

![](PA1_template_files/figure-html/My1stHisto-1.png)<!-- -->

### 3. Calculate and report the mean and median total number of steps taken per day

```r
	StepsMeanDaily <- mean(TotalStepsDaily)
	StepsMedianDaily <- median(TotalStepsDaily)
```
The mean total number of steps daily is 9354.2295082 and the median of total number of steps daily is 10395

## What is the average daily activity pattern?

```r
	StepsByInterval <- aggregate(steps ~ interval, MyData, mean)
```

###1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
	StepsByInterval <- aggregate(steps ~ interval, MyData, mean)
# Create a time series plot(i.e. type="l")
	plot(StepsByInterval$interval, StepsByInterval$steps, type='l', main="Average number of steps taken all days", xlab="5mn-Interval", ylab="Average number of Steps")
```

![](PA1_template_files/figure-html/MyTimeSeries-1.png)<!-- -->

###2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
# Find the 5-minute interval that contains the maximum number of steps
MyInterval <- which.max(StepsByInterval$steps)
time <- StepsByInterval[MyInterval,]$interval
MyTime <- sprintf("%04d", time)
MyTime <- format(strptime(MyTime, format="%H%M"), format="%H:%M")            
```
### The Interval 08:35 has the maximum average value of steps


## Imputing missing values

### 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
# Total number of row's with NA's
sum(is.na(MyData))
```

```
## [1] 2304
```

###2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


###3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
	MyNewData <- MyData

for (i in 1:nrow(MyNewData)){
	if (is.na(MyNewData$steps[i])) {
		interval_value <- MyNewData$interval[i]
		steps_value <- StepsByInterval[StepsByInterval$interval == interval_value,]
		MyNewData$steps[i] <- steps_value$steps
	}
}

	MyFinalData <- aggregate(steps ~ date, MyNewData, sum)
	sum(is.na(MyNewData))
```

```
## [1] 0
```
###4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.

```r
	TotalStepsDaily2 <- tapply(MyNewData$steps, MyNewData$date, sum)
	qplot(TotalStepsDaily2, binwidth=1000, xlab="Total Number of Steps Daily", ylab="Frequency")
```

![](PA1_template_files/figure-html/MyHisto2-1.png)<!-- -->

```r
StepsMeanDaily2 <- mean(TotalStepsDaily2)
StepsMedianDaily2 <- median(TotalStepsDaily2)
```
#### Mean : 1.0766189\times 10^{4}  Median : 1.0766189\times 10^{4} 


###Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

####There is a significant difference between mean and median values computed at the first assignment. the reason is that NA values are considered as 0 values during the first part while in the second part NA values are reaplaced by averaged values.

###Are there differences in activity patterns between weekdays and weekends?

####1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
MyNewData['MyDay'] <- weekdays(MyNewData$date)
MyNewData$MyDay[MyNewData$MyDay %in% c('samedi', 'dimanche')] <- "weekend"
MyNewData$MyDay[MyNewData$MyDay != "weekend"] <- "weekday"
head(MyNewData)
```

```
##       steps       date interval   MyDay
## 1 1.7169811 2012-10-01        0 weekday
## 2 0.3396226 2012-10-01        5 weekday
## 3 0.1320755 2012-10-01       10 weekday
## 4 0.1509434 2012-10-01       15 weekday
## 5 0.0754717 2012-10-01       20 weekday
## 6 2.0943396 2012-10-01       25 weekday
```


```r
MyNewData$MyDay <- as.factor(MyNewData$MyDay)
MyNewData_we <- subset(MyNewData, MyDay == "weekend")
MyNewData_wd <- subset(MyNewData, MyDay == "weekday")

DailyStepWe <- tapply(MyNewData_we$steps, MyNewData_we$interval, mean)
DailyStepWd <- tapply(MyNewData_wd$steps, MyNewData_wd$interval, mean)

par(mfrow=c(2,1))
MyFinalData <- aggregate(steps ~ interval + MyDay, MyNewData, mean)
ggplot(MyFinalData, aes(interval, steps)) + geom_line()+facet_grid(MyDay ~ .) +
xlab(" 5mn Interval") + ylab("Number of steps")
```

![](PA1_template_files/figure-html/MyNewFactor2-1.png)<!-- -->

