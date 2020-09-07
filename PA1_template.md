---
title: "PA1_template"
output: 
  html_document: 
    keep_md: yes
---

#Programming Assignment 1 Reproducible Research Alex Nagar

##Loading and Processing the Data

```r
#Read data
data <- read.csv("activity.csv")
#Set date as date
data$date <- as.Date(data$date, format = "%Y-%m-%d")
```
##Part 1 Mean Number of Daily Steps

```r
#Total number of steps per day
Stepsperday <- tapply(data$steps, data$date, sum, na.rm = TRUE)
#Histogram
hist(Stepsperday, xlab = "Number of steps per day",
     main = "Histogram of Daily Steps", col = "purple")
```

![](PA1_template_files/figure-html/NumberofStepsperDay-1.png)<!-- -->

```r
#Mean
mean(Stepsperday)
```

```
## [1] 9354.23
```

```r
#Median
median(Stepsperday)
```

```
## [1] 10395
```
##Part 2 Averages Daily Activity Pattern

```r
Intervalmean <- tapply(data$steps, data$interval, mean, na.rm =TRUE)
#Time Series Plot
plot(Intervalmean, type = "l", xlab = "Interval", ylab = "Mean Number of Steps")
```

![](PA1_template_files/figure-html/AverageDailyActivityPattern-1.png)<!-- -->

```r
#Maximum
Intervalmean[which.max(Intervalmean)]
```

```
##      835 
## 206.1698
```
##Part 3 Missing Values

```r
#Number of NA
sum(is.na(data))
```

```
## [1] 2304
```

```r
#Filling in Missing Values
datam <- data
for(i in 1:nrow(datam)){if(is.na(datam$steps[i]) == TRUE ){
  datam$steps[i] <- mean(datam$steps[datam$interval == datam$interval[i]],
                         na.rm = TRUE)}}
#Total Number of Steps Filled in
Stepsperdayfilled <- tapply(datam$steps, datam$date, sum, na.rm = TRUE)
#Histogram
hist(Stepsperdayfilled, xlab = "Number of steps per day",
     main = "Histogram of Daily Steps", col = "red")
```

![](PA1_template_files/figure-html/MissingValues-1.png)<!-- -->

```r
#Mean
mean(Stepsperdayfilled)
```

```
## [1] 10766.19
```

```r
#Median
median(Stepsperdayfilled)
```

```
## [1] 10766.19
```

```r
#Comparison
meanrow <- cbind(mean(Stepsperday),mean(Stepsperdayfilled))
medianrow <- cbind(median(Stepsperday), median(Stepsperdayfilled))
Comptab <- rbind(meanrow, medianrow)
colnames(Comptab) <- c("NA Included", "NA Filled")
rownames(Comptab) <- c("Mean", "Median")
Comptab
```

```
##        NA Included NA Filled
## Mean       9354.23  10766.19
## Median    10395.00  10766.19
```
##Part 4 Weekends and Weekdays

```r
#Creating factor variable for weekdays/weekend
weeknames <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
datam$weekday <- ifelse(weekdays(datam$date) %in% weeknames, 1,0 )
datam$weekdayf <- factor(datam$weekday, labels = c("Weekend", "Weekday"))
#Plotting weekday and weekend averages
datamweekday <- subset(datam, datam$weekdayf == "Weekday")
datamweekend <- subset(datam,datam$weekdayf == "Weekend")
Intervalmeanweekday <- tapply(datamweekday$steps, datamweekday$interval,
                              mean, na.rm =TRUE)
Intervalmeanweekend <- tapply(datamweekend$steps, datamweekend$interval,
                              mean, na.rm =TRUE)
par(mfrow = c(2,1))
plot(Intervalmeanweekday, type = "l", ylab = "Mean Number of Steps", main = "Weekday")
plot(Intervalmeanweekend, type = "l", ylab = "Mean Number of Steps", xlab = "Interval"
     ,main = "WeekEnd")
```

![](PA1_template_files/figure-html/WeekendandWeekdays-1.png)<!-- -->
