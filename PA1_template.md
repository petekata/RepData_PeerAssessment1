Personal activity monitoring
========================================================

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a
[Fitbit](http://www.fitbit.com), [Nike Fuelband](http://www.nike.com/us/en_us/c/nikeplus-fuelband), or [Jawbone Up]
(https://jawbone.com/up). These type of devices are part of the 'quantified self' movement  a group of enthusiasts who
take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they
are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of
statistical methods and software for processing and interpreting the data.

Dataset Loaded from: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) 

## Loading and preprocessing the data


```r
col_classes=c("numeric","character","numeric");
act_data <- read.csv('activity.csv',header=TRUE,colClasses=col_classes);
```

```
## Warning: cannot open file 'activity.csv': No such file or directory
```

```
## Error: cannot open the connection
```

```r
act_by_day <- aggregate(x=act_data,by=as.list(act_data$date),FUN=sum);
```

```
## Error: object 'act_data' not found
```

```r
library(sqldf);
```

```
## Loading required package: gsubfn
## Loading required package: proto
## Loading required package: RSQLite
## Loading required package: DBI
## Loading required package: RSQLite.extfuns
```

```r
act_by_Day_sql <- sqldf('select date,sum(steps) Total_Steps from act_data group by date');
```

```
## Loading required package: tcltk
```

```
## Error: RS-DBI driver: (error in statement: no such table: act_data)
```

## What is mean total number of steps taken per day?


```r
hist(as.numeric(act_by_Day_sql$Total_Steps),xlab="Total Steps",
                main="Total number of steps taken each day")
```

```
## Error: object 'act_by_Day_sql' not found
```


Mean total number of steps taken per day:

```r
mean(as.numeric(act_by_Day_sql$Total_Steps), na.rm = TRUE);
```

```
## Error: object 'act_by_Day_sql' not found
```

```r
median(as.numeric(act_by_Day_sql$Total_Steps), na.rm = TRUE);
```

```
## Error: object 'act_by_Day_sql' not found
```

## What is the average daily activity pattern?

A time series plotof the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
act_by_5min <- sqldf('select interval,avg(steps) avg_steps from act_data group by interval');
```

```
## Error: RS-DBI driver: (error in statement: no such table: act_data)
```

```r
library(ggplot2)
```


```r
ggplot(act_by_5min,aes(interval,avg_steps)) + geom_line() +xlab("5 Min Intervals") + ylab("Average Steps for all days");
```

```
## Error: object 'act_by_5min' not found
```

maximum number of steps during 5-minute interval, on average across all the days in the dataset


```r
act_by_5min$interval[act_by_5min$avg_steps == max(act_by_5min$avg_steps)]
```

```
## Error: object 'act_by_5min' not found
```


Number of missing values


```r
sum(is.na(act_data$steps));
```

```
## Error: object 'act_data' not found
```

## Imputing missing values


```r
act_data_clean <- na.omit(act_data);
```

```
## Error: object 'act_data' not found
```

```r
act_day_avg <- sqldf("select date,avg(steps) steps from act_data_clean group by date ");
```

```
## Error: RS-DBI driver: (error in statement: no such table: act_data_clean)
```

```r
act_data$day <- weekdays(as.Date(act_data$date));
```

```
## Error: object 'act_data' not found
```

```r
act_data$day_type <- "Weekday";
```

```
## Error: object 'act_data' not found
```

```r
for(i in 1:nrow(act_data)){
    if (is.na(act_data[i,1])){
        if (is.na(act_day_avg$date[act_data[i,2]])){
          act_data[i,1] <- 0;
        }else{
          act_data[i,1] <- as.numeric(act_day_avg$avg_steps[act_day_avg$date == act_data[i,2] ]);
        }
    }
    if (act_data[i,4] == "Sunday" || act_data[i,4] == "Saturday"){
        act_data[i,5] <- "Weekend";
    }    
    
}
```

```
## Error: object 'act_data' not found
```

```r
act_by_Day_sql <- sqldf('select date,sum(steps) Total_Steps from act_data group by date');
```

```
## Error: RS-DBI driver: (error in statement: no such table: act_data)
```

Histogram of the total number of steps taken each day - with CLEAN data


```r
hist(as.numeric(act_by_Day_sql$Total_Steps),xlab="Total Steps",
     main="Total number of steps taken each day - CLEAN data")
```

```
## Error: object 'act_by_Day_sql' not found
```


Mean total number of steps taken per day - with CLEAN data

```r
mean(as.numeric(act_by_Day_sql$Total_Steps));
```

```
## Error: object 'act_by_Day_sql' not found
```

```r
median(as.numeric(act_by_Day_sql$Total_Steps));
```

```
## Error: object 'act_by_Day_sql' not found
```

Mean and Median after adding missing date differ vastly, hence it is always good idea to fill missing data, and compare difference


```r
act_data$day_type <- factor(act_data$day_type);
```

```
## Error: object 'act_data' not found
```

```r
act_clean_by_weekday <- sqldf("select avg(steps) steps, interval, day_type from act_data group by interval, day_type");
```

```
## Error: RS-DBI driver: (error in statement: no such table: act_data)
```

## Are there differences in activity patterns between weekdays and weekends?


```r
qplot(interval,steps, data=act_clean_by_weekday, facets=.~day_type,color=day_type,geom = c("point","smooth"));
```

```
## Error: object 'act_clean_by_weekday' not found
```
