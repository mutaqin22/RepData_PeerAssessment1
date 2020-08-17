### Loading and prepocessing data
# set directory path
setwd("C:\\Users\\SOBI\\Documents\\GitHub\\RepData_PeerAssessment1")
# some package needed
library(ggplot2)
library(tidyverse)
library(knitr)

# import data
df <- read.csv("activity.csv")
Sys.setlocale("LC_TIME", "English")

# some information about variable
str(df)

### Total number of step taken per day
## Number of steps per day
# create and print number of steps per day
StepsPerDay <- aggregate(df$steps, list(df$date), FUN=sum)
colnames(StepsPerDay) <- c("Date", "Steps")
StepsPerDay

## Histogram of the total number of steps taken each day
# draw histogram
hist_tot_steps <- ggplot(StepsPerDay, aes(Steps)) + geom_histogram(boundary = 0, 
    binwidth = 2500, col = "darkgreen", fill = "lightgreen") + 
    ggtitle("Histogram of steps per day") + xlab("Steps") + ylab("Frequency") + 
    theme(plot.title = element_text(face="bold", size=12)) + 
    scale_x_continuous(breaks = seq(0,25000,2500)) + 
    scale_y_continuous(breaks = seq(0,18,2))
hist_tot_steps

# Mean and median of total number of steps taken per day
#mean
mean_tot_steps <- mean(StepsPerDay$Steps, na.rm=TRUE)
mean_tot_steps
#median
median_tot_steps <- median(StepsPerDay$Steps, na.rm=TRUE)
median_tot_steps

### Average daily activity pattern
## Time series plot of the 5 minute interval (x) and averaged number of steps taken averaged across all days (y)
# create table with steps per time
StepsPerTime <- aggregate(steps~interval, data = df, FUN=mean, 
    na.action = na.omit)
# variable time (more comprensible for the graph axis)
StepsPerTime$time <- StepsPerTime$interval/100
# draw the line plot
line_avg_steps <- ggplot(StepsPerTime, aes(time, steps)) + 
    geom_line(col = "brown") + ggtitle("Average steps per time interval") + 
    xlab("Time") + ylab("Steps") + theme(plot.title = element_text(face = "bold", 
                                        size=12))
line_avg_steps

##  5-minute interval (on average across all the days) with the maximum number of steps
# table for dplyr
ST <- tbl_df(StepsPerTime)
# find the column
ST %>% select(time, steps) %>% filter(steps == max(ST$steps))

### Imputing missing values
## Total number of missing values in dataset
# table for dplyr
ACT <- tbl_df(df)
# find the column
ACT %>% filter(is.na(steps)) %>% summarize(missing_values = n())

## replace missing values
# values without NA are imputed in a new column
df$CompleteSteps <- ifelse(is.na(df$steps), 
    round(StepsPerTime$steps[match(df$interval, 
    StepsPerTime$interval)],0), df$steps)

## New dataset that is equal to the original dataset but with the missing data filled in
# new dataset activityFull
activityFull <- data.frame(steps = df$CompleteSteps, 
    interval = df$interval, date = df$date)
# see first 10 values of the new dataset
head(activityFull, n=10)

##  Histogram of the total number of steps taken each day with missing data filled in
# prepare data
StepsPerDayFull <- aggregate(activityFull$steps, list(activityFull$date), FUN=sum)
colnames(StepsPerDayFull) <- c("Date", "Steps")
# draw the histogram
hist_steps_day <- ggplot(StepsPerDayFull, aes(Steps)) + 
    geom_histogram(boundary = 0, binwidth = 2500, col = "darkblue", 
    fill = "lightblue") + ggtitle("Histogram of steps per day") + 
    xlab("Steps") + ylab("Frequency") + theme(plot.title = element_text(face = "bold",
    size = 12)) + scale_x_continuous(breaks = seq(0,25000,2500)) + 
    scale_y_continuous(breaks = seq(0,26,2))
hist_steps_day

## Calculate and report the mean and median total number of steps taken per day
# Mean
mean(StepsPerDayFull$Steps)
#Median
median(StepsPerDayFull$Steps)

### Are there differences in activity patterns between weekdays and weekends?
## Create a new factor variable in the dataset with two levels - “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
# Create variable with date in correct format
activityFull$RealDate <- as.Date(activityFull$date, format = "%Y-%m-%d")
# create a variable with weekdays name
activityFull$weekday <- weekdays(activityFull$RealDate)
# create a new variable indicating weekday or weekend
activityFull$DayType <- ifelse(activityFull$weekday=='Saturday' | activityFull$weekday=='Sunday', 'weekend','weekday')
# see first 10 values
head(activityFull, n=10)

## Two time series plot of the 5-minute interval (x) and the average number of steps taken averaged across weekday days or weekend days (y).
# create table with steps per time across weekdaydays or weekend days
StepsPerTimeDT <- aggregate(steps~interval + DayType, data = activityFull,
                            FUN = mean, na.action = na.omit)
# variable time (more comprensible for the graph axis)
StepsPerTimeDT$time <- StepsPerTime$interval/100
# draw the line plot
plot_avg_steps <- ggplot(StepsPerTimeDT, aes(time, steps)) + 
    geom_line(col = "darkred") + ggtitle("Average steps per time interval: weekdays vs. weekends") + 
    xlab("Time") + ylab("Steps") + theme(plot.title = element_text(face = "bold", 
                                size = 12)) + facet_grid(DayType ~ .)
plot_avg_steps