# -----------------------------------------------------------------------------
#
# Project 1 
#
# -----------------------------------------------------------------------------


## Loading and preprocessing the data -----------------------------------------
#

workdir<-getwd()

fpath1 = file.path(workdir, "repdata-data-activity.zip")
if (!file.exists(fpath1)){
    fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
    download.file(fileUrl, "./repdata-data-activity.zip")
} 

unzip("repdata-data-activity.zip")

dataset <- read.csv("activity.csv", header=TRUE, sep=",")

# Note: date is a factor with format yyyy-mm-dd in dataset and has to be converted to date 
# type POSIXct
day<-strptime(dataset$date, "%Y-%m-%d")
weekday <- weekdays (day)

# New columns added to dataframe dataset 
dataset <- cbind(dataset,day)
dataset <- cbind(dataset,weekday)

# Checking if package "data.table" is installed. If not, then installing it
if (!"data.table" %in% rownames(installed.packages())){
    install.packages("data.table")
}
library(data.table)

# Checking if package "dplyr" is installed. If not, then installing it
if (!"dplyr" %in% rownames(installed.packages())){
    install.packages("dplyr")
}
library("dplyr")

## What are the mean and median of total number of steps taken per day? -------

# Filtering dataset to obtain the required data

dailysteps <- group_by(dataset, day)
daily_total <- summarize(dailysteps, 
                totday = sum(steps, na.rm = TRUE)) # Excludes NA values

daily_mean <- mean(daily_total$totday, na.rm = TRUE) # Excludes NA values
daily_median <- median(daily_total$totday, na.rm = TRUE) # Excludes NA values

# Histogram
par(mfrow = c(1, 1))  # Resetting layout
his_break <- seq(0,25000,500)
with(dailysteps, hist(daily_total$totday,
                   main = "Total Number of Steps Taken Each Day",
                   xlab="Number of Steps",
                   breaks=his_break,
                   col=2,
                   cex.main=0.75,
                   cex.axis=0.75,
                   cex.lab=0.75,
                   bg = "transparent"
                   
)) 

library(ggplot2)
plotTitle<- "Total Number of Steps Taken Each Day\n"
g <- ggplot(data = as.data.frame(daily_total$totday), aes(x = daily_total$totday, fill="Frequency"), ylab=NULL) 
g <- g + labs(title=plotTitle, x="Number of Steps") 
g <- g + geom_histogram(binwidth=500, colour = "orange")
g

# What is the average daily activity pattern? --------------------------------- 

# Filtering dataset to obtain the required data

intersteps <- group_by(dataset, interval)
avg_daily_pattern <- summarize(intersteps,
                        mean_interval = mean(steps, na.rm = TRUE))

## Resetting layout
par(mfrow = c(1, 1))

## Make plot appear on screen device
with(intersteps, {plot(avg_daily_pattern$interval,avg_daily_pattern$mean_interval,
                    main = "Average Daily Activity Pattern",
                    xlab="Five Minute Interval",
                    ylab="Average Number of Steps",
                    col=1,
                    cex.main=0.75,
                    cex.axis=0.75,
                    cex.lab=0.75,
                    pch=".",
                    bg = "transparent")
               
               lines(avg_daily_pattern$interval,avg_daily_pattern$mean_interval)}
)

max(avg_daily_pattern$mean_interval)
max_interval <- avg_daily_pattern[avg_daily_pattern$mean_interval==max(avg_daily_pattern$mean_interval), 1]
as.numeric(max_interval)                                           

# Inputing missing values -----------------------------------------------------
#

# Total number of missing values

complete<-complete.cases(dataset$steps)
num_missing_val <-length(dataset$steps) - sum(complete)
num_missing_val
# Filling out missing values. Assigns mean number of steps for each 5 minute interval to every missing value

# Solution is to add a new column instead of replacing the old column with the original values

new_dataset<- mutate(dataset, all_steps = ifelse(is.na(dataset$steps), avg_daily_pattern$mean_interval, dataset$steps))
       
new_dailysteps <- group_by(new_dataset, day)
new_daily_total <- summarize(new_dailysteps, 
                         totday = sum(all_steps, na.rm = TRUE)) # Excludes NA values

new_daily_mean <- mean(new_daily_total$totday, na.rm = TRUE) # Excludes NA values
new_daily_median <- median(new_daily_total$totday, na.rm = TRUE) # Excludes NA values


# Histogram
par(mfrow = c(1, 1))  # Resetting layout
his_break <- seq(0,25000,500)
with(dailysteps, hist(new_daily_total$totday,
                      main = "Total Number of Steps Taken Each Day - No Missing Values",
                      xlab="Number of Steps",
                      breaks=his_break,
                      col=2,
                      cex.main=0.75,
                      cex.axis=0.75,
                      cex.lab=0.75,
                      bg = "transparent"
                      
)) 

# Are there differences in activity patterns between weekdays and weekends? ---


new_dataset<-mutate(new_dataset, type_of_day = ifelse(weekday=="Saturday"|weekday=="Saturday","week_end","week_day"))

library(lattice)

new_intersteps <- group_by(new_dataset, interval,type_of_day)
avg_type_of_day_pattern <- summarize(new_intersteps,
                               mean_interval = mean(all_steps, na.rm = TRUE))

xyplot(mean_interval ~ interval | type_of_day, data = avg_type_of_day_pattern, layout = c(1, 2), type="l",
       main = "Average Daily Activity Pattern",
       xlab="Five Minute Interval",
       ylab="Average Number of Steps",
       ) 
