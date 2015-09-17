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

# DT <- data.table(dataset)

## What are the mean and median of total number of steps taken per day? -------
#

# Filtering dataset to obtain the required data

dailysteps <- group_by(dataset, day)
daily_total <- summarize(dailysteps, 
                d_totday = sum(steps, na.rm = FALSE),
                d_meanday = mean(steps, na.rm = FALSE),
                d_medday = median(steps, na.rm = FALSE))

# Histogram

# daily_mean <- rbind(
#     DT[,.(mean(steps)), by=.(day)]
# )[order(day)]

summary (daily_total)


par(mfrow = c(1, 1))  # Resetting layout


# Make plot appear on screen device
with(dailysteps, hist(dailysteps$steps,
                   main = "Frequency of Daily Steps",
                   xlab="Total Daily Steps",
                   col=2,
                   cex.main=0.75,
                   cex.axis=0.75,
                   cex.lab=0.75,
                   bg = "transparent"
                   
)) 


# What is the average daily activity pattern? --------------------------------- 

# Filtering dataset to obtain the required data

intersteps <- group_by(dataset, interval)
avg_daily_pattern <- summarize(intersteps, 
                        i_totday = sum(steps, na.rm = TRUE),
                        i_meanday = mean(steps, na.rm = TRUE),
                        i_medday = median(steps, na.rm = TRUE))



## Resetting layout
par(mfrow = c(1, 1))

## Make plot appear on screen device
with(intersteps, {plot(avg_daily_pattern$interval,avg_daily_pattern$i_meanday,
                    main = "Average Daily Activity Pattern",
                    xlab="Five Minute Interval",
                    ylab="Average interval steps",
                    col=1,
                    cex.main=0.75,
                    cex.axis=0.75,
                    cex.lab=0.75,
                    pch=".",
                    bg = "transparent")
               
               lines(avg_daily_pattern$interval,avg_daily_pattern$i_meanday)}
)

max_interval <- avg_daily_pattern[avg_daily_pattern$i_meanday==max(avg_daily_pattern$i_meanday), 1]
                                           

# Imputing missing values -----------------------------------------------------
#

# Total number of missing values


complete<-complete.cases(dataset$steps)
missing_val <-length(dataset$steps) - sum(complete)

# Filling out missing values  ---------- 

# Assigns mean number of steps for each 5 minute interval to every missing value

dataset$steps[!complete] <- avg_daily_pattern$i_meanday[avg_daily_pattern$interval==dataset$interval[!complete]]

# New data set with all values

# Filtering dataset to obtain the required data
dataset$steps<-as.numeric(dataset$steps)
dailysteps <- group_by(dataset, date)
new_daily_total <- summarize(dailysteps, 
                totday = sum(steps, na.rm = TRUE),
                meanday = mean(steps, na.rm = TRUE),
                medday = median(steps, na.rm = TRUE))

summary (new_daily_total)

# Histogram

# Resetting layout
par(mfrow = c(1, 1))

# Make plot appear on screen device
with(dailysteps, hist(dailysteps$steps,
                      main = "Frequency of Daily Steps",
                      xlab="Total Daily Steps",
                      col=2,
                      cex.main=0.75,
                      cex.axis=0.75,
                      cex.lab=0.75,
                      bg = "transparent"
                      
)) 



# Are there differences in activity patterns between weekdays and weekends? ---

dataset <- cbind(dataset,type_of_day)

test <-dataset$weekday == "Monday" | dataset$weekday =="Tuesday"| dataset$weekday =="Wednesday"| dataset$weekday =="Thursday"| dataset$weekday =="Friday"

ifelse (test,datase$type_of_day <- "week_day",dataset$type_of_day <- "week_end")




library(lattice)

xyplot(steps ~ interval | type_of_day, data = dataset, layout = c(2, 1)) 

group_by(dataset,type_of_day)
summarize(dataset,type_of_day)



