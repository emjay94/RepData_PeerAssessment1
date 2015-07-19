#Download the data set from the web source and unzip it at the local "./data" directory

if(!file.exists("./data")) {
    dir.create("./data")
}
  
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"

if(!file.exists("./data/activity")) {
    download.file(fileUrl, destfile="./data/activity.zip")
    unzip(zipfile="./data/activity.zip", exdir="./data")
}

#Loading and preprocessing the data

move_data <- read.csv("./data/activity.csv")
str(move_data)
head(move_data)
tail(move_data)

move_data$date <- as.Date(move_data$date, format = "%Y-%m-%d")
str(move_data)
str(move_data$date)

library(plyr)
library(ggplot2)


#What is mean total number of steps taken per day?

#Calculate the total number of steps taken per day

total.steps <- ddply(move_data, .(date), summarize, total_steps = sum(steps))
head(total.steps)

#Make a histogram of the total number of steps taken each day

total.plot <- ggplot(total.steps, aes(x = total_steps))
total.plot <- total.plot + geom_histogram(color = "darkgreen", fill = "white")
print(total.plot)

#Calculate and report the mean and median of the total number of steps taken per day

summary(total.steps$total_steps)
summary(total.steps$total_steps)["Mean"]
summary(total.steps$total_steps)["Median"]


#What is the average daily activity pattern?

#Calculate the average number of steps taken per each interval

average.steps <- ddply(move_data, .(interval), summarize, average_steps = mean(steps, na.rm = TRUE))
head(average.steps)
tail(average.steps)

#Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

average.plot <- ggplot(average.steps, aes(x = interval, y = average_steps))
average.plot <- average.plot + geom_line(color = "red", width = 0.4)
print(average.plot)
summary(average.steps$average_steps)
summary(average.steps$average_steps)["Max."]

#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

average.steps[which.max(average.steps$average_steps),]
average.steps[which.max(average.steps$average_steps),]$interval


#Imputing missing values

#Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

table(complete.cases(move_data))

#Create a new dataset that is equal to the original dataset but with the missing data filled in
#Fill in all of the missing values in the dataset using the mean for that 5-minute interval

imputed_data <- move_data
imputed_data$steps <- ifelse(is.na(imputed_data$steps), average.steps$average_steps[match(imputed_data$interval, average.steps$interval)], imputed_data$steps)
table(complete.cases(move_data))
table(complete.cases(imputed_data))

#Make a histogram of the total number of steps taken each day 

total.imputed.steps <- ddply(imputed_data, .(date), summarize, total_steps = sum(steps))
head(total.imputed.steps)

total.imputed.plot <- ggplot(total.imputed.steps, aes(x = total_steps))
total.imputed.plot <- total.imputed.plot + geom_histogram(color = "dark green", fill = "white")
print(total.imputed.plot)

#Calculate and report the mean and median total number of steps taken per day

summary(total.imputed.steps$total_steps)
summary(total.imputed.steps$total_steps)["Mean"]
summary(total.imputed.steps$total_steps)["Median"]


#Are there differences in activity patterns between weekdays and weekends?

#Set time locale as "English_United States.1252" to get the day of week in English

Sys.setlocale("LC_TIME", "English")

#Create a new factor variable in the dataset with two levels ??? ???weekday??? and ???weekend??? indicating whether a given date is a weekday or weekend day

move_data$day <- weekdays(move_data$date)
day.week <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
move_data$weekdivide <- ifelse(move_data$day %in% day.week, "weekday", "weekend")
head(move_data)
table(move_data$weekdivide)

#Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)

average.day.steps <- ddply(move_data, .(interval, weekdivide), summarize, average_steps = mean(steps, na.rm = TRUE))
head(average.day.steps)

average.day.plot <- ggplot(average.day.steps, aes(x = interval, y = average_steps))
average.day.plot <- average.day.plot + geom_line(color = "red", width = 0.4)
average.day.plot <- average.day.plot + facet_grid(. ~ weekdivide)
print(average.day.plot)