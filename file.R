library(dplyr)
library(ggplot2)

# Loading and pre-processing the data

Data <- read.csv(unzip("activity.zip", "activity.csv"))
Data$date <- as.Date(Data$date, format = "%Y-%m-%d")

ASPD <- "average_steps_per_date"
ASPI <- "average_steps_per_interval"
FMI <- "five_min_interval"
ASPW <- "average_steps_per_weekday"

# 1. Calculate the total number of steps taken per day

ASPD <- Data %>% select(steps, date) %>% group_by(date) %>%
        summarise_all(funs(sum))

# 2. Make a histogram of the total number of steps taken each day

ggplot(ASPD, aes(steps)) + 
        geom_histogram(bins = 25, color = "darkblue",
                       fill = "lightblue", alpha = 0.4) + 
        labs(title = "Steps histogram", x = "Number of steps", y = "Count")
dev.copy(png, "plot1.png")
dev.off()
# 3. Calculate and report the mean and median of the total number of steps taken per day

meanData <- mean(ASPD$steps, na.rm = T)
medianData <- median(ASPD$steps, na.rm = T)

# What is the average daily activity pattern?

ASPI <- Data %>% select(steps, interval) %>% group_by(interval) %>%
        summarise_all(funs(mean), na.rm = T)

ggplot(ASPI, aes(interval, steps)) + geom_line() +
        labs(title = "The average daily activity pattern",
             x = "The 5-minute interval", y = "Average number of steps") + theme_classic()
dev.copy(png, "plot2.png")
dev.off()

FMI <- ASPI$interval[which.max(ASPI$steps)]

# Calculate and report the total number of missing values in the data set 
Nas <- sum(is.na(Data))

FillnaData <- Data
na_steps <- is.na(Data$steps)
FillnaData$steps[na_steps] <- ASPI$steps[na_steps]


ASPD2 <- FillnaData %>% select(steps, date, interval) %>% group_by(date) %>%
        summarise_all(funs(sum))

ggplot(ASPD2, aes(steps)) + 
        geom_histogram(bins = 25, color = "darkblue",
                       fill = "lightgreen", alpha = 0.4) + 
        labs(title = "Steps histogram", x = "Number of steps", y = "Count")

dev.copy(png, "plot3.png")
dev.off()

meanFillnaData <- mean(ASPD2$steps, na.rm = T)
medianFillnaData <- median(ASPD2$steps, na.rm = T)

summary(ASPD$steps)
summary(ASPD2$steps)

table(weekdays(Data$date))
weekdays_values = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")

date_type <- ifelse(
        weekdays(FillnaData$date) %in% weekdays_values,
        'weekday',
        'weekend')
table(date_type)

FillnaData$day_type <- factor(date_type)
ASPW <- aggregate(steps ~ interval + day_type, data = FillnaData
                  , FUN = mean, na.rm = T) 

ggplot(ASPW, aes(interval, steps, color = day_type)) +
        geom_line() +
        facet_grid(day_type ~ .) +
        labs(title = "Activity pattern by the week of the day", 
             x = "The 5-minute interval", y = "Average number of steps")
dev.copy(png, "plot4.png")
dev.off()
