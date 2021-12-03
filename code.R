# 1 Code for reading in the dataset and/or processing the data

data <- read_csv("activity.csv")
str(data)
summary(data)
sum(is.na(data))


# 2. Histogram of the total number of steps taken each day

#remove NAs
data_comp <- drop_na(data)

daily_steps <- data_comp %>% 
    group_by(date) %>% 
    summarise(daily_steps = sum(steps))

par(mfrow = c(1,1))
hist(daily_steps$daily_steps,
     main = "Daily Steps",
     xlab = "Daily steps")


# 3. Mean and median number of steps taken each day
daily_steps %>% data %>% 
      summarise(mean = mean(daily_steps, na.rm = TRUE),
                median = median(daily_steps, na.rm = TRUE))


# 4. Time series plot of the average number of steps taken
interval_avg <- data %>% 
    group_by(interval) %>% 
    summarise(mean_steps = mean(steps, na.rm = TRUE))

plot(interval_avg$interval, interval_avg$mean_steps, type = "l",
     col = "blue",
     main = "Average number of steps during day: 5-minutes intervals",
     xlab = "5-minute interval",
     ylab = "mean steps")


# 5. The 5-minute interval that, on average, contains the maximum number of steps
# find interval with highest mean
interval_avg %>% 
    filter(mean_steps == max(mean_steps))

# 6. Code to describe and show a strategy for imputing missing data
# simple mean imputation, impute with mean for each interval

# dataset with imputed values
data_imputed <- data 

#data with NA's removed
data_no_missing <- subset(data, !is.na(data$steps))

# index for obs with missing values
index_missing <- is.na(data$steps)

# calculate mean for each interval
interval_avg <-tapply(data_no_missing$steps, data_no_missing$interval, mean, simplify = TRUE)

# impute the mean 
data_imputed$steps[index_missing] <- interval_avg[as.character(data_imputed$interval[index_missing])]


# 7. Histogram of the total number of steps taken each day after missing values are imputed

daily_steps_imputed <- data_imputed %>% 
                            group_by(date) %>% 
                            summarise(daily_steps = sum(steps))

hist(daily_steps_imputed$daily_steps)

# mean and median after imputation


# 8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
# use function weekdays() to check what day each date is
?weekdays
weekdays(data_imputed$date)

# use ifelse to make column indicating weekday or weekend
data_imputed$day <- ifelse(weekdays(data_imputed$date) == "Saturday" | weekdays(data_imputed$date) == "Sunday", "Weekend", "Weekday")

weekday_data <- data_imputed %>% 
    group_by(day, interval) %>% 
    summarise(mean_steps = mean(steps))

# make plot
ggplot(weekday_data, aes(x = interval, y = mean_steps)) +
    geom_line() +
    facet_wrap(~day, ncol = 1) +
    ylim(0, 300)



