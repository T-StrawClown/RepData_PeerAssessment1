data <- read.csv("activity.csv")
summary(data)

library(dplyr)
library(lubridate)
library(lattice)

# ## What is mean total number of steps taken per day?
# # histogram - steps per day
# steps_per_day <- group_by(data, date = ymd(date)) %>%
#         summarise(steps_total = sum(steps, na.rm = T)) %>%
#         arrange(date)
# plot(steps_per_day,
#      type = "h",
#      xaxt = "n",
#      xlab = "",
#      yaxt = "n",
#      ylab = "",
#      main = "Steps per day")
# # format axis to look abit better than default
# axis(2, las = 1)
# axis.POSIXct(1,
#             at = seq(steps_per_day$date[1],
#                      steps_per_day$date[length(steps_per_day$date)],
#                      by = "weeks"),
#             format = "%Y-%m-%d",
#             las = 2)

# ## What is the average daily activity pattern?
# steps_per_interval <- group_by(data, interval) %>%
#         summarise(steps_avg = mean(steps, na.rm = T)) %>%
#         arrange(interval)
# plot(steps_per_interval, type = "l")
# # max steps has interval
# steps_per_interval$interval[steps_per_interval$steps_avg == max(steps_per_interval$steps_avg)]

## Imputing missing values
# f_data <- data %>%
#         group_by(interval) %>%
#         mutate(steps = replace(steps,
#                                is.na(steps),
#                                round(mean(steps, na.rm = T))))
# f_steps_per_day <- group_by(f_data, date = ymd(date)) %>%
#         summarise(steps_total = sum(steps, na.rm = T)) %>%
#         arrange(date)
# plot(f_steps_per_day,
#      type = "h",
#      xaxt = "n",
#      xlab = "",
#      yaxt = "n",
#      ylab = "",
#      main = "Steps per day")
# # format axis to look abit better than default
# axis(2, las = 1)
# axis.POSIXct(1,
#              at = seq(f_steps_per_day$date[1],
#                       f_steps_per_day$date[length(f_steps_per_day$date)],
#                       by = "weeks"),
#              format = "%Y-%m-%d",
#              las = 2)

# f_data <- f_data %>%
#         mutate(day_type = as.factor(ifelse(wday(date) %in% c(1,7), "weekend", "weekday")))

f_steps <- f_data %>%
        mutate(day_type = as.factor(ifelse(wday(date) %in% c(1,7),
                                           "weekend",
                                           "weekday"))) %>%
        group_by(interval, day_type) %>%
        summarize(steps = mean(steps)) %>%
        arrange(interval)

xyplot(steps ~ interval, data = f_steps, type = "l", groups = day_type, auto.key = T)
