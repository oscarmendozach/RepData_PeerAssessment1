unzip(zipfile = "activity.zip", files = "activity.csv", overwrite = TRUE)
datafile <-read.table(file = "activity.csv", header = TRUE, sep = ",", na.strings = "NA")

datafile$date <- as.Date(x = datafile$date, format = "%Y-%m-%d")
str(datafile)
library(dplyr)

steps_per_day <- datafile %>% group_by(date) %>%  summarize(total = sum(steps), na.rm = TRUE)

library(ggplot2)
steps_per_day %>% ggplot(aes(x = total)) %>% geom_histogram()

steps_per_day %>% ggplot(aes(x = total)) + geom_histogram(bins = 100)

steps_per_interval <- datafile %>% group_by(interval) %>% summarize(average_interval = mean(steps, na.rm=TRUE))

steps_per_interval %>% ggplot(aes(x = interval, y = average_interval)) + geom_line()

steps_per_interval$interval[which.max(steps_per_interval$average_interval)]
which.max(steps_per_interval$average_interval)

