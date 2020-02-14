unzip(zipfile = "activity.zip", files = "activity.csv", overwrite = TRUE)
activitydata <-read.table(file = "activity.csv", header = TRUE, sep = ",", na.strings = "NA")

activitydata$date <- as.Date(x = activitydata$date, format = "%Y-%m-%d")
str(activitydata)
library(dplyr)

totalsteps <- activitydata %>% group_by(date) %>%  summarize(total = sum(steps, na.rm = TRUE))

library(ggplot2)
totalsteps %>% ggplot(aes(x = total)) + geom_histogram()

totalsteps %>% ggplot(aes(x = total)) + geom_histogram(bins = 100)

totalsteps %>% ggplot(aes(x = total)) + 
  geom_histogram(color = "white" ,fill = "orange",bins = 25) + 
  geom_vline(xintercept = mean(totalsteps$total, na.rm=TRUE), color = "blue", lwd = 1, lty = 2) +
  geom_vline(xintercept = median(totalsteps$total, na.rm = TRUE), color = "purple", lwd = 1, lty=3) +
  ggtitle("Histogram of total steps per day") +
  ylab("Count") + 
  xlab("Total steps per day") +
  theme_bw()

intervalsteps <- activitydata %>% group_by(interval) %>% summarize(average_interval = mean(steps, na.rm=TRUE))

intervalsteps %>% ggplot(aes(x = interval, y = average_interval)) + geom_line()

intervalsteps$interval[which.max(intervalstepsl$average_interval)]
which.max(intervalsteps$average_interval)

