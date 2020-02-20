unzip(zipfile = "activity.zip", files = "activity.csv", overwrite = TRUE)
activitydata <-read.table(file = "activity.csv", header = TRUE, sep = ",", na.strings = "NA")

activitydata$date <- as.Date(x = activitydata$date, format = "%Y-%m-%d")
str(activitydata)
library(dplyr)

totalsteps <- activitydata %>% group_by(date) %>%  summarize(total = sum(steps, na.rm = TRUE))

library(ggplot2)
#totalsteps %>% ggplot(aes(x = total)) + geom_histogram()

#totalsteps %>% ggplot(aes(x = total)) + geom_histogram(bins = 100)

totalsteps %>% ggplot(aes(x = total)) + 
  geom_histogram(color = "white" ,fill = "orange",bins = 25) + 
  geom_vline(xintercept = mean(totalsteps$total, na.rm=TRUE), color = "blue", lwd = 1, lty = 2) +
  geom_vline(xintercept = median(totalsteps$total, na.rm = TRUE), color = "purple", lwd = 1, lty=3) +
  ggtitle("Histogram of total steps per day") +
  ylab("Count") + 
  xlab("Total steps per day") +
  theme_bw()

intervalsteps <- activitydata %>% group_by(interval) %>% summarize(average_interval = mean(steps, na.rm=TRUE))

intervalsteps %>% ggplot(aes(x = interval, y = average_interval)) + 
  geom_line(color = "blue4", lwd = .5) + 
  geom_vline(xintercept = intervalsteps$interval[which.max(intervalsteps$average_interval)], color = "red3", lwd = .5, lty = 2) +
  ggtitle("Time plot of average steps per interval") + 
  xlab ("Time interval") +
  ylab ("Average steps number") + 
  theme_bw()



intervalsteps$interval[which.max(intervalsteps$average_interval)]
which.max(intervalsteps$average_interval)

##Imputting Missing values
NAdays <- activitydata %>% filter(is.na(steps) == TRUE)

NAdays %>% ggplot(aes(x = date, y = interval)) + 
  geom_point(color = "green2") + 
  ggtitle("Days with NA data") + 
  xlab("Date") + 
  ylab("Intervals with NA data") + 
  theme(axis.text.x = element_text(angle = 90))

sum(is.na(activitydata$steps))

##copy data to dataframe

activitydatafilled <- activitydata

x1 <- 1
x2 <- 1

for (x1 in 1:nrow(activitydatafilled)){
  if (is.na(activitydatafilled$steps[x1]) == TRUE){
    activitydatafilled$steps[x1] <- intervalsteps$average_interval[x2]
    x2 <- x2 + 1
    if (x2 >= 288){
      x2 <- 1
    }
  }
  else {
    x2 <- 1
  }
}

head(activitydatafilled)

head(activitydata)

##plot histogram
totalstepsfilled <- activitydatafilled %>% group_by(date) %>%  summarize(total = sum(steps, na.rm = TRUE))
totalstepsfilled %>% ggplot(aes(x = total)) + 
  geom_histogram(color = "white" ,fill = "orange",bins = 25) + 
  geom_vline(xintercept = mean(totalstepsfilled$total, na.rm=TRUE), color = "blue", lwd = 1, lty = 2) +
  geom_vline(xintercept = median(totalstepsfilled$total, na.rm = TRUE), color = "purple", lwd = 1, lty=3) +
  ggtitle("Histogram of total steps per day") +
  ylab("Count") + 
  xlab("Total steps per day") +
  theme_bw()

##Differences in pattern according to weekday
activitydatafilledday <- activitydatafilled

weekday <- c("lunes", "martes", "miércoles", "jueves", "viernes")
weekend <- c("sábado", "domingo")

activitydatafilledday$weekday <- weekdays(activitydatafilledday$date)

for (i in 1:nrow(activitydatafilledday)){
  if (activitydatafilledday$weekday[i] %in% weekday){
    activitydatafilledday$weekday[i] <- "weekday"
  }
  else {
    activitydatafilledday$weekday[i] <- "weekend"
  }
}

activitydatafilledday$weekday <- as.factor(activitydatafilledday$weekday)

##make a panel plot
#data wrangling
activitydatafilleddayfinal <- activitydatafilledday %>% 
  group_by(weekday, interval) %>% 
  summarise(avg_steps = mean(steps))


#mean_days stores the step means per factor
mean_days <- activitydatafilledday %>% 
  group_by(weekday) %>% 
  summarise(avg_steps = mean(steps))

#final plot
activitydatafilleddayfinal %>% ggplot(aes(x = interval, y = avg_steps)) +
  geom_line(color="black", lty = 2, lwd = .5) +
  ggtitle("Average steps measured during weekdays and weekends") + 
  ylab("Average steps number") +
  xlab("5-minute interval") +
  facet_wrap(~weekday) + 
  geom_hline(data = mean_days, aes(yintercept = avg_steps)) +
  theme_light()