install.packages("knitr")
library(knitr)


download.file("activity.zip" , destfile = "exdata-data-NEI_data.zip", method = "curl")
file <- read.csv("activity.csv", header = TRUE, sep = ",")

summary(file)

## What is mean total number of steps taken per day?

daysteps <- aggregate(steps ~ date, data = file, sum)
hist(daysteps$steps)

mean(daysteps$steps)
median(daysteps$steps)

averagesteps <- aggregate(steps ~ interval, data = file, mean)
plot(averagesteps$interval,averagesteps$steps, type = "l", xlab = "", ylab = "Steps")
max(file$steps, na.rm = TRUE)

sum(is.na(file$steps))

filefixed <- file
for(i in seq_along(file$steps)){
    dt <- filefixed[i,2]
    if(is.na(filefixed[i,1])){
        daymean <- daysteps[daysteps==as.character(dt),2]
        if(length(daymean)==0){daymean <- 0}
            filefixed[i,1] <- daymean
    }
}
daystepsfixed <- aggregate(steps ~ date, data = filefixed, sum)
hist(daystepsfixed$steps)
mean(daystepsfixed$steps)
median(daystepsfixed$steps)

# filefixed2 <- filefixed

filefixed <- mutate(filefixed, weekday = weekdays(as.Date(date)))
filefixed[filefixed$weekday %in% c("Saturday", "Sunday"), 4] <- "weekend"
filefixed[filefixed$weekday %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), 4] <- "weekday"
filefixed$weekday <- as.factor(filefixed$weekday)

averagesteps2 <- aggregate(steps ~ interval + weekday, data = filefixed, mean)
p <- qplot(interval, steps, data = averagesteps2, geom = c("line", "smooth"), method = "lm", facets = weekday~. )
print(p)



