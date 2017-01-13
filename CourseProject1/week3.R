getwd()
setwd('Documents/R/Coursera/RepResearch/')
data <- read.csv('activity.csv')
head(data)

library(ggplot2)
#Questao 1
dataSteps <- aggregate(steps ~ date, data = data, sum, na.rm = TRUE)
ggplot(data=dataSteps, aes(dataSteps$steps)) + geom_histogram(binwidth = 2500,color='blue')
mean(dataSteps$steps)
median(dataSteps$steps)
#Questao2
dataInterval <- aggregate(steps ~ interval, data = data, mean, na.rm = TRUE)
ggplot(dataInterval,aes(dataInterval$interval,dataInterval$steps)) + geom_line(color='blue')
dataInterval[which.max(dataInterval$steps), ]$interval
#maxAvgInterval <- dataInterval[which.max(dataInterval$steps), ]


#Questao4

sum(is.na(data))
dataAux <- data
for (i in 1:nrow(dataAux)){
  if (is.na(dataAux$steps[i])){
    interval <- dataAux$interval[i]
    id <- which(dataInterval$interval == interval)
    dataAux$steps[i] <- dataInterval$steps[id]
  }
}

dataAuxSteps <- aggregate(steps ~ date , data=dataAux, sum)
ggplot(data=dataAuxSteps, aes(dataAuxSteps$steps)) + geom_histogram(binwidth = 2500,color='red')

#Mean and median of imputed Data
mean(dataAuxSteps$steps)
median(dataAuxSteps$steps)
#Comparando com data sem NA
mean(dataSteps$steps)
median(dataSteps$steps)

#Questao5

data$day <- weekdays(as.Date(data$date))
data$dayType <- c("weekday")
head(data)

# Make daytype as weekend if = Saturday of Sunday
for (i in 1:nrow(data)){
  if (data$day[i] == "sabado" || data$day[i] == "domingo"){
    data$dayType[i] <- "weekend"
  }
}

data$dayType <- as.factor(data$dayType)
dataAux2Steps <- aggregate(steps ~ interval+dayType, data, mean)

ggplot(data=dataAux2Steps,aes(interval, steps)) + geom_line(color='blue') + facet_wrap(~ dayType, ncol=1)

write.csv(data,"data.csv")
write.csv(dataAux,"dataAux.csv")