getwd()
setwd('Documents/R/Coursera/RepResearch/')
data <- read.csv('activity.csv')
head(data)

library(ggplot2)
#Questao 1
dataSteps <- aggregate(steps ~ date, data = data, sum, na.rm = TRUE)
plot <- ggplot(data=dataSteps, aes(dataSteps$steps)) + geom_histogram(binwidth = 2500,color='blue')+ xlab('Total steps in a day') + ylab('Frequency') + ggtitle('Histogram of total number os steps per day')
plot
#save
dev.copy(jpeg,'plot1.jpeg')
dev.off()
mean(dataSteps$steps)
median(dataSteps$steps)
#Questao2
dataInterval <- aggregate(steps ~ interval, data = data, mean, na.rm = TRUE)
plot2 <- ggplot(dataInterval,aes(dataInterval$interval,dataInterval$steps)) + geom_line(color='blue') + ggtitle('Average number of steps avereged over all days') + xlab('Interval') + ylab('Average number of steps')
plot2
#save
dev.copy(jpeg,'plot2.jpeg')
#Maior intervalo
dataInterval[which.max(dataInterval$steps), ]$interval



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
plot3 <- ggplot(data=dataAuxSteps, aes(dataAuxSteps$steps)) + geom_histogram(binwidth = 2500,color='red') + xlab('Total steps per day') + ylab('Steps') + ggtitle('Histogram of total number of steps per day from new data filled')
plot3
dev.copy(jpeg,'plot3.jpeg')
#Mean and median imputed Data
mean(dataAuxSteps$steps)
median(dataAuxSteps$steps)
#Comparando com data sem NA
mean(dataSteps$steps)
median(dataSteps$steps)

#Questao5

data$day <- weekdays(as.Date(data$date))
data$dayType <- c("weekday")
head(data)

# Make daytype as weekend if = sabado or domingo
for (i in 1:nrow(data)){
  if (data$day[i] == "sabado" || data$day[i] == "domingo"){
    data$dayType[i] <- "weekend"
  }
}

data$dayType <- as.factor(data$dayType)
dataAux2Steps <- aggregate(steps ~ interval+dayType, data, mean)
#plot4
plot4<-ggplot(data=dataAux2Steps,aes(interval, steps)) + geom_line(color='blue') + facet_wrap(~ dayType, ncol=1) + xlab('Interval') + ylab('Steps') + ggtitle('Activity patterns in weekdays and weekends')
plot4
#save plot
dev.copy(jpeg,'plot4.jpeg')
dev.off()

#salvando arquivos
write.csv(data,"data.csv")
write.csv(dataAux,"dataAux.csv")

