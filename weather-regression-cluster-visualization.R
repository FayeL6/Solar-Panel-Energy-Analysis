# linear stepwise regression
panel <- read.csv("Panel_Monthly_totals_2020_2021.csv")
weather <- read.csv("Weather_and_energy_Final_2020_2021.csv")
data <- weather[,3:17]
library(mice)
md.pattern(data)
data <- data[,-c(5,6,12)]
head(data)
md.pattern(data)
library(corrplot)
corrplot(cor(data))
energy_production <- data[,1]
cloud_cover <- data[,11]
visibility <- data[,10]
precipitation <- data[,5]
max_temperature <- data[,2]
humidity <- data[,12]
mydata <- data.frame(energy_production,cloud_cover,visibility,precipitation,max_temperature,humidity)
min.model <- lm(energy_production~1,data= mydata)
step.model <- step(min.model, direction = "both", scope = (~cloud_cover+visibility+precipitation+max_temperature+humidity))
summary(step.model)
plot(step.model,which=1)
plot(step.model,which=2)


# K-means cluster
library(mice)
library(tidyverse)
setwd("D:/shuxue/GEARS")
data <- read.csv("weather_in_Antwerp.CSV.csv")
energy <- read.csv("PV_Elec_Gas3.csv")
e <- energy[2625:2983,]
data <- data[1:133669,]
data$wind_km.h <- as.numeric(data$wind_km.h)
data <- data %>% drop_na()
for ( i in 1:dim(data)[1]){
  data$day[i] <- data$day[i] + (data$month[i]-1)*30
}
d <- data %>% group_by(day) %>% summarize(temperature=mean(temp_deg),wind_km.h=mean(wind_km.h),humidity=mean(humidity))
y <- d[,-1]
y <- cbind(y,e[,2])
k <- kmeans(y,4)
y <- cbind(y,k$cluster)


# visualization
colnames(y)[c(4,5)] <- c("energy_production","season")
y$season <- k$cluster
y$season[which(y$season==1)] <- "summer"
y$season[which(y$season==4)] <- "spring"
y$season[which(y$season==2)] <- "winter"
y$season[which(y$season==3)] <- "autumn"
y[,5] <- as.factor(y[,5])
par(mfrow=c(2,2))
p1 <- ggplot(data = y,
             mapping = aes(
               x = temperature, 
               color = season,
               fill = season))+geom_density(alpha=0.3)+labs(y="number of days")
p1 <- p1 + guides(color=FALSE)
p2 <- ggplot(data = y,
             mapping = aes(
               x = wind_km.h, 
               color = season,
               fill = season))+geom_density(alpha=0.3)+labs(y="number of days")
p3 <- ggplot(data = y,
             mapping = aes(
               x = humidity, 
               color = season,
               fill = season))+geom_density(alpha=0.3)+labs(y="number of days")
p3 <- p3+theme(legend.position = "bottom")
p4 <- ggplot(data = y,
             mapping = aes(
               x = energy_production, 
               color = season,
               fill = season))+geom_density(alpha=0.3)+labs(y="number of days")
p5 <- ggplot(data = y,
             mapping = aes(
               x = humidity, 
               color = season,
               fill = season))+geom_density(alpha=0.3)+labs(y="number of days")
library(patchwork)
print((p1 | p2) / (p5 | p4))