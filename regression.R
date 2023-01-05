## Load library
library(neuralnet)
library(mice)
library(corrplot)
library(e1071)
library(boot)
library(plyr)


## Load data
data <- read.csv("ENB2012_data.csv")
datann <- data


## Build Model:
### linear stepwise regression
mydata <- data[,-10]
mydata$Heating.Load <- log(mydata$Heating.Load)
min.model <- lm(Heating.Load~1,data=mydata)
step.model <- step(min.model, direction = "both", 
                   scope = (~Relative.Compactness+Surface.Area+Wall.Area+
                              Roof.Area+Overall.Height+Orientation+
                              Glazing.Area+Glazing.Area.Distribution))
summary(step.model)
par(mfrow = c(2, 2))
plot(step.model)


### neural network
# scale data for neural network
max = apply(datann , 2 , max)
min = apply(datann, 2 , min)
scaled = as.data.frame(scale(datann, center = min, scale = max - min))

# creating training and test set
set.seed(80)
index = sample(1:nrow (datann), 0.6*nrow(datann))
trainNN = scaled[index , ]
testNN = scaled[-index , ]

n <- names(trainNN)
f <- as.formula(paste("Heating.Load ~", paste(n[!n %in% c("Heating.Load", "Cooling.Load")], collapse = " + ")))

# fit neural network
set.seed(2)
NN.model = neuralnet(f,
                     trainNN,
                     hidden = 3 ,
                     linear.output = T)
plot(NN.model)


### SVR
SVR.model <- svm(Heating.Load~.,data)


## Predict
datatest = data[-index, ]
# linear regression
lr.pre <- predict(step.model, datatest[, 1:8])
lr.pre <- exp(lr.pre)

# neural network
NN.pre = compute(NN.model, testNN[, c(1:8)])
NN.pre = (NN.pre$net.result * (max(datann$Heating.Load) - min(datann$Heating.Load))) + min(datann$Heating.Load)

# SVR
SVR.pre <- predict(SVR.model, data)


## Comparision
### plot
par(mfrow = c(2, 2))

plot(
  datatest$Heating.Load,
  lr.pre,
  col = 'red',
  pch = 16,
  ylab = "predicted by linear regression",
  xlab = "real Heating.Load",
  cex=0.6
)
abline(0, 1)

plot(
  datatest$Heating.Load,
  NN.pre,
  col = 'blue',
  pch = 16,
  ylab = "predicted by neural network",
  xlab = "real Heating.Load",
  cex=0.6
)
abline(0, 1)

plot(
  data$Heating.Load,
  SVR.pre,
  col = 'green',
  pch = 16,
  ylab = "predicted by SVR",
  xlab = "real Heating.Load",
  cex=0.6
)
abline(0,1)


### RMSE
NN = (sum((NN.pre-datatest$Heating.Load)^2)/nrow(datatest))^0.5
LR = (sum((lr.pre-datatest$Heating.Load)^2)/nrow(datatest))^0.5
SVR = (sum((SVR.pre-data$Heating.Load)^2)/nrow(data))^0.5
knitr::kable(data.frame(LR,NN,SVR),align="l", caption="RMSE")


### select model for SVR
tuneResult <- tune(svm, Heating.Load~., data = data, ranges = list(epsilon = seq(0,1,0.2),cost = 2^(2:5)))

print(tuneResult)

plot(tuneResult)
# the darker the region is the better our model is (because the RMSE is closer to zero in darker regions).

tuneResult <- tune(svm, Heating.Load~., data = data, ranges = list(epsilon = seq(0,0.2,0.04),cost = 2^(2:5)))

print(tuneResult)

plot(tuneResult)

tunedModel <- tuneResult$best.model

tunedModelY <- predict(tunedModel, data)

error <- data$Heating.Load - tunedModelY
tunedModelRMSR <- sqrt(mean(error^2))
tunedModelRMSR