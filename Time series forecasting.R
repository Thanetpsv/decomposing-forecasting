rm(list=ls())
getwd()
library(data.table)
library(ggplot2)
library(stats)
library(forecast)
library(tseries)
nivea <- read.csv('Nivea-search.csv')
head(nivea)
plot(nivea[,2])
plot(nivea[,2], type='l')
#convert data into time series format
niveaTS <- ts(nivea[,2], start= c(2009,1), frequency=12)
niveaTS
## Plot time series data with labels
plot(niveaTS, xlab= "Years", ylab= "Nivea Websearch",
     main = "Nivea Websearch data")

### Existence of seasonality observed using various plots
### Plot 1 : Seasonal plot Year-wise (using ggseasonalplot())

ggseasonplot(niveaTS, year.labels= TRUE, year.labels.left=TRUE) +
  ylab("degree") + ggtitle("Seasonal plot: Nivea Websearch Data")

### Plot 2: Polar Seasonal plot Year-wise (using ggseasonplot())
### If the plot result is round, it means that the data does not have season
ggseasonplot(niveaTS, polar=TRUE) + 
  ylab("degree") + ggtitle("Polar seasonal plot Nivea Websearch data")

### Plot 3: Seasonal plot Month-wise (using monthplot())
### Find seasons in the columns, across Jan in every year what is the trend
monthplot(niveaTS)

### DECOMPOSING

# Step1: Decompose of TS using decompose()
TSDecompose <- decompose(niveaTS, type= "multiplicative")
plot(TSDecompose)
TSDecompose

#Another type of decomposition using stl()
#add log scale because this function doesn't have multiplicative
#If some part of the line is outside of the error bar, it means that it is statistically sig
# Notice that the error bar of the "remainder" is full thus it is statistically insig
#Therefore we can ignore the "remainder"
TSDecomposeLog <- stl(log10(niveaTS), s.window='periodic')
plot(TSDecomposeLog)

###Spliting data into training and test data sets

TS_train <- window(niveaTS, start=c(2009,1), end=c(2017,12),
                   freq=12)
TS_test <- window(niveaTS, start=c(2018,1), end=c(2019,12), freq=12)

#visualize data
autoplot(TS_train, series="Train") + autolayer(TS_test, series="Test") +
  ggtitle("Nivea Websearch Training and Test data") +
  xlab("Year") + ylab("Sales") + guides(colour=guide_legend(title="Forecast"))

### Forecast methods
#########################################################################################
# Random Walk with drift
TSDecompose_train_log <- stl(log10(TS_train), s.window='periodic')
TS_train_stl <- forecast(TSDecompose_train_log, method="rwdrift", h=24)
# h is how long I want to forecast forward. We chose 24 because we want to forecast 2 years
plot(TS_train_stl)
# The graph is the forecast and the gray is the 95% certainty the light one is 99%
## Accuracy measures: RMSE and MAPE using RWD
Vec2 <- 10^(cbind(log10(TS_test), as.data.frame(forecast(
  TSDecompose_train_log, method="rwdrift", h=24))[,1]))
ts.plot(Vec2, col=c('blue', 'red'), main="Nivea Websearch: Actual vs Forecast")

## Calculation of accuracy
RMSE2 <- round(sqrt(sum(((Vec2[,1]-Vec2[,2])^2)/length(Vec2[,1]))),4)
MAPE2 <- round(mean(abs(Vec2[,1]-Vec2[,2])/Vec2[,1]),4)
RMSE2 #whatever the forecast is, we are confident at 95% that the value is +_ 2*RMSE
MAPE2 #on average how many % away from the actuality
paste("Accuracy Measures: RMSE: ",RMSE2,"and MAPE: ",MAPE2)
10^as.data.frame(TS_train_stl) # give a table of interval of confidence

### Real forecast of future:
TSDecompose_log <- stl(log10(niveaTS), s.window='periodic')
TS_stl <- forecast(TSDecompose_log, method="rwdrift", h=24)
plot(TS_stl)
TS_stl
#########################################################################################

### Holt-Winters exponential smoothing is a time series
### takes the overall level, trend and seasonality of the underlying dataset 
### into account for its forecast.
TS_train_hw<-hw(TS_train,h=24,seasonal="multiplicative")
TS_train_hw
plot(TS_train_hw)
#let's visualize the predicted and actual (actual is blue, forecast is red)
vec<-cbind(TS_test,as.data.frame(TS_train_hw)[,1])
ts.plot(vec,col=c("blue","red"),main="Nivea Websearch: Actual vs Forecast")

#let's see the precision
RMSE<-round(sqrt(sum(((vec[,1]-vec[,2])^2)/length(vec[,1]))),4)
MAPE<-round(mean(abs(vec[,1]-vec[,2])/vec[,1]),4)
paste("Accuracy Measures: RMSE: ",RMSE,"and MAPE: ",MAPE)

##########################################################################################

###Test for stationary using Augmented Dickey Fuller Test
adf.test(niveaTS) # if p value is less than 0.05, it is not stationary
plot(diff(log10(niveaTS)),ylab='Differenced Log (Nivea Websearch)') # made data stationary

### Now we test if data is appropriate to use "AR MA models"
# Plot ACF(Autocorrelation factor) and PACF(Partial Autocorrelation factor)
par(mfrow=c(1,2))
acf(ts(diff(log10(niveaTS))),main='ACF Nivea Websearch')
pacf(ts(diff(log10(niveaTS))),main='PACF Nivea Websearch')
#spikes beyond the significant zone indicate residuals are not random

#Hence let's use ARIMA to test on our test data
ARIMAfit = auto.arima(log10(TS_train), approximation=FALSE,trace=FALSE)
summary(ARIMAfit)

pred<-forecast(ARIMAfit,h=24)
vec3<-10^cbind(log10(TS_test),as.data.frame(pred)[,1])
vec3

par(mfrow=c(1,1))
ts.plot(vec,col=c("blue","red"),main="Nivea Websearch: Actual vs Forecast")

RMSE3<-round(sqrt(sum(((vec3[,1]-vec3[,2])^2)/length(vec3[,1]))),4)
MAPE3<-round(mean(abs(vec3[,1]-vec3[,2])/vec3[,1]),4)
paste("Accuracy Measures: RMSE: ",RMSE,"and MAPE: ",MAPE)


#Now let's use ARIMA to forecast the real future
ARIMAfitR = auto.arima(log10(niveaTS), approximation=FALSE,trace=FALSE)
summary(ARIMAfitR)
#Forecasting
predR = predict(ARIMAfitR, n.ahead = 12) #36 months makes 3 years
predR
plot(niveaTS,type='l',xlim=c(2017,2022),ylim=c(40,140),xlab = 'Year',
     ylab = 'Nivea Websearch', main='Forecast of Nivea Websearch')
lines(10^(predR$pred),col='blue')
lines(10^(predR$pred+2*predR$se),col='orange')
lines(10^(predR$pred-2*predR$se),col='orange')
