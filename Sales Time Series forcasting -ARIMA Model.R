

#====================================Time series Forcasting Case Study Project==========================================

#--------------------------------------By SHASHANK TANWAR------------------------------------------------------


#----------Preparing Environment for Time Series forcasting---------------------------------------------------


list.of.packages <- c("forecast", "ggplot2","MASS","caTools","sqldf","tseries")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos="http://cran.rstudio.com/")
library(forecast)
library(tseries)

#-------------------Impoting dataset--------------------------------------------------------------------------
path<- "C:/Users/Stark/Documents/ivy files/R/Case studies/Time series case study project/IVY_Time Series Project"
setwd(path)

data<- read.csv("1sales.csv")
Tsdata<- data# backup file

dim(Tsdata)
str(Tsdata)

class(Tsdata)

# checking null values
colSums(is.na(Tsdata)) #no null values found

#----------------------Converting into timeseries dataset----------------------------------------

Tsdata<- ts(Tsdata[,2], start = c(2003,1), frequency = 12 ) 

start(Tsdata)
end(Tsdata)
frequency(Tsdata)

StructTS(Tsdata)

#----------------------Ploting data---------------------------------

plot(Tsdata,ylab="Sales", xlab="Year",main="Sales between 2003-2014",col="grey")
abline(reg = lm(Tsdata~time(Tsdata)))
cycle(Tsdata)
plot(aggregate(Tsdata,FUN=mean))

# the data has trend and drift
#clearly the data is non stationary and also dont have any seasonality

#------------Making data stationary------------------------------------------------------------

#Log transformation
Tsdata_diff1<- log10(Tsdata)
par(mfrow = c(1,1))
plot(Tsdata_diff1,ylab="Sales", xlab="Year",main="Sales between 2003-2014",col="grey")

#differencing and log transformation
Tsdata_diff2<- diff(Tsdata_diff1, differences = 2)

plot(Tsdata_diff2, ylab="Sales", xlab="Year",main="Sales between 2003-2014",col="grey")
abline(reg = lm(Tsdata_diff2~time(Tsdata_diff2)))

# with Log10 and 2 order of differencing makes the series stationary

#-----------------------Checking stationarity by Augmented dicky fuller test--------------------
Tsdata_diff2<- diff(Tsdata_diff1, differences = 2)

adf.test(Tsdata_diff2, alternative = "stationary") # p values is less than 0.05 hence the data is now stationary

#KPSS test

kpss.test(Tsdata_diff2) # stationary

#-----------------------Creating ACF and PACF Plots-------------------------------------------------------------

par(mfrow = c(1,2))

acf(Tsdata_diff2, main= "ACF Plot") # ACF plot to get the degree of MA(q)
pacf(Tsdata_diff2, main = "PACF Plot")# PACF Plot to get the degree of AR(p)

#--------------------Running ARIMA Model-----------------------------------------
ARIMAfit<- arima((Tsdata), c(0,2,1))

summary(ARIMAfit)

#Running the ARIMA model-R, gives the best model fit 
require(forecast)
ARIMAFit1=auto.arima(log10(Tsdata),approximation=TRUE,trace=TRUE)
summary(ARIMAFit1)
ARIMAFit1$residuals

#Predicting future values

pred <- predict(ARIMAFit1, n.ahead = 36)
predicted_values<- 10^(pred$pred)
predicted_values
write.csv(predicted_values, "predicted36.csv")

##Ploting the observed data and forecasted data together
par(mfrow=c(1,1))
plot(Tsdata,type="l", xlim = c(2003,2020), ylim = c(1,150000 ),xlab="Year",ylab="Sales")
lines(10^(pred$pred),col="red")
lines(10^(pred$pred+2*pred$se), col = 'blue')
lines(10^(pred$pred-2*pred$se),col="black")
##############################################################################################

# Checking accuracy by taking shorter sample to predict the known future sales value

Tsdata.train<- head(Tsdata, 133)

Tsdata.test<- tail(Tsdata, 36)


Tsdata.train<- ts(Tsdata.train ,start = c(2003,1),frequency = 12 )
Tsdata.test<- ts(Tsdata.test,start = c(2014,2),frequency = 12 )


start(Tsdata.train)
end(Tsdata.train)

start(Tsdata.test)
end(Tsdata.test)

ARIMAfit_new<- arima(log10(Tsdata.train), c(0,2,1))
summary(ARIMAfit_new)

pred<- predict(ARIMAfit_new, n.ahead = 36)
predicted_values1<- 10^pred$pred
predicted_values1


#Ploting actual and predicted
par(mfrow=c(1,1))
plot(Tsdata.train, xlab= 'years', ylab='sales', ,xlim= c(2003, 2018), ylim = c(1, 120000),main = 'Sales actual and pred', col='grey')
lines(predicted_values1, col= 'red')
lines(Tsdata.test,col='blue')

predicted_values1
Tsdata.test

#--------------------------------------End-------------------------------------------------------------








