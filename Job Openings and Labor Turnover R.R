#Theodore Tussing
#Job Growth Forecasting
#Final Project

#transform then difference

install.packages("forecast")
library("forecast")

#Clear work environment
rm(list = ls())
#-------------------First Report----------------------------

#Read data
JG=read.csv("Tussing Final Project/Data/JobGrowth.csv", header=TRUE, sep=",")


#Define as Time Series
JGts <- ts(data=JG$Thousands, start=c(2000, 12), end=c(2015, 12), frequency=12)

#Plot Time Series
plot(JGts, main="Job Growth", xlab="Date", ylab="Jobs (In Thousands)")

decompose(JGts)
plot(decompose(JGts))


#Use original qq plot/line
qqnorm(JGts)
qqline(JGts)

#The qq line already looks well fit, but we can still try to transform

acf(JGts, lag=20)
pacf(JGts, lag=20)

#Transform with log

JGtslog <- log(JGts)


#qq plot/line of log
qqnorm(JGtslog)
qqline(JGtslog)


acf(diff(JGtslog), lag=20)
pacf(diff(JGtslog), lag=20)


#Transform with differencing

#Differencing once
JGtsdiff <- diff(JGts, lag=12)

plot(JGtsdiff)


#qq plot/line of log
qqnorm(JGtsdiff)
qqline(JGtsdiff)

#Differencing twice (lag 12 and lag 1)
JGtsdd <- diff(JGtsdiff)

plot(JGtsdd)


#qq plot/line of log
qqnorm(JGtsdd)
qqline(JGtsdd)
acf(diff(JGtsdd))
pacf(diff(JGtsdd))

#Differencing twice is much better than just once


#Transform with Both log and twice differencing (12 and 1)
JGtsldd <-diff(diff(JGtslog),lag=12)
plot(JGtsldd)


#qq plot/line of log
qqnorm(JGtsldd)
qqline(JGtsldd)


plot(acf(JGtsldd))
plot(pacf(JGtsldd))


#Auto ARIMA

#Of untransformed time series
auto.arima(JGts)
#Unable to fit using maximum likelyhood, Need to transform
#(Approx AIC 2426.43) ARIMA(3,1,1)(2,0,0)

#Of log Transformation
auto.arima(JGtslog)
#Much lower AIC at -491.49,ARIMA(3,1,1)(2,0,0)

#Of twice Differencing transformation
auto.arima(JGtsdd)
#high AIC of 2280.06 ARIMA(1,0,2)(1,0,2)


#Of both log and twice diffrencing transformations
auto.arima(JGtsldd)
#Lowest AIC of -493.3 ARIMA(3,0,1)(2,0,1)

#Best aic is with log transformation and twice differencing
#Auto ARIMA shows that ARIMA (3,0,1)(2,0,1) is best p,d,q, aic=-493.3

JGtsfit <- Arima(JGtsldd, order=c(3,0,1), seasonal=c(2,0,1))
tsdisplay(residuals(JGtsfit))

#Find ARIMA within +/- 1 of p and q

arima(JGtsldd, order = c(4,0,1), seasonal=c(2,0,1))
#aic=-490.87
arima(JGtsldd, order = c(2,0,1), seasonal=c(2,0,1))
#aic=-472.76
arima(JGtsldd, order = c(3,0,2), seasonal=c(2,0,1))
#aic=-490.88
arima(JGtsldd, order = c(3,0,0), seasonal=c(2,0,1))
#aic=-480.91
arima(JGtsldd, order = c(3,0,1), seasonal=c(3,0,1))
#aic=-493.52
arima(JGtsldd, order = c(3,0,1), seasonal=c(1,0,1))
#aic=-494.28
arima(JGtsldd, order = c(3,0,1), seasonal=c(2,0,2))
#aic=-494.37
arima(JGtsldd, order = c(3,0,1), seasonal=c(2,0,0))
#aic=-470.74

a1JGsldd <-arima(JGtsldd, order = c(3,0,1), seasonal=c(2,0,2))
ra1JGsldd <-resid(a1JGsldd)

#Accept null hypothesis that it is white noise
Box.test(ra1JGsldd, lag=16, fitdf=4, type="Ljung")

#ARIMA (3,0,1)(2,0,2) and ARIMA(3,0,1)(1,0,1) are both very close and the lowest
#because of this we choose ARIMA(3,0,1)(1,0,1) because it is simpler

#Will also forecast ARIMA (3,0,1)(2,0,2) just to see
JGtsfit2 <- Arima(JGtsldd, order=c(3,0,1), seasonal=c(2,0,2))
tsdisplay(residuals(JGtsfit2))
plot(forecast(JGtsfit2, h=12))
acf(residuals(JGtsfit2), lag=50)
pacf(residuals(JGtsfit2), lag=50)
Box.test(residuals(JGtsfit2), type="Ljung")
#p-value of .8162




forecast(JGtsfit2, h=12)
plot(forecast(JGtsfit2, h=12))

fcJGtsfit2 <- forecast(JGtsfit2, h=12)
str(fcJGtsfit2)
fcJGtsfit2<-data.frame(fcJGtsfit2)
pfJGtsfit2<-fcJGtsfit2$Point.Forecast

JGtsfd<-c(JGtsldd, pfJGtsfit2)

JGtsf <- ts(data=JGtsfd, start=c(2002, 1), end=c(2016, 12), frequency=12)

plot(JGtsf)

JGtsfdi<-diffinv(diffinv(JGtsf, lag=1), lag=12)
#diff -1, diff -12, exp()

JGtsfdixl <- exp(JGtsfdi)

plot(JGtsfdixl)
#Looking at the forecast I believe that the third fit ARIMA (3,0,1)(2,0,2) is the best
#because it is also simple and it looks to have the most realistic forecast


#On the morning of March 9, 2016 the next measured data point will be revealed and I will compare
#it to the prediction I have made in this graph