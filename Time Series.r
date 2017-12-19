#ARIMA
----

data(AirPassengers)
class(AirPassengers)
#This tells you that the data series is in a time series format

start(AirPassengers)
#This is the start of the time series

end(AirPassengers)
#This is the end of the time series

frequency(AirPassengers)
#The cycle of this time series is 12months in a year

summary(AirPassengers)
#The number of passengers are distributed across the spectrum

plot(AirPassengers)
#This will plot the time series

abline(reg=lm(AirPassengers~time(AirPassengers)))
#This will fit in a line

cycle(AirPassengers)
#This will print the cycle across years.

#Start from here

plot(AirPassengers)
#This will aggregate the cycles and display a year on year trend

boxplot(AirPassengers~cycle(AirPassengers))
#Box plot across months will give us a sense on seasonal effect


#Stationary Series

#There are three basic criterion for a series to be classified as stationary series

#1. The mean of the series should not be a function of time rather should be a constant
#2. The variance of the series should not a be a function of time
#3. The covariance of the i th term and the (i + m) th term should not be a function of time

# Covariance Vs Correlation - http://ci.columbia.edu/ci/premba_test/c0331/s7/s7_5.html

#until unless your time series is stationary, you cannot build a time series model
#Important Inferences

#The year on year trend clearly shows that the #passengers have been increasing without fail.
#The variance and the mean value in July and August is much higher than rest of the months.
#Even though the mean value of each month is quite different their variance is small. 
#Hence, we have strong seasonal effect with a cycle of 12 months or less.

#Introduction to ARMA Time Series Modeling

#ARMA models are commonly used in time series modeling. 
#In ARMA model, AR stands for auto-regression and MA stands for moving average



#We know that we need to address two issues before we test stationary series. 
#One, we need to remove unequal variances. We do this using log of the series. 
#Two, we need to address the trend component. We do this by taking difference of the series
plot(diff(log(AirPassengers)))
#We see that the series is stationary enough to do any kind of time series modelling.
#Augmented Dickey-Fuller Test

library(tseries)
adf.test(diff(log(AirPassengers)), alternative = c("stationary", "explosive"), k=0)

#Next step is to find the right parameters to be used in the ARIMA model. 
#We already know that the 'd' component is 1 as we need 1 difference to make the series stationary. 
#We do this using the Correlation plots. Following are the ACF plots for the series

#AR I MA
#p  d  q
acf(AirPassengers)


acf(diff(log(AirPassengers))) #Determines the value of q

pacf(diff(log(AirPassengers))) #Determines the value of p

plot(diff(log(AirPassengers)))

#Clearly, the decay of ACF chart is very slow, which means that the population is not stationary. 
#We have already discussed above that we now intend to regress on the difference of logs 
#rather than log directly.
#Let's see how ACF and PACF curve come out after regressing on the difference


#Clearly, ACF plot cuts off after the first lag. Hence, we understood that value of p 
#should be 0 as the ACF is the curve getting a cut off. While value of q should be 1 or 2. 
#After a few iterations, we found that (0,1,1) as (p,d,q) comes out to be the combination with 
#least AIC and BIC
# Good post to select p,d,q : https://stats.stackexchange.com/questions/44992/what-are-the-values-p-d-q-in-arima

#Let's fit an ARIMA model and predict the future 10 years
#c(p,d,q)
fit <- arima(log(AirPassengers), c(0, 1, 1),seasonal = list(order = c(0, 1, 1), period = 12))

pred <- predict(fit, n.ahead = 10*12)

pred1 <- 2.718^pred$pred

ts.plot(AirPassengers,2.718^pred$pred, log = "y", lty = c(1,3))


#Testing our Model

datawide <- ts(AirPassengers, frequency = 12, start=c(1949,1), end=c(1959,12))

fit <- arima(log(datawide), c(0, 1, 1),seasonal = list(order = c(0, 1, 1), period = 12))

pred <- predict(fit, n.ahead = 10*12)

pred1 <- 2.718^pred$pred

data1<- head(pred1,12)

predicted_1960  <- round(data1,digits=0)

original_1960  <- tail(AirPassengers,12)

ts.plot(AirPassengers,2.718^pred$pred, log = "y", lty = c(1,3))

