library(forecast)

#Creating time series object in R
#Creating data that represents access to a server for a period of time
serverlog <- c(18, 33, 41, 7, 34, 35, 24, 25, 24, 21, 25, 20, 22, 31,
               40, 29, 25, 21, 22, 54, 31, 25, 26, 35)
tslog <- ts(serverlog, start=c(2003, 1), frequency = 12)
#ts() is part of R's time-series functions which creates a time series 
#object in memory
#Start perimeter is the start time, and frequency indicates monthly data

tslog

plot(tslog)
start(tslog)
end(tslog)
frequency(tslog)
#Decimal place in plot(2003.5) represents July 1st, halfway point of 2003

#Subsetting data
tslog.subset <- window(tslog, start=c(2003, 5), end=c(2004, 6))
tslog.subset

#Add connected, solid filled circle to plot
plot(tslog, type="o", pch=19)

#Using Nile data(flood levels at Aswan over 1000 year period)
#Using simple moving average to detect trends in Nile data

#Simple moving average
opar <- par(no.readonly = TRUE)
par(mfrow = c(2,2))
ylim <- c(min(Nile), max(Nile))
plot(Nile, main="Raw Time Series")
plot(ma(Nile, 3), main="Simple Moving Averages (k=3)", ylim=ylim)
plot(ma(Nile, 7), main="Simple Moving Averages (k=7)", ylim=ylim)
plot(ma(Nile, 15), main="Simple Moving Averages (k=15)", ylim=ylim)
par(opar)
#Increasing values of k ensure data is smoothed out
#Have to find value of k that does not over/under smooth
?par