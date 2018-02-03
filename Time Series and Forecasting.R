install.packages("xts")
library(xts)

install.packages("forecast")
library(forecast)


install.packages("ggplot2")
library(ggplot2)

-------------------------------------------------------------------------------------------

-------------------------------------------------------------------

--------------------------------------------------------------------------------------------------
#Read gold data
gold <- read.csv("C:/ME/NYU/Fall Sem 2016 - I/Business Analytics/Assignments/Assignment 6/gold.csv", header = TRUE, stringsAsFactors = FALSE)
names(gold)
str(gold)

#Putting the data into a Time Series
gold.ts <- ts(gold$value.gold, start = c(1988), frequency = 12)
gold.ts

plot.ts(gold.ts)

#Decomposing into seasonal and trend plots
gold.ts.1 <- decompose(gold.ts)
plot(gold.ts.1)

#Holtwinters for smoothening
gold.holt <- HoltWinters(gold.ts, gamma=FALSE)
gold.holt
plot(gold.holt)

#Forecasting
gold.forecast <- forecast.HoltWinters(gold.holt, h=25)
plot.forecast(gold.forecast)

#Read the oil data
oil <- read.csv("C:/ME/NYU/Fall Sem 2016 - I/Business Analytics/Assignments/Assignment 6/oil.csv", header = TRUE, stringsAsFactors = FALSE)
names(oil)
str(oil)
#Convert oil date to the same frequence as gold date
oil$ts <- xts(oil$value.oil, as.Date(oil$date.oil, "%m/%d/%Y"))

oil_price = apply.monthly(oil$ts, mean)
xts(oil_price)

#Putting the data into time series
oil.ts <- ts(oil_price, start = c(1988), frequency = 12)
oil.ts
plot.ts(oil.ts)

#Decomposing into seasonal and trend plotss
oil.ts.1 <- decompose(oil.ts)
plot(oil.ts.1)

#Smoothening the plot
oil.holt <- HoltWinters(oil.ts, gamma=FALSE)
oil.holt
plot(oil.holt)

#Forecasting the oil price
oil.forecast <- forecast.HoltWinters(oil.holt, h=25)
plot.forecast(oil.forecast)


#Read the sp500 data
sp500 <- read.csv("C:/ME/NYU/Fall Sem 2016 - I/Business Analytics/Assignments/Assignment 6/sp500.csv",header = TRUE, stringsAsFactors = FALSE)
names(sp500)
str(sp500)

#Putting the data into time series
sp500.ts<-ts(sp500$Close,start=c(2010),frequency = 12)
plot(sp500.ts)
sp500.ts.d<-decompose(sp500.ts)
plot(sp500.ts.d)

#Sorting the date into correct order
sp500.sorted<-sp500[order(as.Date(sp500$Date, format="%m/%d/%Y")),]
sp500.sorted.ts<-ts(sp500.sorted$Close,start=c(1988),frequency = 12)
sp500.sorted.ts
plot(sp500.sorted.ts)

#Decomposing into seasonal and trend data
sp500.sorted.ts.d<-decompose(sp500.sorted.ts)
plot(sp500.sorted.ts.d)

#Smoothening the sp500 data
sp500.holt <- HoltWinters(sp500.sorted.ts, gamma=FALSE)
sp500.holt
plot(sp500.holt)

#Forecasting the sp500 Data
sp500.forecast <- forecast.HoltWinters(sp500.holt, h=25)
plot.forecast(sp500.forecast)

#ts.intersect(gold.ts,oil_price, drame = FALSE)

plot(ts.intersect(gold.ts, oil_price, sp500.sorted.ts, dframe = FALSE))
---------------------------------------------------------------------------------------------------------------------

#Different scale 
plot(111)
par(new=true)
plot(222)

