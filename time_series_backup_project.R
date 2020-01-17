#Part A
rain <- scan("http://robjhyndman.com/tsdldata/hurst/precip1.dat",skip=1)

rainseries <- ts(rain,start=c(1813),frequency = 12)
rainseries

# in order to fix the issue Error in plot.new() : figure margins too large I used the below function
graphics.off()
plot(rainseries)

start(rainseries)
end(rainseries)
frequency(rainseries)
rainseries.subset <-window(rainseries,start=c(1813,5),end=c(1821,5))
rainseries.subset
library(forecast)
#decomposition 
# in order to fix the issue Error in plot.new() : figure margins too large I used the below function
graphics.off()
plot(rainseries.subset)
irainseries <- log(rainseries)
# in order to fix the issue Error in plot.new() : figure margins too large I used the below function
graphics.off()

plot(irainseries,ylab="log(rainseries)")
fit <- stl(irainseries,s.window = "period")
# in order to fix the issue Error in plot.new() : figure margins too large I used the below function
graphics.off()
plot(fit)


par(mfrow=c(2,1))
library(forecast)
# in order to fix the issue Error in plot.new() : figure margins too large I used the below function
graphics.off()
seasonplot(rainseries,12,col=rainbow(12))

#seasnal decompostion of Data and smoothening of data
rainseries_components <- decompose(rainseries)
# in order to fix the issue Error in plot.new() : figure margins too large I used the below function
graphics.off()

plot(rainseries_components)
rainseries_adjusted <- rainseries - rainseries_components$seasonal
# in order to fix the issue Error in plot.new() : figure margins too large I used the below function
graphics.off()
plot(rainseries_adjusted,type="l",col=rainbow(1))
seasonplot(rainseries_adjusted, 12, col=rainbow(12))

#finding fitness of the model and geeting this to the forecast method and
#then to accuracy
fit <- ets(rainseries,model="ANN")
fit
forecast(fit,1)
# in order to fix the issue Error in plot.new() : figure margins too large I used the below function
graphics.off()
plot(forecast(fit))
rainseriesforecasts <- HoltWinters(rainseries, beta=FALSE, gamma=FALSE)
rainseriesforecasts

#Part B
#Durbin watson test
#library(tseries)
#library(lmtest)
getwd()
setwd("D:/Intermediate_Analytics")
tms_part_b <- read.csv(file="Sales_data_timeseries_part_b_assognment.csv", sep=",", header=TRUE)
head(tms_part_b)
attach(tms_part_b)
tms_part_b
dwtest(lm(Month ~ Year1,data=tms_part_b))

#ARIMA Model building and forecasting
# Since ARIMA model is univariant so we need to read the Year1 column representing 
#sales data only. We have 2 ways either reading the column 2 or create 2 different
#csv. I tried both and are working fine. However in order to make a clean represen
#tation of data and keeping different variables to store the dataset I choose to
# present the second method i.e using 2 different csv's
getwd()
setwd("D:/Intermediate_Analytics")
tms_part_b2 <- read.csv(file="Sales_data_timeseries_part_b_assognment_part2.csv", sep=",", header=TRUE)
head(tms_part_b2)
attach(tms_part_b2)
tms_part_b2

sales_data_for_arima <- ts(tms_part_b2)
sales_data_for_arima

sales_data_for_arimadiff1 <- diff(sales_data_for_arima, differences=1)
plot.ts(sales_data_for_arimadiff1)

# Finding correlogram
acf(sales_data_for_arimadiff1, lag.max=10)             
acf(sales_data_for_arimadiff1, lag.max=10, plot=FALSE) 

# Partial correlaogram 
pacf(sales_data_for_arimadiff1, lag.max=10)             
pacf(sales_data_for_arimadiff1, lag.max=10, plot=FALSE) 


#Builing ARIMA Model
sales_data_arima <- arima(tms_part_b2, order=c(0,1,1)) # fit an ARIMA(0,1,1) model
sales_data_arima
#forecasting
library(forecast)
sales_data_arima_forecasts <- forecast(sales_data_arima,h=5,level = c(99.5))
sales_data_arima_forecasts
plot(forecast(sales_data_arima_forecasts))


#validation of model 
acf(sales_data_arima_forecasts$residuals, lag.max=10)
Box.test(sales_data_arima_forecasts$resid,lag=5,type="Ljung-Box")
Box.test(sales_data_arima_forecasts$resid,lag=10,type="Ljung-Box")
acf(sales_data_arima_forecasts$residuals,lag.max = 10)

