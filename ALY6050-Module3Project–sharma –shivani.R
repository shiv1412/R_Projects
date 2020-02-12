#question 1: moving average forecast
setwd("D:/Intro to Enterprise")
getwd()
#install.packages("readxl") for reading the excel file
library(readxl)
read_honeywell_file <- read_excel("Honeywell.xlsx")
read_honeywell_file
library(tidyverse)
#install.packages("lubridate")
library(lubridate)
#install.packages("fpp2")
library(fpp2)
#install.packages("zoo")
library(zoo)
library(fpp2)
library(forecast)
#setting the start date into a varia ble to read further
start_date1 <- as.Date('2017-10-16')
end_date1 <- as.Date('2018-04-13')
data <- read_honeywell_file
time_series <- ts(data$Close, start=start_date1,frequency=365)
time_series

plot(time_series)
#applying the ts function to the data set  setting the start date as 3 days before
tsdata <- ts(time_series, frequency=365, end = end_date1)
tsdata
plot(tsdata)


#converting the time  series function into the vector form to provide input ot movavg function
data(tsdata)
AP <- tsdata    
class(AP)
# returns "ts"
AP1 <- as.numeric(AP)
# applying moving average function for forecasting the values for 16 april 2018
#install.packages("pracma")
library(pracma)
#I am taking MA for the past 3 days 
movavg1 <- movavg(AP1,3,type=c("s"))
movavg1
plot(movavg1,col='blue')
movavg2 <- movavg(AP1,3,type=c("t"))
movavg2
plot(movavg2,col='blue')
movavg3 <- movavg(AP1,3,type=c("w"))
movavg3
plot(movavg3,col='blue')
movavg4 <- movavg(AP1,3,type=c("m"))
movavg4
plot(movavg4,col='blue')
movavg5 <- movavg(AP1,3,type=c("e"))
movavg5
plot(movavg5,col='blue')
movavg6 <- movavg(AP1,3,type=c("r"))
movavg6
plot(movavg6,col='blue')

# finding MSEs for each moving average values
#install.packages("hydroGOF")
library(hydroGOF)

m1 <-mse(movavg1,tsdata)
m1
m2 <- mse(movavg2,tsdata)
m2
m3 <- mse(movavg3,tsdata)
m3
m4 <- mse(movavg4,tsdata)
m4
m5 <- mse(movavg5,tsdata)
m5
m6 <- mse(movavg6,tsdata)
m6

# Question 2 : Exponenetial Smoothing with ses function 
#install.packages("forecast")
# Create Training for developing the model and Test data for validating the model before we build the model
# setting seed to reproduce results of random sampling
set.seed(1234)
test_data_set.df<- data.frame(read_honeywell_file)
test_data_set.df
# indexing for training data
training_data_index_values <- sample(1:nrow(test_data_set.df), 0.7*nrow(test_data_set.df))  
# training data set
training_data_set <- test_data_set.df[training_data_index_values , ]
training_data_set
# test data set
test_data_set  <- test_data_set.df[-training_data_index_values , ] 
test_data_set

#MSE values for each of the alpha value provided and plotting of the trend for both training and testing data sets
library(forecast)
fit_ques2_1 <- ses(training_data_set$Close, alpha = 0.15,level = c(80,95), h = 1,start=start_date1,end=end_date1)
summary(fit_ques2_1)
autoplot(fit_ques2_1)

fit_ques2_1_2 <- ses(test_data_set$Close, alpha = 0.15, h = 1,start=start_date1,end=end_date1)
summary(fit_ques2_1_2)
autoplot(fit_ques2_1_2)

fit_ques2_2 <- ses(training_data_set$Close, alpha = 0.35, h = 1,start=start_date1,end=end_date1)
summary(fit_ques2_2)
autoplot(fit_ques2_2)

fit_ques2_2_2 <- ses(test_data_set$Close, alpha = 0.35, h = 1,start=start_date1,end=end_date1)
summary(fit_ques2_2_2)
autoplot(fit_ques2_2_2)


fit_ques2_3 <- ses(training_data_set$Close, alpha = 0.55, h = 1,start=start_date1,end=end_date1)
summary(fit_ques2_3)
autoplot(fit_ques2_3)
fit_ques2_3_2 <- ses(training_data_set$Close, alpha = 0.55, h = 1,start=start_date1,end=end_date1)
summary(fit_ques2_3_2)
autoplot(fit_ques2_3_2)


fit_ques2_4 <- ses(training_data_set$Close, alpha = 0.75, h = 1,start=start_date1,end=end_date1)
summary(fit_ques2_4)
autoplot(fit_ques2_4)

fit_ques2_4_2 <- ses(training_data_set$Close, alpha = 0.75, h = 1,start=start_date1,end=end_date1)
summary(fit_ques2_4_2)
autoplot(fit_ques2_4_2)

#comparison of accuracy for both training and test data set calculated values for RMSE
ac1_1 <- accuracy(fit_ques2_1)
ac1_1
ac1_2 <- accuracy(fit_ques2_1_2)
ac1_2
ac1_3 <- accuracy(fit_ques2_2)
ac1_3
ac1_4 <- accuracy(fit_ques2_2_2)
ac1_4
ac1_5 <- accuracy(fit_ques2_3)
ac1_5
ac1_6 <- accuracy(fit_ques2_3_2)
ac1_6
ac1_7 <- accuracy(fit_ques2_4)
ac1_7
ac1_8 <- accuracy(fit_ques2_4_2)
ac1_8

library(tidyverse)
alpha_fit<- data_frame(alpha= 0.75,ac1_5)
alpha_fit

# alpha_min<- filter(alpha_fit, ac1_5 ==min(ac1_5))
# alpha_min
# 
# ggplot(alpha_fit, aes(alpha= 0.75 , ac1_5))#+ geom_line()+geom_point(alpha_min, aes( 0.75, acc),size=2 , color="blue")

ts_diff <- diff(tsdata)
alpha<-seq(0.15,0.75, by = 0.15)
RMSE<- c()
for (i in alpha){
  fit<- ses(ts_diff, alpha[i],h = 25 )
  RMSE[i]<- accuracy(fit)
  print(RMSE)
}



library(tidyverse)
# convert to a data frame and idenitify min alpha value
alpha.fit_ques2_1 <- data_frame(alpha1, RMSE)
alpha.min <-filter(alpha.fit_ques2_1, RMSE == min(RMSE))

ggplot(alpha.fit_ques2_1, aes(alpha, RMSE)) +
  geom_line() +
  geom_point(data = alpha.min, aes(alpha, RMSE), size = 2, color = "blue") 


#Question 3 Exponential smoothing  with holt function 
fit_ques3_1 <- holt(training_data_set$Close,alpha = 0.75,beta = 0.15,h = 3,start=start_date,end=end_date)
summary(fit_ques3_1)
autoplot(fit_ques3_1)

fit_ques3_1_2 <- holt(test_data_set$Close,alpha = 0.75,beta = 0.15,h = 3,start=start_date,end=end_date)
summary(fit_ques3_1_2)
autoplot(fit_ques3_1_2)

fit_ques3_2 <- holt(training_data_set$Close,alpha = 0.75,beta = 0.25,h = 3,start=start_date,end=end_date)
summary(fit_ques3_2)
autoplot(fit_ques3_2)

fit_ques3_2_2 <- holt(test_data_set$Close,alpha = 0.75,beta = 0.25,h = 3,start=start_date,end=end_date)
summary(fit_ques3_2_2)
autoplot(fit_ques3_2_2)

fit_ques3_3 <- holt(training_data_set$Close,alpha = 0.75,beta = 0.45,h = 3,start=start_date,end=end_date)
summary(fit_ques3_3)
autoplot(fit_ques3_3)

fit_ques3_3_2 <- holt(test_data_set$Close,alpha = 0.75,beta = 0.45,h = 3,start=start_date,end=end_date)
summary(fit_ques3_3_2)
autoplot(fit_ques3_3_2)

#ignored value
# fit_ques3_4 <- holt(read_honeywell_file$Close,alpha = 0.75,beta = 0.85,h = 3)
# summary(fit_ques3_4)
# autoplot(fit_ques3_4)

 
#ignored value
#accuracy(fit_ques3_4)