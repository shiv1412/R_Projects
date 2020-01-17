#reading test data file
getwd()
setwd("D:/Intermediate_Analytics/week 4 assignments")
project_data <- read.table(file="Data_mining_assignment_training_data_binaryform.csv", sep=",", header=TRUE)
head(project_data)
attach(project_data)
project_data


#head(project_data)
#class biasing 
table(project_data$is_rating_above_average)

#creating training and validation data sets 
#Reading the same csv file and creating separate variables for reading
#training,validation and testing data sets. Model will be build on training data set and 
# validation will be done on validation data set. As mentioned in the assignment.
training_data <- project_data[1:6728,]
validation_data <- project_data[3639:5755,]
test_data <- project_data[5756:6728,]


#creating training and test samples

input_ones <- project_data[which(training_data$is_rating_above_average==1),]
input_zeros <- project_data[which(training_data$is_rating_above_average==0),]

#for repeating of samples
set.seed(100)
#creating training data

input_ones_rating_rows <- sample(1:nrow(input_ones),0.5*nrow(input_ones))
input_zeros_rating_rows <- sample(1:nrow(input_zeros),0.5*nrow(input_zeros))

rating_ones <- input_ones[input_ones_rating_rows, ]

rating_zeros <- input_zeros[input_zeros_rating_rows, ]
ratingData <- rbind(rating_ones,rating_zeros)

#creating test data for training set

test_ones <- input_ones[-input_ones_rating_rows, ]
test_zeros <- input_zeros[-input_zeros_rating_rows, ]
testData <- rbind(test_ones,test_zeros)


#building of logic model 
logitMod <- glm(is_rating_above_average ~ rating ,data = testData,family = binomial("logit"),maxit=100)

summary(logitMod)

pred <- predict(logitMod,newdata = testData,type = "response")

y_pred_num <- ifelse(pred > 0.5, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- testData$is_rating_above_average
mean(y_pred == y_act)
#optimal predition probablity cutoff value
library(InformationValue)
optcutOff <-optimalCutoff(actuals = training_data$is_rating_above_average,predictedScores = training_data$is_rating_above_average,optimiseFor = "Both",returnDiagnostics = TRUE)
optcutOff 
#feeding validation data into the model and finding out accuracy using misclassification


fitted.results <- predict(logitMod,newdata = subset(validation_data),type = 'response')
fitted.results <- ifelse(fitted.results>.5,1,0)
fitted.results

#mis classification error 
misClasificError <- mean(fitted.results !=validation_data$is_rating_above_average)
print(paste('Accuracy',1-misClasificError))

#ROC plots for training and validation data
install.packages("ROCR")
library(ROCR)
p <- predict(logitMod,newdata = subset(validation_data),type = "response")
pr <- prediction(p,validation_data$is_rating_above_average)
prf <- performance(pr,measure = "tpr",x.measure = "fpr") 
plot(prf)

auc <-performance(pr,measure = "auc")
auc <- auc@y.values[[1]]
auc
  
#concordance data
data('validation_data')
Concordance(actuals = validation_data$is_rating_above_average,predictedScores = validation_data$is_rating_above_average)

#confusion matrix for 0's and 1's calculation and true positive and false postive cases

confusionMatrix(training_data$is_rating_above_average,sample(training_data$is_rating_above_average))
confusionMatrix(validation_data$is_rating_above_average,sample(validation_data$is_rating_above_average))
