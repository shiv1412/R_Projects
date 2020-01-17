#k-mean clustering
# libraries import
library(tidyverse)
library(cluster)
library(factoextra)
library(gridExtra)
#data set reading
getwd()
setwd("D:/Intermediate_Analytics")
final_project_data <- read.table(file="acs2015_census_tract_data.csv", sep=",", header=TRUE)
head(final_project_data)
attach(final_project_data)
final_project_data
#creating data frames
d_frame <- final_project_data
d_frame <- na.omit(d_frame)
d_frame <- scale(d_frame)
head(d_frame)
#applying k-mean algorithm
kmeans2 <- kmeans(d_frame,centers = 2,nstart = 20) 
str(kmeans2)
fviz_cluster(kmeans2, data = d_frame)
#creating for 3,4 and 5 centers cluster and plotting the k mean 
kmeans3 <- kmeans(d_frame, centers = 3, nstart = 20)
kmeans4 <- kmeans(d_frame, centers = 4, nstart = 20)  
kmeans5 <- kmeans(d_frame, centers = 5, nstart = 20)  
 #Comparing the Plots
fviz_cluster(kmeans2, geom = "point", data = d_frame) + ggtitle("k = 2")
fviz_cluster(kmeans3, geom = "point", data = d_frame) + ggtitle("k = 3")
fviz_cluster(kmeans4, geom = "point", data = d_frame) + ggtitle("k = 4")
fviz_cluster(kmeans5, geom = "point", data = d_frame) + ggtitle("k = 5")

plot1 <- fviz_cluster(kmeans2, geom = "point", data = d_frame) + ggtitle("k = 2")
plot2 <- fviz_cluster(kmeans3, geom = "point", data = d_frame) + ggtitle("k = 3")
plot3 <- fviz_cluster(kmeans4, geom = "point", data = d_frame) + ggtitle("k = 4")
plot4 <- fviz_cluster(kmeans5, geom = "point", data = d_frame) + ggtitle("k = 5")
grid.arrange(plot1, plot2, plot3, plot4, nrow = 2)

#validating the fittness of the model.
df <- scale(df[,1:7])
wss <- (nrow(df)-1)*sum(apply(df,2,var))
for(i in 1:5)wss[i]<- sum(fit=kmeans(df,centers=i,5)$withinss)
plot(1:5,wss,type="b",main="5 clusters",xlab="no. of cluster",ylab="with clsuter sum of squares")
fit <- kmeans(df,3)
fit
fit$withinss
fit$size
graphics.off()
plot(df,col=fit$cluster,pch=15)
points(fit$centers,col=1:8,pch=3)
library(cluster)
library(fpc)
plotcluster(df,fit$cluster)
points(fit$centers,col=1:8,pch=16)


# regression model
#install.packages("MASS")
#install.packages("ggplot2")
library(MASS)
library(ggplot2)
library(ggcorrplot)
setwd("D:/Intermediate_Analytics")
getwd()
RData <- read.table(file="acs2015_census_tract_data.csv", sep=",", header=TRUE)
head(RData)
attach(RData)
summary(RData)
#Regression model
RDatalm <- lm(TotalPop~Men+Women+Income+White+Black+Income+Employed+Unemployment,data=RData)
par(mfrow=c(1,3))
summary(RDatalm)
#ggplot
ggplot(RDatalm)+geom_point(data = RDatalm,aes(Income,TotalPop), colour = 'red', size = 3)
ggplot(RDatalm)+geom_point(data = RDatalm,aes(TotalPop,Men), colour = 'red', size = 3)
ggplot(RDatalm)+geom_point(data = RDatalm,aes(TotalPop,Women), colour = 'red', size = 3)
ggplot(RDatalm)+geom_point(data = RDatalm,aes(TotalPop,Employed), colour = 'red', size = 3)
ggplot(RDatalm)+geom_point(data = RDatalm,aes(TotalPop,White), colour = 'red', size = 3)
ggplot(RDatalm)+geom_point(data = RDatalm,aes(TotalPop,Black), colour = 'red', size = 3)
ggplot(RDatalm)+geom_point(data = RDatalm,aes(TotalPop,Unemployment), colour = 'red', size = 3)
#correlation plot
ggcorrplot(cor(RData))
