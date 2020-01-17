#Problem 1
r<- runif(1000) #generating 1000 random numbers
x<- -log(r,exp(1)) #generating the values of X
x

#relative frequency histogram
#install.packages("HistogramTools")
library(HistogramTools)
PlotRelativeFrequency(hist(x, plot=F),col='blue')

#probablity distribution 
#install.packages("lattice")
library(lattice)
histogram(x, breaks=20,col='blue')

#probablity curve plot
curve(dexp(x, 1), 0, 6,col='blue')

#chi squared test 
bks <-c(0, 1,2,3,4,5,6,7,8,9)
x1_distribution<- table(cut(x, breaks = bks))
x1_distribution

p_dist <-c()

for (i in 1:9)
  
{  
  
  p_dist<-c(p_dist, (pexp(i,1) - pexp(i-1,1))*1000)
  p_dist
  
}

chtest <- chisq.test(p_dist, x1_distribution)
chtest


#Problem 2
r1<- runif(10000) #generating 10000 random numbers
r2<- runif(10000)
r3<- runif(10000)
x<- -log(r1*r2*r3) #generating the values of X
x
#relative frequency histogram
#install.packages("HistogramTools")
library(HistogramTools)
PlotRelativeFrequency(hist(x, plot=F),col='blue')

#probablity distribution 
#install.packages("lattice")
library(lattice)
histogram(x, breaks=10,col='blue')

#probablity curve plot
curve(dexp(x, 1), 0, 20,col='blue')

#chi-squared test
bks <-c(0, 1,2,3,4,5,6,7,8,9)
x_distribution<- table(cut(x, breaks = bks))
x_distribution
p_dist <-c()
for (i in 1:9)
{  
  p_dist<-c(p_dist, (pexp(i,1) - pexp(i-1,1))*1000)
  p_dist
}

chtestresult <-chisq.test(p_dist, x_distribution)
chtestresult

# gamma test
h <- seq(min(x),ceiling(max(x)),by=1)
length(h)
h_cut <- data.frame(table(cut(x,breaks = h)))
h_cut
h_cut$relfreq <- h_cut$Freq / sum(h_cut$Freq)
h_cut$relfreq
m<- mean(x) 
v<- var(x) 
alpha.est<- m/v
alpha.est 
beta.est<-m^2/v
beta.est 
d_x <- dgamma(h_cut$relfreq,shape = 4,scale = 2)
plot(d_x,type='l',col='blue',lwd=2)
d_x

#Problem 3
r1<- runif(1000) 
x1<- -log(r1,exp(1)) 
x1
r2<- runif(1000)
x2<- -log(r2,exp(1))
x2
i=1
Y <- c()
while(i<=1000){
  k = ((x1-1)^2)/2
  if (x2[i] >= k[i])
  {
    r  <-  runif(1,0,1)
    if (r > 0.5)
    {
      Y[i] = x1[i]
    }
    else{
      Y[i] = -x1[i]
    }
    i = i + 1
  }
  else {
    i = i + 1
  }
}
length(Y)
new_Y<- na.omit(Y)
length(new_Y)
new_Y
# k=((x1-1)^2)/2
# 
# for (i in 1:1000) 
# {
#   if(x2>k)
#   {
# {
#   
#   if (x2 >=k) 
#   {
#     r<- runif(1)
#     r
#     
#   }
#   else if (r > 0.5) {
#       Yset <- c(Yset, x1)
#       Yset
#     }
#     else{
#       Yset <- c(Yset, x2)
#       Yset
#     }
# }
#   }
# 
# else if (x2<k)
# {
#   break ; 
# }
# }

#relative frequency histogram
#install.packages("HistogramTools")
library(HistogramTools)
PlotRelativeFrequency(hist(new_Y, plot=F),col='blue')

#probablity distribution 
#install.packages("lattice")
library(lattice)
histogram(new_Y, breaks=20,col='blue')
x <- new_Y
curve(dnorm(x,mean(x),sd(x)),from=20,to=120,main="Normal distribution")

#chi-squared test
bks <-c(0, 1,2,3,4,5,6,7,8,9)
Y_distribution<- table(cut(new_Y, breaks = bks))
Y_distribution
p_dist <-c()

for (i in 1:9)
  
{  
  
  p_dist<-c(p_dist, (pexp(i,1) - pexp(i-1,1))*1000)
  p_dist
  
}
chitest_prob3 <- chisq.test(p_dist, Y_distribution)
chitest_prob3


#Problem 4
i = 1
N =c()
while(i<=100){
  r1 = runif(1000,0,1)
  r2 = runif(1000,0,1)
  x1 = -log(r1)
  x2 = -log(r2)
  j = 1
  Y = c()
  new_Y = c()
  while(j<=1000)
  {
    k = ((x1-1)^2)/2
    if (x2[j] >= k[j]){
      
      r  <-  runif(1,0,1)
      
      if (r > 0.5){
        
        Y[j] = x1[j]
      }
      else{
        
        Y[j] = -x1[j]
      }
      j = j + 1
    }
    else {
      j = j + 1
    }
    new_Y <- na.omit(Y)
  }
  new[i] <- length(new_Y)
  N <- c(N,new[i])
  i = i + 1
}
M = 1000
W = M/N
N
W
sd(W)

#probablity distribution 
#install.packages("lattice")
library(lattice)
histogram(W, breaks=20,col='blue')
x <- W
curve(dnorm(x,mean(x),sd(x)),from=20,to=120,main="Normal distribution")

#probablity distribution 
#install.packages("lattice")
library(lattice)
histogram(x, breaks=10,col='blue')

#chi-squared test
bks <-c(0,1,2,3,4,5,6,7,8,9)
W_distribution<- table(cut(x, breaks = bks))
W_distribution
p_dist <-c()
for (j in 1:9)
{  
  p_dist<-c(p_dist, (pexp(j,1) - pexp(j-1,1))*1000)
  p_dist
}
chitest_prob4 <- chisq.test(p_dist, W_distribution)
chitest_prob4

#problem 4 4th part
Y
m_part4 <- c(10,20,30,40,50,60,70,80,90,100,200,300,400,500,600,700,800,900,1000)
w_part4 <- c()
for(j in m_part4){
  w_part4[j] <- j / (j-sum(is.na(Y[1:j])))
  print(w_part4[j])
}

mean(W)
