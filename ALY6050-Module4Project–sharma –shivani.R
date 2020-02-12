#Libraries 
library(tigerstats)
library(EnvStats)
library(triangle)

#Variable initialization 
min_total_cost <- c()
order_amount <- c()
reorder_seq = seq(100, 1000, by =10)
#iteration for 1000 seq i.e sampling 1000 occurrances 
for (i in seq(1:1000)){
  demand = rtri(1000,14000,17000,15000)
  # initializing the variables 
  total_cost <- c(0)
  min_tot_cost <-  c(9999999)
  order_amount <- c(9999999)
  annual_no_of_orders <-c(99999999)
  cost_per_unit <- c(80)
  #calculation of cost
  average_inventory = reorder_seq/2
  holding_cost=0.18*cost_per_unit*average_inventory
  order_cost  <- ((demand/i) *220)
  for (i in reorder_seq) 
    {
    total_cost = holding_cost + order_cost
    if(total_cost <= min_tot_cost)
      {
      min_tot_cost = total_cost
      order_amount = ((demand/i)*220)
      annual_no_of_orders = demand/i
    }
  }

}
#appending the parameters to the list
total_cost <- c(total_cost)
print(total_cost)
min_tot_cost <- c(min_tot_cost)
print(min_tot_cost)
order_amount <- c(order_amount)
print(order_amount)
annual_no_of_orders <- c(round(annual_no_of_orders))
print(annual_no_of_orders)
#information for 1000 occurrances

demands<- cbind(demand,order_amount,annual_no_of_orders,total_cost)
demands

#graphical represenation for the above parameters using histogram and density plots
hist(min_tot_cost,col='blue')
#or
d1 <- density(min_tot_cost)
plot(d1)
polygon(d1,col='yellow',border = 'blue')

hist(order_amount,col='blue')
d2 <- density(order_amount)
plot(d2)
polygon(d2,col='yellow',border = 'blue')

hist(annual_no_of_orders,col='blue')
d3 <- density(annual_no_of_orders)
plot(d3)
polygon(d3,col='yellow',border = 'blue')


#Probablity distribution for minimum total cost
chisq_low_cost <- chisq.test(min_tot_cost)
chisq_low_cost
library(lattice)
histogram(min_tot_cost, breaks=20,col='blue')
x <- order_amount
curve(pnorm(x,mean(x),sd(x)),from=20,to=120,main="Normal distribution for minimum total cost")

#Probablity distribution for order quantity
chisq_order_quantity <- chisq.test(order_amount)
chisq_order_quantity
library(lattice)
histogram(order_amount, breaks=20,col='blue')
x <- order_amount
curve(pnorm(x,mean(x),sd(x)),from=20,to=120,main="Normal distribution for order amount")


#Probablity distribution for annual number of orders
chisq_annual_no_of_orders <- chisq.test(annual_no_of_orders)
chisq_annual_no_of_orders
library(lattice)
histogram(annual_no_of_orders, breaks=20,col='blue')
x <- annual_no_of_orders
curve(dnorm(x,mean(x),sd(x)),from=20,to=120,main="Normal distribution for annual no of orders")


#minimum values for each occurrence
min(min_tot_cost)
min(annual_no_of_orders)
min(order_amount)