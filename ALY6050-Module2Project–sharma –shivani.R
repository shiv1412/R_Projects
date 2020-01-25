#install.packages('tigerstats')
library(tigerstats)
# Part a: calculating the average number of victims for each hospital. In this part I have calculated
# both mean and standard deviation for the number of victims in each hospital
#install.packages("EnvStats")
library(EnvStats)
r<-round(rtri(5000,20,300,80),0)
  avg_bethisrail <- c(r*0.3)
  avg_bethisrail
  mean1<- mean(avg_bethisrail)
  sd1 <- sd(avg_bethisrail)
  
  avg_tuffmed_center <- c(r*0.15)
  avg_tuffmed_center
  mean2 <- mean(avg_tuffmed_center)
  sd2 <- sd(avg_tuffmed_center)
  
  avg_mass_gen <- c(r*0.2)
  avg_mass_gen
  mean3 <- mean(avg_mass_gen)
  sd3 <- sd(avg_mass_gen)
  
  avg_bost_med <- c(r*0.25)
  avg_bost_med
  mean4 <- mean(avg_bost_med)
  sd4 <-sd(avg_bost_med)
  
  avg_brigh_wom <- c(r*0.10)
  avg_brigh_wom
  mean5 <- mean(avg_brigh_wom)
  sd5 <- sd(avg_brigh_wom)
  
  mean1
  mean2
  mean3
  mean4
  mean5
  # part b average total time (in hours) required to transport the victims for each hospital
  
    library(triangle)
    s <- rtriangle(n = 5000, a = 20, b = 300, c = 80)
    Beth_Med_time = c()
    Tuff_Med_time= c()
    Avg_mass_general=c()
    Avg_boston_medical=c()
    Avg_bringham_women=c()
    for (i in 1:5000){
      Beth_Med_time <- append(Beth_Med_time,((avg_bethisrail[i]/2)*7)/60)
      Tuff_Med_time <- append(Tuff_Med_time,((avg_tuffmed_center[i]/2)*10)/60)
      Avg_mass_general <- append(Avg_mass_general,((avg_mass_gen[i]/2)*15)/60)
      Avg_boston_medical <- append(Avg_boston_medical,((avg_bost_med[i]/2)*15)/60)
      Avg_bringham_women <- append(Avg_bringham_women,((avg_brigh_wom[i]/2)*20)/60)
      i=i+1
      }
  m1 <- mean(Beth_Med_time)
  m2 <- mean(Tuff_Med_time)
  m3 <- mean(Avg_mass_general)
  m4 <- mean(Avg_boston_medical)
  m5 <- mean(Avg_bringham_women)
  
  sd_time1 <- sd(Beth_Med_time)
  sd_time2 <- sd(Tuff_Med_time)
  sd_time3 <- sd(Avg_mass_general)
  sd_time4 <- sd(Avg_boston_medical)
  sd_time5 <- sd(Avg_bringham_women)

  print(paste('Average time for Beth Israel Medical is',m1))
  print(paste('Average time for Tufts Medical is',m2))
  print(paste('Average time for Massachusetts General is',m3))
  print(paste('Average time for Boston Medical is',m4))
  print(paste('Average time for Brigham and Women's is',m5))
  #part c: representation of law of large number for Beth israel medical hospital
  options(width = 100)
  library(ggplot2)
  library(scales)
  cum_sum = cumsum(avg_bethisrail)
  index = c(1:5000)
  length(cum_sum)
  cum_mean = cum_sum/index
  plot(index, cum_mean) +
    geom_line(colour = "blue") +
    abline(h=mean1, col = 'blue') 
   #part d: Exploratory data analysis on transportation time for beth Israel Medical 
  n= length(Beth_Med_time)
  conf_er <- qnorm((1+0.95)/2)*(sd_time1/(sqrt(n)))
  l <- m1 - conf_er
  r <- m1 + conf_er
    conf_interval <- r - l
    conf_interval
    
    min = min(Beth_Med_time)
    max = max(Beth_Med_time)
    
    diff = max - min
    bin_diff = diff/12
    h <- seq(min(Beth_Med_time),ceiling(max(Beth_Med_time)),by=bin_diff)
    h_cut <- data.frame(table(cut(Beth_Med_time,breaks = h, w=2)))
    h_cut$relfreq <- h_cut$Freq / sum(h_cut$Freq)
    barplot(h_cut$relfreq)
    expected <- pnorm(h_cut$relfreq,mean(h_cut$relfreq),sd(h_cut$relfreq))
    
    chisq <- chisq.test(h_cut$Var1,expected)
    chisq
    
    # part e : average transport time per victim for entire process of transporting all the victims
    tot_time_in_minutes = c()
    for (i in 1:5000){
      tot_time_in_minutes <- append(tot_time_in_minutes,((Beth_Med_time+Tuff_Med_time+Avg_mass_general+Avg_boston_medical+ Avg_bringham_women)*60)/mean(s))
      i=i+1
    }
    tot_time <- tot_time_in_minutes*60
    library(psych)
    describe(tot_time)
    hist(tot_time,col='blue')
    n= length(tot_time)
    mean1_tot <- m1+m2+m3+m4+m5
    sd_total_1 <- sd_time1+sd_time2+sd_time3+sd_time4+sd_time5
    sd_total_1
    conf_er <- qnorm((1+0.95)/2)*(sd_total_1/(sqrt(n)))
    l <- mean1_tot - conf_er
    r <- mean1_tot + conf_er
    conf_interval <- r - l
    conf_interval
    
    min = min(tot_time)
    max = max(tot_time)
    
    diff = max - min
    bin_diff = diff/12
    h <- seq(min(tot_time),ceiling(max(tot_time)),by=bin_diff)
    h_cut <- data.frame(table(cut(tot_time,breaks = h, w=2)))
    h_cut$relfreq <- h_cut$Freq / sum(h_cut$Freq)
    barplot(h_cut$relfreq)
    expected <- pnorm(h_cut$relfreq,mean(h_cut$relfreq),sd(h_cut$relfreq))
    
    chisq <- chisq.test(h_cut$Var1,expected)
    chisq
    
    
# Question 2
    #install.packages('tigerstats')
    library(tigerstats)
    # Part a: calculating the average number of victims for each hospital. In this part I have calculated
    # both mean and standard deviation for the number of victims in each hospital
    #install.packages("EnvStats")
    library(EnvStats)
    r1<-rnorm(5000,mean=150,sd=50)
    avg_bethisrail2 <- c(r1*0.3)
    avg_bethisrail2
    mean1_2<- mean(avg_bethisrail2)
    sd1_2 <-  sd(avg_bethisrail2)
    
    avg_tuffmed_center2 <- c(r1*0.15)
    avg_tuffmed_center2
    mean2_2 <- mean(avg_tuffmed_center2)
    sd2_2 <- sd(avg_tuffmed_center2)
    
    avg_mass_gen2 <- c(r1*0.2)
    avg_mass_gen2
    mean3_2 <- mean(avg_mass_gen2)
    sd3_2 <- sd(avg_mass_gen2)
    
    avg_bost_med2 <- c(r1*0.25)
    avg_bost_med2
    mean4_2 <- mean(avg_bost_med2)
    sd4_2 <-sd(avg_bost_med2)
    
    avg_brigh_wom2 <- c(r1*0.10)
    avg_brigh_wom2
    mean5_2 <- mean(avg_brigh_wom2)
    sd5_2 <- sd(avg_brigh_wom2)
    # part b average total time (in minutes) required to transport the victims for each hospital
    s1 <- rnorm(5000,mean=150,sd=50)
    Beth_Med_time2 = c()
    Tuff_Med_time2= c()
    Avg_mass_general2=c()
    Avg_boston_medical2=c()
    Avg_bringham_women2=c()
    for (i in 1:5000){
      Beth_Med_time2 <- append(Beth_Med_time2,((avg_bethisrail2[i]/2)*7))
      Tuff_Med_time2 <- append(Tuff_Med_time2,((avg_tuffmed_center2[i]/2)*10))
      Avg_mass_general2 <- append(Avg_mass_general2,((avg_mass_gen2[i]/2)*15))
      Avg_boston_medical2 <- append(Avg_boston_medical2,((avg_bost_med2[i]/2)*15))
      Avg_bringham_women2 <- append(Avg_bringham_women2,((avg_brigh_wom2[i]/2)*20))
      i=i+1
    }
    m1_2 <- mean(Beth_Med_time2)
    m2_2 <- mean(Tuff_Med_time2)
    m3_2 <- mean(Avg_mass_general2)
    m4_2 <- mean(Avg_boston_medical2)
    m5_2 <- mean(Avg_bringham_women2)
    
    sd_time1_2 <- 2
    sd_time2_2 <- 4
    sd_time3_2 <- 3
    sd_time4_2 <- 5
    sd_time5_2 <- 3
    
    print(paste('Average time for Beth Israel Medical is',m1_2))
    print(paste('Average time for Tufts Medical is',m2_2))
    print(paste('Average time for Massachusetts General is',m3_2))
    print(paste('Average time for Boston Medical is',m4_2))
    print(paste('Average time for Brigham and Women's is',m5_2))
    #part c: representation of law of large number for Beth israel medical hospital
    options(width = 100)
    library(ggplot2)
    library(scales)
    cum_sum = cumsum(avg_bethisrail2)
    index = c(1:5000)
    length(cum_sum)
    cum_mean = cum_sum/index
    plot(index, cum_mean) +
      geom_line(colour = "blue") +
      abline(h=mean1_2, col = 'blue') 
    #part d: Exploratory data analysis on transportation time for beth Israel Medical 
    n= length(Beth_Med_time2)
    conf_er2 <- qnorm((1+0.95)/2)*(sd_time1_2/(sqrt(n)))
    l2 <- m1_2 - conf_er2
    r2 <- m1_2 + conf_er2
    conf_interval2 <- r2 - l2
    conf_interval2
    
    min2 = min(Beth_Med_time2)
    max2= max(Beth_Med_time2)
    
    diff2 = max2 - min2
    bin_diff2 = diff2/12
    h2 <- seq(min(Beth_Med_time2),ceiling(max(Beth_Med_time2)),by=bin_diff2)
    h_cut2 <- data.frame(table(cut(Beth_Med_time2,breaks = h2, w=2)))
    h_cut2$relfreq <- h_cut2$Freq / sum(h_cut2$Freq)
    barplot(h_cut2$relfreq)
    expected2 <- pnorm(h_cut2$relfreq,mean(h_cut2$relfreq),sd(h_cut2$relfreq))
    
    chisq2 <- chisq.test(h_cut2$Var1,expected2)
    chisq2
    
    # part e : average transport time per victim for entire process of transporting all the victims
    tot_time_in_minutes2 = c()
    for (i in 1:5000){
      tot_time_in_minutes2 <- append(tot_time_in_minutes2,((Beth_Med_time2+Tuff_Med_time2+Avg_mass_general2+Avg_boston_medical2+ Avg_bringham_women2))/mean(s1))
      i=i+1
    }
    tot_time2 <- tot_time_in_minutes2
    library(psych)
    describe(tot_time2)
    hist(tot_time2,col='blue')
    n2= length(tot_time2)
    mean2_tot <- m1_2+m2_2+m3_2+m4_2+m5_2
    mean2_tot
    sd_total_2 <- sd_time1_2+sd_time2_2+sd_time3_2+sd_time4_2+sd_time4_2
    sd_total_2
    conf_er2 <- qnorm((1+0.95)/2)*(sd_total_2/(sqrt(n2)))
    l <- mean2_tot - conf_er
    r <- mean2_tot + conf_er
    conf_interval2 <- r - l
    conf_interval2
    
    min2 = min(tot_time2)
    max2 = max(tot_time2)
    
    diff2 = max2 - min2
    bin_diff2 = diff2/12
    h2 <- seq(min(tot_time2),ceiling(max(tot_time2)),by=bin_diff2)
    h_cut2 <- data.frame(table(cut(tot_time2,breaks = h2, w=2)))
    h_cut2$relfreq <- h_cut2$Freq / sum(h_cut2$Freq)
    barplot(h_cut2$relfreq)
    expected2 <- pnorm(h_cut2$relfreq,mean(h_cut2$relfreq),sd(h_cut2$relfreq))
    
    chisq <- chisq.test(h_cut2$Var1,expected2)
    chisq
    
    