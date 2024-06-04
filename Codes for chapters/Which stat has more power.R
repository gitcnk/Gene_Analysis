
# Which statistic has more power, (xbar - ybar) or (xbar/ybar)?

library(mosaic)
n1 <- 30
n2 <- 30
p_value <- 0

# Stat 1: difference in means

## Type 1 error

mu1 <- 10
mu2 <- 10

for(i in 1:100)
{
  print(i)
  x <- rnorm(n1, mean = mu1, sd = 1)
  y <- rnorm(n2, mean = mu2, sd = 1)
  
  
  mydata <- data.frame(value = c(x,y), group = c(rep('g1', n1), rep('g2',n2)))
  
  
  RD <- do(1000)*diffmean(value ~ shuffle(group) ,data = mydata)
  
  #hist(RD$diffmean, col = 'blue')
  observed_mean_difference <- mean(x) - mean(y)
  #abline(v = observed_mean_difference, col = 'red')
  
  p_value[i] <- mean(RD$diffmean >= observed_mean_difference )
  #p_value
  
  
}
mean(p_value < 0.05)


## Power

mu1 <- 12
mu2 <- 10

for(i in 1:100)
{
  print(i)
  x <- rnorm(n1, mean = mu1, sd = 1)
  y <- rnorm(n2, mean = mu2, sd = 1)
  
  observed_mean_difference <- mean(x) - mean(y)
  
  mydata <- data.frame(value = c(x,y), group = c(rep('g1', n1), rep('g2',n2)))
  
  
  RD <- do(1000)*diffmean(value ~ shuffle(group) ,data = mydata)
  
  #hist(RD$diffmean, col = 'blue')
  
  #abline(v = observed_mean_difference, col = 'red')
  
  p_value[i] <- mean(RD$diffmean >= observed_mean_difference )
  #p_value
  
  
}
mean(p_value < 0.05)


# Stat 2 : Ratio of the means

## Type 1 error
mu1 <- 10
mu2 <- 10

for(i in 1:10)
{
  print(i)
  x <- rnorm(n1, mean = mu1, sd = 1)
  y <- rnorm(n2, mean = mu2, sd = 1)
  
  observed_ratio <- mean(x) / mean(y)
  
  mydata <- data.frame(value = c(x,y), group = c(rep('g1', n1), rep('g2',n2)))
  
  ratio_of_means <- 0
  
  for(j in 1:1000)
  {
     temp <- shuffle(mydata$group)
     mydata$new_group <- temp
     
     mean_g1 <- as.numeric(filter(mydata, 
                                  new_group =='g1') %>% 
                                                        summarise(mean(value)))
     mean_g2 <- as.numeric(filter(mydata, 
                                  new_group =='g2') %>% 
                                                        summarise(mean(value)))
     
     ratio_of_means[j] <- mean_g1/mean_g2
  }
  
  
  p_value[i] <- mean(ratio_of_means >= observed_ratio )
  #p_value
  
  
}

mean(p_value < 0.05)



##########

## Power

mu1 <- 11
mu2 <- 10

for(i in 1:100)
{
  print(i)
  x <- rnorm(n1, mean = mu1, sd = 1)
  y <- rnorm(n2, mean = mu2, sd = 1)
  
  observed_ratio <- mean(x) / mean(y)
  
  mydata <- data.frame(value = c(x,y), group = c(rep('g1', n1), rep('g2',n2)))
  
  ratio_of_means <- 0
  
  for(j in 1:1000)
  {
    temp <- shuffle(mydata$group)
    mydata$new_group <- temp
    
    mean_g1 <- as.numeric(filter(mydata, 
                                 new_group =='g1') %>% 
                            summarise(mean(value)))
    mean_g2 <- as.numeric(filter(mydata, 
                                 new_group =='g2') %>% 
                            summarise(mean(value)))
    
    ratio_of_means[j] <- mean_g1/mean_g2
  }
  
  
  p_value[i] <- mean(ratio_of_means >= observed_ratio )
  #p_value
  
  
}

mean(p_value < 0.05)



