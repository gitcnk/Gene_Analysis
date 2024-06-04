# Codes for the CLT chapter

library(mosaic)
library(tidyverse)

#NBA2016 <- read_csv(file = 'https://raw.githubusercontent.com/gitcnk/Data/master/NBA2016.csv')

#NBA2018 <- read_csv(file = 'https://raw.githubusercontent.com/gitcnk/Data/master/NBA2018.csv')

FlightDelays <- read_csv(file = 'https://raw.githubusercontent.com/gitcnk/Data/master/FlightDelays2018.csv')

#colleges <- read_csv(file = 'https://raw.githubusercontent.com/gitcnk/Data/master/colleges2016.csv')
# Note there are about 5000 colleges and universities in th US, 
# but it is hard find complete data on all.  
# This dataset is created by removing any missing values
# in important variables like SAT, admin rate, etc.
# Data source : Mike Chapple, Notre Dame University.


#set.seed(1234)
nsim <- 1E4
n <- 100

#variable_of_interest <- NBA2018$Salary
variable_of_interest <- FlightDelays$`Departure delay (Minutes)`
#variable_of_interest <- colleges$undergrads


pop_mean <- mean(variable_of_interest)
mysamples <- matrix( 0, nrow = nsim, ncol = n)

mysamples <- do(nsim)*sample(variable_of_interest, size = n)

mymeans <- apply(mysamples, MARGIN = 1, FUN = mean)
#mySDs <- apply(mysamples, MARGIN = 1, FUN = sd)
#myMaxs <- apply(mysamples, MARGIN = 1, FUN = max)


myTstats <- apply(mysamples, MARGIN = 1, FUN = t.test )
myCIs <- sapply(myTstats, '[', 'conf.int')
lower_bounds <- sapply(myCIs, '[', 1 )
upper_bounds <- sapply(myCIs, '[', 2 )

capture<-  ifelse(lower_bounds < pop_mean , 
                  ifelse(upper_bounds > pop_mean, 1 , 0), 0)



left_tail_percentage <- mean(mymeans < pop_mean)

write.csv( cbind('mean' = mymeans,
                 'lower_bound' = lower_bounds,
                 'upper_bound' = upper_bounds,
                 'left_tail_percentage' = left_tail_percentage),
           file = paste('colleges_UG_n', n, '.csv', sep = ''))

ggplot() +
  geom_histogram(mapping = aes(x = mymeans)) #+
  #geom_vline(xintercept = pop_mean, col = 'red', linetype ='dashed')
  
left_tail_percentage

mean(capture)



########### Data Cleaning Steps for NBA2018 data ###########
# NBA_2018 <- read_csv(file = 'https://raw.githubusercontent.com/gitcnk/Data/master/NBA_Players_18_19.csv')
 
# x <- NBA_2018$SALARY
# 
# x1 <- gsub(pattern = ',',
#            replacement = '',
#            x = x)
# 
# 
# x2 <- gsub(pattern = 'Not signed',
#            replacement = 'NA',
#            x = x1)
# 
# x2 <- as.numeric(x2)
# summary(x2)
# 
# 
# NBA_2018$Salary <- x2
# 
# 
# NBA_2018_2 <- NBA_2018[,-c(4,9,10)]
# 
# summary(NBA2018$Salary)
# hist(NBA2018$Salary)
# 
# library(tidyverse)
# y <- NBA2018 %>% drop_na('Salary')
# write_csv(y, 'NBA2018.csv')
