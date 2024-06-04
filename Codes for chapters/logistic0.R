# Codes and scrath work for Logistic chapter.
library(tidyverse)
library(KernSmooth)
library(broom)
## Age and Heart Attacks (fake data, I made it up)

mydata <- read_csv(file = 'https://raw.githubusercontent.com/gitcnk/Data/master/Heart_Attacks.csv')


p <- ggplot(mydata) +
        aes( x = Age, y = Heart_Attack ) +
        geom_jitter(aes(col = Heart_Attack), height = 0.05) +
        geom_smooth(method = 'glm',
                    method.args = list(family = "binomial", maxit=100),
                    se = FALSE , 
                    fullrange = TRUE,
                    col = 'red')
p

logistic_data <- ggplot_build(p)$data[[2]][,c(1,2)]

status_table <- table( mydata$Heart_Attack, mydata$group )
status_proportions <- prop.table(status_table, margin = 2)

plot(x = seq(33,65, length.out = 31), y = status_proportions[2, ], 
     ylab = 'proportion of heart attacks',
     xlab = 'Age')



fit <- locpoly( x = mydata$Age, y = mydata$Heart_Attack, bandwidth = 0.4)
lines(fit, col = 'blue')
lines(logistic_data, col = 'red')




## MedSchool Admissions.

mydata <- read_csv(file = 'https://raw.githubusercontent.com/gitcnk/Data/master/MedGPA.csv')




ggplot(mydata) +
  aes( x = GPA, y = Acceptance, col = Accept) +
  geom_point()

## Note 1: Teach why the default category labels is not very good for the plot.  
##         Because 'A' comes before 'D' the plot does not convey the 
##         messeage effectively.  Big idea: Doing the little things right.

# Local averaging, hard cuts

workdata1 <- mydata %>% 
                 select(Accept, Acceptance, GPA)


workdata1$group <- cut(mydata$GPA, breaks = seq(3,4, by = 0.1))

Acceptance_table <- table( workdata1$Accept, workdata1$group )

Acceptance_proportions <- prop.table(Acceptance_table, margin = 1)

barplot(Acceptance_proportions, 
        col = c('green', 'red'),
        legend.text = TRUE,
        args.legend = list(bty = 'n', x ='topleft', ncol = 1),
        xlim = c(0,10))

## Local averaging, soft cuts (overlapping slices)

ggplot(mydata) +
  aes( x = GPA, y = Acceptance) +
  geom_point() +
  geom_smooth(se = FALSE)#, span = 0.9)

## Note 2: Coding to 0 and 1 help the smoothing function.
##         Explain the 'span' parameter.  It is not linked to the scale of the 
##         x directly, simply small values and large values govern the smoothing.


## Logistic Model

ggplot(mydata) +
  aes( x = GPA, y = Acceptance) +
  geom_point() +
  geom_smooth(se = FALSE) +
  geom_smooth(method = 'glm',
            method.args =list(family = "binomial", maxit=100),
            se = FALSE , 
            fullrange = TRUE,
            col = 'red')

### Election

mydata <- read_csv(file = 'https://raw.githubusercontent.com/gitcnk/Data/master/BushClinton1992.csv')
# vote = 1, means vote for Bush

## Ford Accidents

mydata <- read_csv(file = 'https://raw.githubusercontent.com/gitcnk/Data/master/FordTireAccidents.csv')


## South African Heart Data

mydata <- read_csv(file = 'https://raw.githubusercontent.com/gitcnk/Data/master/SouthAfrica_Heart_Disease.csv')


## Breast cancer

mydata <- read.csv('https://academics.hamilton.edu/mathematics/ckuruwit/Data/breastcancer2.csv')

workdata <- mydata %>% 
                select(radius, status, concave.points)

workdata$status_code <- recode(workdata$status, 'B' = 0, 'M' = 1)

# Local Averagin, hard cuts

workdata$group <- cut(workdata$radius, breaks = seq(5,30, by = 1))

status_table <- table( workdata$status, workdata$group )

status_proportions <- prop.table(status_table, margin = 2)

barplot(status_proportions, 
        col = c('green', 'red'),
        legend.text = TRUE,
        args.legend = list(bty = 'n', x ='topleft', ncol = 1),
        xlim = c(0,30),
        xlab = 'radius',
        ylab = 'proportion of cells')

plot(x = seq(5,29), y = status_proportions[2, ], 
     col = 'red',
     ylab = 'proportion of malignant cases',
     xlab = 'radius')

## Local Averaging, soft cuts (overlapping slices)

plot(workdata$radius, workdata$status_code)
fit <- locpoly( x = workdata$radius, y = workdata$status_code, bandwidth = 0.4)
lines(fit, col = 'red')


p1 <-ggplot(workdata) +
        aes( x = radius, y = status_code ) +
        geom_jitter(aes(col = status), height = 0.05) +
        geom_smooth(se = FALSE)

loess_data <- ggplot_build(p1)$data[[2]][,c(1,2)]
plot(x = seq(5,29), y = status_proportions[2, ], 
     col = 'red',
     ylab = 'proportion of malignant cases',
     xlab = 'radius',
     ylim = c(0,1.1)) 
  

lines(loess_data, col = 'blue')


## Logistic Model

p2 <- ggplot(workdata) +
        aes( x = radius, y = status_code ) +
        geom_jitter(aes(col = status), height = 0.05) +
        geom_smooth(se = FALSE) +
        geom_smooth(method = 'glm',
                    method.args = list(family = "binomial", maxit=100),
                    se = FALSE , 
                    fullrange = TRUE,
                    col = 'red')


logistic_data <- ggplot_build(p2)$data[[3]][,c(1,2)]

plot(x = seq(5,29), y = status_proportions[2, ], 
     col = 'red',
     ylab = 'proportion of malignant cases',
     xlab = 'radius',
     ylim = c(0,1.1)) 


lines(logistic_data, col = 'red')

p1
p2


prop_data <- data.frame(x = seq(5,29), y = status_proportions[2, ])


## All in one

ggplot() +
  geom_point( data = prop_data, aes(x = x, y = y)) +
  geom_jitter( data = workdata, aes(x = radius, y = status_code, col = status), height = 0.05) +
  geom_line( data = logistic_data, aes( x = x, y = y), col = 'red') +
  geom_line( data = loess_data, aes( x = x, y = y), col = 'blue') +
  labs(title = 'Risk of breast cancer is related to the size (radius) of the cells',
       subtitle = 'Data source: UC Irvine data repository',
       x = 'Radius',
       y = 'Probability of malignancy')+
  ylim(c(0,1.1)) #+
  #theme(legend.position = 'none')


### Step by step

ggplot() +
  geom_point( data = prop_data, aes(x = x, y = y)) +
  geom_jitter( data = workdata, aes(x = radius, y = status_code, col = status), 
               height = 0.05,
               width = 0.1,
               alpha = 0.3) +
  labs(title = 'Risk of breast cancer is related to the size (radius) of the cells',
       subtitle = 'Data source: UC Irvine data repository',
       x = 'Radius',
       y = 'Probability of malignancy') +
  ylim(c(-0.1,1.1)) 


ggplot() +
  geom_point( data = prop_data, aes(x = x, y = y)) +
  geom_line( data = loess_data, aes( x = x, y = y), col = 'blue') +
  labs(title = 'Risk of breast cancer is related to the size (radius) of the cells',
       subtitle = 'Data source: UC Irvine data repository',
       x = 'Radius',
       y = 'Probability of malignancy') +
  ylim(c(-0.1,1.1)) 


knitr::kable(tidy(m1),format="html",digits=3)  
knitr::kable(tidy(m1),digits=3)
