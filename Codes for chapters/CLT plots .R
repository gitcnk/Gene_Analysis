 # Code for negerating plots

# NBA

x1 <- read_csv('Codes for chapters/NBA2016_n20.csv')
x2 <- read_csv('Codes for chapters/NBA2016_n30.csv')
x3 <- read_csv('Codes for chapters/NBA2016_n50.csv')
x4 <- read_csv('Codes for chapters/NBA2016_n100.csv')

y1 <- x1$mean
y2 <- x2$mean
y3 <- x3$mean
y4 <- x4$mean

N<- 10000

g <- c(rep('n=20', N),rep('n=30', N),rep('n=50', N),rep('n=100', N))

mydata <- data.frame('mean' =c(y1,y2,y3,y4),
                     'group' = g)

write_csv(mydata, 'NBA2016_all_in_one.csv')


## END NBA

## US Colleges - Undergrad


x1 <- read_csv('Codes for chapters/colleges_UG_n20.csv')
x2 <- read_csv('Codes for chapters/colleges_UG_n30.csv')
x3 <- read_csv('Codes for chapters/colleges_UG_n50.csv')
x4 <- read_csv('Codes for chapters/colleges_UG_n100.csv')

y1 <- x1$mean
y2 <- x2$mean
y3 <- x3$mean
y4 <- x4$mean

N<- 10000

g <- c(rep('n=20', N),rep('n=30', N),rep('n=50', N),rep('n=100', N))

mydata <- data.frame('mean' =c(y1,y2,y3,y4),
                     'group' = g)

write_csv(mydata, 'Colleges_all_in_one.csv')

#### End Colleges


## Flight Delays.


x1 <- read_csv('Codes for chapters/FlightDelay_n300.csv')
x2 <- read_csv('Codes for chapters/FlightDelay_n600.csv')
x3 <- read_csv('Codes for chapters/FlightDelay_n1000.csv')
x4 <- read_csv('Codes for chapters/FlightDelay_n2000.csv')

y1 <- x1$mean
y2 <- x2$mean
y3 <- x3$mean
y4 <- x4$mean

N<- 10000

g <- c(rep('n=300', N),rep('n=600', N),rep('n=1000', N),rep('n=2000', N))

mydata <- data.frame('mean' =c(y1,y2,y3,y4),
                     'group' = g)

write_csv(mydata, 'FlightDelays_all_in_one2.csv')



