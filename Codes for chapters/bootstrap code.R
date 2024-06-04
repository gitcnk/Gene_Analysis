# Service time
library(ggplot2)
set.seed(1234)
n <- 30
nsim <- 1E4

pop_mean <- 20

mysample <- rexp(n = n , rate = 1/20)

my_boot_samples <- matrix( 0, nrow = nsim, ncol = n)

my_boot_samples <- do(nsim)*sample(mysample, size = n, replace = TRUE)

my_boot_means <- apply(my_boot_samples, MARGIN = 1, FUN = mean)


my_boot_Tstats <- apply(my_boot_samples, MARGIN = 1, FUN = t.test )
myCIs <- sapply(my_boot_Tstats, '[', 'conf.int')
boot_lower_bounds <- sapply(myCIs, '[', 1 )
boot_upper_bounds <- sapply(myCIs, '[', 2 )

# write.csv( cbind('mean' = my_boot_means,
#                  'lower_bound' = lower_bounds,
#                  'upper_bound' = upper_bounds,
#                  'left_tail_percentage' = left_tail_percentage),
#            file = paste('service_boot_n', n, '.csv', sep = ''))



boot_plot <- ggplot(data = as.data.frame(my_boot_means)) +
                aes( x = my_boot_means) +
                geom_histogram(bins = 40) + 
                labs(title = 'Bootstrap Sampling Distribution of xbar',
                     subtitle = 'Sample size n = 30',
                     x = 'Bootstrap sample means')
                

boot_plot
save(boot_plot,file = './Images_for_chapters/boot30plot.rda')


capture<-  ifelse(boot_lower_bounds < pop_mean , 
                  ifelse(boot_upper_bounds > pop_mean, 1 , 0), 0)
mean(capture)


left_tail_percentage <- mean(my_boot_means < pop_mean)
left_tail_percentage

mean(mysample)
mean(my_boot_means)
sd(my_boot_means)


### Correlation code

set.seed(1234)
r <- 0

n <- 50
x <- rnorm(n, mean = 9, sd = 1)
y <- .3*x + rnorm(n, mean = 0, sd = .2)
cor(x,y)  

cor_data <- data.frame(x,y)
corr_plot <- ggplot(data = cor_data) +
                aes(x,y) +
                geom_point() +
                labs(title = 'Sleeping Increases your GPA?',
                     x = 'Amount of sleep in hours',
                     y = 'GPA')
corr_plot

#save(corr_plot,file = './Images_for_chapters/corrplot.rda')

load('./Images_for_chapters/corrplot.rda')

nsim <- 1E4
mysample <- corr_plot$data

#mysample <- cbind(c(1,2,3),c(40,70,99))
#elm <- matrix(0, nrow = 3, ncol =2)
my_boot_corrs <- 0

for(i in 1:nsim) 
{
  my_boot_corrs[i] <- cor(sample(mysample, size = 50, replace = TRUE)[,1:2])[1,2]
}

boot_corr <- data.frame(my_boot_corrs)
corr_boot_plot <- ggplot(data =boot_corr ) +
                    aes( x = my_boot_corrs) +
                    geom_histogram(bins = 35) +
                    labs(title = 'Bootstrap Sampling Distribution of Correlation Coefficient',
                         subtitle = 'Sample size n = 50',
                         x = 'Bootstrap correlation coefficients (r)')

corr_boot_plot

cor(mysample)
sd(my_boot_corrs)
quantile(my_boot_corrs, p = c(0.025, 0.975))
save(corr_boot_plot,file = './Images_for_chapters/corr_boot_plot.rda')


