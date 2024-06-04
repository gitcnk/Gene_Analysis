

## Larry's p-value idea:
pval_bucket_AB <- 0
pval_bucket_AC <- 0
pval_bucket_CB <- 0


N <- 100
n_a <- 20
n_b <- 20
n_c <- 10000

for(i in 1:N)
{
  A <- rnorm(n = n_a, mean = 2, sd = 0.5)
  B <- rnorm(n = n_b, mean = 2, sd = 0.5)
  
  pval_bucket_AB[i] <- t.test(x = A, y = B)$p.value
  
  C <- rnorm(n = n_c, mean = 2, sd = 0.1)
  
  pval_bucket_AC[i] <- t.test(x = A, y = C)$p.value
  pval_bucket_CB[i] <- t.test(x = C, y = B)$p.value
  
  
}


plot(pval_bucket_AB)
abline(h = 0.05, col = 'red')
sum(pval_bucket_AB < 0.05)/N

plot(pval_bucket_AC)
abline(h = 0.05, col = 'red')
sum(pval_bucket_AC < 0.05)/N

plot(pval_bucket_CB)
abline(h = 0.05, col = 'red')
sum(pval_bucket_CB < 0.05)/N


## A representative figure:
# value <- c(A,B,C)
# 
# group <- as.factor(
#            c(rep('A', n_a),
#            rep('B', n_b),
#            rep('C', n_c)))
# 
# #levels(group) <- c('A', 'C', 'B')
# mydata <- data.frame(value, group)
# 
# 
# ggplot(data = mydata) +
#   aes(x = group, y = value, col = group) +
#   geom_boxplot() +
#   geom_jitter(width = 0.1) #+
#   scale_color_viridis_c() 
# 





## WOW! Larry's argument does not work.  
## Even with 1 million, it does not detect
