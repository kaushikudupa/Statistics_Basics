# POWER OF A TEST

effect_size <- seq(-1,1,0.1) # Effect size = Difference in group means
power <- length(effect_size)
k <- 1000     # Number of replicates
mu1 <- 3      # Mean of the reerence group
alpha <- 0.05 # Type 1 error level
n <- 100      # Sample size

for (i in 1:length(effect_size)) {
  
  # Power of the test is the probability of rejecting the H0, given the H1 is true
  # OR P(rejecting H0 | H1 is true)
  power[i] <- mean(replicate(k,t.test(rnorm(n,mu1),rnorm(n,mu1-effect_size[i]))$p.value) < alpha)
}

plot(effect_size, power, main = 'Effect size vs Power of the test', type = 'b', pch='.',col='red',
     xlab = 'Effect size', ylab = 'Power')

# ------------------------------------------------------------------------------------------------------

# CALCULATE THE SAMPLE SIZE FOR A GIVEN VALUE OF ALPHA, BETA, AND EFFECT SIZE

effect_size <- c(0.2,0.5,0.8)
power_level <- c(0.8,0.9)
alpha <- 0.05

get_sample_size <- function(es,power_crit,alpha,mu1,k) {
  power <- 0
  n <- 0
  while(power < power_crit) {
    n <- n + 10
    power <- mean(replicate(k,t.test(rnorm(n,mu1),rnorm(n,mu1-es))$p.value) < alpha)
    #print(c(power,n))
  }
  return(n)
}

res <- expand.grid(effect_size,power_level,alpha,0)
names(res) <- c('Effect size','Power level', 'Alpha','Sample size')
for (i in 1:nrow(res)) {
  print('Effect size')
  print(res[i,1])
  res[i,4] <- get_sample_size(res[i,1],res[i,2],res[i,3],3,1000)
}
rm(i)
plot(res$`Effect size`,res$`Sample size`, xlab='Effect size',ylab='Sample size',
     main='Effect size v Sample size', type='p',col='red',pch='o')
# We see that as the effect size increases the sample size required decreases for a given alpha and beta level.




