######################################################################
# Solutions for Lab 1
######################################################################

# Problem 1:
# data
x <- read.csv("data1-1.csv")
# just need second column
x <- x[,2]

# expected ratios
p <- c(18,6,6,2,12,4,12,4)
# rescale to sum to 1
p <- p/sum(p)

# expected counts
ex <- sum(x)*p

# chi-square test statistic
chi <- sum((x-ex)^2/ex) # value = 9.53

# LRT statistic
lrt <- 2*sum(x*log(x/ex)) # value = 8.82

# asymptotic p-values
1 - pchisq(chi, 7) # p-value = 0.22
1 - pchisq(lrt, 7) # p-value = 0.27

# simulation-based p-values
# need the function rmultinom
n.sim <- 10000
results <- matrix(ncol=2, nrow=n.sim)
n <- sum(x)
for(i in 1:n.sim) {
  simdat <- rmultinom(n, p)
  # note: we can use the same expected counts calculated above
  simchi <- sum((simdat-ex)^2/ex)
  # a bit of finesse to deal with rare 0 counts
  simlrt <- 2*sum(simdat[simdat>0] * log(simdat[simdat>0]/ex[simdat>0]))
  results[i,] <- c(simchi, simlrt)
  if(i == round(i,-2)) cat(i,"\n")
}
mean(results[,1] >= chi) # simulation-based P-value = 0.22
mean(results[,2] >= lrt) # simulation-based P-value = 0.27

##############################
# CONCLUSIONS
##############################
#   The P-values are large, and so we fail to reject the null
#   hypothesis that the underlying probabilities are according
#   to the ratios 18:6:6:2:12:4:12:4.  In other words, the data
#   conform reasonably well to these proportions.



######################################################################
# Problem 2
######################################################################
# read data
y <- read.csv("data1-2.csv")
# just need the last two columns
y <- y[,4:5]

# chi-square statistic: use chisq.test()
chi <- chisq.test(y) # statistic = 7.25; P-value = 0.40

# LRT
ex <- chi$expected # expected counts
lrt <- 2*sum(y * log(y/ex))  # statistic = 7.25
1 - pchisq(lrt, nrow(y)-1) # p-value = 0.40

# Fisher's exact test
fisher.test(y) # p-value = 0.43

##############################
# CONCLUSIONS
##############################
#   The P-values are large, and so we fail to reject the null
#   hypothesis that the probability that a tick will choose the
#   treated tube is the same for all of the experimental
#   conditions.

##############################
# end of lab1_solns.R
##############################
