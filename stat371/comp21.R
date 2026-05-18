######################################################################
# Computer notes, Lecture 21
# Stat 371: Introductory applied statistics for the life sciences
######################################################################
# These notes provide R code related to the course lectures, to
# further illustrate the topics in the lectures and to assist the
# students to learn R.
#
# Lines beginning with the symbol '#' are comments in R.  All other
# lines contain code.
#
# You can view this file within R by typing:
url.show("https://kbroman.org/teaching/stat371/comp21.R")
######################################################################

######################################################################
# First example: data = (78,22); do they look like 3:1?
######################################################################
ob <- c(78, 22)

# simplest method
chisq.test(ob, p=c(0.75, 0.25))

##############################
# working it out "by hand"
##############################
# expected counts
ex <- c(0.75, 0.25)*sum(ob)

# the statistic
xsq <- sum( (ob-ex)^2 / ex )

# p-value from Chi-square (df=1) distribution
1-pchisq(xsq, 1)

##############################
# p-value by simulation
##############################
sim <- rbinom(10000, 100, 0.75)

xsqsim <- (sim-75)^2/75 + ( (100-sim) - 25)^2/25
mean(xsqsim >= xsq)

######################################################################
# Second Example
######################################################################

# The 35:43:22 table
ob <- c(35, 43, 22)

# Chi-square test to compare to 1:2:1
chisq.test(ob, p=c(0.25, 0.5, 0.25))

##############################
# working it out "by hand"
##############################
# expected counts
ex <- c(0.25, 0.5, 0.25)*sum(ob)

# the statistic
xsq <- sum( (ob-ex)^2 / ex )

# p-value from Chi-square (df=2) distribution
1-pchisq(xsq, 2)

##############################
# p-value by simulation
##############################
sim <- rmultinom(10000, 100, c(0.25, 0.5, 0.25))
# Note that the results are a matrix with 3 rows and 10,000 columns

# the quicket way to get the chi-square statistics from the simulations
xsqsim <- apply(sim, 2, function(a) chisq.test(a, p=c(0.25, 0.5, 0.25))$stat)

# alternatively...
xsqsim <- rep(NA, ncol(sim))
for(i in 1:ncol(sim))
  xsqsim[i] <- chisq.test(sim[,i], p=c(0.25, 0.5, 0.25))$stat

# p-value
mean(xsqsim >= xsq)

######################################################################
# Example 3 (tomatoes)
######################################################################
ob <- c(926, 288, 293, 104)

# the probabilities
p <- c(9, 3, 3, 1)
p <- p/sum(p)

# Chi-square test to compare to 9:3:3:1
chisq.test(ob, p=p)

##############################
# working it out "by hand"
##############################
# expected counts
ex <- p*sum(ob)

# the statistic
xsq <- sum( (ob-ex)^2 / ex )

# p-value from Chi-square (df=3) distribution
1-pchisq(xsq, 3)

##############################
# p-value by simulation
##############################
sim <- rmultinom(10000, sum(ob), p)
# Note that the results are a matrix with 4 rows and 10,000 columns

# the quicket way to get the chi-square statistics from the simulations
xsqsim <- apply(sim, 2, function(a) chisq.test(a, p=c(9/16, 3/16, 3/16, 1/16))$stat)

# alternatively...
xsqsim <- rep(NA, ncol(sim))
for(i in 1:ncol(sim))
  xsqsim[i] <- chisq.test(sim[,i], p=c(9/16, 3/16, 3/16, 1/16))$stat

# p-value
mean(xsqsim >= xsq)

##################
# End of comp21.R
##################
