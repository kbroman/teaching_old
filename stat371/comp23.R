######################################################################
# Computer notes, Lecture 23
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
url.show("https://kbroman.org/teaching/stat371/comp23.R")
######################################################################

######################################################################
# example 1
######################################################################
x <- rbind(c(18, 2), c(11, 9))

# marginal totals
rs <- apply(x, 1, sum) # sum of each row
cs <- apply(x, 2, sum) # sum of each column
n  <- sum(x)

# expected counts
expected <- outer(rs,cs,"*")/n

# chi-square statistic
chi <- sum((x-expected)^2/expected) # result = 6.14
# P-value
1-pchisq(chi,1) # value=0.013

# lrt statistic
lrt <- 2*sum(x * log(x/expected)) # result = 6.52
# P-value
1-pchisq(lrt,1)# value=0.011

# Fisher's exact test
fisher.test(x) # p-value = 3.1%


######################################################################
# example 2
######################################################################
x <- rbind(c(9,9), c(20, 62))

# marginal totals
rs <- apply(x, 1, sum) # sum of each row
cs <- apply(x, 2, sum) # sum of each column
n  <- sum(x)

# expected counts
expected <- outer(rs,cs,"*")/n

# chi-square statistic
chi <- sum((x-expected)^2/expected) # result = 4.70
# P-value
1-pchisq(chi,1) # value=0.030

# lrt statistic
lrt <- 2*sum(x * log(x/expected)) # result = 4.37
# P-value
1-pchisq(lrt,1)# value=0.037

# Fisher's exact test
fisher.test(x) # p-value = 4.4%

######################################################################
# Hypergeometric distribution
######################################################################
# Consider an urn with 30 balls, of which 12 are white and 18 are black
# Suppose we draw 10 balls *without* replacement and count the number
# of white balls obtained.
######################################################################
# Simulate this
rhyper(1, 12, 18, 10) 

# Simulate this 100 times
rhyper(100, 12, 18, 10)

# Probability of getting no white balls in the sample
dhyper(0, 12, 18, 10)

# Probability of getting exactly 2 white balls in the sample
dhyper(2, 12, 18, 10)

# Probability of getting 2 or fewer white balls in the sample
phyper(2, 12, 18, 10)

# 2.5th and 97.5th %iles of number of white balls drawn
qhyper(c(0.025, 0.975), 12, 18, 10)

######################################################################
# McNemar's test
######################################################################
x <- rbind(c(9,9), c(20,62))

mcnemar.test(x)
# p-value = 6.3% (using a "continuity correction")

mcnemar.test(x, correct=FALSE)
# p-value = 4.1% (as shown in lecture)

# exact test
binom.test(9, 29)
# p-value = 6.1%

# equivalently...
binom.test(20, 29)

##################
# End of comp23.R
##################
