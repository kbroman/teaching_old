######################################################################
# Computer notes             Statistics for Laboratory Scientists (2)
# Lecture 3                                  Johns Hopkins University 
######################################################################
# These notes provide R code related to the course lectures, to
# further illustrate the topics in the lectures and to assist the
# students to learn R.
#
# Lines beginning with the symbol '#' are comments in R.  All other
# lines contain code.
#
# In R for Windows, you may wish to open this file from the menu bar
# (File:Display file); you can then easily copy commands into the
# command window.  (Use the mouse to highlight one or more lines; then
# right-click and select "Paste to console".)
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
# Paired data example
######################################################################
# The data
z <- rbind(c(9,9),c(20,62))

# McNemar's test
chi <- (z[1,2]-z[2,1])^2 / (z[1,2]+z[2,1]) # value = 4.17
1 - pchisq(chi, 1) # P = 4.1%

# An exact test
binom.test(z[1,2], z[1,2]+z[2,1]) # P = 6.1%


##################
# End of comp03.R
##################
