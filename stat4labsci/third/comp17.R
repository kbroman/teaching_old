######################################################################
# Computer notes                 Statistics for Laboratory Scientists 
# Lecture 17                                 Johns Hopkins University 
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

###################################
# Power in case pop'n SDs are known
###################################
# population SDs
sigA <- 2
sigB <- 4

# sample sizes
n <- 5
m <- 15

# standard deviation of Xbar - Ybar
se <- sqrt(sigA^2/n + sigB^2/m)

# difference between population means
delta <- 3

# critical value for alpha = 0.05
C <- qnorm(0.975)

# the power (result = 59%)
1 - pnorm(C - delta/se) + pnorm(-C - delta/se)


#####################################################
# Power in case pop'n SDs are not known but are equal
#####################################################
# same parameters as above, but we don't know
#     sigA and sigB

# critical value for alpha = 0.05
C <- qt(0.975, n+m-2)

# the power (result = 55%)
1 - pt(C, n+m-2, delta/se) + pt(-C, n+m-2, delta/se)


#####################################################
# Computer simulation to estimate power in the case
#     that pop'n SDs are neither known nor equal
#####################################################
# A function to simulate normal data and get the
#     p-value from the t-test.
#
# n,m = sample sizes
# sigA,sigB = population SDs
# delta = difference between population means
sim <-
function(n, m, sigA, sigB, delta)
{
  x <- rnorm(n, delta, sigA)
  y <- rnorm(m, 0, sigB)
  t.test(x,y)$p.value
}

# For n=5, m=10, sigA = 1, sigB = 2, and
#     delta = 0, 0.5, 1.0, ..., 2.5,
# Run 1000 simulations and get the p-value
delta <- c(0.0, 0.5, 1.0, 1.5, 2.0, 2.5)
p <- matrix(nrow=1000,ncol=length(delta))
for(i in 1:length(delta))
  for(j in 1:nrow(p)) 
    p[j,i] <- sim(5, 10, 1, 2, delta[i])

# Estimate the power
pow <- apply(p, 2, function(a) mean(a<0.5))

##################
# End of comp17.R
##################
