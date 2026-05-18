######################################################################
# Computer notes             Statistics for Laboratory Scientists (2)
# Lecture 2                                  Johns Hopkins University 
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
# I start here with two functions:
#     lrt,       for calculating the LRT goodness-of fit statistic
#     chisq,     for calculating the chi-square goodness-of-fit stat
#
# Copy and paste these into R before doing the next bits.
######################################################################


######################################################################
# Likelihood ratio test for goodness of fit
######################################################################
lrt <-
function(x=c(35,43,22), p=c(0.25,0.5,0.25))
  2 * (dmultinom(x, prob=x/sum(x),log=TRUE) - dmultinom(x, prob=p,log=TRUE))

# alternatively, this could have been the following,
lrt2 <-
function(x=c(35,43,22), p=c(0.25,0.5,0.25)) {
  ex <- (p*sum(x))[x > 0]
  x <- x[x>0]

  2 * sum(x * log(x / ex) )
}


######################################################################
# chi-square test for goodness of fit
######################################################################
chisq <-
function(x=c(35,43,22), p=c(0.25,0.5,0.25))
{
  # check that x,p are appropirate
  p <- p/sum(p) # ensure that the probabilities sum to 1
  if(any(p < 0)) stop("probabilities p must be non-negative.")
  if(any(x < 0)) stop("x's must all be non-negative.")
  if(length(x) != length(p)) stop("length(x) should be the same as length(p).")
  
  n <- sum(x)
  expected <- n*p

  sum( (x - expected)^2 / expected )
}

######################################################################
# Main Example
######################################################################

# The 35:43:22 table
x <- c(35, 43, 22)

# Note that you might also have started with a vector of 1's, 2's and 3's,
# and used the function "table" instead, as follows:
y <- rep(1:3, c(35, 43, 22))
x <- table(y)

# calculate LRT test statistic to test the 1:2:1 proportions
LRT <- lrt(x, p=c(0.25, 0.5, 0.25)) 

# calculate chi-square statistic to test the 1:2:1 proportions
xsq <- chisq(x, p=c(0.25, 0.5, 0.25))

# you could also use the built-in function, chisq.test
chisq.test(x, p=c(0.25, 0.5, 0.25))

# P-value for LRT statistic, using the asymptotic approximation
1 - pchisq(LRT, 2)

# P-value for chi-square statistic, using the asymptotic approx'n
1 - pchisq(xsq, 2)

# sims to est the null dist'n of the LRT and chi-square stats
simdat <- rmultinom(1000, 100, c(0.25, 0.5, 0.25))
results <- cbind(apply(simdat, 2, lrt, p=c(0.25, 0.5, 0.25)),
                 apply(simdat, 2, chisq, p=c(0.25, 0.5, 0.25)))


# est'd p-value for LRT stat
mean(results[,1] >= LRT)

# est'd p-value for chi-square stat
mean(results[,2] >= xsq)


######################################################################
# Example 1 in "composite hypotheses" section of the lecture
######################################################################
# I start with some additional functions:
#     sim.ex1,   for simulating data under the null hypothesis (H0)
#     mle.ex1,   for estimating the allele frequency under H0
#     lrt.ex1,   calculates LRT statistic for testing this H0
#     chisq.ex1, calculates chi-square stat for testing this H0
######################################################################

# simulate data
sim.ex1 <-
function(n=100, f=0.2)
  table(factor(sample(c("AA","AB","BB"), n, repl=TRUE,
                      prob=c(f^2,2*f*(1-f),(1-f)^2)),
               levels=c("AA","AB","BB")))

# MLE under H0
mle.ex1 <-
function(x=c(5,20,75))
  (x[1]+x[2]/2)/sum(x)
  
# LRT statistic
lrt.ex1 <-
function(x=c(5,20,75))
{
  mle <- mle.ex1(x)
  p0 <- c(mle^2,2*mle*(1-mle),(1-mle)^2)
  2*(dmultinom(x,prob=x/sum(x),log=TRUE) - dmultinom(x,prob=p0,log=TRUE))
}

# chi-square statistic
chisq.ex1 <-
function(x=c(5,20,75))
{
  mle <- mle.ex1(x)
  expected <- sum(x)*c(mle^2,2*mle*(1-mle),(1-mle)^2)
  sum((x-expected)^2/expected)
}

# The example data
x <- c(5, 20, 75)

# The est'd allele frequency
mle <- mle.ex1(x)

# LRT stat
LRT <- lrt.ex1(x)

# chi-square stat
xsq <- chisq.ex1(x)

# Asymptotic approx'n for P-value for LRT stat
1 - pchisq(LRT, 1)

# Asymptotic approx'n for P-value for chi-square stat
1 - pchisq(xsq, 1)

# sims to est the null dist'n of the LRT and chi-square stats
results <- matrix(ncol=2,nrow=1000)
for(i in 1:1000) {
  mytable <- sim.ex1(100, mle)
  results[i,1] <- lrt.ex1(mytable)
  results[i,2] <- chisq.ex1(mytable)
}

# est'd p-value for LRT stat
mean(results[,1] >= LRT)

# est'd p-value for chi-square stat
mean(results[,2] >= xsq)


######################################################################
# Example 3 regarding fit of Poisson counts
######################################################################
# the raw data
x <- c(2, 2, 0, 0, 0,   0, 0, 1, 0, 0,
       0, 0, 4, 0, 0,   3, 0, 0, 0, 3,
       0, 5, 0, 1, 0,   1, 2, 0, 2, 1,
       0, 0, 0, 0, 0,   0, 0, 0)

# counts
ob <- table(x)

# sample mean
mu <- mean(x)

# expected counts; binning 5+ together
ex <- c(dpois(0:4, mu), 1-ppois(4,mu)) * length(x)

# chi-square and LRT statistics
chis <- sum((ob-ex)^2/ex)
lrt <- sum(2*ob*log(ob/ex))

# P-values via asymptotic approximation
1-pchisq(chis, length(ob)-2)
1-pchisq(lrt, length(ob)-2)

# p-value based on simulations
n.sim <- 10000
simd <- apply(matrix(rpois(n.sim*length(x), mu), ncol=n.sim),2,
              function(x) { x <-table(factor(x,levels=0:100)); x[6] <- sum(x[-(1:5)]); x[1:6] })

# function to calculation likelihood ratio test statistic
lrtt <- function(a,b)
  {
    b <- b[a>0]
    a <- a[a>0]
    sum(2*a*log(a/b))
  }

# calculate the statistics
simlrt <- apply(simd, 2, lrtt, ex)
simchisq <- apply(simd, 2, function(a,b) sum((a-b)^2/b), ex)

# the p-values
mean(simchisq >= sum((ob-ex)^2/ex))
mean(simlrt >= sum(2*ob*log(ob/ex)))

##################
# End of comp02.R
##################
