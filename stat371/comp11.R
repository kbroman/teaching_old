######################################################################
# Computer notes, Lecture 11
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
url.show("http://www.biostat.wisc.edu/~kbroman/teaching/stat371/comp11.R")
######################################################################

######################################################################
# Example 1: estimating quartiles
######################################################################
# concerning prob 2.33 (pg 39), here's a function for calculating
# the 25th or 75th percentile by the method in the book
######################################################################
bookquant <-
function(x, prob=0.25)
{
  if(length(prob) > 1) {
    res <- rep(NA, length(prob))
    for(i in seq(along=prob))
      res[i] <- myquant(x, prob[i], method)
    return(res)
  }
  
  n <- length(x)
  if(max(abs(prob - 0.25)) < 1e-10) { # lower quartile
    return(median(sort(x)[1:floor(n/2)]))
  }
  else if(max(abs(prob - 0.75)) < 1e-10) { # upper quartile
    return(median(rev(sort(x))[1:floor(n/2)]))
  }
  else {
    stop("book method only for 25th and 75th quantiles.")
  }
}


######################################################################
# Example 2: 2-point linkage in an intercross
######################################################################
# simulating 2-point intercross data
simf2 <-
function(n=250, rf=0.1)
{
  rfbar <- 1 - rf
  
  dat <- table(factor(sample(1:9, n, repl=TRUE,
                      prob=c(rfbar^2/4,   rf*rfbar/2,         rf^2/4,
                             rf*rfbar/2, (rf^2 + rfbar^2)/2,  rf*rfbar/2,
                             rf^2/4,     rf*rfbar/2,          rfbar^2/4)),
                      levels = 1:9))
  # the above gives counts for numbers 1, 2, ..., 9

  dat <- matrix(dat, ncol=3, nrow=3, byrow=TRUE)
  dimnames(dat) <- list(c("BB","Bb","bb"), c("AA","Aa","aa"))
  dat
}
  

# The log likelihood function for the 2-point intercross data
llf2 <-
function(dat, rf=seq(0,0.5,len=501))
{
  rfbar <- 1 - rf
  
  A <- dat[1,1]+dat[3,3] # non-recombinants
  B <- dat[1,2]+dat[2,1]+dat[2,3]+dat[3,2] # single-recombinants
  C <- dat[1,3]+dat[3,1] # double-recombinants
  D <- dat[2,2] # double heterozygotes

  A * log(rfbar^2/4) + B * log(rf*rfbar/2) + C*log(rf^2/4) +
    D*log((rf^2+rfbar^2)/2)
}
  

# the data in the lecture
mydat <- rbind(c(58,9,0), c(8,95,14), c(1,12,53))

# the MLE; the function "optimize" is for finding the max or min
#     of a function of one variable
mle <- optimize(llf2, c(1e-5, 0.4), dat=mydat, max=TRUE)$maximum

# the log likelihood function for these data
rf <- seq(0, 0.5, len=501)
ll <- llf2(mydat, rf)
plot(rf, ll, type="l", lwd=2, xlab="Recombination Fraction",
     ylab="log likelihood")
abline(v=mle,col="blue",lwd=2)
u <- par("usr")
text(mle+diff(u[1:2])*0.1, mean(u[3:4]), "MLE = 9.4%", cex=1.5, col="blue")

# A closer view of the log likelihood function
rf <- seq(0.05, 0.15, len=501)
ll <- llf2(mydat, rf)
plot(rf, ll, type="l", lwd=2, xlab="Recombination Fraction",
     ylab="log likelihood")
abline(v=mle,col="blue",lwd=2)
u <- par("usr")
text(mle+diff(u[1:2])*0.1, mean(u[3:4]), "MLE = 9.4%", cex=1.5, col="blue")

# A simulation to learn about behavior of MLE vs a simpler estimate
simres <- matrix(ncol=2,nrow=1000)
colnames(simres) <- c("simple","mle")
for(i in 1:1000) {
  dat <- simf2(n=250, rf=0.1)
  simres[i,2] <- optimize(llf2, c(1e-5, 0.5-1e-5), dat=dat, max=TRUE)$maximum
  simres[i,1] <- (dat[1,2]+dat[2,1]+dat[2,3]+dat[3,2]+2*(dat[1,3]+dat[3,1]))/
                 (2*sum(dat))
}
                          
# Histograms of the results
par(mfrow=c(2,1))
hist(simres[,1], breaks=seq(0.05, 0.155, by=0.005),
     xlab="Estimate", ylab="", yaxt="n", main="Simple estimator")
hist(simres[,2], breaks=seq(0.05, 0.155, by=0.005),
     xlab="Estimate", ylab="", yaxt="n", main="MLE")


##################
# End of comp11.R
##################
