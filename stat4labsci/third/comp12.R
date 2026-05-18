######################################################################
# Computer notes                 Statistics for Laboratory Scientists 
# Lecture 12                                 Johns Hopkins University 
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
# Example 1: 2-point linkage in an intercross
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
# End of comp12.R
##################
