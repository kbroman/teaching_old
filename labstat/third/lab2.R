######################################################################
# R code                         Statistics for Laboratory Scientists 
# Lab 2                                      Johns Hopkins University 
######################################################################
# This file contains the miscellaneous R commands that appear in the
# lab handout/pdf file.
#
# Lines beginning with the symbol '#' are comments in R.  All other
# lines contain code.
#
# In R for Windows, you may wish to open this file from the menu bar
# (File:Display file); you can then copy commands into the command
# window.  (Use the mouse to highlight one or more lines; then
# right-click and select "Paste to console".)
######################################################################

##############################
# Functions in R
##############################
cv <-
function(x, na.rm=FALSE)
{
  m <- mean(x, na.rm)
  s <- sd(x, na.rm)
  s/m
}

x <- runif(100)
cv(x)


#################################
# The population genetics example
#################################
dat <- init(25, 0.2)
table(dat)
mean(dat)/2

freq <- 1:1000
for(i in 1:1000)
  freq[i] <- mean(init(25,0.2))/2
hist(freq, breaks=seq(0, 1, 0.01))
abline(v=0.2,lwd=2)

freq <- 1:30
dat <- init(25,0.2)
freq[1] <- mean(dat)/2
for(i in 2:30) {
  dat <- simnewgen(dat)
  freq[i] <- mean(dat)/2
}
plot(freq, type="l", ylim=c(0,1), lwd=2)

plot(freq, type="l", ylim=c(0,1), lwd=2)
for(i in 1:25)
  lines(simfreq(30, 25, 0.2), col="gray")
lines(freq,lwd=2)

freq.sm <- simfreq(100, 25, 0.2)
freq.lg <- simfreq(100, 100, 0.2)
plot(freq.sm, type="l", ylim=c(0,1))
lines(freq.lg, col="red")

plot(simfreq(500, 25, 0.2), type="l", ylim=c(0,1))

res <- matrix(nrow=100,ncol=2)
colnames(res) <- c("allele", "generation")
for(i in 1:100) {
  res[i,] <- fixtime(25, 0.2)
  cat(i, "\n")
}

mean(res[,1]==1)
hist(res[,2], breaks=20)


##############################
# Binomial, etc, probabilities
##############################
x <- rbinom(1000, 120, 0.3)
hist(x, breaks=30)

dbinom(3, 6, 0.512)
dbinom(0:6, 6, 0.512)

pbinom(3, 6, 0.512)
pbinom(2, 6, 0.512)
pbinom(40, 120, 0.512)
sum(dbinom(0:40, 120, 0.512))

pbinom(65, 120, 0.512) - pbinom(54, 120, 0.512)
sum(dbinom(55:65, 120, 0.512))

qbinom(0.75, 120, 0.512)
pbinom( qbinom(0.75,120,0.512), 120, 0.512)

plot(0:10, dbinom(0:10, 5000, 0.001))
lines(0:10, dpois(0:10, 5000*0.001))

x <- dbinom(0:10, 5000, 0.001)
y <- dpois(0:10, 5000*0.001)
plot(0:10, x/y, ylab="binomial/Poisson")

x <- qbinom(0.1, 100:200, 0.1)
max( (100:200)[x < 10]) + 1 

x <- seq(55, 80, by=0.5)
plot(x, dnorm(x, 69, 3), type="l")

me <- 1:1000
mi <- 1:1000
for(i in 1:1000) {
  x <- rnorm(20, 69, 3)
  me[i] <- mean(x)
  mi[i] <- min(x)
}

x <- matrix( rnorm(4000, 69, 3), ncol=4)
me <- apply(x, 1, mean)
mi <- apply(x, 1, min)

mean(me > 70)
mean(mi > 70)

###############
# End of lab2.R
###############
