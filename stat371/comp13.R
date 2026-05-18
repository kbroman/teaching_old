######################################################################
# Computer notes, Lecture 13
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
url.show("http://www.biostat.wisc.edu/~kbroman/teaching/stat371/comp13.R")
######################################################################

###################################################
# Example for CI for population mean
###################################################
# Data
x <- c(0.2, 1.3, 1.4, 2.3, 4.2,
       4.7, 4.7, 5.1, 5.9, 7.0)

# mean and SD
xm <- mean(x)
xs <- sd(x)
xn <- length(x)

# A plot of the data
stripchart(x, method="jitter", pch=1)
abline(h=1,lty=2,col="gray40")
segments(xm, 0.9, xm, 1.1, col="blue", lwd=2)

# t value
tval <- qt(0.975, xn - 1)

# estimated SE of the difference between the means
se <- xs / sqrt(xn)

# confidence interval
round( xm  + c(-1,1)*(tval*se), 1)
# Returns 2.1, 5.3

# Alternatively, you can use the function t.test()
t.test(x)
# Returns the following:
#     95 percent confidence interval:
#     2.08 5.28

###############################
# Second example
###############################
# Example data
x <- c(1.17, 6.35, 7.76)

# 95% CI for population mean 
t.test(x)

##############################
# Third example
##############################
x <- c(34.9, 28.5, 34.3, 38.4, 29.6,
       29.6, 38.7, 22.4, 30.1, 23.1,
       29.6, 33.4, 20.6, 33.6, 42.4,
       28.2, 25.3, 22.1, 37.3, 32.1)

# mean, sd
mean(x)
sd(x)

# standard error
sd(x)/sqrt(length(x))

# confidence interval
t.test(x)$conf.int
           
# a plot
stripchart(x, method="jitter", pch=1)
abline(h=1, lty=2, col="gray")
segments(mean(x), 0.9, mean(x), 1.1, col="blue", lwd=2)

##################
# End of comp13.R
##################
