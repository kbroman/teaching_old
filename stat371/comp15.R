######################################################################
# Computer notes, Lecture 15
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
url.show("http://www.biostat.wisc.edu/~kbroman/teaching/stat371/comp15.R")
######################################################################

######################################################################
# initial example
######################################################################
before <- c(13.04, 11.23, 15.27,  7.56, 10.12, 10.02,  8.25, 13.09, 12.62, 10.15)
after <- c(15.15, 13.96, 16.41, 12.52, 12.57, 11.07, 13.23, 13.13, 10.01, 12.63)

# plot of before vs after
plot(before, after,lwd=2,cex=1.1)
# add a line with slope = 1 and y-intercept = 0
abline(0,1,lwd=2,lty=2,col="blue")

# plot of the differences; x-axis are jittered values
plot(runif(length(after)), after-before, lwd=2,
     xlab="", xaxt="n", ylab="after - before",xlim=c(-1,2),cex=1.1)
abline(h=0,lwd=2,lty=2,col="blue")

# t-test of whether the population mean for the differences
# (after-before) is 0
t.test(after-before)

######################################################################
# The t-test, worked out in detail
######################################################################
# the differences
dif <- after-before

# mean and SD
themean <- mean(dif)
thesd <- sd(dif)

# estimated standard error of the sample mean
se <- thesd / sqrt(length(dif))

# test statistic
t <- abs(themean / se)
# value = 2.72

# critical value for alpha = 0.05
qt(0.975, length(dif)-1)
# value = 2.26

# so we reject the null hypothesis (that the population mean is 0)

# P-value: 
2*pt(-t, length(dif)-1)
# value = 2.3%

######################################################################
# the final example (testing for the differences between the means
# of two populations)
######################################################################
# the data
x <- c(59.2, 54.6, 58.1, 41.8, 64.7, 50.8, 51.6, 47.0, 66.3, 58.1)
y <- c(73.3, 91.4, 69.1, 104.2, 64.6, 78.4, 56.4, 55.6, 61.1, 39.8,
       43.9, 63.6, 78.6, 76.5, 45.1, 90.2)

t.test(x,y)


##################
# End of comp15.R
##################
