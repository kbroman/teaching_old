######################################################################
# Computer notes, Lecture 14
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
url.show("https://kbroman.org/teaching_old/stat371/comp14.R")
######################################################################

###################################################
# Example for CI comparing means of two populations
###################################################
# Data
x <- c(2.78, 5.23, 2.03, 4.78, 3.80, 2.57, 4.63, 1.93, 1.30)
y <- c(3.70, 4.77, 4.59, 5.82, 6.51, 3.88)

# means and SDs
xm <- mean(x)
ym <- mean(y)
xs <- sd(x)
ys <- sd(y)
xn <- length(x)
yn <- length(y)

# A plot of the data
z <- rep(0:1,c(length(x),length(y)))
plot(c(x,y),z,ylim=c(-0.5,1.5),yaxt="n",ylab="",xlab="",lwd=2)
abline(h=0:1,lty=2,col="gray40")
segments(xm, -0.1, xm, 0.1, col="blue", lwd=2)
segments(ym, 0.9, ym, 1.1, col="red", lwd=2)

# pooled estimate of the common population SD
spooled <- sqrt( (xs^2*(xn-1) + ys^2*(yn-1)) / (xn + yn - 2) )

# t value
tval <- qt(0.975, xn + yn - 2)

# estimated SE of the difference between the means
se <- spooled * sqrt(1/xn + 1/yn)

# confidence interval
round( (xm - ym) + c(-1,1)*(tval*se), 1)
# Returns -3.1 -0.2

# Alternatively, you can use the function t.test()
t.test(x,y,var.equal=TRUE)
# Returns the following:
#     95 percent confidence interval:
#     -3.1387982 -0.1623129 

###############################
# Getting confidence intervals
###############################
# Example data
x <- c(59.2, 54.6, 58.1, 41.8, 64.7, 50.8, 51.6, 47.0, 66.3, 58.1)
y <- c(73.3, 91.4, 69.1, 104.2, 64.6, 78.4, 56.4, 55.6, 61.1, 39.8,
       43.9, 63.6, 78.6, 76.5, 45.1, 90.2)

# 95% CI for mean of the first population
t.test(x)

# 95% CI for mean of the second population
t.test(y)

# 95% CI for difference in means
#     assuming pop'n SDs are equal
t.test(x, y, var.equal=TRUE)

# 95% CI for difference in means
#     not assuming pop'n SDs are equal
t.test(x, y)

##############################
# CI for pop'n SD
##############################

# 95% CI for SD of first population
sd(x) * sqrt( (length(x)-1) / qchisq(c(0.975,0.025), length(x)-1) )

# 95% CI for SD of first population
sd(y) * sqrt( (length(y)-1) / qchisq(c(0.975,0.025), length(y)-1) )

##################
# End of comp14.R
##################
