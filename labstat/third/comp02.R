######################################################################
# Computer notes                 Statistics for Laboratory Scientists 
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

# Data from Sokal & Rohlf, exercise 4.2 (pg 59)
#     [mg glycine per mg creatine in the urine of 37 chimpanzees]

x <- c(0.008, 0.018, 0.056, 0.055, 0.135,
       0.052, 0.077, 0.026, 0.044, 0.300,
       0.025, 0.036, 0.043, 0.100, 0.120,
       0.110, 0.100, 0.350, 0.100, 0.300,
       0.011, 0.060, 0.070, 0.050, 0.080,
       0.110, 0.110, 0.120, 0.133, 0.100,
       0.100, 0.155, 0.370, 0.019, 0.100,
       0.100, 0.116)

# number of data points
length(x)

# summary statistics
summary(x)

# various quantiles
quantile(x, 0.1)
quantile(x, c(0.1, 0.95))

# mean, median, SD, variance
mean(x)
median(x)
sd(x)
var(x)

# geometric mean
exp(mean(log(x)))

# harmonic mean
1/mean(1/x)

# histogram
hist(x)

# histograms with different numbers of bins
hist(x,breaks=5)
hist(x,breaks=15)

# plot values by index
plot(x)

# dot plot with jittered y values
#    - runif draws random numbers between 0 and 1
#    - yaxt="n" prevents y-axis from being plotted
#    - ylim changes the limits of the y-axis
#    - ylab="" prevents y label from being plotted
#    - abline(h=0.5) plots a horizontal line at 0.5
y <- runif(length(x))
plot(x, y, ylim=c(-2,3), yaxt="n", ylab="")
abline(h=0.5, lty=2, col="gray70")

# there's also a built-in function to do this, "stripchart"
#     - method="jitter" jitters the values
#     - pch=1 plots circles (the default is pch=0, squares)
#     - I still like to add a horizontal line
stripchart(x, method="jitter", pch=1)
abline(h=1, lty=2, col="gray70")

# the same plot with x-axis on log scale
plot(x, y, ylim=c(-2,3), yaxt="n", ylab="", log="x")
abline(h=0.5, lty=2, col="gray70")

# you can't use log="x" within the "stripchart" function,
#     so you must just take logs
stripchart(log(x), method="jitter", pch=1)
abline(h=1, lty=2, col="gray70")

# boxplot
boxplot(x)

# boxplot without the goofy outlier business
boxplot(x, range=0)

# boxplot horizontally
boxplot(x, range=0, horizontal=TRUE)

# boxplot on log scale
boxplot(x, range=0, horizontal=TRUE, log="x")

# boxplot on log scale, but not horizontal
boxplot(x, range=0, log="y")

##################
# End of comp02.R
##################
