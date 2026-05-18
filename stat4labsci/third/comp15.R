######################################################################
# Computer notes                 Statistics for Laboratory Scientists 
# Lecture 15                                 Johns Hopkins University 
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

##############################
# Generating many QQ plots
##############################
# The following allows me to examine 9 QQ plots with
#     computer-generated, normally distributed data

# figure region will then contain 3 rows and 3 columns of plots
par(mfrow=c(3,3)) 
for(i in 1:9) { # do the following 9 times
  x <- rnorm(20) # generate 20 standard normal "deviates"
  qqnorm(x) # create the qq plot
  qqline(x) # add a line to the qq plot
}

# Alternatively, rather than do all 9 plots simultaneously,
#     have the computer ask you to press a key to see the next plot
par(mfrow=c(1,1)) # return the graphics device to a single plot per figure
par(ask=TRUE) # R will ask you to press a key between each plot
for(i in 1:9) { # do the following 9 times
  x <- rnorm(20) # generate 20 standard normal "deviates"
  qqnorm(x) # create the qq plot
  qqline(x) # add a line to the qq plot
}
par(ask=FALSE) # suspending the "ask" business

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

##################
# End of comp15.R
##################
