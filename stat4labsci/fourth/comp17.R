######################################################################
# Computer notes             Statistics for Laboratory Scientists (2)
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

###############################################
# The first example
###############################################
# The data
h2o2 <- c(0,0,0,10,10,10,25,25,25,50,50,50)
pf3d7 <- c(0.3399,0.3563,0.3538,0.3168,0.3054,0.3174,
        0.2460,0.2618,0.2848,0.1535,0.1613,0.1525)
pyoel <- c(0.3332,0.3414,0.3299,0.2940,0.2948,0.2903,
        0.2089,0.2189,0.2102,0.1006,0.1031,0.1452)

mydat <- data.frame(y = c(pf3d7, pyoel),
                    x1 = rep(h2o2, 2),
                    x2 = rep(0:1, rep(length(h2o2),2)))

# plot the data
par(las=1)
plot(y ~ x1, data=mydat, subset=(x2==0), col="blue", lwd=2,
     xlim=range(mydat$x1), ylim=range(mydat$y),
     xlab=expression(paste(H[2], O[2], " concentration")),
     ylab="OD")
points(y ~ x1, data=mydat, subset=(x2==1), col="green", lwd=2)

# fit the two regression lines
lm.outA <- lm(y ~ x1, data=mydat, subset=(x2==0))
lm.outB <- lm(y ~ x1, data=mydat, subset=(x2==1))

# add lines to the plot
abline(lm.outA$coef, col="blue", lty=2, lwd=2)
abline(lm.outB$coef, col="green", lty=2, lwd=2)

# fit the full model
lm.out <- lm(y ~ x1 * x2, data=mydat)
summary(lm.out) # note that the est'd beta'ss are the same as above

# fit the reduced model (that the two hemes have the same line)
lm.red <- lm(y ~ x1, data=mydat)
summary(lm.red)

# compare the full and reduced models
anova(lm.red, lm.out)

##################
# End of comp17.R
##################
