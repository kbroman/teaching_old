######################################################################
# Computer notes             Statistics for Laboratory Scientists (2)
# Lecture 19                                 Johns Hopkins University 
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
# Striped Bass example
######################################################################
# data available at
#     http://www.biostat.jhsph.edu/~kbroman/teaching/labstat/fourth/bass.csv
bass <- read.csv("bass.csv")

# plot y against each of the x's
par(las=1,mfrow=c(1,3),ask=FALSE)
plot(growth ~ density + temp.rise + flow, data=bass)

# fit linear model
lm.out <- lm(growth ~ density + temp.rise + flow, data=bass)
summary(lm.out)

# plot residuals vs time
resid <- lm.out$resid
plot(resid, xlab="Time index", ylab="Residual", lwd=2)

# plot residual(t) vs residual(t+1)
plot(resid[-length(resid)], resid[-1], xlab="residual(t)",
     ylab="residual(t+1)", lwd=2)

# test for correlation between resid(t) and resid(t+1)
obs.cor <- cor(resid[-length(resid)], resid[-1])
perm.cor <- 1:1000
for(i in 1:1000) {
  x <- sample(resid)
  perm.cor[i] <- cor(x[-1],x[-11])
}
mean(abs(perm.cor) >= abs(obs.cor)) # p-value = approx 78%

# QQ plot of residuals
qqnorm(resid)
qqline(resid,lwd=2,col="blue")

# residuals vs fitted values
plot(lm.out$fitted, resid)
abline(h=0,col="red",lty=2)

# residuals vs each X
plot(bass$density, resid)
plot(bass$temp.rise, resid)
plot(bass$flow, resid)

##############################
# The fake example
##############################
# the data
mydata <- data.frame(y=c(7.70, 4.63, 6.26,-1.49,-4.04,
                         9.84, 0.08, 9.36, 3.65,13.47,
                         8.00,18.93, 0.11,-1.64, 9.86,
                         7.78,13.98, 7.19,22.18, 3.54),
                     x1=c(19.46,15.13,13.67,11.77,18.18,
                          24.64,21.42,10.78,20.01,12.75,
                          26.86,22.91,19.18,16.02,20.15,
                          19.29,28.82,23.99,29.49,11.75),
                     x2=c(45.11,34.18,41.51,38.51,47.15,
                          47.59,45.26,35.39,48.64,34.33,
                          48.93,38.64,51.51,49.50,46.85,
                          44.97,49.44,57.80,53.23,39.46))

# fit y = b0 + b1 x1
out1 <- lm(y ~ x1, data=mydata)
summary(out1)

# plot residuals vs x2
plot(mydata$x2, out1$resid, xlab=expression(x[2]), ylab="Residuals")
abline(h=0, col="red", lty=2, lwd=2)

# fit y = b0 + b1 x1 + b2 x2
out2 <- lm(y~x1+x2, data=mydata)
summary(out2)

##############################
# The final example
##############################
# data available at
#     http://www.biostat.jhsph.edu/~kbroman/teaching/labstat/fourth/sediment.csv
sed <- read.csv("sediment.csv")

# fit model: Need to use I() to get the ^2 and ^3 to work
lm.out <- lm(pellets ~ time + I(time^2) + I(time^3), data=sed)

# plot data and fitted curve
plot(pellets ~ time, data=sed)
u <- par("usr") # get x-axis limits in the plot
x <- seq(u[1], u[2], length=250)
y <- predict(lm.out, data.frame(time=x))
lines(x, y, col="blue", lwd=2)

# diagnostic plots
plot(lm.out)

##################
# End of comp19.R
##################
