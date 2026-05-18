######################################################################
# Solutions to Lab 3
######################################################################
# Note: This is a very informal presentation of the solutions to the
#       lab.  This would be a reasonable appendix for the lab, but the
#       main part should be a written report in prose rather than
#       computer code.
######################################################################

# Read in the data
thedata <- read.csv("lab3.csv")

# take log10(response)
thedata$response <- log10(thedata$response)

##############################
# Problem 1
##############################
# Fit the calibration line with these data
lm.out <- lm(response ~ concentration, data=thedata)

##############################
# Part (a): estimates of beta0, beta1, and sigma:
##############################
# estimate of beta0
lm.out$coef[1] # value = 2.00

# estimate of beta1
lm.out$coef[2] # value = -0.0204

# estimate of sigma
summary(lm.out)$sigma # value = 0.023

##############################
# Part (b)
##############################
# Confidence interval for beta0
est <- lm.out$coef[1]
se <- summary(lm.out)$coef[1,2]
tmult <- qt(0.975, nrow(thedata)-2)
est + c(-1,1)*tmult*se # value = 1.98 to 2.02

# Confidence interval for beta0
est <- lm.out$coef[2]
se <- summary(lm.out)$coef[2,2]
est + c(-1,1)*tmult*se # value = -0.0211 to -0.0197

##############################
# Part (c)
##############################
# Plot residuals vs fitted values
#      and a QQ-plot of the residuals
plot(lm.out)

# Plot the data and the fitted line
plot(thedata)
abline(lm.out$coef, col="blue", lwd=2)

# Everything looks fine.


##############################
# Problem 2
##############################
# The new y value
newy <- log10(35.6)

# The calibrate() function that we need
source("calibrate.R")

# Get the estimated concentration and 95% confidence interval
ci <- calibrate(thedata$concentration, thedata$response, newy)
# estimate = 21.8
# 95% confidence interval = 19.5 to 24.2

##############################
# end of lab3_solns.R
##############################
