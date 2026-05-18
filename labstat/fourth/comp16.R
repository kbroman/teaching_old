######################################################################
# Computer notes             Statistics for Laboratory Scientists (2)
# Lecture 16                                 Johns Hopkins University 
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
# Regression for calibration
######################################################################
# A function to do the work
calibrate <-
function(x, y, newy)
{
  if(length(x) != length(y))
    stop("x and y must be the sample length")

  # get the fitted line
  lm.out <- lm(y~x)
  lm.sum <- summary(lm.out)
  beta <- lm.out$coef   # estimated coefficients
  sigma <- lm.sum$sigma # estimated residual SD

  newyb <- mean(newy)   # take the average of the observed data
  n <- length(x)        # no. calibration data points
  m <- length(newy)     # no. measurements on the unknown

  # estimate of the unknown x
  xhat <- (newyb - beta[1])/beta[2]

  tmult <- qt(0.975, n-2)  # multiplier from the t distribution
  xbar <- mean(x)          # average of the calibration x's
  sxx <- sum((x-xbar)^2)   # SXX

  g <- tmult / (abs(beta[2]) / (sigma / sqrt(sxx)))

  if(g >= 1) {  # can't give a finite interval
    lo <- -Inf
    hi <- Inf
  }
  else {        # we're happy
    updown <- ((xhat - xbar)*g^2 + (tmult * sigma / abs(beta[2])) *
      sqrt((xhat-xbar)^2 / sxx + (1-g^2)*(1/m+1/n)))/(1-g^2)
    lo <- xhat - updown
    hi <- xhat + updown
  }

  result <- c(xhat,lo,hi)
  names(result) <- c("est","lo","hi")
  result
}

# some data: concentration of quinine in a set of standards
dat <- data.frame(quinine=c(0,0,0, 12,12,12, 24,24,24, 36,36,36),
                  fluor=c(100.45,  98.92, 101.33,
                          133.19, 127.33, 126.78,
                          152.72, 157.43, 160.81,
                          188.73, 191.96, 183.70))

# estimate concentration corresponding to fluor = 143.70
calibrate(dat$quinine, dat$fluor, 143.70)

# estimate concentration on the basis of 3 measurements:
newy <- c(148.56, 149.36, 150.29)
calibrate(dat$quinine, dat$fluor, newy)

##################
# End of comp16.R
##################
