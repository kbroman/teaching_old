# x, y = the calibration data
# newy = y observation(s) from a single unknown
#        whose x is to be determined

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
