######################################################################
# Computer notes, Lecture 32
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
url.show("https://kbroman.org/teaching/stat371/comp32.R")
######################################################################

###############################################
# pf3d7 plasmodium data
###############################################

# read the data
h2o2 <- c(0,0,0,10,10,10,25,25,25,50,50,50)
pf3d7 <- c(0.3399,0.3563,0.3538,0.3168,0.3054,0.3174,
           0.2460,0.2618,0.2848,0.1535,0.1613,0.1525)

# calculate some statistics
n <- length(h2o2)
yb <- mean(pf3d7)
xb <- mean(h2o2)
sxy <- sum(h2o2*pf3d7)-n*xb*yb
#    or do: sxy <- sum( (h2o2-xb) * (pf3d7-yb) )
sxx <- sum(h2o2^2)-n*xb^2
#    or do: sxx <- sum( (h2o2-xb)^2 )

# parameter estimates
b1hat <- sxy/sxx
b0hat <- yb-b1hat*xb

# predicted (fitted) values
yhat <- b0hat + b1hat*h2o2

# residual standard deviation
rss <- sum((pf3d7-yhat)^2)
sigmahat <- sqrt(rss/(n-2))

# using the function lm()
lm.pf3d7 <- lm(pf3d7 ~ h2o2)
summary(lm.pf3d7)

# plot the data
par(mfrow=c(1,1),las=1)
plot(h2o2,pf3d7,xaxt="n",xlab="H2O2 concentration",ylab="OD")
axis(1,c(0,10,25,50))
abline(lsfit(h2o2,pf3d7),lwd=2,col="orange",lty=2)

##################
# End of comp32.R
##################
