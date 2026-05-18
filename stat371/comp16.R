######################################################################
# Computer notes, Lecture 16
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
url.show("https://kbroman.org/teaching/stat371/comp16.R")
######################################################################
##############################
# Example 1
##############################
# Example data
x <- c(102.5, 106.6,  99.8, 106.5, 103.7, 105.5,
        98.2, 104.1,  85.6, 105.5, 114.0, 112.2)
y <- c( 93.7,  90.9, 100.4,  92.0, 100.2, 104.6,
        95.4,  96.6,  99.2)

# two-sided t-test allowing un-equal population SDs
t.test(x,y)

# plot the data with confidence intervals
#     (the dotplot function is at the end of this file)
dotplot(x,y)

##############################
# Example 2
##############################
# one-tailed test example
x <- c(59.4, 52.3, 42.6, 45.1, 65.9, 40.8)
y <- c(82.7, 56.7, 46.9, 67.8, 74.8, 85.7)

# one-tailed t-test
t.test(x,y,alt="less")

# the dotplot
dotplot(x,y)

##############################
# Example 3
##############################
# another one-tailed test example
x <- c(63.3, 58.6, 59.0, 60.5, 56.3, 57.4)
y <- c(75.6, 65.9, 72.3, 58.0, 64.4, 66.2)-0.84
t.test(x,y,alt="less")
dotplot(x,y)

######################################################################
# My dotplot function
#     This function plots the data for two samples
#     If includeCI=TRUE, it also includes t-based confidence intervals
######################################################################
dotplot <-
function(x,y,includeCI=TRUE,labels=c("A","B"),xlim)
{
  # re-arrange data
  X <- c(x,y)
  Y <- rep(1:0,c(length(x),length(y)))

  # jitter Y positions
  Y <- Y + runif(length(Y),-0.1,0.1)
 
  # make sure x-limits allow plot of CI's, if requested
  # if CI's will be plotted, 
  if(includeCI) {
    xci <- t.test(x)$conf.int
    yci <- t.test(y)$conf.int
    xlimits <- range(c(x,y,xci,yci))
  }
  else xlimits <- range(c(x,y))

  if(!missing(xlim)) xlimits <- xlim

  # make plot
  plot(X,Y,ylim=c(-0.5,1.5),yaxt="n",lwd=2,xlab="",ylab="",
       xlim=xlimits)
  abline(h=0:1,lty=2,col="gray")
  points(X,Y,lwd=2)

  # add Y-axis labels
  u <- par("usr")
  segments(u[1],0:1,u[1]-diff(u[1:2])*0.03,0:1,xpd=TRUE)
  text(u[1]-diff(u[1:2])*0.08,1:0,labels,xpd=TRUE,cex=1.3)

  # add confidence intervals, if requested
  if(includeCI) {
    segments(xci[1],1.2,xci[2],1.2,lwd=2,col="blue")
    segments(xci,1.18,xci,1.22,lwd=2,col="blue")
    segments(mean(x),1.15,mean(x),1.25,lwd=2,col="blue")

    segments(yci[1],0.2,yci[2],0.2,lwd=2,col="red")
    segments(yci,0.18,yci,0.22,lwd=2,col="red")
    segments(mean(y),0.15,mean(y),0.25,lwd=2,col="red")
  }
}

##############################
# Pre/post example
##############################
x <- c(18.6, 14.3, 21.4, 19.3, 24.0)
y <- c(17.8, 24.1, 31.9, 28.6, 40.0)

t.test(y-x)

##################
# End of comp16.R
##################
