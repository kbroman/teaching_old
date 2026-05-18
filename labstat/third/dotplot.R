######################################################################
# dotplot
#     This function plots the data for two samples
#     If includeCI=TRUE, it also includes t-based confidence intervals
######################################################################
dotplot <-
function(x,y,includeCI=TRUE)
{
  # Arrange the data
  X <- c(x,y)
  Y <- rep(1:0,c(length(x),length(y)))

  # jitter the Y positions
  Y <- Y + runif(length(Y),-0.1,0.1)
 
  # If requested, calculate the CI's
  # and make sure x-limits allow plot of CI's
  if(includeCI) {
    xci <- t.test(x)$conf.int
    yci <- t.test(y)$conf.int
    xlimits <- range(c(x,y,xci,yci))
  }
  else xlimits <- range(c(x,y))

  # make plot
  plot(X,Y,ylim=c(-0.5,1.5),yaxt="n",lwd=2,xlab="",ylab="",
       xlim=xlimits)
  abline(h=0:1,lty=2,col="gray")

  # add Y-axis labels
  u <- par("usr")
  segments(u[1],0:1,u[1]-diff(u[1:2])*0.03,0:1,xpd=TRUE)
  text(u[1]-diff(u[1:2])*0.08,1:0,c("A","B"),xpd=TRUE,cex=1.3)

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
