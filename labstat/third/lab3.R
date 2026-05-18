######################################################################
# R code                         Statistics for Laboratory Scientists 
# Lab 3                                      Johns Hopkins University 
######################################################################
# This file contains the miscellaneous R commands that appear in the
# lab handout/pdf file.
#
# Lines beginning with the symbol '#' are comments in R.  All other
# lines contain code.
#
# In R for Windows, you may wish to open this file from the menu bar
# (File:Display file); you can then copy commands into the command
# window.  (Use the mouse to highlight one or more lines; then
# right-click and select "Paste to console".)
######################################################################

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

##############################
# Hummer et al. (2001) data
##############################
hummer <- read.csv("http://www.biostat.jhsph.edu/~kbroman/teaching/data/hummer.csv")


x <- hummer[4:6,3]
y <- hummer[1:3,3]

x <- hummer[hummer[,1]=="IFN" & hummer[,2]=="--", 3]
y <- hummer[hummer[,1]=="IFN" & hummer[,2]=="++", 3]

dotplot(x,y)

x2 <- hummer[hummer[,1]=="SV" & hummer[,2]=="--", 3]
y2 <- hummer[hummer[,1]=="SV" & hummer[,2]=="++", 3]
dotplot(x2,y2)

t.test(x)

t.test(y)
t.test(x2)
t.test(y2)

?t.test

t.test(x, conf.level=0.99)

t.test(x, y)

t.test(y, x)

t.test(x, y, var.equal=TRUE)

t.test(x2, y2)

t.test(x2, y2, alternative="two.sided")
t.test(x2, y2, alternative="less")
t.test(x2, y2, alternative="greater")

t.test(activity ~ p53, data=hummer, subset=(medium=="IFN"))

t.test(activity ~ p53, data=hummer, subset=(medium=="SV"))

##############################
# Berrios et al. (2001) data
##############################
# The following line will need to be revised,
#     according to the location of the file.
berrios <- read.csv("http://www.biostat.jhsph.edu/~kbroman/teaching/data/berrios.csv")

x <- berrios[berrios[,1]==1,2]
y <- berrios[berrios[,1]==4,2]

dotplot(x,y)


###############
# End of lab3.R
###############
