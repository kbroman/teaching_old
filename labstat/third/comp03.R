######################################################################
# Computer notes                 Statistics for Laboratory Scientists 
# Lecture 3                                  Johns Hopkins University 
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
# Note: The R code for the figures below is probably more complicated
#       then it needs to be.  Some things that I do in R are not very
#       efficient.  And I like to have everything just right.  
######################################################################

##############################
# How I made the dotplot figures on slide 4:
##############################
# First, create fake data
a1 <- 0; b1 <- 35
# below, random numbers that are normally distributed,
#     with numbers below 0 set to 0
a2 <- rnorm(10,5,15); a2[a2<0] <- 0
b2 <- rnorm(10,25,15); b2[b2<0] <- 0

# the maximum of all of the points
ymax <- max(c(a1,b1,a2,b2)) 

##############################
# The first panel of the figure.
##############################
#     par(las=1) makes y-axis numbers horizontal
#     par(mfrow=c(1,2)) makes next two plots on one graphical device
#         with one row and two columns
par(las=1,mfrow=c(1,2))
# note that xaxt="n" supresses plotting of x-axis labels and ticks
#     lwd=2 makes "line width" for the points thicker.
plot(c(0,1),c(a1,b1),xlab="Treatment",ylab="Tumor mass",xaxt="n",
     xlim=c(-0.5,1.5),ylim=c(0,ymax),lwd=2)
# add vertical dashed gray lines for each group
abline(v=c(0,1),lty=2,col="gray")
# plot points again so they are on top of the lines
points(c(0,1),c(a1,b1),lwd=2)
# Add the x-axis
axis(side=1, at=0:1, labels=c("Placebo", "Substance X"))

##############################
# Here's the second panel
##############################
# Creates the x-axis locations, jittered slightly.
x <- rep(c(0,1),c(10,10)) + runif(20,-0.1,0.1)
# all the rest of this is as with the first panel
plot(x,c(a2,b2),xlab="Treatment",ylab="Tumor mass",xaxt="n",
     xlim=c(-0.5,1.5),ylim=c(0,ymax),lwd=2)
abline(v=c(0,1),lty=2,col="gray")
points(x,c(a2,b2),lwd=2)
# Add the x-axis
axis(side=1, at=0:1, labels=c("Placebo", "Substance X"))


######################################################################
# How I made the field plots, illustrating randomization and blocking
######################################################################
# This makes each plot fill the grapics window
par(mfrow=c(1,1))

##############################
# The first plot
##############################
# this plots a rectangle.  type="n" supresses plotting of the point.
plot(0,0,xlim=c(0,100),ylim=c(0,100),xlab="",ylab="",
     xaxt="n",yaxt="n",type="n")
# Get the actual limits of the x-axis and y-axis
u <- par("usr")
# Plot a title in large letters; cex=2.2 increases the letter size.
text(mean(u[1:2]),u[4]+diff(u[3:4])*0.10,"Randomization and blocking",
     cex=2.2,xpd=TRUE)
# make lines around plot *much* thicker
abline(h=u[3:4],v=u[1:2],lwd=4)
# add the interval horizontal and vertical lines; lwd=2 makes them thick
abline(h=seq(u[3],u[4],length=5),lwd=2)
abline(v=seq(u[1],u[2],length=7),lwd=2)

##############################
# The second plot
##############################
# This names of the "treatments"
let <- c("A","B","C","D")
# Create a matrix with A-D in each column
#     The function cbind() "binds" its arguments as columns to
#     form a matrix.
grp <- cbind(let, let, let, let, let, let)
# Most of the rest is as before
plot(0,0,xlim=c(0,100),ylim=c(0,100),xlab="",ylab="",
     xaxt="n",yaxt="n",type="n")
u <- par("usr")
text(mean(u[1:2]),u[4]+diff(u[3:4])*0.10,"Deterministic",
     cex=2.2,xpd=TRUE)
abline(h=u[3:4],v=u[1:2],lwd=4)
abline(h=seq(u[3],u[4],length=5),lwd=2)
abline(v=seq(u[1],u[2],length=7),lwd=2)
# determine the center of each "plot" rectangle
x <- seq(u[1],u[2],length=7)
x <- x[-length(x)] + diff(x)/2
y <- seq(u[3],u[4],length=5)
y <- y[-length(y)] + diff(y)/2
col <- c("black","blue","red","green")
# add the text---the assigned treatments
for(i in 1:ncol(grp))
  for(j in 1:nrow(grp))
    text(x[i],y[j],grp[j,i],cex=1.5,col=col[grp[j,i]==let])

##############################
# The third plot
##############################
# A different deterministic assignment of treatments
grp <- cbind(let, let[c(2,3,4,1)], let[c(3,4,1,2)],
              let[c(4,1:3)], let, let[c(2,3,4,1)])
# the rest is as before
plot(0,0,xlim=c(0,100),ylim=c(0,100),xlab="",ylab="",
     xaxt="n",yaxt="n",type="n")
u <- par("usr")
text(mean(u[1:2]),u[4]+diff(u[3:4])*0.10,"Deterministic",
     cex=2.2,xpd=TRUE)
abline(h=u[3:4],v=u[1:2],lwd=4)
abline(h=seq(u[3],u[4],length=5),lwd=2)
abline(v=seq(u[1],u[2],length=7),lwd=2)
for(i in 1:ncol(grp))
  for(j in 1:nrow(grp))
    text(x[i],y[j],grp[j,i],cex=1.5,col=col[grp[j,i]==let])

##############################
# The fourth plot
##############################
# A random assignment of treatments to groups
#     rep(let,6) creates a vector of length 24, with
#         A-D repeated six times.
#     sample(rep(let,6)) gives a random permutation (shuffle) of
#         this vector
#     matrix(x, nrow=4,ncol=6) turns the vector x into a matrix
#         with four rows and six columns
grp <- matrix(sample(rep(let,6)),nrow=4,ncol=6)
# The rest is as before.
plot(0,0,xlim=c(0,100),ylim=c(0,100),xlab="",ylab="",
     xaxt="n",yaxt="n",type="n")
text(mean(u[1:2]),u[4]+diff(u[3:4])*0.10,"Completely at random",
     cex=2.2,xpd=TRUE)
u <- par("usr")
abline(h=u[3:4],v=u[1:2],lwd=4)
abline(h=seq(u[3],u[4],length=5),lwd=2)
abline(v=seq(u[1],u[2],length=7),lwd=2)
for(i in 1:ncol(grp))
  for(j in 1:nrow(grp))
    text(x[i],y[j],grp[j,i],cex=1.5,col=col[grp[j,i]==let])

##############################
# The fifth plot
##############################
# This makes each column of "grp" a random permutation (shuffle)
#     of the letters A-D.
for(i in 1:6) grp[,i] <- sample(let)
# The rest is as before.
plot(0,0,xlim=c(0,100),ylim=c(0,100),xlab="",ylab="",
     xaxt="n",yaxt="n",type="n")
u <- par("usr")
text(mean(u[1:2]),u[4]+diff(u[3:4])*0.10,"Random within blocks",
     cex=2.2,xpd=TRUE)
abline(h=u[3:4],v=u[1:2],lwd=4)
abline(h=seq(u[3],u[4],length=5),lwd=2)
abline(v=seq(u[1],u[2],length=7),lwd=2)
for(i in 1:ncol(grp))
  for(j in 1:nrow(grp))
    text(x[i],y[j],grp[j,i],cex=1.5,col=col[grp[j,i]==let])

##################
# End of comp03.R
##################
