######################################################################
# Computer notes                 Statistics for Laboratory Scientists 
# Lecture 9                                  Johns Hopkins University 
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

##################################
# Calculating normal probabilities
##################################
# The key function to use in calculating normal
#     probabilities is pnorm().

# For X ~ Normal(mean=10, SD=2),
#     Pr(9 < X < 13)
pnorm(13,10,2) - pnorm(9,13,2)

# For X ~ Normal(mean=3, SD=0.5),
#     Pr(X > 4.3)
1 - pnorm(4.3, 3, 0.5)

# For X ~ Normal(mean=10, SD=0.1),
#     Pr(|X -10| > 0.1)
pnorm(9.9,10,0.1 + (1-pnorm(10.1,10,0.1))


##############################
# A normal curve picture
##############################
par(bty="n") # suppresses the "box" around the figure
# plot of the standard normal curve from -4 to +4
#     Note: xaxt and yaxt suppress the axes
x <- seq(-4,4,length=501)
plot(x,dnorm(x),type="l",xaxt="n",yaxt="n",xlab="",ylab="")
# The following defines a polygon to be shaded in.
#     X = sequence from -1.3 to 0.7 and then 0.7 and -1.3
#     Y = top of normal curve and then 0, 0 (the corners)
a <- seq(-1.3,0.7,length=501)
X <- c(a,0.7,-1.3)
Y <- c(dnorm(a), 0, 0)
# The following draws a polygon
#     density defines the spacing of the hatching
#     angle defines the angle of the hatching
polygon(X,Y,density=8,angle=-65)
# re-draw the normal curve so that it's thick
lines(x,dnorm(x),lwd=2)
# add a line below the curve
abline(h=0)
# draw tick marks + labels
x <- c(-1.3,0.7)
# segments draws a line segment
#     xpd allows plotting outside the figure region
segments(x, 0, x, -0.015, xpd=TRUE)
# text adds text
#     cex make the text larger
text(x, -0.05, c("-1.3 ", "0.7"), xpd=TRUE,cex=1.3)
# Note the space in "-1.3 "; I didn't like the way it
#     was centered below the tick mark without this


##############################
# Joint density pictures
##############################
# grids for x and y
x <- seq(-3,3,length=51)
y <- seq(-3,3,length=51)
# empty matrix to contain z
z <- matrix(nrow=51,ncol=51)
# calculate the fake density function
for(j in 1:51)
    z[,j] <- exp(-0.5*(x*x+y[j]*y[j]-0.5*x*y[j]))

# "perspective" plot
#     theta, phi define the "viewing direction"
#     ltheta defines the "lighting direction"
#     expand "expands" the z-axis (the vertical axis)
persp(x,y,z, theta=30, phi=30, col="lightblue", ltheta=120,
      expand=0.5) 

# contour plot in color
#     image creates the colors
#         the "col" arg specifies the set of colors to use
image(x,y,z,col=rev(rainbow(256,start=0,end=2/3)))
#     contour adds the contour lines
contour(x,y,z,drawlabels=FALSE,add=TRUE,levels=seq(0.1,0.9,by=0.2))

##################
# End of comp08.R
##################
