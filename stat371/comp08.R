######################################################################
# Computer notes, Lecture 8
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
url.show("https://kbroman.org/teaching_old/stat371/comp08.R")
######################################################################

##############################
# Poisson probabilities
##############################
# dpois calculates the Poisson probability function
# ppois calculates the Poisson cumulative distribution function (cdf)
# Suppose X ~ Poisson(lambda=1.25)
dpois(0, 1.25) # Pr(X = 0)
1-dpois(0, 1.25) # Pr(X > 0)
dpois(2, 1.25) # Pr(X = 2)


##############################
# Draw a normal curve
##############################
x <- seq(20, 100, length=501)
par(las=1) # Make labels of Y-axis horizontal 
plot(x, dnorm(x, 60, 10), type="l", lwd=2,
     xlab="x", ylab="f(x)",
     main=expression(paste("Normal(", mu, "=60, ", sigma, "=10)")))
# Note: type="l" forces a line plot (rather than a bunch of points)
#       lwd=2 results in a thicker line
#       xlab and ylab change the x- and y-axis labels
#       main="blah blah blah" puts a title on the plot
#       expression() is used so that I can get the Greek symbols on the plot
#       paste() pastes a bunch of things together into one character string.


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
pnorm(9.9,10,0.1) + (1-pnorm(10.1,10,0.1))

##############################
# Another normal curve picture
# (It's a bit complicated; feel free to ignore it.)
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
segments(x, 0, x, -0.04, xpd=TRUE)
# text adds text
#     cex make the text larger
text(x, -0.08, c("-1.3 ", "0.7"), xpd=TRUE,cex=1.3)
# Note the space in "-1.3 "; I didn't like the way it
#     was centered below the tick mark without this

##################
# End of comp08.R
##################
