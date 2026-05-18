######################################################################
# Computer notes                 Statistics for Laboratory Scientists 
# Lecture 7                                  Johns Hopkins University 
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

####################################
# Calculating binomial probabilities
####################################
# dbinom calculates the Binomial probability function
# Pr(X=0) if X ~ binomial(n=9,p=20%)
dbinom(0, 9, 0.2)        
# Pr(X=0), Pr(X=1), Pr(X=2) and Pr(X=9) if X ~ binomial(n=9,p=20%)
dbinom(c(0,1,2,9), 9, 0.2) 
# Pr(X=7) if X ~ binomial(n=10,p=90%)
dbinom(7,10,0.9)

# binomial coefficients "n choose k"
choose(9,2)

# pbinom calculates the CDF of the binomial distribution
# Pr(X < 9) if X ~ Binomial(n=10, p=90%)
pbinom(8, 10, 0.9)
# Pr(X <= 20) if X ~ Binomial(n=80, p=30%)
pbinom(20,80,0.3)

# With the pbinom function, you can use the argument "lower.tail=FALSE"
#     to get Pr(X > k) rather than Pr(X <= k)
pbinom(8, 10, 0.9)                   # Pr(X <= 8)
pbinom(8, 10, 0.9, lower.tail=FALSE) # Pr(X > 8)

############################################
# Initial pictures of a probability function
#     and the corresponding CDF
############################################
p <- c(0.5, 0, 0.1, 0, 0.1, 0, 0.2)
par(las=1) # labels on y-axis horizontal
# bar plot with bars 1 unit wide and not separated
#     "names" puts the labels on the bars
barplot(p,col="gray40",width=1,space=0,ylab="p(x)",xlab="x",
        main="Probability function",names=1:7) 
    
# The CDF picture
# create an empty frame for the plot
plot(0,0,type="n",xlab="x",ylab="F(x)", ylim=c(0,1),xlim=c(0,8),
     main="Cumulative distribution function (cdf)")
# gets the exact limits of the figure
u <- par("usr") 
# add the horizontal line segments
segments(u[1],0,1,0,lwd=2)
segments(1,0.5,3,0.5,lwd=2)
segments(3,0.6,5,0.6,lwd=2)
segments(5,0.7,7,0.7,lwd=2)
segments(7,1.0,u[2],1.0,lwd=2)
# add the points at each end of each line segment
points(c(1,1,3,3,5,5,7,7),c(0,0.5,0.5,0.6,0.6,0.7,0.7,1.0),
       pch=16,col="white")
points(c(1,1,3,3,5,5,7,7),c(0,0.5,0.5,0.6,0.6,0.7,0.7,1.0),
       pch=c(1,16,1,16,1,16,1,16))


##############################
# Binomial probability plots
##############################

# a function to cut down on the typing a bit
prob.hist <-
function(y, x=(1:length(y))-1, ...)
{
  barplot(y, width=1, space=0, names=x, xlab="x",
          ylab="Probability", col="gray40", ...)
}

# First set of plots
par(las=1,mfrow=c(2,2))
prob.hist(dbinom(0:10, 10, 0.1), main="Binomial(n=10, p=0.1)", ylim=c(0,0.4))
prob.hist(dbinom(0:10, 10, 0.3), main="Binomial(n=10, p=0.3)", ylim=c(0,0.4))
prob.hist(dbinom(0:10, 10, 0.5), main="Binomial(n=10, p=0.5)", ylim=c(0,0.4))
prob.hist(dbinom(0:10, 10, 0.9), main="Binomial(n=10, p=0.9)", ylim=c(0,0.4))

# Second set of plots
par(las=1,mfrow=c(2,2))
prob.hist(dbinom(0:10, 10, 0.5), main="Binomial(n=10, p=0.5)", ylim=c(0,0.25))
prob.hist(dbinom(0:20, 20, 0.5), main="Binomial(n=20, p=0.5)", ylim=c(0,0.25))
prob.hist(dbinom(0:50, 50, 0.5), main="Binomial(n=50, p=0.5)", ylim=c(0,0.25))
prob.hist(dbinom(0:100, 100, 0.5), main="Binomial(n=100, p=0.5)", ylim=c(0,0.25))

# Third set of plots
par(las=1,mfrow=c(2,2))
prob.hist(dbinom(0:10, 10, 0.5), 0:10, main="Binomial(n=10, p=0.5)", 
          ylim=c(0,max(dbinom(0:10, 10, 0.5))))
prob.hist(dbinom(3:17, 20, 0.5), 3:17, main="Binomial(n=20, p=0.5)", 
          ylim=c(0,max(dbinom(3:17,20,0.5))))
prob.hist(dbinom(14:36, 50, 0.5), 14:36, main="Binomial(n=50, p=0.5)", 
          ylim=c(0,max(dbinom(14:36,50,0.5))))
prob.hist(dbinom(35:65, 100, 0.5), 35:65, main="Binomial(n=100, p=0.5)", 
          ylim=c(0,max(dbinom(35:65,100,0.5))))

# Fourth set of plots
par(las=1,mfrow=c(2,2))
prob.hist(dbinom(0:5, 10, 0.1), main="Binomial(n=10, p=0.1)", 
          ylim=c(0,max(dbinom(0:5, 10, 0.1))))
prob.hist(dbinom(0:7, 20, 0.1), main="Binomial(n=20, p=0.1)", 
          ylim=c(0,max(dbinom(0:7,20,0.1))))
prob.hist(dbinom(0:13, 50, 0.1), main="Binomial(n=50, p=0.1)",
          ylim=c(0,max(dbinom(0:13,50,0.1))))
prob.hist(dbinom(0:20, 100, 0.1), main="Binomial(n=100, p=0.1)", 
          ylim=c(0,max(dbinom(0:20,100,0.1))))

# Fifth set of plots
par(las=1,mfrow=c(2,2))
prob.hist(dbinom(0:24, 100, 0.1), main="Binomial(n=100, p=0.1)", 
          ylim=c(0,max(dbinom(0:24,100,0.1))))
prob.hist(dbinom(5:39, 200, 0.1), 5:39, main="Binomial(n=200, p=0.1)", 
          ylim=c(0,max(dbinom(5:39,200,0.1))))
prob.hist(dbinom(18:66, 400, 0.1), 18:66, main="Binomial(n=400, p=0.1)", 
          ylim=c(0,max(dbinom(18:66,400,0.1))))
prob.hist(dbinom(48:116, 800, 0.1), 48:116, main="Binomial(n=800, p=0.1)", 
          ylim=c(0,max(dbinom(48:116,800,0.1))))


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

##################
# End of comp07.R
##################
