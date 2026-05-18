######################################################################
# Computer notes                 Statistics for Laboratory Scientists 
# Lecture 5                                  Johns Hopkins University 
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

###############################################
# Prop'n of heads in many tosses of a fair coin
###############################################
# toss 10000 coins; 1=heads and 0=tails
x <- sample(0:1, 10000, repl=TRUE)

# The cumulative number of heads
nH <- cumsum(x)

# Note: look at how the "cumsum" function works:
y <- c(0,1,0,0,1)
z <- cumsum(y)
# z = c(0, 1, 1, 1, 2)

# To get the *proportion* of heads, divide each
#     element of x by the number of tosses so far
pH <- nH/(1:10000)

# plot the proportion of heads against the number of tosses
plot(pH, type="l", ylim=c(0,1))
abline(h=0.5, col="gray", lty=2)

# plot just the first 100 tosses
plot(pH[1:100], type="l", ylim=c(0,1))
abline(h=0.5, col="gray", lty=2)

# plot the difference between the observed number of
#     heads and the expected number
eH <- (1:10000)/2
plot(nH - eH, type="l")
abline(h=0, col="gray", lty=2)

##################
# End of comp05.R
##################
