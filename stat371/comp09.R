######################################################################
# Computer notes, Lecture 9
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
url.show("http://www.biostat.wisc.edu/~kbroman/teaching/stat371/comp09.R")
######################################################################

# Assume heights of men are normally distributed with mean 69 and SD 3
# What is the 25th percentile?
qnorm(0.25, 69, 3)

qnorm(0.25)*3 + 69

##################
# End of comp09.R
##################
