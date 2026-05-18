######################################################################
# Computer notes, Lecture 2
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
url.show("http://www.biostat.wisc.edu/~kbroman/teaching/stat371/comp02.R")
######################################################################

##############################
# Dotplots
##############################

# simulate some fake data
x <- rnorm(15, 30, 5)
y <- rnorm(15, 38, 5)

# need to combine the data into a single vector
z <- c(x,y)

# need to create another vector with indicators of the two groups
groups <- rep( c("x","y"), c(length(x), length(y)) )

# The function stripchart is used to make a dot plot
#   method="jitter" jitters the points
#   pch=1 makes the points circles rather than squares
#   las=1 makes the y-axis labels horizontal 
stripchart(z ~ groups, method="jitter", pch=1, las=1)

# add some horizontal lines
#   lty=2 makes these dashed
abline(h=1:2, lty=2)

##############################
# Histograms
##############################
# simulate some fake data
x <- rnorm(1000, 30, 5)

# The function hist is used to make a histogram
hist(x)

# By default, hist uses a small number of bins
# Use 'breaks' to indicate the number of bins
hist(x, breaks=60)

# Alternatively, give a vector of actual breakpoint locations
hist(x, breaks=seq(0, 50, by=1))

# Use par(mfrow=c(2,1)) to make multiple histograms together
#    [The first number is the number of rows, the second is
#     the number of columns]
y <- rnorm(1000, 38, 5)
par(mfrow=c(2,1))
hist(x, breaks=seq(0, 60, by=1))
hist(y, breaks=seq(0, 60, by=1))

##############################
# boxplots
##############################
# The function boxplot is for making boxplots
boxplot(x)

# You can give multiple data sets to make side-by-side boxplots
boxplot(x, y)

# Use the argument 'names' to add labels
boxplot(x, y, names=c("x","y"))

##############################
# Summary statistics
##############################
x <- c(69, 27, 87, 64, 58, 58, 98, 25, 50, 47, 
       76, 37, 98, 70, 74, 71,  2, 38, 90, 37)
# mean
mean(x)

# median
median(x)

# SD
sd(x)

# 25th percentile (aka quantile)
# [note that it does a bit of interpolation]
quantile(x, 0.25)

# 25th and 75th percentiles
quantile(x, c(0.25, 0.75))

# inter-quartile range
diff(quantile(x, c(0.25, 0.75)))

# range
range(x)

# geometric mean
exp(mean(log(x)))

##################
# End of comp02.R
##################
