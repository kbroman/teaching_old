######################################################################
# Computer notes, Lecture 4                 
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
url.show("https://kbroman.org/teaching_old/stat371/comp04.R")
######################################################################

# Note the reference to "url.show" above, which you can use to download
# and open the computer notes related to a lecture, directly within R.

# For Homework 2, the data set for the first problem (2.79) is
# available at the following:
#   https://kbroman.org/teaching_old/stat371/data_2-79.txt

# You can load this into R with the following:
x <- scan("https://kbroman.org/teaching_old/stat371/data_2-79.txt")

# Type the name of the object (x) and you'll see the data.
x

# As an alternative, you can download the file to your computer and then
# load it into R, again using the scan() function, using a reference to
# the location and name of the file.
#
# It is easiest if you change your working directory (using the menu bar)
# to the location of the file.  You can then just type:
# x <- scan("data_2-79.txt")

# As an alternative, you can use the function read.table().  You'll want
# to use the argument header=FALSE since the file doesn't have a label
# for the data:
x <- read.table("https://kbroman.org/teaching_old/stat371/data_2-79.txt",
                header=FALSE)

# This is a bit harder to work with, since the data are then a "data frame".
# That is, a rectangular 'matrix' with columns being variables and rows being
# subjects.  (Here there's just one column.)

# Refer to columns and rows using [ , ] with rows being before the comma
# and columns being after the comma.

# The first five rows
x[1:5,]

# Save just the first column, to make things easier
x <- x[,1]

# Now you can calculate the mean, median, and SD and make a dotplot or histogram
summary(x)
median(x)
mean(x)
sd(x)

hist(x, breaks=12)
stripchart(x, method="jitter", pch=1)

# For a bit more on data frames...note that R comes with a variety of data sets,
# most in the form of data frames.
#
# Type data() to see the available data sets.
data()

# Type data(ChickWeight) to load the data set 'ChickWeight' into your workspace.
data(ChickWeight) 

# Type ls() or objects() to see that the data are now in your workspace
ls()
objects()

# summary of the four variables in the data
summary(ChickWeight)

# the names of the variables in the data
names(ChickWeight)

# number of rows
nrow(ChickWeight)

# number of columns
ncol(ChickWeight)

# the first five rows
ChickWeight[1:5,]

# the first column/variable
ChickWeight[,1]

# you can also refer to the columns by name, using a $
ChickWeight$weight

# This way you can make a histogram of the first variable
hist(ChickWeight$weight, breaks=50)

##################
# End of comp04.R
##################
