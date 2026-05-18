######################################################################
# Computer notes, Lecture 3                 
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
url.show("https://kbroman.org/teaching/stat371/comp03.R")
######################################################################

######################################################################
# RANDOMIZED DESIGN
# How can one use R to assign subjects to treatment groups at random?
#
# First, consider the case of 20 subjects to be split into two groups
# with ten individuals in each.
#
# Use the function sample() to permute the 20 subjects
# Make the first ten the treatment group and the second ten the controls
######################################################################
sample(1:20)

######################################################################
# Alternatively, use sample() to pull out a random ten individuals
######################################################################
sample(1:20, 10)

######################################################################
# RANDOMIZED BLOCK DESIGN (aka stratified)
# How about the example with 20 males, 20 females,
# and 4 per day, and we want (on each day) one treated male,
# one control male, one treated female, and one control female
#
# The R code must be (much) more complicated.  
# (Perhaps it would be easier to use a deck of cards?)
######################################################################
# call the females individuals 1, 2, ..., 20 and the males 21, ..., 40
females <- 1:20
males <- 21:40
# randomize the order of these
females <- sample(females)
males <- sample(males)
# create treatment assignements
femalettt <- rep(c("C","T"), rep(10, 2))
malettt <- rep(c("C","T"), rep(10, 2))
# paste the treatment assignments onto the subject IDs
females <- paste(females, femalettt, sep="")
males <- paste(males, malettt, sep="")
# create a matrix with one row per day
assignments <- matrix(c(females, males), ncol=4)
# randomize the rows
assignments <- apply(assignments, 1, sample)
assignments

##################
# End of comp03.R
##################
