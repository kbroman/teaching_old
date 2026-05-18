######################################################################
# Computer notes, Lecture 33
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
url.show("https://kbroman.org/teaching_old/stat371/comp33.R")
######################################################################

######################################################################
# Fathers' and daughters' heights
######################################################################
# Pearson & Lee (1906) data; 
pear <- read.csv("https://kbroman.org/teaching_old/stat371/father_daughter.csv",
                 comment.char="#")

# plot the data
plot(pear)

# calculate the mean and SD of fathers' and daughters' heights
mes <- apply(pear, 2, mean)
sds <- apply(pear, 2, sd)

# calculate the correlation between them
cor(pear) # the upper-right and lower-left number is the correlation

# or do this:
cor(pear[,1], pear[,2])

# calculate the regression of daughter's ht on father's ht
#     (i.e., for predicting daughter from father)
lm.outA <- lm(daughter ~ father, data=pear)
summary(lm.outA)

# calculate the regression of father's ht on daughter's ht
#     (i.e., for predicting father from daughter)
lm.outB <- lm(father ~ daughter, data=pear)
summary(lm.outB)

# plot the data with the two regression lines
coA <- lm.outA$coef # intercept and slope for regr A
coB <- lm.outB$coef # intercept and slope for regr B

# transform regression B coefficients
#     y = mx + b   ->  x = y/m - b/m
coB[1] <- -coB[1]/coB[2]
coB[2] <- 1/coB[2]

# now make the plot
plot(pear)
abline(coA,lwd=2,col="green")
abline(coB,lwd=2,col="orange")

##################
# End of comp33.R
##################
