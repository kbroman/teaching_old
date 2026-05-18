######################################################################
# Computer notes             Statistics for Laboratory Scientists (2)
# Lecture 14                                 Johns Hopkins University 
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
# David Sullivan's pf3d7 plasmodium data
###############################################

# read the data
h2o2 <- c(0,0,0,10,10,10,25,25,25,50,50,50)
od <- c(0.3399,0.3563,0.3538,0.3168,0.3054,0.3174,
        0.2460,0.2618,0.2848,0.1535,0.1613,0.1525)

# make it a data frame
pf3d7 <- data.frame(h2o2=h2o2, od=od)

# the ANOVA table
aov.out <- aov(od ~ h2o2, data=pf3d7)
anova(aov.out)

######################################################################
# Fathers' and daughters' heights
######################################################################
# Pearson & Lee (1906) data; available at:
#     http://www.biostat.jhsph.edu/~kbroman/labstat/fourth/father_daughter.csv
pear <- read.csv("father_daughter.csv")

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
# End of comp14.R
##################
