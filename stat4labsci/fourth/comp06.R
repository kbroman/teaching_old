######################################################################
# Computer notes             Statistics for Laboratory Scientists (2)
# Lecture 6                                  Johns Hopkins University 
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

######################################################################
# Example data
######################################################################
x <- c(672, 747, 749, 792, 875, 888, 930, 962, 994,1295)
y <- c(290, 359, 384, 466, 510, 516, 522, 532, 595, 706)

# plot the data
par(las=1) # make the Y-axis labels vertical rather than horizontal
stripchart(list(x,y), method="jitter",
           group.names=c("X","Y"), pch=1) # pch=1 makes the points circles
# add horizontal lines
abline(h=1:2,lty=2)
# add line segments at the sample means
mx <- mean(x); my <- mean(y)
segments(c(mx,my), 1:2-0.1, c(mx,my), 1:2+0.1, lwd=2, col="blue")

# perform test of whether the population variances are equal
var.test(x,y)

# get 95% confidence interval for the ratio of population SDs
sqrt( var.test(x,y)$conf.int )

######################################################################
# Brute force method
######################################################################
# the ratio of the sample variances
sampvar.ratio <- var(x)/var(y)

# the 2.5th and 97.5th percentiles of the F distribution
quant <- qf(c(0.025, 0.975), length(x)-1, length(y)-1)

# the 95% CI interval for the ratio of the population variances
sampvar.ratio * rev(1/quant) # reverse() reverses the order of a vector

# the 95% CI for the ratio of the population SDs
sqrt(sampvar.ratio * rev(1/quant))

# P-value for the test of whether the two population variances are equal
#     Note: this works only when sampvar.ratio > 1
2*(1 - pf(sampvar.ratio, length(x)-1, length(y)-1))

# If we had focussed on var(y) / var(x) rather than var(x) / var(y),
# we would do the following:
sampvar.ratio <- var(y) / var(x)
quant <- qf(c(0.025, 0.975), length(y)-1, length(x)-1)
sqrt(sampvar.ratio * rev(1/quant)) # 95% for ratio of pop'n SDs
2*pf(sampvar.ratio, length(y)-1, length(x)-1) # use this since sampvar.ratio < 1



######################################################################
# Example data
######################################################################
coag <- c(62, 60, 63, 59,
          63, 67, 71, 64, 65, 66,
          68, 66, 71, 67, 68, 68,
          56, 62, 60, 61, 63, 64, 63, 59)
ttt <-  c("A","A","A","A",
          "B","B","B","B","B","B",
          "C","C","C","C","C","C",
          "D","D","D","D","D","D","D","D")

# make the treatment "ttt" a factor
ttt <- factor(ttt, levels=c("A","B","C","D"))

# plot the data
par(las=1) # makes Y-axis labels horizontal
stripchart(coag ~ ttt, pch=1, method="jitter") # pch=1 makes the points circles

# the within-group means
tapply(coag, ttt, mean)
# note: tapply splits the first vector into groups according to the values
#       in the second vector, and then "applies" the function, mean(), to
#       each group.

# number of data points per group
tapply(coag, ttt, length)

# the ANOVA table in R
summary( aov(coag ~ ttt) ) 


##################
# End of comp06.R
##################
