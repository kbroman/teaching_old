######################################################################
# Computer notes             Statistics for Laboratory Scientists (2)
# Lecture 1                                  Johns Hopkins University 
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

##############################
# Example data
##############################
obs <- c(35, 43, 22)

# expected proportions (i.e., the null hypothesis)
p0 <- c(0.25, 0.5, 0.25)

# expected counts (note: sum(obs) = total count)
exp <- sum(obs) * p0

# LRT statistic = 4.958
lrt <- 2 * sum(obs * log(obs/exp)) 

# chi-square statistic = 5.34
chis <- sum( (obs-exp)^2 / exp)

# P-value for LRT = 0.084
1 - pchisq(lrt, 3-1)

# P-value for chi-square test = 0.069
1 - pchisq(chis, 3-1)

##############################
# the built-in function, chisq.test
##############################
# you can use chisq.test() to do the chi-square test
chisq.test(obs, p=c(1/4,1/2,1/4))

##################
# End of comp01.R
##################
