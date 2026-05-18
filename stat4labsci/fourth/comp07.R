######################################################################
# Computer notes             Statistics for Laboratory Scientists (2)
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

# The data used in this lecture are available at
#   http://www.biostat.jhsph.edu/~kbroman/teaching/labstat/fourth/meioses.csv
# Reference: Broman et al. (1998) Am J Hum Genet 63:861-869
#
# The data have three columns: family, female, male.  These last two give
# the total number of crossovers on the 22 autosomes in each of the male
# and female meioses.

meioses <- read.csv("meioses.csv")

# need to make the family column a "factor" to get anova to work properly
meioses$family <- as.factor(meioses$family)

# plot the data
par(las=1) # make y-axis labels horizontal
stripchart(meioses$female ~ meioses$family, method="jitter", pch=1)

# get the within-group means
female.me <- tapply(meioses$female, meioses$family, mean)

# add them to the plot
segments(female.me, (1:8)-0.25, female.me, (1:8)+0.25, lwd=2, col="blue")

# same for the male counts
par(las=1)
stripchart(meioses$male ~ meioses$family, method="jitter", pch=1)
male.me <- tapply(meioses$male, meioses$family, mean)
segments(male.me, (1:8)-0.25, male.me, (1:8)+0.25, lwd=2, col="blue")

# ANOVA tables
out.fem <- aov(female ~ family, data=meioses)
summary(out.fem)

out.mal <- aov(male ~ family, data=meioses)
summary(out.mal)

# permutation tests
n.sim <- 1000
perm.fem <- perm.mal <- 1:n.sim
simdat <- meioses
for(i in 1:n.sim) {
  simdat$family <- sample(simdat$family)
  perm.fem[i] <- anova(aov(female ~ family, data=simdat))[1,4]
  perm.mal[i] <- anova(aov(male ~ family, data=simdat))[1,4]
  # Note: the function anova() [rather than summary()] allows me
  #       to pull out the F statistic from the anova table.
}

# observed F statistics
obs.fem <- anova(out.fem)[1,4]
obs.mal <- anova(out.mal)[1,4]

# estimated P-values
mean(perm.fem >= obs.fem)
mean(perm.mal >= obs.mal)


##################
# End of comp07.R
##################
