######################################################################
# Computer notes             Statistics for Laboratory Scientists (2)
# Lecture 11                                  Johns Hopkins University 
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
# The anova() function in R doesn't
# do quite what we want for nested ANOVA;
# it compares each MS to the residual
# (within-subgroup) MS.
#
# The following function does what we want.
##############################
nested.anova <-
function(aov.output)
{
  anova.output <- anova(aov.output)
  nr <- nrow(anova.output) # number of rows in ANOVA table
  
  ms <- anova.output[,3] # mean squares
  df <- anova.output[,1] # degrees of freedom

  F <- ms[-nr] / ms[-1] # new F ratios
  pval <- 1-pf(F, df[-nr], df[-1]) # new p-values

  anova.output[-nr,4] <- F
  anova.output[-nr,5] <- pval

  anova.output
}

##############################
# Mosquito example:
#     data at http://www.biostat.jhsph.edu/~kbroman/teaching/labstat/fourth/mosq.csv
##############################
# read data; convert "individual" column to factor
mosq <- read.csv("mosq.csv")
mosq$individual <- factor(mosq$individual)

# the nested anova; we use the symbol "/" in the aov() function
#     to indicate the nesting
mosq.aov <- aov(length ~ cage / individual, data=mosq)
nested.anova(mosq.aov)

# what happens when we use the average of the two measurements
#     for each mosquito?
aves <- tapply(mosq$length, list(mosq$individual, mosq$cage), mean)
aves <- as.numeric(aves) # turn it into a vector (rather than a matrix)
cage <- factor(rep(c("A","B","C"), rep(4,3)))
anova(aov(aves ~ cage))

# what happens when we ignore the cages and just do
#     anova with individual mosquitos as the groups?
ind <- factor( paste(as.character(mosq$cage), as.character(mosq$ind), sep=":") )
anova(aov(mosq$length ~ ind))

# what happens when we ignore the individual mosquitoes and just
#     do ANOVA with the cages as the groups?
anova(aov(length ~ cage, data=mosq))

##############################
# 2nd example: flies
#     data at http://www.biostat.jhsph.edu/~kbroman/teaching/labstat/fourth/flies.csv
##############################
flies <- read.csv("flies.csv")
nested.anova(aov(rsp ~ strain / jar, data=flies))

##################
# End of comp11.R
##################
