######################################################################
# Computer notes, Lecture 31
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
url.show("http://www.biostat.wisc.edu/~kbroman/teaching/stat371/comp31.R")
######################################################################

# The data
lard.dat <- data.frame(rsp = c(709,592,679,538,699,476,657,508,594,505,677,539),
                       sex = factor( rep(c("M","F"), rep(6,2)) ),
                       lard = factor( rep(c("fresh","rancid"),6) ))

# The one-way anova; using the ":" symbol pastes the two factors together
anova( aov(rsp ~ sex:lard, data=lard.dat) )

# The two-way anova; using the "*" symbol indicates to look at the interaction
anova( aov(rsp ~ sex*lard, data=lard.dat) )

# A two-way anova with an additive model:
#     using the "+" symbol says "no intxn" 
#     the iteraction SS is then included in the "residual" SS
anova( aov(rsp ~ sex+lard, data=lard.dat) )

# within-group means
means <- tapply(lard.dat$rsp, list(lard.dat$sex, lard.dat$lard), mean)

# means by sex
sex.means <- tapply(lard.dat$rsp, lard.dat$sex, mean)

# means by sex
lard.means <- tapply(lard.dat$rsp, lard.dat$lard, mean)

# an interaction plot
interaction.plot(lard.dat$sex, lard.dat$lard, lard.dat$rsp,
                 lty=1, col=c("blue","red"), lwd=2)

# the same, but reversing the role of "sex" and "lard"
interaction.plot(lard.dat$lard, lard.dat$sex, lard.dat$rsp,
                 lty=1, col=c("blue","red"), lwd=2)

######################################################################
# Second example: mouse: food x temp
######################################################################
mouse <- read.csv("http://www.biostat.wisc.edu/~kbroman/teaching/stat371/mousedata.csv")

# numbers of individuals per condition
tapply(mouse$rsp, list(mouse$food, mouse$temp), length)

# two-way anova 
aov.out <- aov(log(rsp) ~ food * temp, data=mouse)
anova(aov.out)

# diagnostic plots
plot(aov.out)

# interaction plot
interaction.plot(mouse$food, mouse$temp, log2(mouse$rsp),
                 lwd=2, col=c("blue","red"), lty=1)

# reverse role of food and temp
interaction.plot(mouse$temp, mouse$food, log2(mouse$rsp),
                 lwd=2, col=c("blue","red"), lty=1)

######################################################################
# Third example: flies: density x strain, no replicates
######################################################################
# The data
flies <- data.frame(rsp = c( 9.6, 9.3, 9.3,       10.6, 9.1, 9.2,
                             9.8, 9.3, 9.5,       10.7, 9.1,10.0,
                            11.1,11.1,10.4,       10.9,11.8,10.8,
                            12.8,10.6,10.7),
                    strain = factor(rep(c("OL","BELL","bwb"),7)),
                    density = factor(rep(c(60,80,160,320,640,1280,2560),rep(3,7))))

interaction.plot(flies$density, flies$strain, flies$rsp,
                 lwd=2, col=c("blue","red","green"), lty=1)

flies.aovA <- aov(rsp ~ strain * density, data=flies)
anova(flies.aovA) # no ability to get error SS, and so no P-values

flies.aovB <- aov(rsp ~ strain + density, data=flies) # assume additive
anova(flies.aovB) 

# diagnostic plot
plot(flies.aovB)

######################################################################
# Fourth example: Beetles -- block design
######################################################################
# The data
beetles <- data.frame(rsp = c(0.958,0.986,0.925,
                              0.971,1.051,0.952,
                              0.927,0.891,0.829,
                              0.971,1.010,0.955),
                      genotype = factor(rep(c("++","+b","bb"),4)),
                      block = factor(rep(1:4,rep(3,4))))

# The ANOVA: assume blocks and genotypes are additive
beetles.aov <- aov(rsp ~ genotype+block, data=beetles)
anova(beetles.aov)

# diagnostic plot
plot(beetles.aov)

# interaction plot
interaction.plot(beetles$genotype, beetles$block, beetles$rsp,
                 lwd=2,col=c("black","blue","red","green"),lty=1)



##################
# End of comp31.R
##################
