######################################################################
# Computer notes             Statistics for Laboratory Scientists (2)
# Lecture 10                                  Johns Hopkins University 
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
# Functions to get permutation-based
#     P-values for the ANOVA and
#     Kruskal-Wallis tests
##############################
perm.aov <-
function(x,g,n.perm=1000)
{
  obs <- anova(aov(x~g))[1,4]
  perm <- 1:n.perm
  for(i in 1:n.perm) {
    perm[i] <- anova(aov(x~sample(g)))[1,4]
    if(i == round(i,-2)) cat(i,"\n") # print every 100th iteration
  }

  # return just the estimated P-value
  mean(perm >= obs)
}

perm.kw <-
function(x,g,n.perm=1000)
{
  obs <- kruskal.test(x~g)$stat
  perm <- 1:n.perm
  for(i in 1:n.perm) {
    perm[i] <- kruskal.test(x~sample(g))$stat
    if(i == round(i,-2)) cat(i,"\n") # print every 100th iteration
  }

  # return just the estimated P-value
  mean(perm >= obs)
}

##############################
# IL10 analysis 
# data at http://www.biostat.jhsph.edu/~kbroman/teaching/labstat/fourth/il10.csv
##############################
# read in the data
il10 <- read.csv("il10.csv")
# add a column with the log10 of the IL10 responses
il10 <- cbind(il10,logIL10=log10(il10$IL10))

# anova with original scale
il10.aov <- aov(IL10 ~ Strain, data=il10)
anova(il10.aov)

# anova with log10 scale
logil10.aov <- aov(logIL10 ~ Strain, data=il10)
anova(logil10.aov)

# kruskal-wallis test
kruskal.test(IL10 ~ Strain, data=il10)

# permutation test with ANOVA, original scale
il10.perm.aov <- perm.aov(il10$IL10,il10$Strain)
il10.perm.aov[[1]] # the p-value

# permutation test with ANOVA, log10 scale
logil10.perm.aov <- perm.aov(il10$logIL10,il10$Strain)

# permutation test with K-W statistic
il10.perm.kw <- perm.kw(il10$IL10,il10$Strain)

##############################
# blood coagulation times
##############################
# create dataset
rsp <- c(62,60,63,59,63,67,71,64,65,66,68,66,71,67,68,68,56,62,60,61,63,64,63,59)
treat <- factor(rep(LETTERS[1:4],c(4,6,6,8)))
dat <- data.frame(rsp=rsp, ttt=treat)

# Do Kruskal-Wallis test
kruskal.test(rsp~ttt, data=dat)

# Do K-W by brute-force
# rank the observations (rank() assigns average ranks to ties)
ranks <- rank(dat$rsp)
# average rank with each treatment group
rbar <- tapply(ranks, dat$ttt, mean)
# sample sizes within each group
nt <- tapply(ranks, dat$ttt, length)
# overall sample size
N <- sum(nt)
# expected average rank
e <- (N+1)/2
# the statistic
H <- 12/(N*(N+1)) * sum(nt * (rbar - e)^2)
# the correction factor
ties <- table(dat$rsp)
D <- 1 - sum(ties^3-ties)/(N^3-N)

# the final statistic
kw <- H/D
# the p-value
1 - pchisq(kw, length(nt)-1)

############################### 
# fake data
##############################
datA <- data.frame(x=c(33,34,35,36,39,  30,31,32,32,33,  31,33,33,34,36),
                   g=factor(rep(c("A","B","C"), rep(5,3))))
datB <- datA
datB$x[5] <- 50

# anova with dataset A
anova(aov(x ~ g, data=datA))

# anova with dataset B
anova(aov(x ~ g, data=datB))

# Kruskal-Wallis
kruskal.test(x ~ g, data=datA)

# permutation test: ANOVA, dataset A
perm.aov(datA$x, datA$g)

# permutation test: ANOVA, dataset B
perm.aov(datB$x, datB$g)

# permutation test: Kruskal-Wallis
perm.kw(datA$x, datA$g)

##################
# End of comp10.R
##################
