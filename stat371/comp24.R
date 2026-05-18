######################################################################
# Computer notes, Lecture 24
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
url.show("https://kbroman.org/teaching/stat371/comp24.R")
######################################################################

######################################################################
# example 1: blood groups by state
######################################################################
# the data
x <- rbind(c(122, 117, 19, 244),
           c(1781, 1351, 288,3301),
           c(353, 269, 60, 713))

# chi-square test for independence
chi <- chisq.test(x)

# expected counts
ex <- chi$expected

# a direct method to get the expected counts
rs <- apply(x,1,sum) # row sums
cs <- apply(x,2,sum) # col sums
n <- sum(rs) # total sample size
ex <- outer(rs,cs,"*")/n # expected counts under independence

# lrt statistic
lrt <- 2*sum(x * log(x/ex)) # result = 5.55

# degrees of freedom
df <- prod(dim(x)-1)  # value = 6

# another way to get deg of freedom
df <- (ncol(x) - 1) * (nrow(x) - 1)

# P-value
1-pchisq(lrt, df)# value = 0.48

######################################################################
# approximate fisher's exact test
######################################################################
fisher <-
function(tab, n.sim=1000, return.all=FALSE, prnt=FALSE)
{
  bot0 <- sum(lgamma(tab+1))

  bot <- 1:n.sim
  a <- list(rep(row(tab),tab), rep(col(tab),tab))
  for(i in 1:n.sim) {
    a[[1]] <- sample(a[[1]])
    bot[i] <- sum(lgamma(table(a)+1))
    if(prnt) { if(i == round(i/10)*10) cat(i,"\n") }
  }
  if(return.all) return(list(bot0,bot))
  mean(bot0 <= bot)
}

# apply Fisher's exact test, using 1000 simulated tables
fisher(x) # p-value = 0.499 when I tried it

######################################################################
# Example 2: Survival rates in five strains
######################################################################
# the data
y <- rbind(c(15,5),c(17,3),c(10,10),c(17,3),c(16,4))

# chi-square test
chi <- chisq.test(y) # P-value = 0.059

# likelihood ratio test
ex <- chi$expected # expected counts
lrt <- 2 * sum(y * log(y/ex)) # value = 8.41
df <- chi$parameter # degrees of freedom
1 - pchisq(lrt, df) # p-value = 0.078

# Fisher's exact test
fisher.test(y) # p-value = 0.087

######################################################################
# Example 3: Paired data
######################################################################
# The data
z <- rbind(c(9,9),c(20,62))

# McNemar's test
chi <- (z[1,2]-z[2,1])^2 / (z[1,2]+z[2,1]) # value = 4.17
1 - pchisq(chi, 1) # P = 4.1%

# An exact test
binom.test(z[1,2], z[1,2]+z[2,1]) # P = 6.1%

##################
# End of comp24.R
##################
