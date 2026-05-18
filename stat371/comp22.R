######################################################################
# Computer notes, Lecture 22
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
url.show("https://kbroman.org/teaching_old/stat371/comp22.R")
######################################################################

######################################################################
# Example 1 
######################################################################
# the data
ob1 <- c(5, 20, 75)

# Function to perform chi-square test
chisq.ex1 <-
function(ob)
{
  # no. data points
  n <- sum(ob)

  # estimate of f
  fhat <- (ob[1]+ob[2]/2)/n

  # expected counts
  ex <- n * c(fhat^2, 2*fhat*(1-fhat), (1-fhat)^2)

  # chi-square statistic
  sum( (ob-ex)^2/ex )
}

# calculate statistic
print(stat1 <- chisq.ex1(ob1))
# value = 4.65

# p-value from the chi-square approximation
1-pchisq(stat1, 1)
# value = 3.1%

##############################
# simulations
##############################
fhat <- (ob1[1] + ob1[2]/2)/sum(ob1)
p <- c(fhat^2, 2*fhat*(1-fhat), (1-fhat)^2)
sims <- rmultinom(10000, sum(ob1), p)

simstat1 <- apply(sims, 2, chisq.ex1)

# p-value
mean(simstat1 >= stat1)
# value = 0.265

# histogram of results
hist(simstat1, breaks=200, prob=TRUE)
x <- seq(0, 20, len=201)
lines(x, dchisq(x, 1), lwd=2, col="blue")


######################################################################
# Example 2 
######################################################################
# the data
ob2 <- c(104, 91, 36, 19) # [O, A, B, AB]

# Function to get MLEs of pA, pB, pO
# [I won't try to explain...]
mle.ex2 <-
function(ob, maxit=1000, tol=1e-6)
{
  nalle <- sum(ob)*2
  f <- ob/sum(ob)
  pOstart <- sqrt(f[1])
  pAstart <- -pOstart + sqrt(2*f[1] + 4*f[2])/2
  pBstart <- 1 - pOstart - pAstart
  pnew <- pcur <- c(pOstart, pAstart, pBstart)
#  cat("0: ", llik.ex2(ob, pcur), "\n")
  converged <- FALSE
  for(s in 1:maxit) {
    ahom <- ob[2] * pcur[2]^2 / (pcur[2]^2 + 2*pcur[2]*pcur[1])
    bhom <- ob[3] * pcur[3]^2 / (pcur[3]^2 + 2*pcur[3]*pcur[1])
    pnew[1] <- (2*ob[1] + ob[2]-ahom + ob[3]-bhom)/nalle
    pnew[2] <- (ob[4] + 2*ahom + ob[2]-ahom)/nalle
    pnew[3] <- 1-pnew[1]-pnew[2]
    
#    cat(s, ": ", llik.ex2(ob, pnew), "\n")
    if(all(abs(pcur-pnew)<tol)) {
      converged <- TRUE
      break
    }
    pcur <- pnew
  }
  if(!converged) warning("Didn't converge.")
  pnew
}
      
llik.ex2 <-
function(ob, p)
{
  f <- c(p[1]^2, p[2]^2+2*p[1]*p[2], p[3]^2+2*p[1]*p[3], 2*p[2]*p[3])
  sum(ob * log(f))
}

# calculate mle
print(mle <- mle.ex2(ob))
# results = (0.634, 0.250, 0.116)

# Function to perform chi-square test
chisq.ex2 <-
function(ob)
{
  # no. data points
  n <- sum(ob)

  # mle
  themle <- mle.ex2(ob)

  # expected counts
  ex <- n * c(themle[1]^2, themle[2]^2 + 2*themle[1]*themle[2],
              themle[3]^2 + 2*themle[1]*themle[3], 2*themle[2]*themle[3])

  # chi-square statistic
  sum( (ob-ex)^2/ex )
}

# calculate statistic
print(stat2 <- chisq.ex2(ob2))
# value = 2.10

# p-value from the chi-square approximation
1-pchisq(stat2, 1)
# value = 15%

##############################
# simulations
##############################
p <- c(mle[1]^2, mle[2]^2 + 2*mle[1]*mle[2],
       mle[3]^2 + 2*mle[1]*mle[3], 2*mle[2]*mle[3])
sims2 <- rmultinom(10000, sum(ob2), p)

simstat2 <- apply(sims2, 2, chisq.ex2)

# p-value
mean(simstat2 >= stat2)
# value = 0.265

# histogram of results
hist(simstat2, breaks=200, prob=TRUE)
x <- seq(0, 20, len=201)
lines(x, dchisq(x, 1), lwd=2, col="blue")

######################################################################
# Example 3 (I'll skip the simulations part...if you are interested, just ask)
######################################################################
ob3 <- c(6,10,15,7,8,1,3,0,0,0,0)

# estimate of p
phat <- sum(ob3*(0:10)) / (sum(ob3) * 10)

# expected counts
ex3 <- dbinom(0:10, 10, phat) * sum(ob3)

# chi-square statistic
print(stat3 <- sum( (ob3-ex3)^2/ex3 ) )
# value = 15.4

# p-value from chi-square approximation
1-pchisq(stat3, length(ob3)-2)
# value = 0.082

##############################
# binned version
##############################
ob3bin <- c(ob3[1:5], sum(ob3[-(1:5)]))
ex3bin <- c(ex3[1:5], sum(ex3[-(1:5)]))

# chi-square statistic
print(stat3bin <- sum( (ob3bin-ex3bin)^2/ex3bin ) )
# value = 4.55

# p-value from chi-square approximation
1 - pchisq(stat3bin, length(ob3bin)-2)
# value = 0.34

##################
# End of comp22.R
##################
