######################################################################
# Computer notes, Lecture 18
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
url.show("http://www.biostat.wisc.edu/~kbroman/teaching/stat371/comp18.R")
######################################################################

########################################
# QQ plots
########################################
# simulate some normally distributed data
# (sample size 10; mean 50 and SD 10)
x <- rnorm(10, 50, 10)
qqnorm(x)
qqline(x)


#######################################
# Functions for doing permutation tests
#######################################
# Utility function
#     returns binary representation of 1:(2^n)
binary.v <-
function(n)
{
  x <- 1:(2^n)
  mx <- max(x)
  digits <- floor(log2(mx))
  ans <- 0:(digits-1); lx <- length(x)
  x <- matrix(rep(x,rep(digits, lx)),ncol=lx)
  x <- (x %/% 2^ans) %% 2
}

# Function to perform a paired permutation test
#     Input: differences, d
#            no. permutations
# Use paired.perm.test(d, n.perm=NULL) to 
#     do the exact test
paired.perm.test <-
function(d, n.perm=1000, pval=FALSE)
{
  n <- length(d)
  obs <- t.test(d)$statistic
  if(is.null(n.perm)) { # do exact test
    ind <- binary.v(n)
    allt <- apply(ind,2,function(x,y)
                  t.test((2*x-1)*y)$statistic,d)
  }
  else { # do n.perm samples
    allt <- 1:n.perm
    for(i in 1:n.perm) 
      allt[i] <- t.test(d*sample(c(-1,1),n,repl=TRUE))$statistic
  }

  if(pval) return(mean(abs(allt) >= abs(obs)))
  allt
}


# Function to perform permutation test
#     x, y = the two samples
#     n.perm = number of permutations
#     var.equal = passed to the function t.test
# Use perm.test(x, y, n.perm=NULL)
#     to get exact P-value
perm.test <-
function(x, y, n.perm=1000, var.equal=TRUE, pval=FALSE)
{
  # number of data points
  kx <- length(x)
  ky <- length(y)
  n <- kx + ky

  # observed statistic
  obs <- t.test(x,y, var.equal=var.equal)

  # Data re-compiled
  X <- c(x,y)
  z <- rep(1:0,c(kx,ky))

  if(is.null(n.perm)) { # do exact permutation test
    o <- binary.v(n)  # indicator of all possible samples
    o <- o[,apply(o,2,sum)==kx]  
    nc <- choose(n,kx)
    allt <- 1:nc
    for(i in 1:nc) {
      xn <- X[o[,i]==1]
      yn <- X[o[,i]==0]
      allt[i] <- t.test(xn,yn,var.equal=var.equal)$statistic
    }
  }
  else { # do 1000 permutations of the data
    allt <- 1:n.perm
    for(i in 1:n.perm) {
      z <- sample(z)
      xn <- X[z==1]
      yn <- X[z==0]
      allt[i] <- t.test(xn,yn,var.equal=var.equal)$statistic
    }
  }

  if(pval) return(mean(abs(allt) >= abs(obs)))
  allt
}

##############################
# paired example
##############################
# the data 
d <- c(28.6, -5.3, 13.5, -12.9, 37.3, 25.0,
       5.1, 34.6, -12.1, 9.0, 39.4)
x <- c(117.3, 100.1, 94.5, 135.5, 92.9, 118.9,
       144.8, 103.9, 103.8, 153.6, 163.1)
y <- x+d

# T-test
tobs <- t.test(d)$statistic

# Sign test
2*pbinom(sum(d < 0), length(d), 0.5)

# Signed rank test
wilcox.test(d)

# Permutation test: all possible permutations
paired.perm.test(d, n.perm=NULL, pval=TRUE)

# Permutation test: 1000 permutations
paired.perm.test(d, n.perm=1000, pval=TRUE)


##############################
# Two-sample example
##############################
x2 <- c(43.3, 57.1, 35.0, 50.0, 38.2, 61.2)
y2 <- c(51.9, 95.1, 90.0, 49.7, 101.5, 74.1, 
        84.5, 46.8, 75.1)

# t-test
tobs2 <- t.test(x2,y2, var.equal=TRUE)$statistic

# Permutation test: all possible permutations
perm.test(x2, y2, n.perm=NULL, pval=TRUE)

# Permutation test: 1000 permutations
perm.test(x2, y2, n.perm=1000, pval=TRUE)

# Wilcoxon rank-sum test
wilcox.test(x2,y2)

##################
# End of comp18.R
##################
