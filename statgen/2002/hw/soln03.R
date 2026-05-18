######################################################################
# Biostat 140.668
#
# Solution to Problem Set 3
#
# Karl W Broman, 4 Nov 2002
#
######################################################################

######################################################################
# simf2: simulate complete intercross data for two genetic markers
#
#   INPUT:  n = no. individuals
#           r = recombination fraction
#
#   OUTPUT: 3x3 table indicating number of individuals 
#           with each possible two-locus genotype
######################################################################

simf2 <-
function(n=1000, r=0.2)
{
  # simulate genotype data for first marker
  m1 <- sample(c("A","H","B"), n, repl=TRUE, prob=c(1,2,1))

  m2 <- rep(NA,n)
  if(any(m1=="A"))
    m2[m1=="A"] <- sample(c("A","H","B"), sum(m1=="A"), repl=TRUE,
                          prob=c((1-r)^2,2*r*(1-r),r^2))
  if(any(m1=="B"))
    m2[m1=="B"] <- sample(c("A","H","B"), sum(m1=="B"), repl=TRUE,
                          prob=c(r^2,2*r*(1-r),(1-r)^2))
  if(any(m1=="H")) 
    m2[m1=="H"] <- sample(c("A","H","B"), sum(m1=="H"), repl=TRUE,
                          prob=c(r*(1-r),r^2+(1-r)^2,r*(1-r)))

  table(factor(m1,levels=c("A","H","B")),
        factor(m2,levels=c("A","H","B")))
}

######################################################################
# estRF: estimate the recombination fraction between two markers in
#        an intercross
#
# starting value: assume all HH individuals are non-recombinant
#
#   INPUT:  dat = 3x3 table like the output from simf2()
#           tol = TOLERANCE for determining convergence
#           maxit = maximum no. iterations
#
#   OUTPUT: the estimated recombination fraction
######################################################################

estRF <-
function(dat=simf2(), tol=1e-6, maxit=1000)
{
  n <- sum(dat) # total number of individuals
  n1 <- dat[1,2]+dat[2,1]+dat[2,3]+dat[3,2] # single recombinants
  n2 <- dat[1,3]+dat[3,1] # double recombinants
  nHH <- dat[2,2] # HH individuals

  oldr <- (n1+2*n2)/(2*n)

  flag <- FALSE
  for(i in 1:maxit) {
    e <- nHH * oldr^2 / (oldr^2 + (1-oldr)^2) # expected double-recomb'ts

    r <- (n1+2*n2+2*e)/(2*n) # new estimate of rec fract

    if(abs(r-oldr) < tol) {
      flag <- TRUE
      break
    }
    oldr <- r
  }
  if(!flag) warning("Failed to converge.")
  
  r
}
