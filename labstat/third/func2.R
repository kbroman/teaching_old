######################################################################
# R functions                    Statistics for Laboratory Scientists 
# Lab 2                                      Johns Hopkins University 
######################################################################
# This file contains the major R functions to be used in this lab.
#
# Lines beginning with the symbol '#' are comments in R.  All other
# lines contain code.
######################################################################

#################################
# The population genetics example
#################################

# Function to simulate initial generation
#     m=number of couples
#     p=allele frequency in generating the couples
init <-
function(m=25, p=0.5)
{
  # Hardy-Weinberg genotype probabilities
  genop <- c( (1-p)^2, 2*p*(1-p), p^2)

  # simulate genotypes of males and females
  males   <- sample(0:2, m, repl=TRUE, prob=genop)
  females <- sample(0:2, m, repl=TRUE, prob=genop)

  # return data as a matrix
  cbind(males, females)
}

# simulate two children, given parents' genotypes
#     parents=vector of length 2=(dad, mom)
#             [containing numbers that are 0, 1, or 2]
simkids <-
function(parents)
{
  dad <- parents[1]
  mom <- parents[2]

  # alleles from dad
  if(dad==2) fromdad <- c(1,1)
  else if(dad==1) fromdad <- sample(0:1, 2, repl=TRUE)
  else fromdad <- c(0,0)

  # alleles from mom
  if(mom==2) frommom <- c(1,1)
  else if(mom==1) frommom <- sample(0:1, 2, repl=TRUE)
  else frommom <- c(0,0)

  # return kids' genotypes
  fromdad + frommom
}

# simulate the next generation
#     oldgen = matrix with two columns
#              column 1 = genotypes of males
#              column 2 = genotypes of females
simnewgen <-
function(oldgen)
{
  # shuffle the males 
  oldgen[,1] <- sample(oldgen[,1])

  # get new kids; t is used to "transpose" the matrix output from apply
  t(apply(oldgen, 1, simkids))
}

# Simulate many generations
#     n=number of generations
#     m=number of couples
#     p=allele frequency for generating initial generation
simfreq <-
function(n=100, m=25, p=0.5)
{
  # create a vector to store the allele frequncies
  freq <- 1:n

  # initial generation
  data <- init(m,p)
  freq[1] <- mean(data)/2

  # subsequent generations
  for(i in 2:n) {
    data <- simnewgen(data)
    freq[i] <- mean(data)/2
  }

  # return frequencies
  freq
}

# Simulate a population until the allele is fixed,
# and return the 0/1 (according to whether A/a is fixed)
# and generation at which fixation occurred
#     m=number of couples
#     p=allele frequency to generate initial generation
#     n=maximum number of generations to simulate
fixtime <- 
function(m=25, p=0.5, n=2000)
{
  dat <- init(m, p)
  freq <- mean(dat)/2

  # fixed at first generation?
  if(freq==0 || freq==1) return(c(freq, 1))

  for(i in 2:n) {
    dat <- simnewgen(dat)
    freq <- mean(dat)/2

    # fixed?
    if(freq==0 || freq==1)
      return(c(allele=freq, generation=i))
  }

  # allele frequency wasn't fixed
  return(c(allele=NA,generation=NA))
}
    
################
# End of func2.R
################
