######################################################################
# Biostat 140.668
#
# Solution to Problem Set 4
# Note: This is not very well written code.  See R/qtl
#       (http://www.biostat.jhsph.edu/~kbroman/qtl) for better stuff.
#
# Karl W Broman, 6 Nov 2002
#
######################################################################

######################################################################
# simbc: simulate genotype and phenotype data for two markers
#        @ recombination fraction r, with a QTL 
#
#   INPUT:  n = number of backcross individuals
#           r = recombination fraction between markers
#           r.qtl = recombination fraction between left marker and QTL
#                   (must have 0 <= r.qtl <= 0)
#           mu  = vector of length 2 (the phenotype means for QTL
#                 genotype = A, H)
#           sig = residual SD
#
#   OUTPUT: data.frame with three columns: the genotypes at markers 1 and 2
#           followed by the phenotypes [genotypes coded as A, H]
#           
######################################################################

simbc <-
function(n=250, r=0.2, r.qtl=0.1, mu=c(20,30), sig=5)
{
  # error checking
  if(r < 0 || r > 0.5)
    stop("r must be between 0 and 1/2")
  if(r.qtl > r || r.qtl < 0)
    stop("r.qtl must be between 0 and r")
  if(length(mu) != 2)
    stop("mu must be length 2")

  # simulate genotype data for left marker
  m1 <- sample(c("A","H"), n, repl=TRUE)

  # simulate QTL genotypes
  qtl <- rep(NA,n)
  if(any(m1=="A"))
    qtl[m1=="A"] <- sample(c("A","H"), sum(m1=="A"), repl=TRUE,
                          prob=c((1-r.qtl),r.qtl))
  if(any(m1=="H")) 
    qtl[m1=="H"] <- sample(c("A","H"), sum(m1=="H"), repl=TRUE,
                          prob=c(r.qtl,(1-r.qtl)))

  R <- (r-r.qtl)/(1-2*r.qtl) # recombination fraction between QTL and right marker

  # simulate right marker genotypes
  m2 <- rep(NA,n)
  if(any(qtl=="A"))
    m2[qtl=="A"] <- sample(c("A","H"), sum(qtl=="A"), repl=TRUE,
                          prob=c((1-R),R))
  if(any(qtl=="H")) 
    m2[qtl=="H"] <- sample(c("A","H"), sum(qtl=="H"), repl=TRUE,
                          prob=c(R,(1-R)))

  # simulate phenotype
  phe <- rnorm(n,mu[match(qtl,c("A","H"))],sig)

  data.frame(m1,m2,phe)
}

######################################################################
# im: interval mapping in the case of a backcross with two markers
#     displaying complete genotype data
#
#   INPUT:  dat = data.frame with 3 columns, like the output from
#                 simbc (above)
#           r   = recombination fraction between the markers
#           steps = number of steps between the markers at which
#                   to calculate a LOD score
#                   (I'll use haldane's map function to convert to genetic
#                    distance, and then do equally-spaced locations on that
#                    scale)
#           tol = TOLERANCE for determining convergence
#           maxit = maximum no. iterations
#
#   OUTPUT: a 6-column matrix containing the following columns
#              r   = rec frac from left marker
#              d   = distance (in cM) from the left marker
#              LOD = lod score vs null model of no QTL
#              muAA, muAB, sigma
#           Note: the matrix will have steps+2 rows (for the two markers
#                 plus "steps" positions between them
#             
######################################################################

im <-
function(dat=simf2(r=0.2), r=0.2, steps=18, tol=1e-6, maxit=1000)
{
  n <- nrow(dat) # number of individuals
  
  L <- -0.5*log(1-2*r) # length of interval in Morgans
  d <- seq(0, L, length=steps+2) # distance from left marker in Morgans
  rL <- 0.5*(1-exp(-2*d)) # recombination fraction with left marker
  rR <- 0.5*(1-exp(-2*(L-d))) # recombination fraction with right marker

  # to simplify later code...
  m1 <- dat[,1]
  m2 <- dat[,2]
  phe <- dat[,3]
  


  # first calculate an n x steps+2 matrix of Pr(geno = A | marker data)
  probs <- matrix(nrow=n,ncol=steps+2)
  # let's fill in the probabilities for the two markers first
  probs[,1] <- 0
  probs[m1=="A"] <- 1
  probs[,steps+2] <- 0
  probs[m2=="A",steps+2] <- 1
  
  # For simplicity, I'll use a for loop for the other markers
  for(i in (1:steps)+1) {
    probs[m1=="A" & m2=="A",i] <- (1-rL[i])*(1-rR[i])/(1-r)
    probs[m1=="H" & m2=="H",i] <- rL[i]*rR[i]/(1-r)
    probs[m1=="A" & m2=="H",i] <- (1-rL[i])*rR[i]/r
    probs[m1=="H" & m2=="A",i] <- rL[i]*(1-rR[i])/r
  }

  # first fit NULL model
  mu <- mean(phe)
  sig <- sd(phe)*sqrt((n-1)/n) # the MLE rather than the usual
  log10lik0 <- sum(dnorm(phe,mu,sig,log=TRUE))/log(10)
  
  results <- matrix(ncol=6,nrow=steps+2)
  colnames(results) <- c("r","d","LOD","muAA","muAB","sigma")
  results[,1] <- rL
  results[,2] <- d*100 # in cM

  # now do EM
  for(i in 1:(steps+2)) { # loop over positions
    w <- probs[,i] # start EM by taking weights = initial probs

    oldmu <- c(sum(phe*w)/sum(w),sum(phe*(1-w))/sum(1-w))
    oldsig <- sqrt(sum((phe-oldmu[1])^2*w + (phe-oldmu[2])^2*(1-w))/n)

    flag <- FALSE
    for(j in 1:maxit) {
      # The E-step
      w <- probs[,i]*dnorm(phe,oldmu[1],oldsig)
      w <- w/(w+(1-probs[,i])*dnorm(phe,oldmu[2],oldsig))

      # The M-step
      mu <- c(sum(phe*w)/sum(w),sum(phe*(1-w))/sum(1-w))
      sig <- sqrt(sum((phe-mu[1])^2*w + (phe-mu[2])^2*(1-w))/n)

      # Check for convergence
      if(max(abs(c(mu,sig)-c(oldmu,oldsig))) < tol) {
        flag <- TRUE
        break
      }
      oldmu <- mu
      oldsig <- sig
    }
    if(!flag) warning("Didn't converge at position", i)

    # calculate LOD score
    results[i,3] <- sum(log10(dnorm(phe,mu[1],sig)*probs[,i] +
                              dnorm(phe,mu[2],sig)*(1-probs[,i]))) - log10lik0

    results[i,4:6] <- c(mu,sig)
  }
  results
}

