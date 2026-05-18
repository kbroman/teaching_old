######################################################################
# Computer notes             Statistics for Laboratory Scientists (2)
# Lecture 4                                  Johns Hopkins University 
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
# Functions for the two-point linkage in an intercross
######################################################################
# Simulating data
sim.2pt <-
function(n=100, theta=0.25)
{
  m1 <- sample(c("AA","Aa","aa"), n, repl=TRUE, prob=c(1,2,1))
  m2 <- m1
  if(any(m1=="AA")) 
    m2[m1=="AA"] <- sample(c("BB","Bb","bb"), sum(m1=="AA"), repl=TRUE,
                        prob=c((1-theta)^2, 2*theta*(1-theta), theta^2))
  if(any(m1=="Aa")) 
    m2[m1=="Aa"] <- sample(c("BB","Bb","bb"), sum(m1=="Aa"), repl=TRUE,
                        prob=c(theta*(1-theta), theta^2+(1-theta)^2,
                               theta*(1-theta)))
  if(any(m1=="aa")) 
    m2[m1=="aa"] <- sample(c("BB","Bb","bb"), sum(m1=="aa"), repl=TRUE,
                        prob=c(theta^2, 2*theta*(1-theta), (1-theta)^2))

  table(m1,m2)
}
         
# Calculating the MLE for the recombination fraction
mle.2pt <-
function(dat, tol=1e-12, maxit=1000,prnt=FALSE)
{
  twon <- 2*sum(dat)
  simplepart <- (dat[1,2]+2*dat[1,3] + dat[2,1] + dat[2,3] + 2*dat[3,1]+dat[3,2])
  theta <- simplepart/twon 

  flag <- 0
  if(prnt) print(c(0,theta,llik.2pt(dat,theta)))
  for(i in 1:maxit) {
    hardpart <- 2*dat[2,2]*(theta^2 / (theta^2 + (1-theta)^2))
    newtheta <- (simplepart + hardpart)/twon
    if(prnt) print(c(i,theta,llik.2pt(dat,newtheta)))
    if(abs(theta-newtheta) < tol) {
      flag <- 1
      break
    }
    theta <- newtheta
  }
  if(flag==0) warning("Didn't converge.")

  newtheta
}

# The log likelihood function
llik.2pt <-
function(dat, theta)
{
  prob <- rbind(c(0.25*(1-theta)^2, 0.5*theta*(1-theta), 0.25*theta^2),
                c(0.5*theta*(1-theta), 0.5*(theta^2+(1-theta)^2), 0.5*theta*(1-theta)),
                c(0.25*theta^2, 0.5*theta*(1-theta), 0.25*(1-theta)^2))

  sum( dat * log(prob))
}

######################################################################
# Example 1: two-point linkage
######################################################################
# The data
x <- rbind(c(6,15,3),c(9,29,6),c(3,16,13))

# chi-square test for independence
chi <- chisq.test(x) # p-value = 0.035

# general likelihood ratio test for independence
ex <- chi$expected # expected counts
lrt <- 2*sum(x * log(x/ex)) # value = 9.98
df <- chi$parameter # degrees of freedom = 4
1 - pchisq(lrt, df) # p-value = 0.041

# fisher's exact test
fisher.test(x) # p-value = 0.046

# the specific likelihood ratio test
mle <- mle.2pt(x) # estimate of recombination fraction = 0.359
lrt <- 2 * (llik.2pt(x,mle) - llik.2pt(x,0.5)) # statistic = 7.74
1 - pchisq(lrt, 1) # p-value = 0.0054

######################################################################
# Functions for estimating power
######################################################################
# simulate 2x2 table using conditional probs
sim.kby2 <-
function(n=rep(20,5), p=c(rep(0.3,4),0.1))
{
  x <- rbinom(length(n),n,p)
  cbind(x,n-x)
}

# Get P-values for all three tests
all.tests <-
function(dat)
{
  rs <- apply(dat,1,sum)
  cs <- apply(dat,2,sum)

  ex <- outer(rs,cs,"*")/sum(rs)

  df <- prod(dim(dat)-1)
  c(chi=1-pchisq(sum((dat-ex)^2/ex),df),
    lrt=1-pchisq(2*sum(dat[dat>0]*log(dat[dat>0]/ex[dat>0])),df),
    fisher=fisher.test(dat)$p.value)
}

######################################################################
# Example 1: 2 groups, n=20 in each group; p=30% in first group;
#            p varies in second group
######################################################################

n.sim <- 250 # number of simulations to perform
p <- seq(0,1,by=0.05) # conditional probability for second group

# empty 3-dimensional array for the results
results <- array(dim=c(n.sim, length(p), 3)) # 3 = no. tests
dimnames(results) <- list(NULL, as.character(p), c("chisq","lrt","fisher"))

# do the simulations
for(i in 1:length(p)) {
  for(j in 1:n.sim) {
    dat <- sim.kby2( c(20, 20), c(0.3, p[i])) # simulate data
    results[j,i,] <- all.tests(dat) # apply all tests
  }
}

# calculate the power
power <- apply(results, c(2,3), function(a) mean(a < 0.05))

# plot the results
plot(p*100, power[,1]*100, ylim=c(0,100),
     xlab="p[B]", ylab="Power", lwd=2) # chi-square; note lwd=2 makes points thicker
points(p*100, power[,2]*100, col="blue", lwd=2, cex=1.3) # LRT
              # [note cex=1.3 makes points bigger]
points(p*100, power[,3]*100, col="red", lwd=2) # Fisher

##################
# End of comp04.R
##################
