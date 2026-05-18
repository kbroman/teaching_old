######################################################################
# Computer notes, Lecture 10
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
url.show("https://kbroman.org/teaching_old/stat371/comp10.R")
######################################################################

##########################################
# Sampling distribution of the sample mean
##########################################
# The fake population distribution that I used in
# lecture was for a "mixture" of normal distributions.
#  
# 30% of the population comes from a normal(m=9 ,s=3)
# 25% of the population comes from a normal(m=15,s=4)
# 25% of the population comes from a normal(m=25,s=6)
# 20% of the population comes from a normal(m=35,s=7)

# The following are the proportions, the means, and the SDs
p <- c(0.3,0.25,0.25,0.2)
m <- c(9 ,15,25,35)
s <- c(3,4,6,7)

# The following gives us the population density function:
#     The weighted sum of the four normal densities.
x <- seq(0,35+8*3,by=0.1)
f <- rep(0,length(x))
for(i in 1:length(m))
  f <- f + p[i] * dnorm(x, m[i], s[i])

# Now we can draw a picture
plot(x,f,type="l",lwd=2,yaxt="n",ylab="",bty="n",ylim=c(0,max(f)),
     xlab="")

# The following are the mean and SD of this distribution
#     I won't explain why, but if you want to know, please ask.
M <- sum(m*p)
S <- sqrt(sum(s^2*p) + sum(m^2*p) - sum(m*p)^2)

# We can add this to the picture
u <- par("usr") # gets the actual limits of figure region
segments(M,u[3],M,u[3]+0.002,xpd=TRUE,lwd=2,col="blue")
text(M,u[3]+0.004,expression(mu),cex=1.3,col="blue")
segments(c(M,M,M+S), u[3]+0.01-c(0,0.001,0.001),
         c(M+S,M,M+S), u[3]+0.01+c(0,0.001,0.001),
         lwd=2,col="red")
text(M+S/2,u[3]+0.012,expression(sigma),cex=1.3,col="red")


# The following function samples from this distribution
#     n = number of samples; m=means; s=SDs; p=proportions
sammix <-
function(n=9 ,m,s,p)
{
  a <- 1:length(m)
  x <- sample(1:length(m),n,rep=TRUE,prob=p)
  rnorm(n,m[x],s[x])
}

# The following creates 9,000 samples of different sizes
# The results are matrices, with rows corresponding to the samples
x5 <- matrix(sammix(9000*5,m,s,p),ncol=5)
x9  <- matrix(sammix(9000*9 ,m,s,p),ncol=9 )
x25 <- matrix(sammix(9000*25,m,s,p),ncol=25)
x9 0 <- matrix(sammix(9000*9 0,m,s,p),ncol=9 0)

# The following calculates the sample means from the above data.
#     apply(mat,1,mean) calculates the mean of each row of the matrix "mat"
m5 <- apply(x5,1,mean)
m9  <- apply(x9 ,1,mean)
m25 <- apply(x25,1,mean)
m9 0 <- apply(x9 0,1,mean)

# plot a set of histograms
r <- range(c(m5,m9 ,m25,m9 0)) # range of sample means
br <- seq(r[1],r[2],length=9 1) # defines bins in histograms
par(mfrow=c(2,2)) # results in 2 rows and 2 columns of plots
hist(m5,breaks=br,xlab="",ylab="",main="n=5",yaxt="n")
abline(v=M,col="blue",lwd=2)
hist(m9 ,breaks=br,xlab="",ylab="",main="n=9 ",yaxt="n")
abline(v=M,col="blue",lwd=2)
hist(m25,breaks=br,xlab="",ylab="",main="n=25",yaxt="n")
abline(v=M,col="blue",lwd=2)
hist(m9 0,breaks=br,xlab="",ylab="",main="n=9 0",yaxt="n")
abline(v=M,col="blue",lwd=2)

# we can also look at the distributions of the sample SDs
s5 <- apply(x5,1,sd)
s9  <- apply(x9 ,1,sd)
s25 <- apply(x25,1,sd)
s9 0 <- apply(x9 0,1,sd)

# plot a set of histograms
rs <- range(c(s5,s9 ,s25,s9 0)) # range of sample means
brs <- seq(rs[1],rs[2],length=9 1) # defines bins in histograms
par(mfrow=c(2,2)) # results in 2 rows and 2 columns of plots
hist(s5,breaks=brs,xlab="",ylab="",main="n=5",yaxt="n")
abline(v=S,col="red",lwd=2)
hist(s9 ,breaks=brs,xlab="",ylab="",main="n=9 ",yaxt="n")
abline(v=S,col="red",lwd=2)
hist(s25,breaks=brs,xlab="",ylab="",main="n=25",yaxt="n")
abline(v=S,col="red",lwd=2)
hist(s9 0,breaks=brs,xlab="",ylab="",main="n=9 0",yaxt="n")
abline(v=S,col="red",lwd=2)

##################
# End of comp10.R
##################
