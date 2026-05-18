
# solutions: R problem set for 140.778
#
# part B

# load data
mites <- read.table("mites.txt",header=T)

# number of replicates at each does
n <- table(mites$dose)

# proportion dead mites at each dose
p <- tapply(mites$n.dead,mites$dose,sum)/(tapply(mites$n.dead,mites$dose,length)*10)

# numbers of dead mice, split by dose
x <- split(mites$n.dead,mites$dose)

# calculate generalized Pearson X^2 statistic
chisq <- sapply(x,function(a) sum((a-10*mean(a/10))^2/(10*mean(a/10)*mean(1-a/10))))

# p-values
pval <- 1-pchisq(chisq,n-1)

# plot sample variance vs overall proportion dead
samvar <- sapply(x,var)
plot(p, samvar,
     xlim=c(0,1), # limits of x-axis
     xlab=expression(hat(p)), ylab="Sample variance",  # x- and y-axis titles
     las=1)  # las : make labels on y-axis horizontal
p0 <- seq(0,1,by=0.01)
lines(p0,10*p0*(1-p0),
      lty=2,  # make a dashed line
      lwd=2)  # make that a thick dashed line

# fit glm
out <- glm(n.dead/n.mites ~ dose, weights=n.mites, data=mites,
           family=binomial(link=logit)) 
summary(out)$coef
