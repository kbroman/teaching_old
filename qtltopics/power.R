# code related to power calculations
# Karl Broman, 2 Oct 2012

##############################
# analytic calculations
##############################

# to install the qtlDesign package:
install.packages("qtlDesign")

# then load it
library(qtlDesign)

# heritability (proportion variance) calculations
prop.var("f2", c(1, 0), 1)
prop.var("bc", 1, 1)

gmeans2effect("f2", c(-1, 0.5, 1))
prop.var("f2", gmeans2effect("f2", c(-1, 0.5, 1)), 1)

gmeans2effect("f2", c(-1, 0, 1))
gmeans2effect("bc", c(-1, 0))

# power
powercalc("f2", n=250, effect=c(0.5,0), sigma2=1, thresh=3.5)
powercalc("bc", n=250, effect=0.5, sigma2=1, thresh=3)

# sample size
samplesize("f2", power=0.8, effect=c(0.5,0), sigma2=1, thresh=3.5)
samplesize("bc",  power=0.8, effect=0.5, sigma2=1, thresh=3)

# detectable effect
detectable("f2", n=250, power=0.8, sigma2=1, thresh=3.5)
detectable("bc", n=250, power=0.8, sigma2=1, thresh=3.5)

# CI length
ci.length("f2", n=250, effect=c(0.5, 0), sigma2=1, p=0.95)
ci.length("bc", n=250, effect=0.5, sigma2=1, p=0.95)

##############################
# Simulations
##############################

####
# generate map
####
# built-in 10 cM map for mouse
data(map10)
plot(map10)
summary(map10)

# grab lengths
L <- sapply(map10, function(a) diff(range(a)))

# equally spaced markers, 5 cM
map <- sim.map(L, n.mar=ceiling(L/5)+1, anchor.tel=TRUE,
               include.x=TRUE, eq.spacing=TRUE)
plot(map)
summary(map)

# 75 competely random markers
set.seed(68526309) # seed generated from runif(1, 0, 10^8)
tot.mar <- 75
n.mar <- table(factor(sample(1:20, tot.mar, prob=L/sum(L), repl=TRUE), levels=1:20))
randomMap <- sim.map(L[n.mar > 0], n.mar[n.mar > 0], anchor.tel=FALSE, include.x=TRUE,
                     eq.spacing=n.mar[20] > 0)
names(randomMap) <- c(1:19,"X")[n.mar > 0]
plot(randomMap)

####
# null sims
####
n.ind <- 250
n.sim <- 1000
x <- sim.cross(map10, n.ind=n.ind, type="f2")
x$pheno <- data.frame(matrix(rnorm(n.ind*n.sim), ncol=n.sim))

x <- calc.genoprob(x, step=2.5)
maxlod0 <- apply(scanone(x, method="hk", phe=1:n.sim)[,-(1:2)], 2, max)
thr <- quantile(maxlod0, 0.95)

####
# power sims
####
n.sim <- 100
qtlloc <- 27.5
out <- vector("list", n.sim)
for(i in 1:n.sim) {
  x <- sim.cross(map10[1], n.ind=n.ind, type="f2", model=c(1, qtlloc, 0.5, 0))
  x <- calc.genoprob(x, step=1)
  out[[i]] <- scanone(x, method="hk")
}
maxlod1 <- sapply(out, function(a) max(a[,3]))
mean(maxlod1 > thr) # power
binom.test(sum(maxlod1 > thr), length(maxlod1))$conf.int # conf int'l for power

####
# precision
####
mle <- sapply(out, function(a) mean(a[a[,3]==max(a[,3]),2])) # if mutiple locations sharing max lod, take mean of them
sqrt(mean((mle-qtlloc)^2)) # rms error

####
# coverage
####
li <- vector("list", n.sim)
for(i in 1:n.sim)
  li[[i]] <- range(lodint(out[[i]], chr=1, drop=1)[,2])
cover <- sapply(li, function(a,b) b>=a[1] & b<=a[2], qtlloc)
mean(cover) # coverage
binom.test(sum(cover), length(cover))$conf.int # conf int'l for coverage
