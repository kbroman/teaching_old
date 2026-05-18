
######################################################################
# load data (this is the first data set from the QTL archive)
######################################################################
x <- read.cross("csv", "", "Beamer.csv", genotypes=c("A","H","B"),
                alleles=c("A","B"))

# summary of the data
summary(x)

# Note that the 20th chromosome is being considered an autosome,
# but should be the X chromosome.  Either modify the csv file to
# have the 20th chromosome having identifier X, or fix it as follows:

names(x$geno)[20] <- "X"
class(x$geno[[20]]) <- "X"

# summary plot of the data
plot(x)

# first diagnostic: estimate recombination fractions between all pairs
#     of markers
x <- est.rf(x)

# plot of that
plot.rf(x)

# plot of just chr 6 and 8:
#    third marker on chr 8 looks like it should be on chr 6
plot.rf(x, chr=c(6,8))

# figure out which marker to move and where
pull.map(x, chr=8)
pull.map(x, chr=6)

# move the mis-placed marker
x <- movemarker(x, "D8Mit76", 6, 32)

# plot the recombination fractions again
plot.rf(x, chr=c(6,8))

# re-estimate genetic map
nm <- est.map(x, err=0.01)

# plot the map in the data vs that estimated from the data
plot.map(x, nm)

# replace the map in the data with the newly estimated one
x <- replace.map(x, nm)

# Study marker order:
#     for each chromsome, count number of obligate crossovers
#     for each order of markers (permuting markers within sliding window)
rip <- vector("list", 20)
for(i in 1:20) {
  cat(i, "\n")
  rip[[i]] <- ripple(x, chr=i, window=5)
}

# possible errors in order:
summary(rip[[5]])
summary(rip[[6]])
summary(rip[[13]])
summary(rip[[14]])

# study marker order via likelihood, switching adjacent markers
rip[[5]] <- ripple(x, chr=5, window=2, method="lik", err=0.01)
summary(rip[[5]])
# chr 5 looks okay

rip[[13]] <- ripple(x, chr=13, window=2, method="lik", err=0.01)
summary(rip[[13]])
# switch to new order
x <- switch.order(x, chr=13, rip[[13]][2,])

rip[[14]] <- ripple(x, chr=14, window=2, method="lik", err=0.01)
summary(rip[[14]])
# switch to new order
x <- switch.order(x, chr=14, rip[[14]][2,])

rip[[6]] <- ripple(x, chr=6, window=2, method="lik", err=0.01)
summary(rip[[6]])
# switch to new order
x <- switch.order(x, chr=6, rip[[6]][2,])

######################################################################

# calculate genotyping error LOD scores
x <- calc.errorlod(x, err=0.01)
top.errorlod(x)

# look at plot of genotypes 
plot.geno(x, chr=13, ind=276:300)
plot.geno(x, chr=1, ind=376:400)

