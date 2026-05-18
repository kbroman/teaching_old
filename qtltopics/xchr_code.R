# load R/qtlbook package
library(qtlbook)

# lod 'gutlength' data
data(gutlength)

# summary
summary(gutlength)

# phenotypes for first 5 individuals
gutlength$pheno[1:5,]

# pgm vs cross description
table(gutlength$pheno$pgm, gutlength$pheno$cross)

# calculate QTL genotype probabilities
gutlength <- calc.genoprob(gutlength, step=1)

# genome scan
out <- scanone(gutlength)

# plot LOD curves
plot(out)

# permutation test, constant threshold for whole genome
operm1 <- scanone(gutlength, method="hk", n.perm=1000)

# permutation test, separate thresholds for X chr and autosomes
operm2 <- scanone(gutlength, method="hk", n.perm=1000, perm.Xsp=TRUE)

# plot histograms
plot(operm2)

# thresholds
summary(operm2)

# summary of scanone output, constant threshold
summary(out, perms=operm1, alpha=0.05, pval=TRUE)

# summary of scanone output, X- and autosome-specific thresholds
summary(out, perms=operm2, alpha=0.05, pval=TRUE)

# add threholds to plot
plot(out)
abline(h=3) # add horizontal line at 3

# add horizontal lines at X- and autosome-specific thresholds
add.threshold(out, perms=operm2, alpha=0.05, col="green", lwd=3)
