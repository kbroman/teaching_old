# first calculate QTL genotype probabilities, given available marker data
#     step=1 indicates that genome scans will be performed at 1 cM steps
x <- calc.genoprob(x, step=1, err=0.01)

# Genome scans by EM and Haley-Knott
out.em <- scanone(x, method="em", pheno.col=2)
out.hk <- scanone(x, method="hk", pheno.col=2)

# plot the results
plot(out.em, out.hk, col=c("blue", "red"))
plot(out.em, out.hk, col=c("blue", "red"), chr=c(1,14))

# Haley-Knott regression is performing badly, due to the selective genotyping
plot.missing(x, chr=c(1,14), reorder=2)

# Number of missing genotypes for each individual
nmis <- nmissing(x)
plot(nmis)
abline(h=c(20, 100))

# permutation test via Haley-Knott regression
operm.hk <- scanone(x, method="hk", pheno.col=2, n.perm=1000)

# Set-up strata for a stratified permutation test
strata <- cut(nmis, c(-Inf, 20, 100, Inf))
table(strata)
strata <- as.numeric(strata)

# stratified permutation test
operm.hk.strat <- scanone(x, method="hk", pheno.col=2, n.perm=1000, perm.strata=strata)

# get LOD thresholds
summary(operm.hk)
summary(operm.hk.strat)

# get summary, with P-values, of the results
summary(out.hk, perms=operm.hk, alpha=0.05, pvalues=TRUE)
summary(out.hk, perms=operm.hk.strat, alpha=0.05, pvalues=TRUE)

# note: can do the above again with standard interval mapping; it's
#       just slower.
