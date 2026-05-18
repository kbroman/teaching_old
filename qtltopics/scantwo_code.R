
# load the data
data(hyper)

# the following contains the permutation results, the 2d scan results,
#     and a whole bunch of other stuff (for later)
load("hyper_demo_stuff.RData")

# 2d-scan by Haley-Knott regression
#     behaves badly due to the selective genotyping
hyper <- calc.genoprob(hyper, step=2.5)
out2 <- scantwo(hyper, method="hk")

# 2d-scan by maximum likelihood / standard interval mapping / EM algorithm
out2 <- scantwo(hyper)

# plot result (includes full LOD in lower triangle and int've LOD in upper
plot(out2)

# plot "fv1" LOD in lower triangle
plot(out2, lower="fv1")

# plot "fv1" in upper triangle and int've LOD in lower triangle
#  (selected chromosomes)
plot(out2, upper="fv1", lower="int", chr=c(1,4,6,15))

# plot "av1" in upper triangle and "fv1" in lower triangle, just chr 1
plot(out2, lower="fv1", upper="av1", chr=1)

# summary of the results
summary(out2, perms=operm2, alpha=0.1, pval=TRUE)
