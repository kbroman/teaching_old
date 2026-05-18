######################################################################
# Demo regarding multiple-QTL mapping in R/qtl, using the hyper data
#
# Karl W Broman
# 30 March 2011
# revised 4 April 2011
#
######################################################################

# we'll fill in the missing genotype data
# [DON'T DO THIS IN PRACTICE!]
data(hyper)
hyper <- fill.geno(hyper)

# calculate QTL genotype probabilities
hyper <- calc.genoprob(hyper, step=1, error.prob=0.01)

# scanone 
out <- scanone(hyper, method="hk")

# scanone permutations
operm <- scanone(hyper, method="hk", n.perm=1000, n.cluster=8)

# scanone summaries
summary(out, perms=operm, alpha=0.05, pval=TRUE)
summary(out, perms=operm, alpha=0.05, format="tabByCol", pval=TRUE)
summary(out, perms=operm, alpha=0.5, pval=TRUE)

# control for chr 4
# ver 1: pull out genotype
g <- pull.geno(hyper, chr=4)[,"D4Mit164"]
out.c4a <- scanone(hyper, addcovar=g, method="hk")

# ver 2: pull out genotype probs
gp <- hyper$geno[[4]]$prob[,"D4Mit164",2]
out.c4b <- scanone(hyper, addcovar=gp, method="hk")

# ver 3: use makeqtl/addqtl
c4qtl <- makeqtl(hyper, chr=4, pos=29.5, what="prob")
out.c4c <- addqtl(hyper, qtl=c4qtl, method="hk")

# plots
plot(out, out.c4a, chr=c(1,4,6,15), col=c("blue", "red"))
plot(out.c4a, out.c4b, chr=c(1,4,6,15), col=c("blue", "red"), lty=1:2)
plot(out.c4b, out.c4c, chr=c(1,4,6,15), col=c("blue", "red"), lty=1:2)

plot(out.c4a - out.c4b, ylim=c(-0.8, 0.8))
plot(out.c4b - out.c4c, ylim=c(-0.8, 0.8))

# scantwo
out2 <- scantwo(hyper, method="hk")

# scantwo permutations
operm2 <- scantwo(hyper, method="hk", n.perm=1000, n.cluster=8)

# scantwo plots
plot(out2)
plot(out2, lower="fv1")
plot(out2, lower="fv1", chr=c(1,4,6,15))
plot(out2, lower="fv1", chr=c(6,15))
plot(out2, lower="fv1", chr=c(6,15), point.at.max=TRUE, contours=TRUE)
plot(out2, lower="fv1", upper="av1", chr=1, point.at.max=TRUE, contours=TRUE)

# scantwo maxima
max(out2)
max(subset(out2, chr=1))
max(subset(out2, chr=c(6,15)))
max(subset(out2, chr=c(7,15)))

# scantwo summaries
summary(out2, perms=operm2, alpha=0.05)
summary(out2, perms=operm2, alpha=c(0.05, 0.05, 0, 0.05, 0.05))

# fitqtl
qtl <- makeqtl(hyper, chr=c(1,4,6,15), pos=c(71.3,30,55,20.5), what="prob")
summary(out.fq <- fitqtl(hyper, qtl=qtl, formula=y~Q1+Q2+Q3*Q4, method="hk"), pval=FALSE)
qtl <- refineqtl(hyper, qtl=qtl, formula=y~Q1+Q2+Q3*Q4, method="hk")
summary(out.rfq <- fitqtl(hyper, qtl=qtl, formula=y~Q1+Q2+Q3*Q4, method="hk"), pval=FALSE)
plotLodProfile(qtl)
summary(out.rfq <- fitqtl(hyper, qtl=qtl, formula=y~Q1+Q2+Q3*Q4, method="hk",
                          get.ests=TRUE), pval=FALSE)

# drop-one-QTL doesn't account for uncertainty in the other QTL
rqtl.n1 <- refineqtl(hyper, qtl=qtl, formula=y~Q2+Q3*Q4, method="hk", verbose=FALSE)
out.fqn1 <- fitqtl(hyper, qtl=rqtl.n1, formula=y~Q2+Q3*Q4, method="hk", dropone=FALSE)
out.fqn1p <- fitqtl(hyper, qtl=qtl, formula=y~Q1+Q3*Q4, method="hk", dropone=FALSE)
out.rfq$lod - out.fqn1$lod
out.rfq$lod - out.fqn1p$lod

rqtl.n4 <- refineqtl(hyper, qtl=qtl, formula=y~Q1+Q3*Q4, method="hk", verbose=FALSE)
out.fqn4 <- fitqtl(hyper, qtl=rqtl.n4, formula=y~Q1+Q3*Q4, method="hk", dropone=FALSE)
out.fqn4p <- fitqtl(hyper, qtl=qtl, formula=y~Q1+Q3*Q4, method="hk", dropone=FALSE)
out.rfq$lod - out.fqn4$lod
out.rfq$lod - out.fqn4p$lod

rqtl.n6 <- refineqtl(hyper, qtl=qtl, formula=y~Q1+Q2+Q4, method="hk", verbose=FALSE)
out.fqn6 <- fitqtl(hyper, qtl=rqtl.n6, formula=y~Q1+Q2+Q4, method="hk", dropone=FALSE)
out.fqn6p <- fitqtl(hyper, qtl=qtl, formula=y~Q1+Q2+Q4, method="hk", dropone=FALSE)
out.rfq$lod - out.fqn6$lod
out.rfq$lod - out.fqn6p$lod

rqtl.n15 <- refineqtl(hyper, qtl=qtl, formula=y~Q1+Q2+Q3, method="hk", verbose=FALSE)
out.fqn15 <- fitqtl(hyper, qtl=rqtl.n15, formula=y~Q1+Q2+Q3, method="hk", dropone=FALSE)
out.fqn15p <- fitqtl(hyper, qtl=qtl, formula=y~Q1+Q2+Q3, method="hk", dropone=FALSE)
out.rfq$lod - out.fqn15$lod
out.rfq$lod - out.fqn15p$lod

# 1.5-lod intervals
lodint(qtl, qtl.index=1)
lodint(qtl, qtl.index=2)
lodint(qtl, qtl.index=3)
lodint(qtl, qtl.index=4)

# scan for an additional QTL
out.aq <- addqtl(hyper, qtl=qtl, formula=y~Q1+Q2+Q3*Q4, method="hk")
plot(out.aq)
max(out.aq)

out.iq <- addqtl(hyper, qtl=qtl, formula=y~Q1+Q2+Q3*Q4+Q5+Q4:Q5, method="hk")
plot(out.iq, out.aq, out.iq - out.aq)
max(out.iq)
max(out.iq-out.aq)

# study uncertainty in QTL location on 6, 15
out.6n15 <- addpair(hyper, qtl=qtl, formula=y~Q1+Q2+Q5*Q6, chr=c(6,15),
                    method="hk")
plot(out.6n15, zscale=FALSE, point.at.max=TRUE, contours=TRUE)
max(out.6n15)

# two QTL on chr 1?
out2.c1 <- addpair(hyper, qtl=qtl, formula=y~Q2+Q3*Q4+Q5+Q6, chr=1, method="hk")
out1.c1 <- addqtl(hyper, qtl=qtl, formula=y~Q2+Q3*Q4+Q5, chr=1, method="hk")
plot(out1.c1)
plot(out2.c1)
max(out1.c1)
max(out2.c1)

# QTL on chr 7 that interacts with the chr 15 locus?
out.7n15 <- addpair(hyper, qtl=qtl, formula=y~Q1+Q2+Q3+Q3:Q6+Q5*Q6, chr=c(7,15),
                    method="hk")
summary(out.7n15)

# calculate penalties
print(pen <- calc.penalties(operm2))

# stepwise QTL, assuming additive
out.sqa <- stepwiseqtl(hyper, method="hk", additive.only=TRUE, keeplodprofile=TRUE,
                       keeptrace=TRUE, max.qtl=6)
out.sqa
par(mfrow=c(1,1))
plotLodProfile(out.sqa)
themod <- attr(out.sqa, "trace")
par(mfcol=c(3,4))
for(i in seq(along=themod))
  plotModel(themod[[i]], main=myround(attr(themod[[i]], "pLOD"), 2))

# stepwise QTL, assuming additive, starting at our estimated model
out.sqa2 <- stepwiseqtl(hyper, method="hk", additive.only=TRUE, keeplodprofile=TRUE,
                        keeptrace=TRUE, max.qtl=6, qtl=qtl, formula=y~Q1+Q2+Q3*Q4)
out.sqa2
par(mfrow=c(1,1))
plotLodProfile(out.sqa2)
themod <- attr(out.sqa2, "trace")
par(mfcol=c(3,4))
for(i in seq(along=themod))
  plotModel(themod[[i]], main=myround(attr(themod[[i]], "pLOD"), 2))


# stepwise QTL, allowing epistasis
out.sqi <- stepwiseqtl(hyper, method="hk", keeplodprofile=TRUE,
                       keeptrace=TRUE, max.qtl=8)
out.sqi
par(mfrow=c(1,1))
plotLodProfile(out.sqi)
themod <- attr(out.sqi, "trace")
par(mfcol=c(4,5))
for(i in seq(along=themod))
  plotModel(themod[[i]], main=myround(attr(themod[[i]], "pLOD"), 2))

# stepwise QTL, allowing epistasis, starting at our estimated model
out.sqi2 <- stepwiseqtl(hyper, method="hk", keeplodprofile=TRUE,
                        keeptrace=TRUE, max.qtl=8, qtl=qtl, formula=y~Q1+Q2+Q3*Q4)
out.sqi2
par(mfrow=c(1,1))
plotLodProfile(out.sqi2)
themod <- attr(out.sqi2, "trace")
par(mfcol=c(4,4))
for(i in seq(along=themod))
  plotModel(themod[[i]], main=myround(attr(themod[[i]], "pLOD"), 2))


# save stuff
hyper <- clean(hyper)
save(out.sqa, out.sqa2, out.sqi, out.sqi2, out.6n15, out2.c1, out1.c1,
     out2, operm2, operm, hyper, out.7n15, file="hyper_demo_stuff.RData")
