# code related to QTL mapping with covariates
# Karl Broman, 11 Oct 2012

# load data: a fake backcross with two phenotypes and sex and age covariates
data(fake.bc)

# quick look at the phenotypes for the first five individuals
fake.bc$pheno[1:5,]

# t-test for sex difference in the phenotypes
t.test(pheno1 ~ sex, data=fake.bc$pheno)
t.test(pheno2 ~ sex, data=fake.bc$pheno)

# plot of the two phenotypes against each other
plot(pheno1 ~ pheno2, data=fake.bc$pheno)

# colored by sex
plot(pheno1 ~ pheno2, data=fake.bc$pheno,
     pch=21, bg=c("red", "blue")[fake.bc$pheno$sex+1])


# calculate QTL genotype probabilities
fake.bc <- calc.genoprob(fake.bc, step=1)

# scan with no covariates
out0 <- scanone(fake.bc)

# scan with sex as an additive covariate
out.a <- scanone(fake.bc, addcovar=fake.bc$pheno$sex)

# plot the two against each other
plot(out0, out.a, col=c("blue", "red"))

# scan with sex as an interactive covariate
out.f <- scanone(fake.bc, intcovar=fake.bc$pheno$sex)

# plot of the three things
plot(out0, out.a, out.f, col=c("blue", "red", "green"))

# plot of the interaction lod curves
plot(out.f - out.a)

# combine the three sets of LOD scores into one object
out <- cbind(out.f, out.a, out.f-out.a, labels=c("full","add","int"))

# look at the first few
out[1:5,]

# plots of the three LOD curves
plot(out, lod=1:3, col=c("blue", "red", "green"))
plot(out, lod=1:3, col=c("blue", "red", "green"), chr=c(2, 5, 17))

##############################
# for permutations, we need to like those calculating the full LOD
#     and those calculating the additive LOD
##############################

# simulate a large number to use as a seed for the random number generator
seed <- runif(1, 0, 10^8)

# set the seed
set.seed(seed)

# permutations with sex as additive covariate
operm.a <- scanone(fake.bc, addcovar=fake.bc$pheno$sex, n.perm=1000, method="hk", n.cluster=4)

# re-set the seed
set.seed(seed)

# permutations with sex as interactive covariate
operm.f <- scanone(fake.bc, intcovar=fake.bc$pheno$sex, n.perm=1000, method="hk", n.cluster=4)

# plot the two permutations results against each other
plot(operm.a, operm.f) # <- this doesn't work :(

# two ways to get the plot I want
plot(as.numeric(operm.a), as.numeric(operm.f))
plot(unclass(operm.a), unclass(operm.f))

# combine the permutations results for the three sets of LOD scores into one object
operm <- cbind(operm.f, operm.a, operm.f-operm.a, labels=c("full", "add", "int"))

# summary of the results
summary(out, perms=operm, format="tabByChr", alpha=0.1, pval=TRUE)
