
# solutions: R problem set for 140.778
#
# Part A

# load data
expr <- read.table("expr.csv",sep=",",header=T,na.strings=c(".","-99"))
biol <- read.table("biol.csv",sep=",",header=T,na.strings=c(".","-99"))

# matrix, list or data.frame?
is.matrix(expr);is.list(expr);is.data.frame(expr)
is.matrix(biol);is.list(biol);is.data.frame(biol)

# columns = numeric or factor (or both)?
sapply(biol,mode)
sapply(biol,is.factor)
sapply(biol,is.numeric)

sapply(expr,mode)
sapply(expr,is.factor)
sapply(expr,is.numeric)

# find rows in biol with at least one NA
biol[apply(biol,1,function(a) any(is.na(a))),]

# find means of each column in expr
sapply(expr,mean,na.rm=T)

# make sample number the row names
dimnames(expr)[[1]] <- as.character(expr$sample)
expr <- expr[,-1]
sapply(expr,mean,na.rm=T)
sapply(expr,sd,na.rm=T)
sapply(expr,range,na.rm=T)

# correlation matrix
round(cor(expr,use="complete.obs"),2)
round(cor(expr,use="pairwise.complete.obs"),2)

# subtract mean of first two columns from each of the other columns
expr <- expr[,-(1:2)] - apply(expr[,1:2],1,mean,na.rm=T)

# scatterplot matrix
pairs(expr)

# fix up the biol data.frame
biol$f3 <- factor(substring(biol$sample,1,1))
dimnames(biol)[[1]] <- substring(biol$sample,2)
biol <- biol[,-1]

# check that the rownames for biol and expr are the same, just in different orders
all(sort(dimnames(biol)[[1]]) == sort(dimnames(expr)[[1]]))

# sort the rows in biol and expr by their rownames
biol <- biol[sort(dimnames(biol)[[1]]),]
expr <- expr[sort(dimnames(expr)[[1]]),]

# find mean & SD of each column in expr 
apply(expr,2,function(a,b)
      tapply(a,b,function(x) c(mean(x,na.rm=T), sd(x,na.rm=T))),biol$f1)

# t.test comparing groups defined by biol$f2 for each column of expr
library(ctest)
apply(expr,2,function(a,b) {
  x <- split(a,b)
  t.test(x[[1]],x[[2]])$p.value },
      biol$f2)

