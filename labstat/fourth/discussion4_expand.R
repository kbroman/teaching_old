###############################
# Author:    Qing Li, qli@jhsph.edu
# Created:   April 26, 06
# Updated:
# Purpose:   Lab discussion session 140.616(Statistics for laboratory scientists)
#            functions that expands and matrix manipulation
# R version: R2.1.1
###############################


##############################
# Calculate Bonferroni-corrected confidence intervals by Dr. Broman from lecture 9
##############################
# It's a bit complicated, so I've created my own function:
ci.bonf <-
function(response, group, alpha=0.05)
{
  # calculate anova table
  anova.out <- anova(aov(response~group))

  # within-group mean square
  ms <- anova.out[2,3]

  # number of individuals per group 
  n <- tapply(response, group, length)

  # se's: different for each pair if the sample sizes are different
  se <- sqrt(ms * outer(n,n, function(a,b) 1/a + 1/b))
  # pick out lower triangle
  se <- se[lower.tri(se)]

  # multipier from t distribution
  df <- anova.out[2,1]  # degrees of freedom 
  k <- length(n)
  ntests <- choose(k, 2) # number of pairs 
  tmult <- qt(1 - alpha/ntests, df) 

  # calculate the pairwise differences between the sample means
  me <- tapply(response, group, mean)
  diffs <- outer(me,me,"-")
  # pick out the negative of the lower triangle
  d <- -diffs[lower.tri(diffs)]
  # assign names to these
  rn <- rownames(diffs)[row(diffs)[lower.tri(row(diffs))]]
  cn <- colnames(diffs)[col(diffs)[lower.tri(col(diffs))]]
  names(d) <- paste(cn,rn,sep=" - ")

  cbind(est=d, lower=d-tmult*se, upper=d+tmult*se)
}

##############################
## They expand!!!
##############################

## outer: Imagine create a cross-tab table, apply the function, the results are stored in the cell 
x = 1:3
y = 12:15
# Multiplication & Power Tables
x %o% x
## %o% is an alias for outer (where FUN cannot be changed from "*"). Not very user friendly syntax. 

outer(x, x, "*")
outer(y, x, "^")

outer(month.abb, 1999:2001, FUN = "paste")

## matrix multiplcation
ma1 = matrix(1:6, ncol = 2, nrow=3, byrow=F)
ma2 = matrix(rep(2,2), ncol=1, nrow= 2)

ma1
ma2
ma1%*%ma2

## expand.grid, creat combinations of values from multiple variables
expand.grid(x,y)
expand.grid(x, y, x)


##############################
## Matrix operation
##############################

## matrix: identity, lower/upper
diag(rep(1,3))
ma = matrix(1:9, ncol=3)
filter = lower.tri(ma, diag = FALSE)
filter

## numbers in lower triangular part, excluding diagonal line
ma
ma[filter]

## creat a upper triangular matrix
ma[filter]=0
ma

## matrix manipulation
## [,], dim, nrow, ncol
## rbind, cbind, t
## items in matrix are stored as a vector with elements in matrix by column
maRow = matrix(1:9, ncol=3, byrow=T)
maRow
maCol = matrix(1:9, ncol=3, byrow=F)
maCol
as.vector(maRow)
as.vector(maCol)
as.vector(t(maRow))

#####################################
## a function take advantage of how matrix item is stored
#####################################

# title@ Make duplication of a matrix n times and append them together
#
# argu@ 
# ma@      a matrix which will be duplicated
# n@       an integer referring to the number of time of the duplication
# rowAppend@ a boolean whether the duplicate is append to the matrix vertically or horizontally.
#
# value@
# re@ the matrix after ducpliation and appending
util.matrix.clone = function(ma, n, rowAppend=T){
  rnm = dim(ma)[1]
  cnm = dim(ma)[2]
  if(rowAppend){
    t1 = replicate(n, t(ma))
    re = t(matrix(t1, nrow=cnm, byrow=F))
    re
  }else{
    t1 = replicate(n, ma)
    re = matrix(t1, nrow=rnm, byrow=F)
    re
  }
  return(re)
}
util.matrix.clone(ma, 2)
util.matrix.clone(ma, 3, rowAppend=F)

####################################################
## special math functions for probability calculation and log of things
## ...\base\html\Special.html
####################################################
## beta(a, b)
## lbeta(a, b)
## gamma(x)
## lgamma(x)
## psigamma(x, deriv = 0)
## digamma(x)
## trigamma(x)
## choose(n, k)
## lchoose(n, k)
## factorial(x)
## lfactorial(x)


