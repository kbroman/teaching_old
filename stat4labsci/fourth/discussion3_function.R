
###############################
# Author:    Qing Li, qli@jhsph.edu
# Created:   April 12, 06
# Updated:
# Purpose:   Lab discussion session 140.616(Statistics for laboratory scientists):: write functions for code reuse
# R version: R2.1.1
###############################

#####################################
# definition: parameter, return value


# title@ select rows from a matrix if their primary key column contained in the keys we want to select from
#
# argu@
# ma@     a matrix upon which the function will operate
# keyCol@ the column index (base 1) in the argu, ma, where the primary key information stores
# keys@   a vector referring to the value of keys
#
# value@
# maNew@  a matrix whose primary key column contain the values we want to select
#
# seealso@ util.vec.exByKey, util.lst.ex 
util.matrix.exByKeyInRow = function(ma, keyCol, keys){

    maNew = ma[ is.element( ma[,keyCol], keys), ]

    return(maNew)
}


makey = 1:4
ma = matrix(rnorm(12), ncol=3, byrow =T)
ma = cbind(makey, ma)

util.matrix.exByKeyInRow(ma=ma, keyCol=1, keys=c(2,1))
util.matrix.exByKeyInRow(ma, 1, c(2,1))
util.matrix.exByKeyInRow(m=ma, keyC = 1, keys=c(2,1))


#compare with
ma[is.element (ma[,1], c(2,1)), ]

#################################
## capsulate, object-oriented
## Advantage: code readability, code reuse, easy for code maintainance
## Disadvantege: dependence between code modules

## For a list of matrix, obtain rows with the key values=1 or 2
matrixList = rep(list(ma), 3)
lapply(matrixList, util.matrix.exByKeyInRow, keyCol = 1, keys = c(2,1))

## versus
lapply(matrixList, FUN=function(item, keyCol, keys){
                      item[ is.element (item[,keyCol], keys), ]}, keyCol=1, keys=c(2,1))

lapply(matrixList, FUN=function(item, keyCol, keys){
                      item[ is.element (item[,keyCol], keys), ]}, 1, c(2,1))



##################################
## default value
## Advantage: increase usability

chisq <-
function(x=c(35,43,22), p=c(0.25,0.5,0.25))
{
  # check that x,p are appropirate
  p <- p/sum(p) # ensure that the probabilities sum to 1
  if(any(p < 0)) stop("probabilities p must be non-negative.")
  if(any(x < 0)) stop("x's must all be non-negative.")
  if(length(x) != length(p)) stop("length(x) should be the same as length(p).")
  
  n <- sum(x)
  expected <- n*p

  sum( (x - expected)^2 / expected )
}

chisqMy = function(x, p)
{
  # check that x,p are appropirate
  p <- p/sum(p) # ensure that the probabilities sum to 1
  if(any(p < 0)) stop("probabilities p must be non-negative.")
  if(any(x < 0)) stop("x's must all be non-negative.")
  if(length(x) != length(p)) stop("length(x) should be the same as length(p).")
  
  n <- sum(x)
  expected <- n*p

  sum( (x - expected)^2 / expected )
}

chisq()
## will give you error
chisqMy()
chisqMy(c(35,43,22), c(0.25,0.5,0.25))


###################################
## passing parameter value to inside function and ...
##


## obtain a list of chisq
simdat <- rmultinom(10, 100, c(0.25, 0.5, 0.25))

anOutsideFun = function(ma, prop){
  re = NULL
  for( i in 1:ncol(ma)){
        oneRep  = chisq(ma[,i], p=prop)
        re = c(re, oneRep)
  }
  return(re)
}
anOutsideFun(simdat, prop=c(.25, .5, .25))

## give you error
anOutsideFun(simdat)

anOutsideFun2 = function(ma, prop=c(.25, .5, .25)){
  re = NULL
  for( i in 1:ncol(ma)){
        ## next line should not be
        ## oneRep  = chisq(ma[,i], p=c(.25, .5, .25))
        oneRep  = chisq(ma[,i], p=prop)
        re = c(re, oneRep)
  }
  return(re)
}
## would not give you error
anOutsideFun2(simdat)


anOutsideFun3 = function(ma, ...){
  re = NULL
  for( i in 1:ncol(ma)){
        oneRep  = chisq(ma[,i], ...)
        re = c(re, oneRep)
  }
  return(re)
}
## would not give you error
anOutsideFun3(simdat, p=c(.25, .5, .25))
anOutsideFun3(simdat, c(.25, .5, .25))
