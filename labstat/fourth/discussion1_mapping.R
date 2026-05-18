
###############################
# Author:    Qing Li, qli@jhsph.edu
# Created:   March 29, 06
# Updated:
# Purpose:   Lab discussion session 140.616(Statistics for laboratory scientists)
#            Mapping For Subsetting in R 
# R version: R2.1.1
###############################


# table(), sample(), rmultinom().

numbers = rpois(100,5)
table(numbers)

quantile(numbers)
table(cut(numbers, quantile(numbers)))


sample(seq(0,1), size = 3, replace=T)
sample(seq(0,2), size = 3, replace=F)
numbers = sample(seq(0,1), size = 1000, replace = T, prob =c(.1, .9) )

table(numbers)



numbers = rmultinom(5, size = 12, prob=c(0.1,0.2,0.8,0.4))
numbers

numbers = rmultinom(500, size = 12, prob=c(0.2,0.7,.1))
test = sapply(1:3, FUN=function(row, tbl){ sum(tbl[row,])}, tbl = numbers) 
test/sum(test)


dmultinom(c(1,2,1),  prob=c(.1,.2,.8))





# Vector, array, list, matrix, data.frame are stored in order
# Extraction can be done using subsetting function
# [filter], [filter,], [,filter], where filter usually has the same length as the object
# you try to extract element from.

letterSeq = letters[1:10]
letterSeq

filter = is.element(letterSeq, c("h", "i", "f"))
filter

exLetters = letterSeq[filter]
exLetters

#####################################################
# Big deal! Don't impress me much!
charBag = list(lowletter=c("a", "b", "c"), upperletter = c("A", "C", "E"), numbers = c(1, 3))
filter = names(charBag) == "upperletter" | names(charBag) == "numbers"
filter

exBag = charBag[filter]
exBag



############################################################
# Mapping from key value to complicated objects
data1 = data.frame(id=seq(1,10), y=rnorm(10), snp = rep("1-1", 10))
data2 = data.frame(id=seq(1,10), y=rnorm(10), snp = rep("1-2", 10))
data3 = data.frame(id=seq(1,10), y=rnorm(10), snp = rep("1-3", 10))

dframeList = c(list(data1), list(data2), list(data3))
dframeList

keySeq = sapply(dframeList, FUN=function(item, keyCol=3){ as.character(item[1,keyCol]) })
keySeq

filter = keySeq == "1-2"
exDframe = dframeList[!filter]
exDframe




############################################################
# Functions that will produce the filter
# Basically any logicial operation that will produce T/F: ==, >=, <, is.na, is.null, is.element,
# returns indexes of elements in an ordered object which meet certain criterion: which
oriSeq = rep(c(1,2,3), times =3)
oriSeq
filter = which(oriSeq == 1)
filter
exSeq = oriSeq[filter]
exSeq


#############################################################
# CAUTION: using : can be dangerous!!!!
# want to the group every four consecutive numbers together in a sequence, called oriSeq
oriSeq = seq(1,8)
firstIndex = seq(1, 8, by =4)
firstIndex
exGroups = lapply(firstIndex, FUN=function(fIndex, wholeList){
                                wholeList[fIndex:fIndex+3]
                              }, wholeList = oriSeq)

exGroups

exGroups = lapply(firstIndex, FUN=function(fIndex, wholeList){
                                wholeList[fIndex:(fIndex+3)]
                              }, wholeList = oriSeq)

exGroups





#############################################################
# An wild, wild example
# want to return associate fruit name with certain colors
# having
fruit = NULL
fruit$citrus = c("green", "yellow", "orange")
fruit$banana = c("yellow")
fruit$apple  = c("red", "green", "yellow")

# want a variable colors so that
## > colors
## $green
## [1] "citrus" "apple" 
## 
## $orange
## [1] "citrus"
## 
## $red
## [1] "apple"
## 
## $yellow
## [1] "citrus" "banana" "apple" 

## Dr. Broman's code

nameArray = rep(names(fruit), sapply(fruit, length))
valueArray = unlist(fruit)
nameArray
valueArray
colors = split(nameArray, valueArray)
colors
