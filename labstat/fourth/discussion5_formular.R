
###############################
# Author:    Qing Li, qli@jhsph.edu
# Created:   May 3, 06
# Updated:
# Purpose:   Lab discussion session 140.616(Statistics for laboratory scientists):: Use of formular in regression
# R version: R2.1.1
###############################


################################3
## Environments consist of a frame, or collection of named objects, and a pointer to an enclosing environment. 
data = read.csv("http://www.biostat.jhsph.edu/~kbroman/teaching/labstat/fourth/lab3.csv")

## Error 
reg = lm(concentration ~ response)

## No error
reg = lm(concentration ~ response, data=data)


## or using attach
attach(data)
concentration
reg = lm(concentration ~ response)
ls()
ls(pos=1)
ls(pos=2)

aList = list(a="this", b="is", c="a test")
attach(aList)
ls()
ls(pos=1)
ls(pos=2)



## or using column directly
reg = lm(data$conc ~ data$response)
reg = lm(data[,1] ~ data[,2])
## Why? performance reasons



###################################################
## formular

formu1 = as.formula("concentration ~ response")
str(formu1)
all.vars(formu1)
terms(formu1)

reg = lm(formu1, data=data)
reg = lm("concentration ~ response", data=data)

plot(concentration ~ response, data=data)



###################################################
##
## create dynamic regression analysis, and extract result from regression object
caseControlAssigner = function(prob){
  coin = runif(1)
  re = 1
  if(coin >= prob) re = 0
  return(re)
}

xnam <- paste("x", 1:10, sep="")
xnorm = rnorm(1000)
xCov = matrix(xnorm, ncol=10)
colnames(xCov) = xnam

y = sapply(rep(.5, times = 100), caseControlAssigner)

data = data.frame(y, xCov)
fmla <- as.formula(paste("y ~ ", paste(xnam, collapse= "+")))

reg = lm(fmla, data=data)
names(reg)
reg$coef
reg$fitted


sumReg = summary(reg)
names(sumReg)
sumReg$fstat
