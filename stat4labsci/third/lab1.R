######################################################################
# R commands                     Statistics for Laboratory Scientists 
# Lab 1                                      Johns Hopkins University 
######################################################################
# This file contains the R commands that appear in the lab
# handout/pdf file.
#
# Lines beginning with the symbol '#' are comments in R.  All other
# lines contain code.
#
# In R for Windows, you may wish to open this file from the menu bar
# (File:Display file); you can then copy commands into the command
# window.  (Use the mouse to highlight one or more lines; then
# right-click and select "Paste to console".)
######################################################################

##############################
# Reading data into R
##############################
mat <- read.csv("http://www.biostat.jhsph.edu/~kbroman/teaching/data/mathura.csv")

##############################
# A few quick things
##############################
summary(mat)
plot(mat)

plot(velo.vo ~ velo.rest, data=mat)
points(velo.vo ~ velo.rest, data=mat,
       subset=(method=="capillaroscopy"), col="red")
abline(0,1)

plot(diam.vo ~ diam.rest, data=mat)
points(diam.vo ~ diam.rest, data=mat,
       subset=(method=="capillaroscopy"), col="red")
abline(0,1)

##############################
# Creating simple vectors
##############################
x <- c(1, 3.5, -28.4, 10)
x
c("cat", "dog", "mouse", "monkey")
c(TRUE, TRUE, TRUE, FALSE, FALSE)

1:10
3:8
-3:8
8:2
10:10
5.2:20

seq(1, 10, by=1)
seq(3, 9, by=3)
seq(3, 9, length=10)
seq(2, by=0.2, length=8)

rep(2, 10)
rep(c(1,2,3), 5)
rep(c(1,2,3), c(2,4,5))
rep(1:3, 4)
rep(1:3, rep(4,3))

##############################
# Subsetting vectors
##############################
x <- seq(2, 40, by=2)
length(x)
x[5]
x[c(1,3,9)]
x[-(1:10)]
x[-5]

z <- c(1, 3, 5, 9)
z
z[2] <- -3
z

y <- c(rep(TRUE,4), rep(FALSE,14), TRUE, TRUE)
length(y)
x[y]

a <- c(rep(c(TRUE, FALSE), 2), NA)
b <- c(rep(c(TRUE, FALSE), c(2,2)), FALSE)
a
b
!a
a & b
a | b
!(a | b)
!a | b

x <- c(1,5,3,NA,9,11,2,3)
x<=5
x>3 & x<11
is.na(x)
x[!is.na(x)]
x[!is.na(x) & x<5]

##############################
# Subsetting matrices
##############################
library(datasets)
data(PlantGrowth)

summary(PlantGrowth)
PlantGrowth[PlantGrowth[,2]=="ctrl",]
summary(PlantGrowth[PlantGrowth[,2]=="ctrl",])
summary(PlantGrowth[PlantGrowth[,2]=="trt1",])
summary(PlantGrowth[PlantGrowth[,2]=="trt2",])

mat[1:5,]
mat[,2]
mat[11:20, c(1,4:5)]
mat[is.na(mat[,2]), ]
mat[is.na(mat[,2]) & is.na(mat[,3]), ]
mat[is.na(mat[,2]) | is.na(mat[,3]), ]
mat[mat[,1]=="OPS imaging", ]

##############################
# R as a calculator
##############################
2 + 3 - 2^3
(2 + 3 - 2^3)*4
(2 + 3 - 2^3)/4
3^4
2 + 3^4
2 + (1:4)^4
sin(0.5)
log(seq(1, 2, length=11))
log10(seq(1, 100, length=11))
log2(c(1, 2, 4, 8, 16, 32))
x <- c(1, 5, 10, NA, 15)
sum(x)
sum(x, na.rm=TRUE)
prod(x, na.rm=TRUE)

##############################
# Summary statistics
##############################
data(PlantGrowth)
mean(PlantGrowth[PlantGrowth[,2]=="ctrl",1])
mean(PlantGrowth[PlantGrowth[,2]=="trt1",1])
mean(PlantGrowth[PlantGrowth[,2]=="trt2",1])

z <- PlantGrowth[PlantGrowth[,2]=="ctrl", 1]
median(z)
sd(z)
x <- quantile(z, c(0.25, 0.75))
x
diff(x)
range(z)
diff(range(z))

x <- mat[mat[,1]=="capillaroscopy",2]
x
sum(is.na(x))
mean(x)
mean(x, na.rm=TRUE)
median(x, na.rm=TRUE)
sd(x, na.rm=TRUE)
diff(quantile(x, c(0.25,0.75), na.rm=TRUE))
diff(range(x,na.rm=TRUE))

##############################
# Loops
##############################
me <- 1:4
for(z in 1:4) me[z] <- mean(mat[,z+1], na.rm=TRUE)
me

me <- matrix(nrow=2,ncol=4)
le <- levels(mat[,1])
for(i in 1:2)
  for(j in 1:4)
    me[i,j] <- mean(mat[mat[,1]==le[i], j+1], na.rm=TRUE)
me

##############################
# The apply functions
##############################
me <- apply(mat[,-1], 2, mean, na.rm=TRUE)

me <- sapply(mat[,-1], mean, na.rm=TRUE)

x <- mat[mat[,1]=="capillaroscopy",]
me <- sapply(x[,-1], mean, na.rm=TRUE)
sd <- sapply(x[,-1], sd, na.rm=TRUE)

data(PlantGrowth)
tapply(PlantGrowth[,1], PlantGrowth[,2], mean)
tapply(PlantGrowth[,1], PlantGrowth[,2], sd)

sapply(mat[,-1], tapply, mat[,1], mean, na.rm=TRUE)
sapply(mat[,-1], tapply, mat[,1], sd, na.rm=TRUE)

##############################
# Simple graphics
##############################
x <- mat[,2]
y <- rep(2, length(x))
y[ mat[,1]=="capillaroscopy" ] <- 1

plot(x, y)

u <- runif(length(y), -0.1, 0.1)
plot(x, y+u, xlab="RBC velocity (at rest)", ylim=c(0.5, 2.5),
     yaxt="n", ylab="")

abline(h=c(1,2), lty=2, col="gray")

boxplot(velo.rest ~ method, data=mat)

par(mfrow=c(2,1))
hist(mat[ mat[,1]=="capillaroscopy", 2], main="Capillaroscopy",
     xlab="RBC velocity (at rest)")
hist(mat[ mat[,1]=="OPS imaging", 2], main="OPS imaging",
     xlab="RBC velocity (at rest)")

##################
# End of lab1.R
##################
