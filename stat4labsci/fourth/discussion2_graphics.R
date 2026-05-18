
###############################
# Author:    Qing Li, qli@jhsph.edu
# Created:   April 05, 06
# Updated:
# Purpose:   Lab discussion session 140.616(Statistics for laboratory scientists)
#            Graphics in R 
# R version: R2.1.1
###############################

# reference card: http://www.rpad.org/Rpad/R-refcard.pdf

# hist(), qqnorm(), boxplot(), plot(), lines(), points()
# jitter(), sort(), axis(), title(), box()
# par(), mfrow, main, xlab, ylab, xlim, ylim, pch with ps, lty with lwd, mar, mgp
# pdf(), postscript(), win.metafile()
par(mfrow=c(1,1))


#############################
# hist(), qqnorm(), plot(regression object)
n = 100
nums = rnorm(n, mean=3, sd=10)
hist(nums)

qqnorm(nums)

boxplot(nums)

boxplot(nums,
        main = "Boxplot for Normal Random Variable",
        xlab = "random variable",
        ylab = "distribution")



x = sample(1:10, size = n, replace=T)
y = x*2 + rnorm(n, mean=0, sd=2)
reg = lm(nums ~1+x)
par(mfrow=c(2,2))
plot(reg)

par(mfrow=c(1,1))
boxplot(x, y,
        main = "Boxplot for Normal Random Variable",
        xlab = "random variable",
        ylab = "distribution", axes=F)
axis(2)
#axis(1)
axis(1, 1:2, c("var X", "var Y"))
box()

#############################
# lines(), points(), jitter(), sort(), ylim
par(mfrow=c(2,1))
plot(x, nums)
plot(jitter(x), jitter(nums), pch = 20)



newY = sort(y)
plot(1:length(newY), newY, pch = 20)
lines(1:length(newY), newY, col ="red")
lines(1:length(newY), 2*newY, col ="blue")

plot(1:length(newY), newY, pch = 20, ylim = c(min(newY),max(newY*2)))
lines(1:length(newY), 2*newY, col ="blue")
lines(1:length(newY), newY, col ="red", xlim = c(40,50))



#############################
# graphics parameters: par
# font size
par(mfrow=c(2,1))
par(cex.axis=.5)
par(cex.lab=.5)
par(cex.main=.5)
boxplot(nums,
        main = "Boxplot for Normal Random Variable",
        xlab = "random variable",
        ylab = "distribution")


par(cex.axis=1)
par(cex.lab=1)
par(cex.main=1)
boxplot(nums,
        main = "Boxplot for Normal Random Variable",
        xlab = "random variable",
        ylab = "distribution")


# font type
par(mfrow=c(2,1))
par(font.axis=2)
par(font.lab=2)
par(font.main=2)
boxplot(nums,
        main = "Boxplot for Normal Random Variable",
        xlab = "random variable",
        ylab = "distribution")


par(font.axis=1)
par(font.lab=1)
par(font.main=1)
boxplot(nums,
        main = "Boxplot for Normal Random Variable",
        xlab = "random variable",
        ylab = "distribution")


#############################
# graphics parameters: par
# layout: mfrow, mar, mgp
par(mfrow=c(1,1))
par(mar=c(0,3,6,12))
plot(1,1)
box()

par(mfrow=c(1,1))
par(mar=c(7,12,4,4))
par(mgp=c(6,3,1))

newY = sort(y)
plot(1:length(newY), newY, pch = 20,
     main = "Plot for Y",
     ylab = "y value",
     xlab = "sequence")


#############################
# Saving graphics
# pdf, postscript, win.metafile
pdf(file = "boxPlot.pdf")
par(mfrow=c(1,1))
par(mar=c(7,12,4,4))
par(mgp=c(6,3,1))

newY = sort(y)
plot(1:length(newY), newY, pch = 20,
     main = "Plot for Y",
     ylab = "y value",
     xlab = "sequence")
dev.off()



