######################################################################
# Computer notes, Lecture 28
# Stat 371: Introductory applied statistics for the life sciences
######################################################################
# These notes provide R code related to the course lectures, to
# further illustrate the topics in the lectures and to assist the
# students to learn R.
#
# Lines beginning with the symbol '#' are comments in R.  All other
# lines contain code.
#
# You can view this file within R by typing:
url.show("https://kbroman.org/teaching/stat371/comp28.R")
######################################################################

# The data used in this lecture are available at
#   https://kbroman.org/teaching/stat371/il10.csv

# read data
il10 <- read.csv("https://kbroman.org/teaching/stat371/il10.csv")

# include log10(IL10) as a column
il10 <- cbind(il10, logIL10=log10(il10$IL10))

# reorder the factor levels
temp <- as.character(il10$Strain)
il10$Strain <-
  factor(temp, levels=rev(c("A","B6",as.character(c(1,2,4:8,10:15,17:19,24:26)))))

# calculate the strain means
me <- tapply(il10$IL10, il10$Strain, mean)
logme <- tapply(il10$logIL10, il10$Strain, mean)

######################################################################
# plot the data
######################################################################
par(las=1, mfrow=c(1,2))
# Note: mfrow makes it so you get two plots in one graphical device
#       c(1,2) indicates 1 row and 2 columns of plots
stripchart(il10$IL10 ~ il10$Strain, method="jitter", pch=1,
           ylab="Strain",xlab="IL10 response", jitter=0.2)
segments(me, (1:21)-0.3, me, (1:21)+0.3, lwd=2, col="blue")
abline(h=1:21, lty=2, col="gray",lwd=1)
stripchart(il10$logIL10 ~ il10$Strain, method="jitter",pch=1,jitter=0.2,
           ylab="Strain",xlab=expression(paste(log[10], " IL10 response")))
segments(logme, (1:21)-0.3, logme, (1:21)+0.3, lwd=2, col="blue")
abline(h=1:21, lty=2, col="gray",lwd=1)
# Note: segments adds line segments at the strain means
#       abline adds horizontal lines to indicate the groups
#       expression allows you to put fancy things in the axis labels

######################################################################
# Analysis of variance tables
######################################################################
aov.il10 <- aov(IL10 ~ Strain, data=il10)
anova(aov.il10)

aov.logil10 <- aov(logIL10 ~ Strain, data=il10)
anova(aov.logil10)

######################################################################
# Plot the residuals
######################################################################
# Note: you can get the residuals and fitted values from
#       the output of the aov() function.
par(las=1, mfrow=c(1,2), lwd=2) # Note: lwd=2 makes the lines thicker
stripchart(aov.il10$residuals ~ il10$Strain, method="jitter", pch=1,
           ylab="Strain",xlab="residuals (IL10)", jitter=0.2)
abline(h=1:21, lty=2, col="gray",lwd=1)
abline(v=0,lty=2,col="green",lwd=1)
stripchart(aov.logil10$residuals ~ il10$Strain, method="jitter",pch=1,jitter=0.2,
           ylab="Strain",xlab=expression(paste("residuals (", log[10], " IL10)")))
abline(h=1:21, lty=2, col="gray",lwd=1)
abline(v=0,lty=2,col="green",lwd=1)

######################################################################
# QQ plots for 6 of the 21 strains
######################################################################
wh <- c("A","B6","2","4","8","12")
# ordinary scale
par(las=1,mfrow=c(2,3))
for(i in wh) {
  qqnorm(il10$IL10[il10$Strain==i],main="")
  mtext(paste("Strain", i), side=3, line=1, cex=1.5)
  qqline(il10$IL10[il10$Strain==i],col="blue",lty=2,lwd=1)
}
  
# log10 scale
par(las=1,mfrow=c(2,3))
for(i in wh) {
  qqnorm(il10$logIL10[il10$Strain==i],main="")
  mtext(paste("Strain", i), side=3, line=1, cex=1.5)
  qqline(il10$logIL10[il10$Strain==i],col="blue",lty=2,lwd=1)
}

######################################################################
# QQ plots of all residuals, with histograms
######################################################################
par(las=1, mfcol=c(2,2), lwd=2)  
hist(aov.il10$residuals, breaks=30, yaxt="n", ylab="", main="", xlab="Residuals")
mtext("IL10", side=3, line=1, cex=1.5)
qqnorm(aov.il10$residuals, main="")
qqline(aov.il10$residuals,col="blue",lty=2,lwd=1)
hist(aov.logil10$residuals, breaks=30, yaxt="n", ylab="", main="", xlab="Residuals")
mtext(expression(paste(log[10], " IL10")), side=3, line=1, cex=1.5)
qqnorm(aov.logil10$residuals, main="")
qqline(aov.logil10$residuals,col="blue",lty=2,lwd=1)
# Note: mfcol vs mfrow leads you to fill up the different plots working
#           down columns rather than across rows.
#       qqline adds a line to the qqnorm plot
#       mtext adds text in the margins of the plot

######################################################################
# Residuals vs fitted values
######################################################################
# plot residuals vs fitted
par(las=1, mfrow=c(1,2), lwd=2)
plot(aov.il10$fitted, aov.il10$residuals, pch=1,
     xlab="fitted values (IL10)",ylab="residuals (IL10)")
abline(h=0, lty=2, col="green",lwd=1)
plot(aov.logil10$fitted, aov.logil10$residuals, pch=1,
     xlab=expression(paste("fitted values (", log[10], " IL10)")),
     ylab=expression(paste("residuals (", log[10], " IL10)")))
abline(h=0,lty=2,col="green",lwd=1)

######################################################################
# plot SDs vs means
######################################################################
# calculate the within-strain SDs
s <- tapply(il10$IL10,il10$Strain, sd)  
logs <- tapply(il10$logIL10,il10$Strain, sd)

# make the plot
par(las=1, mfrow=c(1,2), lwd=2)
plot(me, s, pch=1, xlab="Mean (IL10)", ylab="SD (IL10)")
plot(logme, logs, pch=1, xlab=expression(paste("Mean (", log[10], " IL10)")),
     ylab=expression(paste("SD (", log[10], " IL10)")))

######################################################################
# Bartlett's test for equality of variances
######################################################################
# the simple way, using the built-in function
bartlett.test(il10$IL10 ~ il10$Strain) # p-value = 3 x 10^-8

bartlett.test(il10$logIL10 ~ il10$Strain) # p-value = 0.045

# The brute-force method: IL10
s <- tapply(il10$IL10/1000, il10$Strain, sd)
n <- table(il10$Strain)
spsq <- sum((n-1)*s^2)/sum(n-1)

xsq <- (sum(n)-length(n)) * log(spsq) - sum((n-1)*log(s^2))
K <- 1 + (sum(1/(n-1)) - 1/sum(n-1))/(3*(length(n)-1))

stat <- xsq/K
1 - pchisq(stat, length(n) - 1)

# brute-force method: log10(IL10)
logs <- tapply(il10$logIL10, il10$Strain, sd)
logspsq <- sum((n-1)*logs^2)/sum(n-1)
logxsq <- (sum(n)-length(n)) * log(logspsq) - sum((n-1)*log(logs^2))
logstat <- logxsq/K
1 - pchisq(logstat, length(n) - 1)

##################
# End of comp28.R
##################
