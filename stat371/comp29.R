######################################################################
# Computer notes, Lecture 29
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
url.show("https://kbroman.org/teaching_old/stat371/comp29.R")
######################################################################

##############################
# sugar example
##############################
# the data:
x <- c(75,67,70,75,65,71,67,67,76,68,
       57,58,60,59,62,60,60,57,59,61,
       58,61,56,58,57,56,61,60,57,58,
       58,59,58,61,57,56,58,57,57,59,
       62,66,65,63,64,62,65,65,62,67)
treat <- factor(rep(c("C","G","F","G+F","S"),rep(10,5)))
sugar <- data.frame(rsp=x, ttt=treat)

# group sample means
me <- tapply(sugar$rsp, sugar$ttt, mean)

# Make a picture
par(las=1)
stripchart(sugar$rsp ~ sugar$ttt, method="jitter", pch=1)
segments(me, 1:5-0.1, me, 1:5+0.2, lwd=2,col="blue")

# anova table
aov.out <- aov(rsp ~ ttt, data=sugar)
anova.out <- anova(aov.out)

######################################################################
# Calculating Bonferroni-corrected confidence intervals
######################################################################
# this is a complicated, so I wrote a function, placed in the file
# func29.R; load it as follows:
source("https://kbroman.org/teaching_old/stat371/func29.R")

# calculate the bonferroni confidence intervals for the sugar data
sugar.bonf <- ci.bonf(sugar$rsp, sugar$ttt)

# plot the results
k <- nrow(sugar.bonf)
par(mar=c(5.1,5.1,3.1,0.6))
plot(0,0,type="n", xlab="Difference in response", ylab="", yaxt="n",
     ylim=c(0.5, k+0.5), xlim=range(sugar.bonf),main="Bonferroni CIs")
abline(v=0,lty=2,col="blue")
segments(sugar.bonf[,2], 1:k, sugar.bonf[,3], 1:k, lwd=2)
segments(sugar.bonf[,1], 1:k - 0.2, sugar.bonf[,1], 1:k + 0.2, lwd=2)
segments(sugar.bonf[,2], 1:k - 0.1, sugar.bonf[,2], 1:k + 0.1, lwd=2)
segments(sugar.bonf[,3], 1:k - 0.1, sugar.bonf[,3], 1:k + 0.1, lwd=2)
u <- par("usr")
segments(u[1], 1:k, u[1]-diff(u[1:2])*0.02, 1:k, xpd=TRUE)
text(u[1]-diff(u[1:2])*0.03, 1:k, rownames(sugar.bonf), xpd=TRUE, adj=1)


##############################
# Tukey HSD confidence intervals
##############################
# the easy way: use the function TukeyHSD(), with the output from aov()
sugar.tuk <- TukeyHSD(aov.out)

# the easy way to plot these
plot(sugar.tuk)


##############################
# Newman-Keuls procedure
##############################
# This is also complicated and tricky; I wrote a function.
# I also wrote functions to "print" and "plot" the results.
# 
# These are **so** complicated, that I definitely don't want to display them
# here.  Load them as follows:
source("https://kbroman.org/teaching_old/stat371/func29.R")

######################################################################
# Now to the example...
######################################################################
# use the functions to apply the Newman-Keuls method for these data
out <- newmankeuls(sugar$rsp, sugar$ttt)

# plot the results
plot(out)

######################################################################
# The blood chemistry in rats example from the text (Ex 11.28, pg 409)
######################################################################
# The data (this is actually made up, to give values corresponding
#           exactly to the means and ANOVA table in the text)
x <-  c(39.51700, 39.69400, 42.18900, 38.60000,
        38.68825, 37.77725, 39.27825, 47.05625,
        33.80550, 42.56650, 31.71650, 23.51150,
        28.81225, 31.22925, 27.83625, 30.52225,
        47.13900, 55.27000, 44.15600, 48.63500)
grp <- factor(rep(LETTERS[1:5],rep(4,5)))
rats <- data.frame(rsp=x, ttt=grp)

# the anova table
anova(aov(rsp ~ ttt, data=rats))

# the newman-keuls procedure
out <- newmankeuls(rats$rsp, rats$ttt)

# plot the result
plot(out)

##################
# End of comp29.R
##################
