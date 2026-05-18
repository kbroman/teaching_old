
# data
datA <- c(19,28,34,42)
datB <- c(44,54,65,72)
datC <- c(72,68,60,49)

# plot
par(mfrow=c(1,3),las=1)
barplot(datA, col=c("black","gray50","gray75","white"),
        names=c("W","Af Am", "H Am", "As Am"), ylim=c(0,100),
        main="Participate in\n\ncaring for parents",
        ylab="Percentage")
barplot(datB, col=c("black","gray50","gray75","white"),
        names=c("W","Af Am", "H Am", "As Am"), ylim=c(0,100),
        main="Feel they should\n\ndo more",
        ylab="Percentage")
barplot(datC, col=c("black","gray50","gray75","white"),
        names=c("W","Af Am", "H Am", "As Am"), ylim=c(0,100),
        main="Do not expect care\n\nfrom their children",
        ylab="Percentage")


