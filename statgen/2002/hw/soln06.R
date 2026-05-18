######################################################################
# Biostat 140.668
#
# Solution to Problem Set 6
#
# Karl W Broman, 3 Dec 2002
#
######################################################################

######################################################################
#
# Probability that a particular autosomal locus is fixed after n
# generations of sibling matings.
#
######################################################################

# equivalence classes
nam <- c("fixed","A:B","H:H","A:H")

# transition matrix
A <- rbind(c(1,0,0,0), c(0,0,1,0),
           c(1/8,1/8,1/4,1/2), c(1/4,0,1/4,1/2))
dimnames(A) <- list(nam, nam)

pi <- matrix(ncol=4,nrow=26)
dimnames(pi) <- list(paste(0:25), nam)

pi[1,] <- c(0,1,0,0) # frequencies at generation 0
for(i in 2:26) pi[i,] <- pi[i-1,] %*% A

# plot prob'y fixed
plot(1:25, pi[-1,1], xlab="Generation", ylab="Probability fixed",las=1,
     lwd=2, main="Autosomal locus")



######################################################################
#
# Probability that a particular X-linked locus is fixed after n
# generations of sibling matings.
#
######################################################################

# At generation 0, the female is AA and the male is BY

# At each generation, the genotypes of the female and male can be
#      1: AA/AY or BB/BY (fixed)
#      2: AB/AY or AB/BY
#      3: AA/BY or BB/AY

namX <- c("AA:AY", "AB:AY", "AA:BY")

# transition matrix
AX <- rbind(c(1,0,0), c(1/4,1/2,1/4), c(0,1,0))
dimnames(AX) <- list(namX, namX)

piX <- matrix(ncol=3,nrow=26)
dimnames(piX) <- list(paste(0:25), namX)

piX[1,] <- c(0,0,1) # frequencies at generation 0
for(i in 2:26) piX[i,] <- piX[i-1,] %*% AX

# plot prob'y fixed
plot(1:25, piX[-1,1], xlab="Generation", ylab="Probability fixed",las=1,
     lwd=2, main="X-linked locus")


# Plot both
plot(1:25, pi[-1,1], xlab="Generation", ylab="Probability fixed",las=1,
     lwd=2)
points(1:25, piX[-1,1], lwd=2, col="blue")
legend(18, 0.4, c("Autosomal", "X-linked"), pch=1, col=c("black","blue"))
