######################################################################
# Computer notes, Lecture 25
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
url.show("http://www.biostat.wisc.edu/~kbroman/teaching/stat371/comp25.R")
######################################################################
# Example regarding comparison of two population variances
A <- c(672, 747, 749, 792, 875, 888, 930, 962, 994,1295)
B <- c(290, 359, 384, 466, 510, 516, 522, 532, 595, 706)

# ratio of sample variances
print(Fstat <- var(A)/var(B))

# test for difference of two variances
var.test(A,B)

# direction calculation of P-value
2*( 1-pf(Fstat, length(A)-1, length(B)-1) )

######################################################################
# ANOVA example
######################################################################
coag <- data.frame(ttt=factor(rep(LETTERS[1:4],c(4,6,6,8)), levels=LETTERS[1:4]),
                   resp=c(c(62,60,63,59), c(63,67,71,64,65,66),
                     c(68,66,71,67,68,68), c(56,62,60,61,63,64,63,59)))

# a plot
stripchart(resp~ttt, data=coag, pch=1, method="jitter")

# add vertical lines at the means
me <- tapply(coag$resp, coag$ttt, mean)
segments(me, 1:4-0.2, me, 1:4+0.2, lwd=3, col="blue")

# anova table
anova(aov(resp ~ ttt, data=coag))

# another way to do it
anova(aov(coag$resp ~ coag$ttt))

##################
# End of comp25.R
##################
