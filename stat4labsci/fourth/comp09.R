######################################################################
# Computer notes             Statistics for Laboratory Scientists (2)
# Lecture 9                                 Johns Hopkins University 
######################################################################
# These notes provide R code related to the course lectures, to
# further illustrate the topics in the lectures and to assist the
# students to learn R.
#
# Lines beginning with the symbol '#' are comments in R.  All other
# lines contain code.
#
# In R for Windows, you may wish to open this file from the menu bar
# (File:Display file); you can then easily copy commands into the
# command window.  (Use the mouse to highlight one or more lines; then
# right-click and select "Paste to console".)
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

##############################
# Calculate Bonferroni-corrected confidence intervals
##############################
# It's a bit complicated, so I've created my own function:
ci.bonf <-
function(response, group, alpha=0.05)
{
  # calculate anova table
  anova.out <- anova(aov(response~group))

  # within-group mean square
  ms <- anova.out[2,3]

  # number of individuals per group 
  n <- tapply(response, group, length)

  # se's: different for each pair if the sample sizes are different
  se <- sqrt(ms * outer(n,n, function(a,b) 1/a + 1/b))
  # pick out lower triangle
  se <- se[lower.tri(se)]

  # multipier from t distribution
  df <- anova.out[2,1]  # degrees of freedom 
  k <- length(n)
  ntests <- choose(k, 2) # number of pairs 
  tmult <- qt(1 - alpha/ntests, df) 

  # calculate the pairwise differences between the sample means
  me <- tapply(response, group, mean)
  diffs <- outer(me,me,"-")
  # pick out the negative of the lower triangle
  d <- -diffs[lower.tri(diffs)]
  # assign names to these
  rn <- rownames(diffs)[row(diffs)[lower.tri(row(diffs))]]
  cn <- colnames(diffs)[col(diffs)[lower.tri(col(diffs))]]
  names(d) <- paste(cn,rn,sep=" - ")

  cbind(est=d, lower=d-tmult*se, upper=d+tmult*se)
}

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
# These are **so** complicated, that I probably shouldn't have
# displayed them here!
newmankeuls <-
function(response, group, alpha=0.05)
{
  na <- as.character(unique(group))
  if(length(grep("\\|", na)))
    stop("the treatment names cannot use the | symbol.")

  # calculate anova table
  anova.out <- anova(aov(response~group))

  # within-group mean square
  ms <- anova.out[2,3]

  # number of individuals per group 
  n <- tapply(response, group, length)

  # group means
  me <- tapply(response,group,mean)

  # sort me and reorder n in the same way
  o <- order(me)
  me <- me[o]
  n <- n[o]

  # se's: different for each pair if the sample sizes are different
  se <- sqrt(ms * outer(n,n, function(a,b) 1/a + 1/b))

  # multipiers from Tukey ("studentized range") distribution
  k <- length(n)
  df <- anova.out[2,1]  # degrees of freedom 
  qmult <- qtukey(1-alpha, 2:k, df)
  # turn into a matrix
  temp <- matrix(ncol=k, nrow=k)
  for(i in 1:length(qmult))
    temp[row(temp) == col(temp)+i | row(temp)+i == col(temp)] <- qmult[i]
  qmult <- temp

  # critical values
  R <- qmult * se / sqrt(2)

  # calculate the pairwise differences between the sample means
  diffs <- outer(me,me,"-")

  # determine which differences are above the corresponding critical values
  compare <- abs(diffs) > R

  # now we figure out which line segments to include
  results <- NULL
  for(i in (k-1):1) {
    wh <- compare[row(compare) == col(compare)+i]
    if(any(!wh)) {
      wh <- (1:length(wh))[!wh]
      cur <- rep("",length(wh))
      for(j in seq(along=wh)) 
        cur[j] <- paste(rownames(compare)[wh[j] + 0:i],collapse="|")
      flag <- rep(0,length(cur))
      if(length(results)>0) {
        for(j in seq(along=wh)) {
          nc <- nchar(cur[j])
          temp1 <- paste(cur[j],"|",sep="")
          temp2 <- paste("|", cur[j],sep="")
          for(k in 1:length(results)) {
            for(s in 1:(nchar(results[k])-nc)) {
              XX <- substr(results[k],s,s+nc)
              if(XX==temp1 || XX==temp2) {
                flag[j] <- 1
              }
            }
          }
        }
        if(any(flag==1)) cur <- cur[flag == 0]
      }
      results <- c(results,cur)
    }
  }
  class(results) <- "newmankeuls"
  attr(results,"means") <- me
  results
}
  
print.newmankeuls <-
function(x, ...)
{
  print(as.character(x))
}

plot.newmankeuls <-
function(x, ...)   
{
  
  par(mar=c(0.1,0.1,0.1,0.1))
  plot(0,0,type="n", bty="n",yaxt="n", xaxt="n", xlim=c(0,100),
       ylim=c(0,100), xlab="", ylab="")

  me <- attr(x,"means")
  me2 <- as.character(me)
  k <- length(me)
  na <- names(me)


  # fix number of digits in me2
  temp <- strsplit(me2, "\\.")
  len <- sapply(temp,length)
  if(any(len>1)) {
    npast <- sapply(temp,function(a)
                    if(length(a)<2) return(0) else return(nchar(a[2])))
    if(length(unique(npast)) > 1) {
      mx <- max(npast)
      for(i in 1:length(me2)) {
        if(npast[i] == 0) me2[i] <- paste(me2[i], ".", sep="")
        if(npast[i] < mx)
          me2[i] <- paste(me2[i], rep("0",mx-npast[i]), sep="")
      }
    }
  }

  # plot means and group names
  X <- seq(0,100,length=k*2+1)[seq(2,k*2,by=2)]
  text(X,80,na,cex=1.3)
  text(X,70,me2, cex=1.3)

  midpts <- (c(0,X)+c(X,100))/2

  temp <- strsplit(x,"\\|")
  temp <- lapply(temp,function(a) a[c(1,length(a))])

  cur <- 60
  for(i in seq(along=temp)) {
    wh <- match(temp[[i]],na)
    segments(midpts[wh[1]],cur,midpts[wh[2]+1],cur,lwd=2)
    cur <- cur - 5
  }
  

}

######################################################################
# Finally, those functions are over with.  Now to the example...
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
# End of comp09.R
##################
