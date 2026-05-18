######################################################################
# Computer notes             Statistics for Laboratory Scientists (2)
# Lecture 21                                 Johns Hopkins University 
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
# Spider mites example
##############################
# Data available at:
#  http://www.biostat.jhsph.edu/~kbroman/teaching/labstat/fourth/spiders.csv
spiders <- read.csv("spiders.csv")

# Fit the logistic regression model
glm.out <- glm(n.dead/n ~ dose, data=spiders, weights=n,
               family=binomial(link=logit))
summary(glm.out)$coef

# Plot the data with the fitted curve
par(las=1)
plot(n.dead/n ~ dose, data=spiders,
     lwd=2, ylim=c(0,1), ylab="proportion dead")
u <- par("usr")
x <- seq(u[1],u[2],len=250)
y <- predict(glm.out, data.frame(dose=x), type="response")
lines(x,y,col="blue",lwd=2)

# LD50
co <- glm.out$coef
ld50 <- -co[1]/co[2]
glm.sum <- summary(glm.out)
se.co <- glm.sum$coef[,2]
cov.co <- glm.sum$cov.scaled[1,2]
se.ld50 <- abs(ld50) * sqrt( (se.co[1]/co[1])^2 + (se.co[2]/co[2])^2 -
                             2*cov.co/(co[1]*co[2]) )
ci.ld50 <- ld50 + c(-1,1) * qnorm(0.975) * se.ld50


##############################
# Worms example
##############################
# Data available at:
#  http://www.biostat.jhsph.edu/~kbroman/teaching/labstat/fourth/worms.csv
worms <- read.csv("worms.csv")

# plot the data
par(las=1)
plot(n.dead/n ~ dose, data=worms, col=ifelse(sex=="male","blue","red"),
     lwd=2, ylim=c(0,1), ylab="proportion dead")
u <- par("usr")
legend(u[2],u[3],c("Male","Female"),pch=1,col=c("blue","red"),xjust=1,yjust=0,
       cex=1.3)

# no sex difference
glm.out <- glm(n.dead/n ~ dose, weights=n, data=worms, 
               family=binomial(link=logit))
summary(glm.out)$coef

# sexes completely different
glm.outB <- glm(n.dead/n ~ sex*dose, weights=n, data=worms,
                family=binomial(link=logit))
summary(glm.outB)$coef

# different slopes but common "intercepts"
glm.outC <- glm(n.dead/n ~ dose + sex:dose, weights=n, data=worms,
                family=binomial(link=logit))
summary(glm.outC)$coef


# plot the data with fitted curves
par(las=1)
plot(n.dead/n ~ dose, data=worms, col=ifelse(sex=="male","blue","red"),
     lwd=2, ylim=c(0,1), ylab="proportion dead")
u <- par("usr")
x <- seq(u[1],u[2],len=250)
y <- predict(glm.out, data.frame(dose=x), type="response")
lines(x,y,col="green",lwd=2)
ym <- predict(glm.outB, data.frame(dose=x,sex=factor(rep("male",length(x)))),
              type="response")
lines(x,ym,col="blue",lwd=2, lty=2)
yf <- predict(glm.outB, data.frame(dose=x,sex=factor(rep("female",length(x)))),
              type="response")
lines(x,yf,col="red",lwd=2, lty=2)
ym <- predict(glm.outC, data.frame(dose=x,sex=factor(rep("male",length(x)))),
              type="response")
lines(x,ym,col="blue",lwd=2)
yf <- predict(glm.outC, data.frame(dose=x,sex=factor(rep("female",length(x)))),
              type="response")
lines(x,yf,col="red",lwd=2)
legend(u[2], u[3], c("Common intercept","Separate intercepts","Same curve"),lwd=2,
       col=c("blue","blue","green"),lty=c(1,2,1),xjust=1,yjust=0,cex=1.3)

# convert dose to log2(dose)
worms$dose <- log2(worms$dose)

# no sex difference
glm.out <- glm(n.dead/n ~ dose, weights=n, data=worms, 
               family=binomial(link=logit))
summary(glm.out)$coef

# sexes completely different
glm.outB <- glm(n.dead/n ~ sex*dose, weights=n, data=worms,
                family=binomial(link=logit))
summary(glm.outB)$coef

# different slopes but common "intercepts"
glm.outC <- glm(n.dead/n ~ dose + sex:dose, weights=n, data=worms,
                family=binomial(link=logit))
summary(glm.outC)$coef

# plot the data with fitted curves
par(las=1)
plot(n.dead/n ~ dose, data=worms, col=ifelse(sex=="male","blue","red"),
     lwd=2, ylim=c(0,1), ylab="proportion dead",
     xlab=expression(paste(log[2]," dose")))
u <- par("usr")
x <- seq(u[1],u[2],len=250)
y <- predict(glm.out, data.frame(dose=x), type="response")
lines(x,y,col="green",lwd=2)
ym <- predict(glm.outB, data.frame(dose=x,sex=factor(rep("male",length(x)))),
              type="response")
lines(x,ym,col="blue",lwd=2, lty=2)
yf <- predict(glm.outB, data.frame(dose=x,sex=factor(rep("female",length(x)))),
              type="response")
lines(x,yf,col="red",lwd=2, lty=2)
ym <- predict(glm.outC, data.frame(dose=x,sex=factor(rep("male",length(x)))),
              type="response")
lines(x,ym,col="blue",lwd=2)
yf <- predict(glm.outC, data.frame(dose=x,sex=factor(rep("female",length(x)))),
              type="response")
lines(x,yf,col="red",lwd=2)
legend(u[2], u[3], c("Common intercept","Separate intercepts","Same curve"),lwd=2,
       col=c("blue","blue","green"),lty=c(1,2,1), xjust=1,yjust=0,cex=1.3)

##################
# End of comp21.R
##################
