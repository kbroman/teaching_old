######################################################################
# Computer notes, Lecture 37
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
url.show("http://www.biostat.wisc.edu/~kbroman/teaching/stat371/comp37.R")
######################################################################
##############################
# Michaelis-Menten example
##############################
puro <- read.csv("http://www.biostat.wisc.edu/~kbroman/teaching/stat371/puromycin.csv")

# Plot the data (just for the untreated cells)
plot(vel ~ conc, data=puro, subset=(state=="untreated"),
     xlab="concentration",ylab="initial velocity", lwd=2)

# Regression 1/V on 1/C, just with the data on untreated cells
temp <- lm(1/vel ~ I(1/conc), data=puro, subset=(state=="untreated"))
vmax <- 1/temp$coef[1]
km <- temp$coef[2]*vmax

# Plot the data, with the regression line
plot(I(1/vel) ~ I(1/conc), data=puro, subset=(state=="untreated"),
     xlab="1 / concentration",ylab="1 / initial velocity", lwd=2)
abline(temp$coef, col="blue",lty=2,lwd=2)

# The non-linear fit
nls.out <- nls(vel ~ (Vm * conc) / (K + conc),
               data=puro, subset=(state=="untreated"),
               start = c(Vm=143, K=0.031))
summary(nls.out)$param

# Plot the data, with both fitted curves
plot(vel ~ conc, data=puro, subset=(state=="untreated"),
     xlab="concentration",ylab="initial velocity", lwd=2)
u <- par("usr")
x <- seq(u[1], u[2], length=250)
y1 <- 1/predict(temp, data.frame(conc=x))
y2 <- predict(nls.out, data.frame(conc=x))
lines(x,y2, lwd=2, col="blue")
lines(x,y1, lwd=2, col="red", lty=2)

# Nonlinear regression estimation for just the treated cells
nls.outB <- nls(vel~(Vm*conc)/(K+conc), data=puro,
                subset=(puro$state=="treated"),
                start=c(Vm=160, K=0.048))
summary(nls.outB)$param

# Fit both regressions all in one
puro$x <- 2-as.numeric(puro$state) # 0/1 = untreated/treated
nls.outC <- nls(vel ~ ((Vm +dV * x) * conc)/(K + dK*x + conc),
                data=puro,
                start=c(Vm=160, K=0.048, dV=0, dK=0))
summary(nls.outC)$param

# Fit with K's constrained to be equal
nls.outD <- nls(vel ~ ((Vm +dV * x) * conc)/(K + conc),
                data=puro,
                start=c(Vm=160, K=0.048, dV=0))
summary(nls.outD)$param

# Plot data and both fits
plot(vel ~ conc, data=puro, xlab="concentration", lwd=2,
     ylab="initial velocity",
     pch=as.numeric(puro$state)-1,
     col=ifelse(puro$state=="treated","blue","red"))
u <- par("usr")
x <- seq(u[1],u[2],len=250)
y1 <- predict(nls.out, data.frame(conc=x))
y2 <- predict(nls.outB, data.frame(conc=x))
y3 <- predict(nls.outD, data.frame(conc=x, x=rep(0,length(x))))
y4 <- predict(nls.outD, data.frame(conc=x, x=rep(1,length(x))))
lines(x,y1,lwd=2,col="red",lty=2)
lines(x,y2,lwd=2,col="blue",lty=2)
lines(x,y3,lwd=2,col="red")
lines(x,y4,lwd=2,col="blue")

##############################
# From last time...
##############################
sed <- read.csv("http://www.biostat.wisc.edu/~kbroman/teaching/stat371/sediment.csv",
                comment.char="#")

# fit cubic model: Need to use I() to get the ^2 and ^3 to work
lm.out <- lm(pellets ~ time + I(time^2) + I(time^3), data=sed)

# fit "linear spline" model
f <- function(x,b0,b1,x0)
       ifelse(x<x0, b0+b1*x, b0+b1*x0)
nls.out <- nls(pellets ~ f(time,b0,b1,x0), data=sed,
               start=c(b0=200,b1=100,x0=30))
summary(nls.out)$param

# plot data and fitted curves
plot(pellets ~ time, data=sed)
u <- par("usr") # get x-axis limits in the plot
x <- seq(u[1], u[2], length=250)
y <- predict(lm.out, data.frame(time=x))
lines(x, y, col="blue", lwd=2)
y2 <- predict(nls.out, data.frame(time=x))
lines(x,y2,col="red",lwd=2)

##############################
# The final example
##############################
# The data:
cl <- read.csv("http://www.biostat.wisc.edu/~kbroman/teaching/stat371/chlorine.csv",
               comment.char="#")

# Fit the model
nls.out <- nls(chlorine ~ a + b*exp(-c*time), data=cl,
               start=c(a=0.05, b=0.49, c=0.1))
summary(nls.out)$param


# Plot the data and the fitted curve
plot(chlorine ~ time, data=cl, lwd=2)
u <- par("usr")
x <- seq(u[1], u[2], len=250)
y <- predict(nls.out, data.frame(time=x))
lines(x, y, lwd=2, col="blue")
text(32.5,0.47,expression(y == a + b * e^{-c*t} + epsilon),col="blue",
     cex=1.5)

##################
# End of comp37.R
##################
