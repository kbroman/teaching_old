######################################################################
# Computer notes, Lecture 34
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
url.show("https://kbroman.org/teaching_old/stat371/comp34.R")
######################################################################

###############################################
# pf3d7 plasmodium data
###############################################

# read the data
h2o2 <- c(0,0,0,10,10,10,25,25,25,50,50,50)
od <- c(0.3399,0.3563,0.3538,0.3168,0.3054,0.3174,
        0.2460,0.2618,0.2848,0.1535,0.1613,0.1525)

# make it a data frame
pf3d7 <- data.frame(h2o2=h2o2, od=od)

# the regression of od (optical density) on h2o2 (hydrogen
#     peroxide concentration)
lm.out <- lm(od ~ h2o2, data=pf3d7)

# summary table: the key bit is the table with
#                the est'd coefficients, SEs, and P-values
lm.sum <- summary(lm.out)

# just the table of coefficients
lm.sum$coef

# the residual SD
lm.sum$sigma

# 95% confidence intervals for the true intercept and slope
df <- lm.out$df
tmult <- qt(0.975, df)  # multiplier from the t distribution
beta <- lm.sum$coef[,1] # estimated coefficients
se <- lm.sum$coef[,2]   # estimated SEs

# 95% CI for intercept
beta[1] + c(-1,1)*tmult*se[1]

# 95% CI for slope
beta[2] + c(-1,1)*tmult*se[2]



######################################################################
# estimated SE of ratio (complicated stuff at the end of the lecture)
######################################################################
# read the data
h2o2 <- c(0,0,0,10,10,10,25,25,25,50,50,50)
od <- c(0.3399,0.3563,0.3538,0.3168,0.3054,0.3174,
        0.2460,0.2618,0.2848,0.1535,0.1613,0.1525)

# function to do the work
estratio <-
function(x,y)
{  
  lm.sum <- summary(lm(y~x))

  # coefficients
  betahat <- lm.sum$coef[,1]

  # estimated ratio
  ratio <- betahat[2]/betahat[1]

  # SEs
  ses <- lm.sum$coef[,2]

  # covariance between estimated coefficients
  covar <- lm.sum$cov.unscaled[1,2]*lm.sum$sigma^2

  # SE of ratio
  se <- abs(ratio) * sqrt( sum( (ses/betahat)^2 + 2*covar/prod(betahat)) )

  print(betahat)
  print(ses)
  print(covar)

  c(ratio=ratio, se=se)
}

estratio(h2o2, od)*100


##################
# End of comp34.R
##################
