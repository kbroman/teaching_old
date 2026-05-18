######################################################################
# Computer notes             Statistics for Laboratory Scientists (2)
# Lecture 22                                 Johns Hopkins University 
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

###############################################
# Leukemia example
###############################################

# load the "survival" package
library(survival)

# get access to the data
data(leukemia)

# add a column with survival time treated appropriately
leukemia$survival <- Surv(leukemia$time, leukemia$status)


##############################
# Just look at the "maintained" group
##############################
maint <- subset(leukemia, x == "Maintained")

# Kaplan-Meier estimate of survival function
surv.km <- survfit(survival ~ 1, data=maint)
summary(surv.km)
plot(surv.km)

##############################
# Second example, for log-rank test
##############################
y <-   c(3,5,7,9,18,  12,19,20,20,33)
d <-   c(1,1,1,0, 1,   1, 1, 1, 0, 0)
grp <- c(0,0,0,0, 0,   1, 1, 1, 1, 1)
time <- Surv(y,d)

# log-rank test
survdiff(time ~ grp)



##############################
# Back to first example
##############################
# fit survival curves
surv.km <- survfit(survival ~ x, data=leukemia)
summary(surv.km)

# plot Kaplan-Meier estimate for both groups
plot(surv.km,col=c("red","blue"),lwd=2)

# that for 2nd group, with confidence limits
plot(surv.km[2],conf.int=TRUE)

# both groups, with confidence limits
plot(surv.km,conf.int=TRUE, col=c("red","blue"), lwd=2)

# log-rank test to compare survival curves
survdiff(survival ~ x, data=leukemia)

# Cox proportional hazards model
surv.cph <- coxph(survival ~ x, data=leukemia)
summary(surv.cph)

##################
# End of comp22.R
##################

