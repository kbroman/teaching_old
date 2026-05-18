######################################################################
# Solutions to Lab 2
######################################################################

# Note: This is a very informal presentation of the solutions to the
#       lab.  This would be a reasonable appendix for the lab, but the
#       main part should be a written report in prose rather than
#       computer code.

######################################################################
# Problem 1(a)
######################################################################
# The data
cancer <- read.csv("cancer.csv")

# Make sure that "type" is a factor
sapply(cancer, is.factor)

# Apply one-way ANOVA
aov.out <- aov(response ~ type, data=cancer)

# Look at diagnostic plots.  
plot(aov.out)

# Note: The variance is not constant, and the residuals are not normal.
#       We'll try taking logs.

# take logs
cancer$response <- log10(cancer$response)

# re-do the ANOVA
aov.out <- aov(response ~ type, data=cancer)

# re-do the diagnostic plots
plot(aov.out)

# Now everything looks fine.  We go ahead and calculate the anova table
anova(aov.out)

# The ANOVA table:
#            Df  Sum Sq Mean Sq F value   Pr(>F)
#  type       4    4.62    1.15    4.29   0.0041
#  Residuals 59   15.89    0.27                 

# Conclusion: There are clear differences between the cancer types.

######################################################################
# Problem 1(b)
######################################################################
ferm <- read.csv("fermentation.csv")

# check that things are factors that should be
sapply(ferm, is.factor)

# make "oxygen" a factor
ferm$oxygen <- factor(ferm$oxygen)

# Do the two-way ANOVA
aov.out <- aov(ethanol ~ oxygen * sugar, data=ferm)

# Do the diagnostic plots
plot(aov.out)

# Again, it looks like there is non-constant variance.  
# I'll take square-roots.
ferm$ethanol <- sqrt(ferm$ethanol)

# Re-do the two-way ANOVA
aov.out <- aov(ethanol ~ oxygen * sugar, data=ferm)

# Re-do the diagnostic plots
plot(aov.out)

# Looks okay...get the ANOVA table
anova(aov.out)

# The ANOVA table:
#               Df  Sum Sq Mean Sq  F value   Pr(>F)
#  oxygen        3  0.19   0.064      3.01    0.094
#  sugar         1  0.40   0.40      18.62    0.0026
#  oxygen:sugar  3  0.0014 0.00047    0.022   0.995
#  Residuals     8  0.17   0.021                 

# Conclusions:
#    - There is a clear sugar effect.
#    - There is some evidence for an oxygen effect, but it is not significant.
#    - There doesn't appear to be an interaction.


######################################################################
# Problem 2
######################################################################
# The data
caffeine <- read.csv("caffeine.csv")

# check whether things are factors that should be
sapply(caffeine, is.factor)

# make the "dose" column a factor
caffeine$dose <- factor(caffeine$dose)

##############################
# (a)
##############################
aov.out <- aov(response ~ dose, data=caffeine)
anova(aov.out)  # P-value = 0.006

# The ANOVA table:
#            Df  Sum Sq Mean Sq  F value   Pr(>F)
#  dose       2    61.4    30.7     6.18   0.0062
#  Residuals 27   134.1     5.0                 

# Conclusion: There really are differences between the doses

##############################
# (b)
##############################
# use the function ci.bonf defined in the notes for lec 11 ("comp11.R")
ci.bonf(caffeine$response, caffeine$dose)

# Result:
#               est  lower  upper
#    0 - 100   -1.6   -3.8    0.6
#    0 - 200   -3.5   -5.7   -1.3
#  100 - 200   -1.9   -4.1    0.3

# Conclusion: The response to the 200mg dose is larger than that to the
#             0mg dose, but there are no significant differences between the
#             0mg and 100mg doses or the 100mg and 200mg doses.

# (c)
# use the function TukeyHSD using the output from aov()
tuk <- TukeyHSD(out)

# Result:
#         diff   lwr   upr
# 100-0    1.6  -0.9   4.1
# 200-0    3.5   1.0   6.0
# 200-100  1.9  -0.6   4.4

# Conclusion: Same as for part (b).

##############################
# end of lab2_solns.R
##############################
