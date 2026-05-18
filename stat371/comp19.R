######################################################################
# Computer notes, Lecture 19
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
url.show("https://kbroman.org/teaching/stat371/comp19.R")
######################################################################

##############################
# Tick example 1
##############################
# Suppose X ~ binomial(n=29, p) and we wish to test H0: p=1/2
# Our observed data is X = 24.

# The easy way:
binom.test(24, 29)

# The drawn-out way:
# Lower endpoint of rejection region:
qbinom(0.025, 29, 0.5) # ans = 9
pbinom(9, 29, 0.5) # ans = 0.031 (9 is too big)
pbinom(8, 29, 0.5) # ans = 0.012 (8 is the lower critical value
# 29-8 = 21 is the upper critical value

# Actual significance level = Pr(X <= 8 or X >= 21 | p = 1/2)
pbinom(8, 29, 0.5) + 1 - pbinom(20, 29, 0.5) # ans = 0.024

# P-value for the data X = 24: 2*Pr(X >= 24 | p = 1/2)
2*(1 - pbinom(23, 29, 0.5)) # ans = 0.00055


##############################
# Tick example 2
##############################
# Suppose X ~ binomial(n=25, p) and we wish to test H0: p=1/2
# Our observed data is X = 17.

# The easy way:
binom.test(17, 25)

# The drawn-out way:
# Lower endpoint of rejection region:
qbinom(0.025, 25, 0.5) # ans = 8
pbinom(8, 25, 0.5) # ans = 0.054 (8 is too big)
pbinom(7, 25, 0.5) # ans = 0.021 (7 is the lower critical value
# 25-7 = 18 is the upper critical value

# Actual significance level = Pr(X <= 7 or X >= 18 | p = 1/2)
pbinom(7, 25, 0.5) + 1 - pbinom(17, 25, 0.5) # ans = 0.043

# P-value for the data X = 17: 2*Pr(X >= 17 | p = 1/2)
2*(1 - pbinom(16, 25, 0.5)) # ans = 0.11


##############################
# Tick example 1
##############################
# X ~ binomial(n=29, p)
# Actually observe X = 24
# Want 95% confidence interval for p

# The easy way:
binom.test(24,29) # 95% CI = (0.642, 0.942)

# The drawn-out way:
p <- seq(0, 1, by=0.001) # vector of length 1001
# upper value:
min(p[pbinom(24, 29, p) <= 0.025]) # ans = 0.942

# lower value:
max(p[1-pbinom(23, 29, p) <= 0.025]) # ans = 0.642


##############################
# Tick example 2
##############################
# X ~ binomial(n=25, p)
# Actually observe X = 17
# Want 95% confidence interval for p

# The easy way:
binom.test(17,25) # 95% CI = (0.465, 0.851)

# The drawn-out way:
p <- seq(0, 1, by=0.001) # vector of length 1001
# upper value:
min(p[pbinom(17, 25, p) <= 0.025]) # ans = 0.851

# lower value:
max(p[1-pbinom(16, 25, p) <= 0.025]) # ans = 0.464


##############################
# Case X = 0
##############################
# X ~ binomial(n=15, p)
# Actually observe X = 0
# Want 95% confidence interval for p

# The easy way:
binom.test(0, 15)

# The direct way:
# lower limit = 0
# upper limit:
1-(0.025)^(1/15) # ans = 0.218


##################
# End of comp19.R
##################
