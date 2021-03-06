# TODO:
# - "manipulation check - what did we do to ensure that people were receiving treatment? (wk 9 ?)"
# - "effect size - "how small an effect could we have detected if one was there"?"

library(data.table)
library(MASS)

# insert your own path:
d = fread('/Users/koza/Documents/UCBerkeley/241/w241Project/data/data_sans_population.csv')

# data manipulation
d[city==1, population := 2.1]
d[city==2, population := 7]
d[city==3, population := 3.25]
d[city==4, population := 8.4]
d[city==5, population := 18.55]

d$avgoffer = as.numeric(d$avgoffer)

d$city = factor(d$city)

# descriptive statistics - randomization
sum(duplicated(d, by=c("pairid", "city")))  # each pair split across 2 cities (0 by design)
d[,mean(treatment),by=city]                 # treatment/control split by city (50% for each city by design)
d[,mean(treatment),by=day]                  # treatment/control split by day (random)
table(d$day, d$author)                      # author distribution by day (random)
table(d$city, d$author)                     # author distribution by city (random)

# descriptive statistics - observations
mean(d$rtotal)                              # mean responses
mean(d$roffer)                              # mean responses with offer
nrow(d[rtotal > 0])                         # number of ads that received responses
sum(d[, .(pairsum = sum(rtotal) > 0), by=pairid]$pairsum)
                                            # number of pairs that received responses
sum(d[, .(pairsum = sum(roffer) > 0), by=pairid]$pairsum)
                                            # number of pairs that received offers



# rtotal visualization
hist(d$rtotal)
hist(log(d$rtotal))
boxplot(d$rtotal ~ d$treatment)

# try randomized inference for rtotal ~ treatment
est.ate <- function(outcome, treat) { 
  mean(outcome[treat==1]) - mean(outcome[treat==0])
} 
ate = est.ate(d$rtotal, d$treatment)
# Made 2 mistakes first time we did RI. Turns out they cancelled each other out and we got about the right answer...
# Mistake 1. the first randomize function we used produces a random sequence of 1s and 0s with 50 of each, but that's not what
# we want. We want to "block" on the matched features, which means assigning one member of
# each pair to treatment and the other to control.
#randomize <- function(num.control, num.treat){
#  sample(c(rep(0,num.control),rep(1,num.treat)))
#}
# this randomize function does what we want
randomize <- function(num.control, num.treat){
  first = sample(c(rep(0, num.control / 2),rep(1, num.treat / 2)))
  second = 1 - first
  c(rbind(first, second))
}
distribution.under.sharp.null = replicate(10000, est.ate(d$rtotal, randomize(50,50)))
# Mistake 2. We took the 1-tailed p-value, which is not consistent with our regression analysis
#p = mean(ate < distribution.under.sharp.null)
# Here's the 2-tailed version
p = mean(abs(ate) < abs(distribution.under.sharp.null))
p

plot(density(distribution.under.sharp.null))# the first 
abline(v=ate)

# 

# rtotal regression...
summary(lm(rtotal ~ treatment, data=d))
summary(lm(rtotal ~ treatment + factor(pairid), data=d)) # <-- very close to randomized inference results
summary(lm(log(rtotal+ 1) ~ treatment, data=d))
summary(lm(log(rtotal+ 1) ~ treatment + factor(pairid), data=d))
summary(glm(rtotal ~ treatment, data=d, family = "poisson"))
summary(glm(rtotal ~ treatment + factor(pairid), data=d, family = "poisson"))
summary(glm.nb(rtotal ~ treatment, data=d))
summary(glm.nb(rtotal ~ treatment + factor(pairid), data=d)) # <-- the right one? negative binomial

summary(lm(rtotal ~ treatment + factor(pairid) + population, data=d))
summary(glm.nb(rtotal ~ treatment + factor(pairid) + population, data=d))

summary(lm(rtotal ~ treatment + factor(pairid) + population + factor(city) + factor(day) + author, data=d))
summary(glm.nb(rtotal ~ treatment + factor(pairid) + population + factor(city) + factor(day) + author, data=d))

# Plot the distribution of residuals for OLS regression
ols.stdres = rstandard(lm(rtotal ~ treatment + factor(pairid), data=d))
qqnorm(ols.stdres, 
       ylab="Standardized Residuals", 
       xlab="Normal Scores",
       main=""
       ) 
qqline(ols.stdres)

# Because the residuals are not normally distributed and data are positively skewed, let's
# settle on poisson distribution (neg binomial is getting into territory we don't grok)
summary(glm(rtotal ~ treatment, data=d, family = "poisson"))
summary(glm(rtotal ~ treatment + factor(pairid), data=d, family = "poisson"))
summary(glm(rtotal ~ treatment + factor(pairid) + population, data=d, family = "poisson"))
summary(glm(rtotal ~ treatment + factor(pairid) + population + factor(city) + factor(day) + author, data=d, family = "poisson"))



# roffer visualization
hist(d$roffer)
hist(log(d$roffer))
boxplot(d$roffer ~ d$treatment)

# roffer regression

summary(lm(roffer ~ treatment + factor(pairid), data=d))
summary(glm.nb(roffer ~ treatment + factor(pairid), data=d))

summary(lm(roffer ~ treatment + factor(pairid) + population, data=d))
summary(glm.nb(roffer ~ treatment + factor(pairid) + population, data=d))

summary(lm(roffer ~ treatment + factor(pairid) + population + factor(city) + factor(day) + author, data=d))
summary(glm.nb(roffer ~ treatment + factor(pairid) + population + factor(city) + factor(day) + author, data=d))

# avgoffer visualization

hist(d$avgoffer)
hist(log(d$avgoffer))
boxplot(d$avgoffer ~ d$treatment)

summary(lm(avgoffer ~ treatment, data=d))
summary(lm(avgoffer ~ treatment + factor(pairid), data=d))
summary(lm(avgoffer ~ treatment + factor(pairid) + population, data=d))
summary(lm(avgoffer ~ treatment + factor(pairid) + population + factor(city) + factor(day) + author, data=d))

# Statistical power: 
# true treatment effect/standard error of estimated effect
# Standard error of estimated effect determined by square root of sample size and variation of outcome.

varInOutcome = var(d$rtotal) 
N = nrow(d)
SE_hat = sqrt(varInOutcome/N)

trueTreatEffect = lm(rtotal ~ treatment, data=d)$coefficients[2]
power = SE_hat/trueTreatEffect

# t-test and power calculations
library(pwr)
library(lsr)

t.test(d[treatment==1]$rtotal, d[treatment==0]$rtotal, paired=TRUE)
cohensD(d[treatment==1]$rtotal, d[treatment==0]$rtotal, method="paired")

# this does the same thing but combines it all into a descriptive output
pairedSamplesTTest(rtotal ~ treatment, data=d, id="pairid", one.sided = "treatment")

# cohensD above is 0.15. What is this experiment's power to decect that effect size?
pwr.t.test(n = 50, d = 0.15, sig.level = 0.05, power = NULL, type = "paired")
# What effect size could the experiment detect with 80% power?
pwr.t.test(n = 50, d = NULL, sig.level = 0.05, power = 0.8, type = "paired")
# How many pairs needed to detect a 0.15 effect size with 80% power?
pwr.t.test(n = NULL, d = 0.15, sig.level = 0.05, power = 0.8, type = "paired")


# Variance of rtotal (there's got to be a better way to calculate this)
pair.means = d[, mean(rtotal), by=pairid]$V1
pair.means = c(rbind(pair.means, pair.means))
var(d$rtotal - pair.means)
# ...is actually pretty close to the mean (so poisson is appropriate)
mean(d$rtotal)
