# TODO:
# - "manipulation check - what did we do to ensure that people were receiving treatment? (wk 9 ?)"
# - "effect size - "how small an effect could we have detected if one was there"?"

library(data.table)
library(MASS)

d = fread('/Users/koza/Documents/UCBerkeley/241/w241Project/R/daniel_data.csv')
head(d)
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



# rtotal visualization
hist(d$rtotal)
hist(log(d$rtotal))
boxplot(d$rtotal ~ d$treatment)

# try randomized inference for rtotal ~ treatment
est.ate <- function(outcome, treat) { 
  mean(outcome[treat==1]) - mean(outcome[treat==0])
} 
ate = est.ate(d$rtotal, d$treatment)
randomize <- function(num.control, num.treat){
  sample(c(rep(0,num.control),rep(1,num.treat)))
}
distribution.under.sharp.null = replicate(10000, est.ate(d$rtotal, randomize(50,50)))
p = mean(ate < distribution.under.sharp.null)
p
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

