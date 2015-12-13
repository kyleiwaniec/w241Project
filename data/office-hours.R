d <- read.csv("~/Documents/UCBerkeley/241/w241Project/data/Cleaned Data - Final Project New (V5) - Cleaned Data Sort.csv")
names(d)

d$rtotal = d$Total.Response
d$treatment = d$TREATMENT
d$author = d$Author
d$city = d$City
d$day = d$Posting.Day
d$population = d$Population

hist(log(d$Total.Response))
boxplot(d$Total.Response ~ d$TREATMENT)
mod = lm(Total.Response ~ TREATMENT, data=d)
summary(mod)

mod2 = glm(Total.Response ~ TREATMENT + Author + City + Posting.Day, 
           data=d,
           family="poisson"  
           )
summary(mod2)

summary(glm(Total.Response ~ TREATMENT + Population + factor(City) + factor(Posting.Day) + Author, data=d, family = "poisson"))

library(MASS)
summary(glm.nb(rtotal ~ treatment + population + factor(city) + factor(day) + author, data=d))


mod4 = glm(Total.Response ~ offset(log(Total.Response/Population)) + TREATMENT, 
           data = d,
           family=poisson(link=log) )
# log of 0 - boo hoo.

summary(lm(log(rtotal+ 1) ~ treatment + factor(pairid), data=d))


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



#is the observed variance greater than the mean? if so, the problem of overdispersion can be solved by using quasi-likelihood estimation or a negative binomial distribution

observed_mean = mean(d$Total.Response)
observed_variance = var(d$Total.Response)
observed_variance > observed_mean
#TRUE!

# descriptive statistics
sum(duplicated(d, by=c("pairid", "city")))  # each pair split across 2 cities (0 by design)
d[,mean(treatment),by=city]                 # treatment/control split by city (50% for each city by design)
d[,mean(treatment),by=day]                  # treatment/control split by day (random)
table(d$day, d$author)                      # author distribution by day (random)
table(d$city, d$author)                     # author distribution by city (random)

mod1<-lm(Total.Response~TREATMENT,data=subset(d,Author="Jonathan"))
summary(mod1)
## DON'T DO THIS!
mod3 = glm(Total.Response ~ TREATMENT + Author, 
           data=subset(d, Total.Response > 0),
           family="poisson"  
)
summary(mod3)

summary(lm(rtotal ~ treatment, data=d))
summary(lm(rtotal ~ treatment + factor(pairid), data=d))

## negative binomial 
?glm.nb
