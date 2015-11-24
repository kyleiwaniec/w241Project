library(data.table)

posters = c(1,2,3,4,5) # 1:Kyle 2:Raja 3:Daniel 4:Jonathan 5:Umber
days = c(1,2,3,4) # mapping to real days TBD

### simulate data
n = 100
# create data table with id column as 1,2,3,...,n and pairid column as 1,1,2,2,3,3,...,n/2,n/2
d = data.table(id = seq(1,n), pairid = as.integer((seq(1,n) + 1) / 2))
# assign treatment to all the odd rows
d$treatment = rep(c(1,0), n / 2)

### assign poster for first (treatment==1) in each pair
# create vector replicating enough posters evenly for half the sample
posters.tosamp =rep(posters, n / length(posters) / 2)
# sample from replicated posters (without replacement so we have complete randomization)
d$poster[d$treatment==1] = sample(posters.tosamp)

### assign poster for second (treatment==0) in each pair
# do the same as we did for the first in each pair, then check to see if
# we have duplicate pairid/poster rows, i.e. the same poster posting both
# ads in one pair. If so, draw again.
# (This is a hack, and is only feasible with a relatively small n. Even 100 is pushing it.)
hasdup = T
while(hasdup){
  d$poster[d$treatment==0] = sample(posters.tosamp)
  hasdup = sum(duplicated(d, by=c("pairid", "poster"))) > 0
}

### assign day
# create vector replicating enough days for each poster
days.tosamp = rep(days, n / length(days) / length(posters))
for(p in posters){
  # for each poster, sample from replicated days (without replacement so we have complete randomization)
  d$day[d$poster==p] = sample(days.tosamp)
}