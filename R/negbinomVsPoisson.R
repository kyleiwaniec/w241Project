library(foreign); library(stats); library(ggplot2); library(MASS)

dat <- read.dta("http://www.ats.ucla.edu/stat/stata/dae/nb_data.dta")

dat <- within(dat, {
    prog <- factor(prog, levels = 1:3, labels = c("General", "Academic", "Vocational"))
    id <- factor(id)
})

hist(dat$math)
hist(dat$daysabs)

ggplot(dat, aes(daysabs, fill = prog)) +
	geom_histogram(binwidth = 1) +
	facet_grid(prog ~ ., scales = "free")
	

poisson.model <- glm(daysabs ~ math + prog, family = "poisson", data = dat)
	res.poisson <- resid(poisson.model)
	predict.poisson <- predict(poisson.model)
	pois <- data.frame(
		"resid" = res.poisson, 
		"predict" = predict.poisson,
		"model" = rep("poisson", length(res.poisson)) 
		)
nb.model <- glm.nb(daysabs ~ math + prog, data = dat)
	res.nb <- resid(nb.model)
	predict.nb <- predict(nb.model)
	nb <- data.frame(
		"resid" = res.nb,
		"predict" = predict.nb, 
		"model" = rep("nb", length(res.nb)) 
		)

plot.data <- rbind(pois,nb)

p <- ggplot(aes(x = predict, y = resid, group = as.factor(model)), data = plot.data) + 
		geom_point(aes(colour = as.factor(model), shape = as.factor(model)), alpha = .5) + 
		stat_smooth(aes(colour = as.factor(model), group = as.factor(model)))


###---- Try with some Fake Data ----####
set.seed(2)
n <- 1000

x <- runif(n, min = .5, max = 1)
for(i in 1:n){
	y[i] <- rnbinom(1,1,x[i])
	}

poisson.model <- glm(y ~ x, family = "poisson")
	res.poisson <- resid(poisson.model)
	predict.poisson <- predict(poisson.model)
	pois <- data.frame(
		"resid" = res.poisson, 
		"predict" = predict.poisson,
		"model" = rep("poisson", length(res.poisson)) 
		)
nb.model <- glm.nb(y~x)
	res.nb <- resid(nb.model)
	predict.nb <- predict(nb.model)
	nb <- data.frame(
		"resid" = res.nb,
		"predict" = predict.nb, 
		"model" = rep("nb", length(res.nb)) 
		)


plot.data <- rbind(pois,nb)

p <- ggplot(aes(x = predict, y = resid, group = as.factor(model)), data = plot.data) + 
		geom_point(aes(colour = as.factor(model), shape = as.factor(model)), alpha = .5) + 
		stat_smooth(aes(colour = as.factor(model), group = as.factor(model)))

cbind(mean(y),var(y))