library(rethinking)
data(WaffleDivorce)

d <- WaffleDivorce

# plot(Divorce ~ WaffleHouses, data = d)
# text(Divorce ~ WaffleHouses, labels = d$Loc, data = d, pos = 1)

# linreg <- lm(Divorce ~ WaffleHouses, data = d)
# abline(a = linreg$coefficients[1], b = linreg$coefficients[2])

standardize <- function(values) (values - mean(values))/sd(values) 

d$Marriage.s <- standardize(d$Marriage)
d$MedianAgeMarriage.s <- standardize(d$MedianAgeMarriage)

plot(Divorce ~ Marriage.s, data = d)
# mreg <- lm(Divorce ~ Marriage.s, data = d)
# abline(a = mreg$coefficients[1], b = mreg$coefficients[2])


plot(Divorce ~ MedianAgeMarriage.s, data = d)
# areg <- lm(Divorce ~ MedianAgeMarriage.s, data = d)
# abline(a = areg$coefficients[1], b = areg$coefficients[2])

MAM_model <- alist(
	Divorce ~ dnorm(mu, sigma),
	mu <- a + b * MedianAgeMarriage.s,
	a ~ dnorm(10,10),
	b ~ dnorm(0,1),
	sigma ~ dunif(0, 10))

mMAM <- map(MAM_model, data = d)
MAM_samples <- extract.samples(mMAM, 1e4)

# Sample simulated divorce rates with HPDI interval
MAM_range <- seq(from = 20, to = 31, by = .5 )
MAM_range <- sapply(MAM_range, function(val) (val - mean(d$MedianAgeMarriage))/sd(d$MedianAgeMarriage))
MAM_form <- function (age) rnorm(n = nrow(MAM_samples), mean = MAM_samples$a + MAM_samples$b * age, sd = MAM_samples$sigma)
sim_MAM <- sapply(MAM_range, MAM_form)
sim_MAM.mean <- apply(sim_MAM, 2, mean)
sim_MAM.HPDI <- apply(sim_MAM, 2, HPDI)
shade(sim_MAM.HPDI, MAM_range)

# Look at possible MAM-divorce rate mu
MAM_mu <- sapply(MAM_range, function (age) MAM_samples$a + MAM_samples$b * age)
MAM_mu.mean <- apply(MAM_mu, 2, mean)
MAM_mu.HPDI <- apply(MAM_mu, 2, HPDI)
lines(MAM_range, MAM_mu.mean)
shade(MAM_mu.HPDI, MAM_range)

# Model divorce and marriage rates
Marriage_model <- alist(
	Divorce ~ dnorm(mu, sigma),
	mu <- a + b * Marriage.s,
	a ~ dnorm(10,10),
	b ~ dnorm(0,1),
	sigma ~ dunif(0,10))

mMarriage <- map(Marriage_model, data = d)
precis(mMarriage)