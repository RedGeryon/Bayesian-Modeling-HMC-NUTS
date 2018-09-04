library(rethinking)
data(milk)
d <- milk
str(d)

# Simple bivarte between kcal and neocortex.perc
# Only use complete cases for data
dcc <- d[ complete.cases(d) , ]

m1 <- alist(
	kcal.per.g ~ dnorm(mu, sig),
	mu <- a + b * neocortex.perc,
	a ~ dnorm(0,100),
	b ~ dnorm(0,1),
	sig ~ dunif(0,1))

map_m1 <- map(m1, data = dcc)

m1_post <- extract.samples(map_m1, 1e4)
m1_x_range <- 0:100
m1_mu <- sapply(m1_x_range, function(ncp) m1_post$a + m1_post$b * ncp)
m1_mu.mean <- apply(m1_mu, 2, mean)
m1_mu.PI <- apply(m1_mu, 2, PI)
m1_sample <- sapply(m1_x_range,
	function(ncp) rnorm(n = 1e4, mean = m1_post$a + m1_post$b * ncp, sd = m1_post$sig))
m1_sample.HPDI <- apply(m1_sample, 2, HPDI)

# We see wide uncertainty and relatively low levels of association
plot(kcal.per.g ~ neocortex.perc, data = dcc)
lines(m1_x_range, m1_mu.mean)
lines(m1_x_range, m1_mu.PI[1,] , lty = 2)
lines(m1_x_range, m1_mu.PI[2,], lty = 2)

#### Bivariate between kcal.per.g and log(mass)
dcc$log.mass <- log(dcc$mass)

m2 <- alist(
	kcal.per.g ~ dnorm(mu, sig),
	mu <- a + b * log.mass,
	a ~ dnorm(0,100),
	b ~ dnorm(0,1),
	sig ~ dunif(0,1))

map_m2 <- map(m2, data = dcc)

m2_post <- extract.samples(map_m2, 1e4)
m2_x_range <- seq(from = -3, to = 5, length.out = 30)
m2_mu <- sapply(m2_x_range, function(ncp) m2_post$a + m2_post$b * ncp)
m2_mu.mean <- apply(m2_mu, 2, mean)
m2_mu.PI <- apply(m2_mu, 2, PI)

plot(kcal.per.g ~ log.mass, data = dcc)
lines(m2_x_range, m2_mu.mean)
lines(m2_x_range, m2_mu.PI[1,] , lty = 2)
lines(m2_x_range, m2_mu.PI[2,], lty = 2)

# Also weak, but negatively correlated

##### Multi-variate regression
m3 <- alist(
	kcal.per.g ~ dnorm(mu, sig),
	mu <- a + b * neocortex.perc + c * log.mass,
	a ~ dnorm(0,100),
	b ~ dnorm(0,1),
	c ~ dnorm(0,1),
	sig ~ dunif(0,1))

map_m3 <- map(m3, data = dcc)

m3_post <- extract.samples(map_m3, 1e4)
m3_range_n <- 0:100
m3_range_m <- seq(from = -3, to = 5, length.out = 100)


# Create counterfactual for neocortex_perc utilizing mean log.mass
m3_mu <- mapply(function(n, m) m3_post$a + m3_post$b * n + m3_post$c * m, m3_range_n, mean(dcc$log.mass))
m3_mu.mean <- apply(m3_mu, 2, mean)
m3_mu.PI <- apply(m3_mu, 2, PI)

plot(kcal.per.g ~ neocortex.perc, data = dcc)
lines(m3_range_n, m3_mu.mean)
lines(m3_range_n, m3_mu.PI[1,] , lty = 2)
lines(m3_range_n, m3_mu.PI[2,] , lty = 2)

# Create counterfactual for mass utilizing neocortex_perc at 60%
m3_mu <- mapply(function(n, m) m3_post$a + m3_post$b * n + m3_post$c * m, 60, m3_range_m)
m3_mu.mean <- apply(m3_mu, 2, mean)
m3_mu.PI <- apply(m3_mu, 2, PI)

plot(kcal.per.g ~ log.mass, data = dcc)
lines(m3_range_m, m3_mu.mean)
lines(m3_range_m, m3_mu.PI[1,] , lty = 2)
lines(m3_range_m, m3_mu.PI[2,] , lty = 2)