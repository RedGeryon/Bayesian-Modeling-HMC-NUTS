library(rethinking)
data(rugged)
d <- rugged
d$log_gpd <- log(d$rgdppc_2000)
dd <- d[complete.cases(d$rgdppc_2000),]

# Model 1 w/o African country var
m1 <- map(alist(
	log_gpd ~ dnorm(mu, sig),
	mu <- a + br * rugged,
	a ~ dnorm(8,100),
	br ~ dnorm(0,1),
	sig ~ dunif(0,10)),
data = dd)

m1_post <- extract.samples(m1, 1e4)

r_range <- seq(0, 7, length.out = 30)
mu1 <- sapply(r_range, function(r) m1_post$a + m1_post$br * r)
mu1.HPDI <- apply(mu1, 2, HPDI)

m1_formula <- function(r) rnorm(n = nrow(m1_post), mean = m1_post$a + m1_post$br * r, sd = m1_post$sig)
m1_sim <- sapply(r_range, m1_formula)
m1_sim.mean <- apply(m1_sim, 2, mean)
m1_sim.HPDI <- apply(m1_sim, 2, HPDI)

# plot(log_gpd ~ rugged, data = dd)
# shade(mu.HPDI, r_range)
# lines(r_range, m1_sim.mean)
# shade(m1_sim.HPDI, r_range)

# Model 2 w/ African country var

m2 <- map(alist(
	log_gpd ~ dnorm(mu, sig),
	mu <- a + br * rugged + bc * cont_africa,
	a ~ dnorm(8,100),
	br ~ dnorm(0,1),
	bc ~ dnorm(0,1),
	sig ~ dunif(0,10)),
data = dd)

m2_post <- extract.samples(m2, 1e4)

# Counterfactual with cont_africa == 1
mu2 <- sapply(r_range, function(r) m2_post$a + m2_post$br * r + m2_post$bc)
mu2.HPDI <- apply(mu2, 2, HPDI, prob = .97)

m2_formula <- function(r) rnorm(n = nrow(m2_post), mean = m2_post$a + m2_post$br * r + m2_post$bc, sd = m2_post$sig)
m2_sim <- sapply(r_range, m2_formula)
m2_sim.mean <- apply(m2_sim, 2, mean)
m2_sim.HPDI <- apply(m2_sim, 2, HPDI)

# Counterfactual with cont_africa == 0
mu3 <- sapply(r_range, function(r) m2_post$a + m2_post$br * r)
mu3.HPDI <- apply(mu3, 2, HPDI, prob = .97)

m3_formula <- function(r) rnorm(n = nrow(m2_post), mean = m2_post$a + m2_post$br * r, sd = m2_post$sig)
m3_sim <- sapply(r_range, m3_formula)
m3_sim.mean <- apply(m3_sim, 2, mean)
m3_sim.HPDI <- apply(m3_sim, 2, HPDI)

africa_col = rgb(0,0,1,.3)

plot(log_gpd ~ rugged, data = dd[dd$cont_africa == 1,], col = africa_col)
points(log_gpd ~ rugged, data = dd[dd$cont_africa == 0,])

shade(mu2.HPDI, r_range, col = africa_col)
lines(r_range, m2_sim.mean, col = africa_col)

shade(mu3.HPDI, r_range)
lines(r_range, m3_sim.mean)
