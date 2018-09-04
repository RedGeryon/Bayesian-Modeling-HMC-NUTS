library(rethinking)

N <- 1000
x_real <- rnorm(N)
# mean for x_spur is x_real
x_spur <- rnorm(N, x_real)
y <- rnorm( N, x_real)
d <- data.frame(y, x_real, x_spur)

model <- alist(
	y ~ dnorm( mu, sig),
	mu ~ a + bR * x_real + bS * x_spur,
	a ~ dnorm(0,10),
	bR ~ dnorm(0,1),
	bS ~ dnorm(0,1),
	sig ~ dunif(0,10))

m <- map(model, data = d)

real_model <- alist(
	x_real ~ dnorm(mu, sig),
	mu ~ a + bS * x_spur,
	a ~ dnorm(0,10),
	bS ~ dnorm(0,1),
	sig ~ dunif(0,10))

mreal <- map(real_model, data = d)

# Plot x_real ~ x_spur
plot(x_real ~ x_spur, data = d)
real_post <- extract.samples(mreal, 1e4)
spur.seq <- seq(from = -5, to = 5, length.out = 60)
realmu <- sapply(spur.seq, function(spur) real_post$a + real_post$bS * spur)
realmu.mean <- apply(realmu, 2, mean)
realmu.PI <- apply(realmu, 2, PI)
real_sim <- sapply(spur.seq, function(spur) rnorm(n = 1e4, mean = real_post$a + real_post$bS * spur, sd = real_post$sig))
real_sim.mean <- apply(real_sim, 2, mean)
real_sim.HPDI <- apply(real_sim, 2, HPDI)
lines(spur.seq, realmu.mean)
shade(realmu.PI, spur.seq)
shade(real_sim.HPDI, spur.seq)

# Plot real residuals
real_map_vals <- sapply(x_spur, function(spur) coef(mreal)['a'] + coef(mreal)['bS'] * spur )
real_residual <- d$x_real - real_map_vals
plot( d$y ~ real_residual)
lm(d$y ~ real_residual)

# Do the same for other direction, x_spur ~ x_real
spur_model <- alist(
	x_spur ~ dnorm(mu, sig),
	mu ~ a + bR * x_real,
	a ~ dnorm(0,10),
	bR ~ dnorm(0,1),
	sig ~ dunif(0,10))

mspur <- map(spur_model, data = d)
spur_post <- extract.samples(mspur, 1e4)

real.seq <- seq(from = -5, to = 5, length.out = 60)
spur_map_vals <- sapply(x_real, function(real) coef(mspur)['a'] + coef(mspur)['bR'] * real)
spur_resid <- x_spur - spur_map_vals

plot(d$y ~ spur_resid )
lm(d$y ~ spur_resid)

# We knew this from precis, but we can see better
# that after taking into account x_real, x_spur is
# a totally uncorrelated Gaussian