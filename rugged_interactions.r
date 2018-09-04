library(rethinking)
data(rugged)
d <- rugged
d$log_gdp <- log(d$rgdppc_2000)

# Use countries with GDP data
dd <- d[complete.cases(d$rgdppc_2000),]

# Split into countries that are African/non-African
d.A1 <- dd[dd$cont_africa == 1, ]
d.A2 <- dd[dd$cont_africa == 0, ]

# MAP for African nations GDP v. terrain ruggedness
m1 <- map(alist(
	log_gdp ~ dnorm(mu, sig),
	mu <- a + br * rugged,
	a ~ dnorm(8,100),
	br ~ dnorm(0,1),
	sig ~ dunif(0,10)),
data = d.A1)

m1_samples <- extract.samples(m1, 1e4)

plot( log_gdp ~ rugged, d.A1)
abline(a = coef(m1)['a'], b = coef(m1)['br'])

rugged_range <- seq(0, 7, length.out = 30)

mu1 <- sapply(rugged_range, function(r) m1_samples$a + m1_samples$br * r)
mu1.HPDI <- apply(mu1, 2, HPDI)
shade(mu1.HPDI, rugged_range)

m1_sim <- sapply(rugged_range, function(r) rnorm(n = nrow(m1_samples), mean = m1_samples$a + m1_samples$br * r, sd = m1_samples$sig))
rugged1.HPDI <- apply(m1_sim, 2, HPDI)
shade(rugged1.HPDI, rugged_range)

# MAP for non-African nations GDP v terrain ruggedness
m2 <- map(alist(
	log_gdp ~ dnorm(mu, sig),
	mu <- a + br * rugged,
	a ~ dnorm(8,100),
	br ~ dnorm(0,1),
	sig ~ dunif(0,10)),
data = d.A2)

m2_samples <- extract.samples(m2, 1e4)

plot( log_gdp ~ rugged, d.A2)
abline(a = coef(m2)['a'], b = coef(m2)['br'])

mu2 <- sapply(rugged_range, function(r) m2_samples$a + m2_samples$br * r)
mu2.HPDI <- apply(mu2, 2, HPDI)
shade(mu2.HPDI, rugged_range)

m2_sim <- sapply(rugged_range, function(r) rnorm(n = nrow(m2_samples), mean = m2_samples$a + m2_samples$br * r, sd = m2_samples$sig))
rugged2.HPDI <- apply(m2_sim, 2, HPDI)
shade(rugged2.HPDI, rugged_range)
