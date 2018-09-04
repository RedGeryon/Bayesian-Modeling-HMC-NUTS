# Try with interaction term
library(rethinking)
data(rugged)
d <- rugged
d$log_gpd <- log(d$rgdppc_2000)
dd <- d[complete.cases(d$rgdppc_2000),]

m4 <- map(alist(
	log_gpd ~ dnorm(mu, sig),
	mu <- a + gamma * rugged + bc * cont_africa,
	gamma <- br + brc * cont_africa,
	a ~ dnorm(8,100),
	br ~ dnorm(0,1),
	brc ~ dnorm(0,1),
	bc ~ dnorm(0,1),
	sig ~ dunif(0,10)),
data = dd)

m4_post = extract.samples(m4, 1e4)

# Counter factual with coun_africa == 1
r_range <- seq(0, 7, length.out = 30)
m4_mu_form <- function(r, c) m4_post$a + m4_post$br * r + m4_post$brc * r * c + m4_post$bc * c 
mu4 <- mapply(m4_mu_form, r_range, 1)
mu4.HPDI <- apply(mu4, 2, HPDI, prob = .97)

m4_sim_form <- function(r, c) rnorm(n = nrow(m4_post), mean = m4_post$a + m4_post$br * r + m4_post$brc * r * c + m4_post$bc * c, sd = m4_post$sig)
m4_sim <- mapply(m4_sim_form, r_range, 1)
m4_sim.mean <- apply(m4_sim, 2, mean)
m4_sim.HPDI <- apply(m4_sim, 2, HPDI)

# Counter factual with coun_africa == 0
mu5 <- mapply(m4_mu_form, r_range, 0)
mu5.HPDI <- apply(mu5, 2, HPDI, prob = .97)

m5_sim <- mapply(m4_sim_form, r_range, 0)
m5_sim.mean <- apply(m5_sim, 2, mean)
m5_sim.HPDI <- apply(m5_sim, 2, HPDI)

# Plot
africa_col = rgb(0,0,1,.3)

plot(log_gpd ~ rugged, dd[dd$cont_africa == 1, ], col = africa_col)
points(log_gpd ~ rugged, dd[dd$cont_africa == 0, ])

lines(r_range, m4_sim.mean, col = africa_col)
shade(mu4.HPDI, r_range, col = africa_col)

lines(r_range, m5_sim.mean)
shade(mu5.HPDI, r_range)
