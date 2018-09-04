library(rethinking)
data(rugged)
d <- rugged
d$loggdp <- log(d$rgdppc_2000)
dd <- d[complete.cases(d$loggdp), ]

m1 <- map(alist(
	loggdp ~ dnorm(mu, sig),
	mu ~ a + br * rugged + ba * cont_africa + bar * rugged * cont_africa,
	a ~ dnorm(0,100),
	br ~ dnorm(0,10),
	ba ~ dnorm(0,10),
	bar ~ dnorm(0,10),
	sig ~ dunif(0,10)), data = dd)

# Trim down and pre-process all transformations then fit
# necessary params in one dataframe

dd.trim <- dd[c('loggdp', 'rugged', 'cont_africa')]

m1_stan <- map2stan(
	alist(
		loggdp ~ dnorm(mu, sig),
		mu ~ a + br * rugged + ba * cont_africa + bar * rugged * cont_africa,
		a ~ dnorm(0,100),
		br ~ dnorm(0,10),
		ba ~ dnorm(0,10),
		bar ~ dnorm(0,10),
		sig ~ dcauchy(0,2)),
	data = dd.trim)



m1_stan_post <- extract.samples(m1_stan)
m1_stan_post <- as.data.frame(m1_stan_post)

rugged_r <- seq(-1, 7, out.length = 60)

mu_eq <- function(r, c) m1_stan_post$a + m1_stan_post$br * r + m1_stan_post$ba * c + m1_stan_post$bar * r * c
mu_sim_eq <- function(r, c) rnorm(n=nrow(m1_stan_post), mean=m1_stan_post$a + m1_stan_post$br * r + m1_stan_post$ba * c + m1_stan_post$bar * r * c, sd = m1_stan_post$sig)

# African countries
a_col <- rgb(0,0,1, 0.3)
a_col2 <- rgb(0,0,1, 0.1)

mu_a <- mapply(mu_eq, rugged_r, 1)
mu_a.PI <- apply(mu_a, 2, PI)
mu_sim_a <- mapply(mu_sim_eq, rugged_r, 1)
mu_sim_a.mean <- apply(mu_sim_a, 2, mean)

mu_nota <- mapply(mu_eq, rugged_r, 0)
mu_nota.PI <- apply(mu_nota, 2, PI)
mu_sim_nota <- mapply(mu_sim_eq, rugged_r, 0)
mu_sim_nota.mean <- apply(mu_sim_nota, 2, mean)


# Plot loggdp ~ rugged, African conts in blue, non-African conts in black
plot(loggdp ~ rugged, dd[dd$cont_africa == 1, ], col = a_col)
shade(mu_a.PI, rugged_r, col = a_col)
lines(rugged_r, mu_sim_a.mean, col = a_col)

points(loggdp ~ rugged, dd[dd$cont_africa == 0, ])
shade(mu_nota.PI, rugged_r)
lines(rugged_r, mu_sim_nota.mean)

