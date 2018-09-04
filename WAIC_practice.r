library(rethinking)
data(cars)

m <- map( alist(
		dist ~ dnorm(mu, sig),
		mu <- a + b * speed,
		a ~ dnorm(0,100),
		b ~ dnorm(0,10),
		sig <- dunif(0,30)),
	data = cars)

n_samples <- 1e4
post <- extract.samples(m, n_samples)

# log-likelihood of each observation s given the sampled params (50 here)
ll <- sapply(1:n_samples, 
	function(s) {
		mu <- post$a[s] + post$b[s]*cars$speed
		dnorm(cars$dist, mu, post$sig[s], log = TRUE)
	})

n_obs <- nrow(cars)

# log-pointwise-predictive-density
lppd_vec <- sapply(1:n_obs, function(s) log_sum_exp(ll[s,]) - log(n_samples) )
lppd <- sum(lppd_vec)

pWAIC_vec <- sapply(1:n_obs, function(s) var(ll[s,]))
pWAIC <- sum(pWAIC)

WAIC <- -2*(lppd - pWAIC)
WAIC_vec <- -2*(lppd_vec - pWAIC_vec)
std_error <- sqrt (n_obs*var(WAIC_vec))

WAIC(m, pointwise = TRUE)