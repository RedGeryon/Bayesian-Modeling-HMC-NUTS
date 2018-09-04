library(rethinking)
data(Howell1)

d <- Howell1

model <- alist(
	height ~ dnorm(mu, sig),
	mu <- a + b * log(weight),
	a ~ dnorm(178,100),
	b ~ dnorm(0,40),
	sig ~ dunif(0,50))

plot(height ~ weight, data = d, col = col.alpha(rangi2, 0.4))

m3 <- map(model, data = d)

weight_range <- seq(from = 0, to = 65, by = 1)
post <- extract.samples(m3, 1e4)

# mu samples from posterior
mu_formula <- function(weight) post$a + post$b * log(weight)
mu_h <- sapply(weight_range, mu_formula)
mu_h.mean <- apply(mu_h, 2, mean)
mu_h.HPDI <- apply(mu_h, 2, HPDI, prob = .97)
lines(weight_range, mu_h.mean)
shade(mu_h.HPDI, weight_range)

# height samples
h_formula <- function(weight) rnorm(post$a + post$b * log(weight), sd = post$sig)
h_sim <- sapply(weight_range, h_formula)
h_sim.mean <- apply(h_sim, 2, mean)
h_sim.HPDI <- apply(h_sim, 2, HPDI, prob = .97)
shade(h_sim.HPDI, weight_range)