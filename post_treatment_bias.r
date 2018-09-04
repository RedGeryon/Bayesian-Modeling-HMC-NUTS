library(rethinking)

N <- 100
h0 <- rnorm(N, 10, 2)
treatment <- rep(0:1, each = N/2)
fungus <- rbinom(N, size = 1, prob = .5 - treatment *.4)
h1 <- h0 + rnorm(N, 5 - 3*fungus)

d <- data.frame(h0=h0, h1=h1, treatment=treatment, fungus=fungus)

m1 <- alist(
	h1 ~ dnorm(mu, sig),
	mu <- a + bh * h0 + bt * treatment + bf * fungus,
	a ~ dnorm(0, 100),
	c(bh, bf, bf) ~ dnorm(0,10),
	sig ~ dunif(0,10))

map_m1 <- map(m1, d)