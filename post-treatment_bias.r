
# We are simulating whether or not fungal treatment has impact on plant
# growth with suspicion that fungus presence negatively impacts growth.

library(rethinking)

# no of plants
N <- 100

# simulate inital plant heights
h0 <- rnorm(N, 100, 2)

# assign treatments and sim plant growth
treatment <- rep(0:1, each = N/2)
fungus <- rbinom(N, size = 1, prob = 0.5 - treatment * .4)
h1 <- h0 + rnorm(N, 5 - 3 * fungus)

# put into df
d <- data.frame(h0 = h0, h1 = h1, treatment = treatment, fungus = fungus)

# Model using all vars/post-treat bias

model <- alist(
	h1 ~ dnorm(mu, sig),
	mu <- a + bh*h0 + bt*treatment + bf*fungus,
	a ~ dnorm(0,100),
	c(bh, bf, bt) ~ dnorm(0,10),
	sig ~ dunif(0,10))

map_model <- map(model, data = d)

precis(map_model, corr = TRUE)

# model without post-treatment bias:
m2 <- alist(
	h1 ~ dnorm(mu, sig),
	mu <- a + bh*h0 + bt*treatment,
	a ~ dnorm(0,100),
	c(bh, bt) ~ dnorm(0,10),
	sig ~ dunif(0,10))

map_m2 <- map(m2, data = d)

precis(map_m2, corr = TRUE)