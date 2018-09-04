library(rethinking)
data(Howell1)
# d2 <- Howell1[Howell1$age >= 18, ]

# d2$weight.c <- d2$weight - mean(d2$weight)

# flist <- alist(
# 	height ~ dnorm(mu, sigma),
# 	mu <- a + b*weight.c,
# 	a ~ dnorm(178, 100),
# 	b ~ dnorm(0, 10),
# 	sigma ~ dunif(0, 50))

# m4.3 <- map(flist, data = d2)

# summary, var-covar, correlation
# precis(m4.3)
# vcov(m4.3)
# cov2cor(vcov(m4.3))

# sample multi-dimensional MAP
# post <- extract.samples(m4.3, 1e4)
# precis(m4.3, corr = TRUE)
# dens(post)

# plot inference (better than reading tabular data)
# plot(height ~ weight.c, data = d2, col = col.alpha(rangi2, 0.5))

# for (i in 1:900)
# 	abline(a = post$a[i], b=post$b[i], col=col.alpha("blue",0.01))

# Look at how less data creates a wider posterior where the mean is more uncertain
# N <- 10
# dN <- d2[1:N,]
# mlist <- alist(
# 	height ~ dnorm(mu, sigma),
# 	mu <- a + b*weight.c,
# 	a ~ dnorm(178, 100),
# 	b ~ dnorm(0, 10),
# 	sigma ~ dunif(0, 50))

# mN <- map(mlist, data = dN)

# mpost <- extract.samples(mN, 1e4)
# plot(height ~ weight.c, data = dN)
# for (i in 1:900)
# 	abline( a = mpost$a[i], b=mpost$b[i], col=col.alpha("blue",0.01))

# Lets get an estimate of height for weight at 50
# mu_at_50 <- post$a + post$b*50
# dens(mu_at_50)

# What we really want is to see the uncertainty around the MAP line with 
# a range of weight values, so we will do that
# Note: when using apply, the 2nd arg tells what to apply func (3rd arg) over
# dimension 1, which is rows, 2 is col, c(1, 2) is both row and cols
# weight_range <- seq(from = 20, to = 100, by = 1)

# Need to remember our transformations
# weight_range <- weight_range - mean(d2$weight)
# mu.apply_post <- function(weight) post$a + post$b*weight
# mu <- sapply(weight_range, mu.apply_post)
# mu.mean <- apply(mu, 2, mean)
# mu.HPDI <- apply(mu, 2, HPDI, prob = .87)

# Plot our HDPI corresponding to weights
# for (i in 1:100)
# 	points(weight_range, mu[i,], pch = 16, col = col.alpha(rangi2, 0.1))

# Draw the mean line of our prediction and shade the HDPI
# lines(weight_range, mu.mean)
# shade(mu.HPDI, weight_range)

# Lets use our model mu and the uncertainty in the sd to sample heights
# sim_func <- function (weight) rnorm(
# 	n = nrow(post), mean = post$a + post$b * weight, sd = post$sigma)
# height_sim <- sapply(weight_range, sim_func)
# height_sim.mean <- apply(height_sim, 2, mean)
# height_sim.HPDI <- apply(height_sim, 2, HPDI, prob = .87)
# lines(weight_range, height_sim.mean)
# shade(height_sim.HPDI, weight_range)

##############
# Run prediction on ages below 18
##############

# model building
d3 <- Howell1[Howell1$age < 18, ]
d3$weight.c <- d3$weight - mean(d3$weight)
d3_model <- alist( height ~ dnorm(mu, sig),
		mu <- a + b * weight.c,
		a ~ dnorm(155, 20),
		b ~ dunif(0,10),
		sig ~ dunif (0, 11))
m3 <- map(d3_model, data = d3)
post3 <- extract.samples(m3, 1e4)
plot(height ~ weight.c, data = d3, col = col.alpha("blue", 0.5))

weight_range <- seq(from = 0, to= 40, by = 1)
weight_range <- weight_range - mean(d3$weight)

# mean simulation (sampled posterior of mean)
model_func <- function(weight) post3$a + post3$b *weight
mu <- sapply(weight_range, model_func)
mu.mean <- apply(mu, 2, mean)
mu.HPDI <- apply(mu, 2, HPDI)
lines(weight_range, mu.mean)
shade(mu.HPDI, weight_range)

# height simulation
height_func <- function(weight) rnorm(n = nrow(post3), mean = post3$a + post3$b *weight, sd = post3$sig)
h_sim <- sapply(weight_range, height_func)
h_sim.mean <- apply(h_sim, 2, mean)
h_sim.HPDI <- apply(h_sim, 2, HPDI)
shade(h_sim.HPDI, weight_range)

for (i in 1:500)
	abline(a = post3$a[i], b = post3$b[i], col=col.alpha("blue",0.01))
