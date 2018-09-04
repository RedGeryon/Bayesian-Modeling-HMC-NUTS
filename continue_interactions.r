library(rethinking)
data(tulips)
d <- tulips

water.mean <- mean(d$water)
water.sd <- sd(d$water)
d$water.s <- (d$water - water.mean)/water.sd

shade.mean <- mean(d$shade)
shade.sd <- sd(d$shade)
d$shade.s <- (d$shade - shade.mean)/shade.sd

m1 <- map(alist(
	blooms ~ dnorm(mu, sig),
	mu <- a + bw * water.s + bs * shade.s,
	a ~ dnorm(130,100),
	bw ~ dnorm(0,100),
	bs ~ dnorm(0,100),
	sig ~ dunif(0,100)),
data = d, start=list(a=mean(d$blooms), bw=0, bs=0, sig=sd(d$blooms)))

m2 <- map(alist(
	blooms ~ dnorm(mu, sig),
	mu <- a + gamma * water.s + bs * shade.s,
	gamma <- bw + bws * shade.s,
	a ~ dnorm(130,100),
	bw ~ dnorm(0,100),
	bws ~ dnorm(0,100),
	bs ~ dnorm(0,100),
	sig ~ dunif(0,100)),
data = d, start=list(a=mean(d$blooms), bw=0, bws=0, bs=0, sig=sd(d$blooms)))

plot(blooms ~ shade.s, d)
abline()