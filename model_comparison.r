library(rethinking)
data(milk)

# predict kcal.per.g w/ neocortex and log(mass)
d <- milk[complete.cases(milk),]
d$neocortex <- d$neocortex.perc/100

# we use exp(log(x)) to constrain sig to be positive
# notice these are with flat priors
a.start <- mean(d$kcal.per.g)
sig.start <- log(sd(d$kcal.per.g))

m0 <- map(alist(
	kcal.per.g ~ dnorm(a, exp(log.sig))),
	data = d, start = list(a = a.start, log.sig = sig.start))

m1 <- map(alist(
	kcal.per.g ~ dnorm(mu, exp(log.sig) ),
	mu <- a + bn *neocortex),
	data = d, start = list(a = a.start, bn = 0, log.sig = sig.start))

m2 <- map(alist(
	kcal.per.g ~ dnorm(mu, exp(log.sig) ),
	mu <- a + bm*log(mass)),
	data = d, start = list(a = a.start, bm = 0, log.sig = sig.start))

m3 <- map( alist(
	kcal.per.g ~ dnorm(mu, exp(log.sig) ),
	mu <- a + bn * neocortex + bm * log(mass)),
	data = d, start = list(a = a.start, bn = 0, bm = 0, log.sig = sig.start))

milk.models <- compare(m0, m1, m2, m3)