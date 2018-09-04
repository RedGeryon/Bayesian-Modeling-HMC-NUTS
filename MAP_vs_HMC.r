library(rethinking)
data(rugged)
d <- rugged
d$log_gdp <- log(d$rgdppc_2000)
dd <- d[complete.cases(d$rgdppc_2000), ]

m8.1 <- map(
	alist(
		log_gdp ~ dnorm(mu, sigma),
		mu <- a + bR*rugged + bA * cont_africa + bAR*rugged*cont_africa,
		a ~ dnorm(0,100),
		bR ~ dnorm(0,100),
		bA ~ dnorm(0,100),
		bAR ~ dnorm(0,100),
		sigma ~ dunif(0,10)),
	data = dd)

precis(m8.1)

# Lets compare with STAN, we need to trim to relevant cases and perform all transformations first (done)
dd.stan <- dd[ , c("log_gdp", "rugged", "cont_africa")]

s8.1 <- map2stan(
	alist(
		log_gdp ~ dnorm(mu, sigma),
		mu <- a + bR*rugged + bA * cont_africa + bAR*rugged*cont_africa,
		a ~ dnorm(0,100),
		bR ~ dnorm(0,100),
		bA ~ dnorm(0,10),
		bAR ~ dnorm(0,100),
		sigma ~ dexp(0.0001)),
	data = dd.stan)

precis(s8.1)
plot(s8.1)

# running 4 chains on 4 cores
# s8.1_4chains <- map2stan(s8.1, chains = 4, cores = 4)
# post <- extract.samples(s8.1)
# post <- as.data.frame(post)

# trace plot to see the path of exploration of our chain
# plot(s8.1)

mp <- map2stan(
    alist(
        a ~ dnorm(0,1),
        b ~ dcauchy(0,1)
    ),
    data=list(y=1),
    start=list(a=0,b=0),
    iter=1e4, warmup=100 , WAIC=FALSE )