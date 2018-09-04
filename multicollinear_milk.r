library(rethinking)
data(milk)
d <- milk

# model kcal.per.g with perc.fat, perc.lactose 
# Standardize
standard <- function(df) (df - mean(df))/sd(df)
d$perc.fat.s <- standard(d$perc.fat)
d$perc.lactose.s <- standard(d$perc.lactose)

m1 <- alist(
	kcal.per.g ~ dnorm(mu, sig),
	mu <- a + b * perc.fat.s + c * perc.lactose.s,
	a ~ dnorm(0,10),
	b ~ dnorm(0,1),
	c ~ dnorm(0,1),
	sig ~ dunif(0,10))

map_m1 <- map(m1, data = d)

# Counterfactuals
fat_seq <- seq(from = -3, to = 3, by = .1)
lac_seq <- seq(from = -3, to = 3, by = .1)

m1_post <- extract.samples(map_m1, 1e4)

m1_fat_mu <- sapply(fat_seq, function(fat) m1_post$a + m1_post$b * fat)
m1_fat_mu.mean <- apply(m1_fat_mu, 2, mean)
m1_fat_mu.PI <- apply(m1_fat_mu, 2, PI)

plot(kcal.per.g ~ perc.fat.s, data = d)
lines(fat_seq, m1_fat_mu.mean)
lines(fat_seq, m1_fat_mu.PI[1,], lty=2)
lines(fat_seq, m1_fat_mu.PI[2,], lty=2)

m1_lac_mu <- sapply(lac_seq, function(lac) m1_post$a + m1_post$c * lac)
m1_lac_mu.mean <- apply(m1_lac_mu, 2, mean)
m1_lac_mu.PI <- apply(m1_lac_mu, 2, PI)

plot(kcal.per.g ~ perc.lactose.s, data = d)
lines(lac_seq, m1_lac_mu.mean)
lines(lac_seq, m1_lac_mu.PI[1,], lty=2)
lines(lac_seq, m1_lac_mu.PI[2,], lty=2)

# Model relationship between perc.fat.s and per.lactose.s
plot(perc.lactose.s ~ perc.fat.s, data = d)
abline(lm(perc.lactose.s ~ perc.fat.s, data = d))

# Display model with one regressor
m2 <- alist(
	kcal.per.g ~ dnorm(mu, sig),
	mu <- a + b * perc.fat.s,
	a ~ dnorm(0,10),
	b ~ dnorm(0,1),
	sig ~ dunif(0,10))

map_m2 <- map(m2, data = d)

m3 <- alist(
	kcal.per.g ~ dnorm(mu, sig),
	mu <- a + b * perc.lactose.s,
	a ~ dnorm(0,10),
	b ~ dnorm(0,1),
	sig ~ dunif(0,10))

map_m3 <- map(m3, data = d)

precis(map_m2)
precis(map_m3)
coef(map_m1)['b'] + coef(map_m1)['c']

pairs( ~ kcal.per.g + perc.fat.s + perc.lactose.s,  data = d, col = rangi2)
