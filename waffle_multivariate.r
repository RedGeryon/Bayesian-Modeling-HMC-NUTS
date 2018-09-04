library(rethinking)
data(WaffleDivorce)

d <- WaffleDivorce

MAM.mu <- mean(d$MedianAgeMarriage)
MAM.sd <- sd(d$MedianAgeMarriage)
M.mu <- mean(d$Marriage)
M.sd <- sd(d$Marriage)

d$MedianAgeMarriage.s <- (d$MedianAgeMarriage - MAM.mu)/ MAM.sd
d$Marriage.s <- (d$Marriage - M.mu)/ M.sd

model <- alist(
	Divorce ~ dnorm(mu, sig),
	mu <- a + bR * Marriage.s + bA * MedianAgeMarriage.s,
	a ~ dnorm(10,10),
	bR ~ dnorm(0,1),
	bA ~ dnorm(0,1),
	sig ~ dunif(0, 10))

m <- map(model, data = d)



# We will model one against the other to get the predictor residuals.
# Rate of marriage model
# r_model <- alist(
# 	Marriage.s ~ dnorm(mu, sig),
# 	mu <- a + b * MedianAgeMarriage.s,
# 	a ~ dnorm(10,10),
# 	b ~ dnorm(0,1),
# 	sig ~ dunif(0,10))

# rm <- map(r_model, data = d)

# d$Marriage.mu <- coef(rm)['a'] + coef(rm)['b'] * d$MedianAgeMarriage.s
# d$Marriage.residual <- d$Marriage.s - d$Marriage.mu

plot( Marriage.s ~ MedianAgeMarriage.s, data = d, col = col.alpha(rangi2, .8))
abline(rm)

for (i in 1:length(d$Marriage.mu)) {
	x <- d$MedianAgeMarriage.s[i]
	y <- d$Marriage.s[i]
	lines( c(x,x), c(d$Marriage.mu[i], y), lwd = .5)
}

# Create counterfactuals
A.avg <- mean(d$MedianAgeMarriage.s)
R.seq <- seq(from = -3, to = 3, length.out = 50)

# Sample posterior, compute mu for potential Marriage rates while hodling MAM at avg val
# mu <- link(m, data = pred.data)
# mu <- sapply(R.seq, function(rate) post$a + post$bR * rate)
post = extract.samples(m, 1e4)
mu <- mapply(function(rate, age) post$a + post$bR * rate + post$bA * age, R.seq, A.avg)
mu.mean <- apply(mu, 2, mean)
mu.HPDI <- apply(mu, 2, HPDI)

# Simulate counterfactual divorce outcomes
# R.sim <- sim(m, data = pred.data, n = 1e4)
# R.sim <- sapply(R.seq, function(rate) rnorm(n = 1e4, mean= post$a + post$bR * rate, sd = post$sig))
R.sim <- mapply(function(rate, age) rnorm(n = 1e4, mean = post$a + post$bR * rate + post$bA * age, sd = post$sig), R.seq, A.avg)
R.PI <- apply(R.sim, 2, PI)

# plot Divorce ~ Marriage counterfactuals
plot( Divorce ~ Marriage.s, data = d)
lines(R.seq, mu.mean)
shade(mu.HPDI, R.seq)
shade(R.PI, R.seq)

# Create counterfactuals for Divorce ~ MAM taking into account Marriage.s
# First we model if MAM can be predicted by marriage rate

MAM_model <- alist(
	MedianAgeMarriage.s ~ dnorm(mu, sig),
	mu <- a + bR * Marriage.s,
	a ~ dnorm(0,10),
	bR ~ dnorm(0,1),
	sig ~ dunif(0,10))

MAMm <- map(MAM_model, data = d)

plot(MedianAgeMarriage.s ~ Marriage.s, data = d)
abline(MAMm)

d$MedianAgeMarriage.mu <- coef(MAMm)['a'] + coef(MAMm)['bR'] * d$Marriage.s

# Draw residual lines
for (i in 1:length(d$Marriage.s)) {
	x <- d$Marriage.s[i]
	y <- d$MedianAgeMarriage.s[i]
	lines( c(x,x) , c(d$MedianAgeMarriage.mu[i], y), lwd = .5)
}

d$MedianAgeMarriage.residual <- d$MedianAgeMarriage.s - d$MedianAgeMarriage.mu

# Plot MAM vs residuals
# plot( Divorce ~ MedianAgeMarriage.residual, data = d)

# Plot counterfactuals, start with mean distribution, then PI for possible range
# We are holding Marriage rate constant while simulating Divorce results for various MAM range
R.avg <- mean(d$Marriage.s)
A.seq <- seq(from = -3, to = 3, length.out = 50)

# We can use link function here, but better to practice sampling
# Above, we already crated mu 

Amu <- mapply(function(rate, age) post$a + post$bR * rate + post$bA * age, R.avg, A.seq)
Amu.mean <- apply(Amu, 2, mean)
Amu.HPDI <- apply(Amu, 2, HPDI)
A.sim <- mapply(function(rate, age) rnorm(n = 1e4, mean = post$a + post$bR * rate + post$bA * age, sd = post$sig), R.avg, A.seq)
A.PI <- apply(A.sim, 2, PI)

# Plot counterfactuals on a graph of Divorce ~ MAM.s:
plot(Divorce ~ MedianAgeMarriage.s, data = d)
lines(A.seq, Amu.mean)
shade(Amu.HPDI, A.seq)
shade(A.PI, A.seq)

######## Create posterior predictive plots

# Because we are dealing with two parameters here, we need to create 2 ranges to sample predictions
# From above, we created R.seq and A.seq
mu_sim <- mapply(function(rate, age) post$a + post$bR * rate + post$bA * age, d$Marriage.s, d$MedianAgeMarriage.s)

post_sim <- mapply(function(rate, age) rnorm(n = 1e4, mean = post$a + post$bR * rate + post$bA * age, sd = post$sig), d$Marriage.s, d$MedianAgeMarriage.s)
mu_sim.mean <- apply(mu_sim, 2, mean)
mu_sim.PI <- apply(mu_sim, 2, PI)
post_sim.PI <- apply(post_sim, 2, PI)

# We have choices for plots, first we plot actual vs predicted and a diagonal line which represents matched observation and prediction
plot( mu_sim.mean ~ d$Divorce, xlab = "Observed", ylab = "Predicted")
abline( a = 0, b = 1)
for (i in 1:nrow(d)) {
	lines( rep(d$Divorce[i], 2), c(mu_sim.PI[1,i],mu_sim.PI[2,i]))
}
# identify( x=d$Divorce , y=mu_sim.mean , labels=d$Loc , cex=0.8 )

# Create chart of residual errors by state (ordered in residual maginitude)
divorce.resid <- d$Divorce - mu_sim.mean
o <- order(divorce.resid)

dotchart(divorce.resid[o], labels = d$Loc[o], xlim=c(-6,5), cex=.6)
abline(v = 0, col=col.alpha("black", .2))
for (i in 1:nrow(d)) {
	j <- o[i]
	lines( d$Divorce[j]-c(mu_sim.PI[1,j],mu_sim.PI[2,j]), rep(i,2))
	points( d$Divorce[j]-c(post_sim.PI[1,j],post_sim.PI[2,j]) , rep(i,2),pch=3 , cex=0.6 , col="gray" )
}
