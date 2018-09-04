N <- 50
spacing = 300
trials = 20

rang <- seq(0,6, length.out = spacing)
rho <- 1-exp(-rang)

x <- rnorm(N, 0, 1)
y <- rnorm(N, x)

sample_sd <- matrix(ncol = trials, nrow = spacing)
for (i in 1:trials) {
	x_dum <- sapply(rho, function(s) rnorm(N, s*x, sqrt(1-s^2)))
	sample_sd[,i] <- apply(x_dum, 2, function(sample) coef(lm( y ~ x + sample))[2])}
sample_mean <- apply(sample_sd, 1, mean)

plot(sample_mean ~ rho, col = rgb(0,0,0, alpha = .3))