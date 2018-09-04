library(rethinking)
data(Howell1)
d <- Howell1
d2 <- d[ d$age >= 18, ]

sample_mu <- rnorm( 1e4, 178, 20)
sample_sig <- runif( 1e4, 0, 50)
prior_h <- rnorm( 1e4, sample_mu, sample_sig)

mu.list <- seq(from = 120, to = 160, length.out = 200)
sig.list <- seq(from = 4, to = 9, length.out = 200)
post <- expand.grid(mu = mu.list, sig = sig.list)
post$LL <- sapply( 1:nrow(post), function(i) sum( dnorm(
	d2$height,
	mean = post$mu[i],
	sd = post$sig[i],
	log = TRUE)))
post$prod <- post$LL + dnorm(post$mu, 178, 20, TRUE) + dunif(post$sig, 0, 50, TRUE)
post$prob <- exp(post$prod - max(post$prod))
# contour_xyz( post$mu , post$sig , post$prob, ylim = c(7.1, 8.4), xlim = c(153.5,155.5))
# image_xyz( post$mu , post$sig , post$prob, ylim = c(7.1, 8.4), xlim = c(153.5,155.5))


# We sample from the distribtion

sample.row <- sample( 1:nrow(post), 1e4, replace = TRUE, prob = post$prob)
sample.mu <- post$mu[sample.row]
sample.sig <- post$sig[sample.row]
plot( sample.mu , sample.sig , cex=0.5 , pch=16 , col=col.alpha(rangi2,0.1) )