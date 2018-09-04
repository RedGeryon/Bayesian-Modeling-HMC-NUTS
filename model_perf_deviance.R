# We will simulate data with known model of 3 params
# End goal is to compare best model estimates between train/test set
# given varying # of params

ntrials = 300
train_sim = data.frame(matrix(NA, nrow = ntrials, ncol = 6))
test_sim = data.frame(matrix(NA, nrow = ntrials, ncol = 6))

N <- 50
x1 <- runif(N,-10, 5)
x2 <- runif(N,-5,10)
y <- rnorm(N, mean = .1*x1 - 0.5*x2, sd = 2)
d <- data.frame(y = y, x1 = x1, x2 = x2)

for (j in 1:ntrials) {
	d <- d[sample(1:nrow(d)),]
	train <- d[1:(N/2),]
	test <- d[(N/2 + 1):N,]

	# 2 param
	p1 <- lm(y ~ x1, data = train)
	coef1 <- coef(p1)
	likely1 = -2 * logLik(p1)
	testdev1 <- -2*sum(dnorm(
		test$y,
		mean = coef1[1] + coef1[2]*test$x1,
		sd = sigma(p1),
		log = TRUE))

	# 3 params
	p2 <- lm(y ~ x1 + x2, data = train)
	coef2 <- coef(p2)
	likely2 = -2 * logLik(p2)
	testdev2 <- -2*sum(dnorm(
		test$y,
		mean = coef2[1] + coef2[2]*test$x1 + coef2[3]*test$x2,
		sd = sigma(p2),
		log = TRUE))

	# 4 params
	p3 <- lm(y ~ x1 + x2 + I(x1^2), data = train)
	coef3 <- coef(p3)
	likely3 = -2 * logLik(p3)
	testdev3 <- -2*sum(dnorm(
		test$y,
		mean = coef3[1] + coef3[2]*test$x1 + coef3[3]*test$x2 + coef3[4]*(test$x1)^2,
		sd = sigma(p3),
		log = TRUE))

	# 5 params
	p4 <- lm(y ~ x1 + x2 + I(x1^2) + I(x2^2), data = train)
	coef4 <- coef(p4)
	likely4 = -2 * logLik(p4)
	testdev4 <- -2*sum(dnorm(
		test$y,
		mean = coef4[1] + coef4[2]*test$x1 + coef4[3]*test$x2 + coef4[4]*(test$x1)^2 + coef4[5]*(test$x2)^2,
		sd = sigma(p4),
		log = TRUE))

	# 6 params
	p5 <- lm(y ~ x1 + x2 + I(x1^2) + I(x2^2) + I(x1^3), data = train)
	coef5 <- coef(p5)
	likely5 = -2 * logLik(p5)
	testdev5 <- -2*sum(dnorm(
		test$y,
		mean = coef5[1] + coef5[2]*test$x1 + coef5[3]*test$x2 + coef5[4]*(test$x1)^2 + coef5[5]*(test$x2)^2 + coef5[6]*(test$x1)^3,
		sd = sigma(p5),
		log = TRUE))

	# 7 params
	p6 <- lm(y ~ x1 + x2 + I(x1^2) + I(x2^2) + I(x1^3) + I(x2^3), data = train)
	coef6 <- coef(p6)
	likely6 = -2 * logLik(p6)
	testdev6 <- -2*sum(dnorm(
		test$y,
		mean = coef6[1] + coef6[2]*test$x1 + coef6[3]*test$x2 + coef6[4]*(test$x1)^2 + coef6[5]*(test$x2)^2 + coef6[6]*(test$x1)^3 + coef6[7]*(test$x2)^3,
		sd = sigma(p6),
		log = TRUE))

	train_dev <- c(likely1, likely2, likely3, likely4, likely5, likely6)
	test_dev <- c(testdev1, testdev2, testdev3, testdev4, testdev5, testdev6)

	train_sim[j,] = train_dev
	test_sim[j,] = test_dev
}

avg_train <- apply(train_sim, 2, mean)
avg_test <- apply(test_sim, 2, mean)

train_col <- rgb(1, 0, 0, 0.4)
test_col <- rgb(0, 0, 1, 0.4)
x_params <- seq(2,7,1)

# Plot average deviance
plot(x_params, avg_train, col = "red", ylim = c(min(avg_train) - 5, max(avg_test) + 5), type = "p",
	ylab = "Avg. Deviance (300 trials)", xlab = "No. Parameters", pch=19)
lines(x_params, avg_train, col = train_col, lty=3)
points(x_params, avg_test, col = "blue", pch=19)
lines(x_params, avg_test, col = test_col, lty=3)
legend( 5, max(avg_test) + 5, c("Test", "Train"), lty = c(3,3), col=c(test_col,train_col))

# Plot 1st-3rd quartile deviance uncertainty
for (i in 1:ncol(train_sim)) {
	n_param = i + 1
	train_sig <- quantile(train_sim[,i], c(0.25, 0.75))
	test_sig <- quantile(test_sim[,i], c(0.25, 0.75))
	lines( c(n_param,n_param), c(train_sig[1],train_sig[2]), col = train_col)
	lines( c(n_param,n_param), c(test_sig[1],test_sig[2]), col = test_col)
}