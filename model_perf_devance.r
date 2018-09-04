# We will simulate data with known model of 3 params
# End goal is to compare best model estimates between train/test set
# given varying # of params

library(rethinking)

ntrials = 30
train_sim = data.frame(matrix(NA, nrow = ntrials, ncol = 5))
test_sim = data.frame(matrix(NA, nrow = ntrials, ncol = 5))

N <- 50
x1 <- runif(N,-10, 5)
x2 <- runif(N,-5,10)
y <- rnorm(N, mean = .15*x1 - 0.4*x2, sd = 1)
x1.s <- (x1 - mean(x1))/sd(x1)
x2.s <- (x2 - mean(x2))/sd(x2)
y <- (y - mean(y))/sd(y)
d <- data.frame(y = y, x1 = x1.s, x2 = x2.s)

for (j in 1:ntrials) {

	d <- d[sample(1:nrow(d)),]
	train <- d[1:(N/2),]
	test <- d[(N/2 + 1):N,]

	# 2 params
	m1 <- alist(
		y ~ dnorm(mu, sig),
		mu <- a + b1*x1,
		a ~ dnorm(0,.2),
		b1 ~ dnorm(0,.2),
		sig ~ dunif(0,3))

	map_m1 <- map(m1, data = train)

	theta1 <- coef(map_m1)
	devtrain1 <- -2*sum(dnorm(
		train$y,
		mean = theta1[1] + theta1[2]*train$x1,
		sd = theta1[3],
		log = TRUE))

	devtest1 <- -2*sum(dnorm(
		test$y,
		mean = theta1[1] + theta1[2]*test$x1,
		sd = theta1[3],
		log = TRUE))

	# 3 params
	m2 <- alist(
		y ~ dnorm(mu, sig),
		mu <- a + b1*x1 +b2*x2,
		a ~ dnorm(0,.05),
		b1 ~ dnorm(0,.3),
		b2 ~ dnorm(0,.3),
		sig ~ dunif(0,1))

	map_m2 <- map(m2, data =  train)

	theta2 <- coef(map_m2)
	devtrain2 <- -2*sum(dnorm(
		train$y,
		mean = theta2[1] + theta2[2]*train$x1 + theta2[3]*train$x2,
		sd = theta2[4],
		log = TRUE))

	devtest2 <- -2*sum(dnorm(
		test$y,
		mean = theta2[1] + theta2[2]*test$x1 + theta2[3]*test$x2,
		sd = theta2[4],
		log = TRUE))

	# 4 params
	m3 <- alist(
		y ~ dnorm(mu, sig),
		mu <- a + b1*x1 +b2*x2 + b3*x1^2,
		a ~ dnorm(0,.05),
		b1 ~ dnorm(0,.3),
		b2 ~ dnorm(0,.3),
		b3 ~ dnorm(0,.05),
		sig ~ dunif(0,1))

	map_m3 <- map(m3, data =  train)

	theta3 <- coef(map_m3)
	devtrain3 <- -2*sum(dnorm(
		train$y,
		mean = theta3[1] + theta3[2]*train$x1 + theta3[3]*train$x2 + theta3[4]*(train$x1)^2,
		sd = theta3[5],
		log = TRUE))

	devtest3 <- -2*sum(dnorm(
		test$y,
		mean = theta3[1] + theta3[2]*test$x1 + theta3[3]*test$x2 + theta3[4]*(test$x1)^2,
		sd = theta3[5],
		log = TRUE))

	# 5 params
	m4 <- alist(
		y ~ dnorm(mu, sig),
		mu <- a + b1*x1 +b2*x2 + b3*(x1^2) + b4*(x2^2),
		a ~ dnorm(0,.05),
		b1 ~ dnorm(0,.3),
		b2 ~ dnorm(0,.3),
		b3 ~ dnorm(0,.05),
		b4 ~ dnorm(0,.05),
		sig ~ dunif(0,1))

	map_m4 <- map(m4, data =  train)

	theta4 <- coef(map_m4)
	devtrain4 <- -2*sum(dnorm(
		train$y,
		mean = theta4[1] + theta4[2]*train$x1 + theta4[3]*train$x2 + theta4[4]*(train$x1)^2 + theta4[5]*(train$x2)^2,
		sd = theta4[6],
		log = TRUE))

	devtest4 <- -2*sum(dnorm(
		test$y,
		mean = theta4[1] + theta4[2]*test$x1 + theta4[3]*test$x2 + theta4[4]*(test$x1)^2 + theta4[5]*(test$x2)^2,
		sd = theta4[6],
		log = TRUE))

	# 6 params
	m5 <- alist(
		y ~ dnorm(mu, sig),
		mu <- a + b1*x1 +b2*x2 + b3*(x1^2) + b4*(x2^2) + b5*(x1^3),
		a ~ dnorm(0,.05),
		b1 ~ dnorm(0,.3),
		b2 ~ dnorm(0,.3),
		b3 ~ dnorm(0,.05),
		b4 ~ dnorm(0,.05),
		b5 ~ dnorm(0,.05),
		sig ~ dunif(0,1))

	map_m5 <- map(m5, data =  train)

	theta5 <- coef(map_m5)
	devtrain5 <- -2*sum(dnorm(
		train$y,
		mean = theta5[1] + theta5[2]*train$x1 + theta5[3]*train$x2 + theta5[4]*(train$x1)^2 + theta5[5]*(train$x2)^2 + theta5[6]*(train$x1)^3,
		sd = theta5[7],
		log = TRUE))

	devtest5 <- -2*sum(dnorm(
		test$y,
		mean = theta5[1] + theta5[2]*test$x1 + theta5[3]*test$x2 + theta5[4]*(test$x1)^2 + theta5[5]*(test$x2)^2 + theta5[6]*(test$x1)^3,
		sd = theta5[7],
		log = TRUE))

	train_dev <- c(devtrain1, devtrain2, devtrain3, devtrain4, devtrain5)
	test_dev <- c(devtest1, devtest2, devtest3, devtest4, devtest5)

	train_sim[j,] = train_dev
	test_sim[j,] = test_dev
}

avg_train <- apply(train_sim, 2, mean)
avg_test <- apply(test_sim, 2, mean)

train_col <- rgb(1, 0, 0, 0.4)
test_col <- rgb(0, 0, 1, 0.4)
x_params <- seq(2,6,1)

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