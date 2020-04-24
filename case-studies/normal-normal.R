y <- rnorm(50, 5, sd = 10) ## N.B., rnorm takes sd, not variance or precision
y_bar <- mean(y)
s_hat <- sd(y)
print(c(y_bar, s_hat)) ## how accurate are the estimates?


hist(y, freq = FALSE)
lines(density(y))


mu_0 <- 0 
sigma_0 <- 100
tau_0 <- 1/sigma_0^2

tau <- 1/10^2
n <- length(y)

## compute posterior coefficients
tau_n <- n * tau + tau_0
mu_n <- (n * tau * y_bar + tau_0 * mu_0) / tau_n

print(c(mu_n, tau_n))

## Easier to interpret tau_n in terms of standard deviation
1/sqrt(tau_n)

mu_range <- qnorm( c(0.005, 0.995), mu_n, 1/sqrt(tau_n))
xseq <- seq(mu_range[1], mu_range[2], length.out = 100)
plot(xseq, dnorm(xseq, mu_n, 1/sqrt(tau_n)), type="l", xlab = expression(mu), main = "Posterior density for mu")
abline(v = 5) ## true value for mu

a0 <- 1
b0 <- 100
tau_range <- qgamma( c(0.005, 0.995), a0, rate = b0) ## N.B., you can define dgamma in terms of scale or rate parameter, make sure you specify the correct one!
xseq <- seq(tau_range[1], tau_range[2], length.out = 100)
plot(xseq, dgamma(xseq, a0, b0), type="l", xlab = expression(tau), main = "Prior for precision parameter")
abline(v = 0.04) ## true value of tau


## prior density plot for standard deviation sigma
tau_prior_samp <- rgamma(1000, shape = a0, rate = b0)
hist(1/sqrt(tau_prior_samp), main = "Prior for standard deviation", xlab = expression(sigma), freq = FALSE)
lines(density(1/sqrt(tau_prior_samp)))

mu <- 5
a_n <- n / 2 + a0
b_n <- b0 + 0.5 * sum( (y - mu)^2 )

tau_range <- qgamma( c(0.005, 0.995), a_n, rate = b_n) ## N.B., you can define dgamma in terms of scale or rate parameter, make sure you specify the correct one!
xseq <- seq(tau_range[1], tau_range[2], length.out = 100)
plot(xseq, dgamma(xseq,  a_n, rate = b_n), type="l", xlab = expression(tau), main = "Posterior for precision parameter")
abline(v = 0.01) ## true value of tau


## prior density plot for standard deviation sigma
tau_post_samp <- rgamma(1000,  a_n, rate = b_n)
hist(1/sqrt(tau_post_samp), main = "Posterior for standard deviation", xlab = expression(sigma), freq = FALSE)
lines(density(1/sqrt(tau_post_samp)))
abline(v = 10) ## true value of sd


