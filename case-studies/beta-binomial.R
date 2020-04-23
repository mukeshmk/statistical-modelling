library("ggplot2")

n <- 20
theta <- 0.4
y <- rbinom(n, size = 1, prob = theta)
#print(y)

yhat <- mean(y)
yhat ## should be close to 0.4

a_0 <- b_0 <- 1
a_n <- sum(y) + a_0
b_n <- n - sum(y) + b_0

a_n / (a_n + b_n) ## expected value of theta -- should also be close to 0.4

x <- seq(0, 1, length.out= 100)
f_y1 <- dbeta(x, shape1 = a_0, shape2 = b_0)
f_y2 <- dbeta(x, shape1 = a_n, shape2 = b_n)
## Need to supply data to be plotted as a data frame
plot_dat <- data.frame(x, f_y1, f_y2)

## We'll build the plot in layers - first add the prior and posterior
p <- ggplot(data = plot_dat) + geom_line(aes(x, f_y1)) + geom_line(aes(x, f_y2), col = "red")
## Now the true value
p <- p + geom_vline(mapping = aes(xintercept = 0.4), col = "green") 
## Add text - note we use a different data frame here
p <- p + geom_text(data = data.frame(x = c(0.8, 0.3, theta), y = c(1.2, 3.5, 2)), aes(x, y, label = c("p(theta)", "p(theta | y)", "true value")), col = c("black", "red", "green") )
## Add title and axis labels
p <- p +  ggtitle("Posterior and prior comparison") + ylab("Posterior density") + xlab(expression(theta))  
p

pbeta(0.5, a_n, b_n, lower.tail = FALSE)

qbeta(c(0.025, 0.975), a_n, b_n)

################ MC Method
sim_theta <- rbeta(5000, a_n, b_n)

## estimate expected value of theta
mean(sim_theta)

mean( sim_theta > 0.5)
quantile(sim_theta, c(0.025, 0.975))

hist(sim_theta, freq=FALSE, main = "Histogram of Monte Carlo samples", xlab = expression(theta), ylab = "Posterior density")
lines(density(sim_theta))

