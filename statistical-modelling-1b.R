# library imports
library(ggplot2)
library(dplyr)
library(sqldf)

# dataset is taken from https://www.kaggle.com/zynicide/wine-reviews

# The winemag-data-130k-v2.csv dataset
# This dataset has the following colummns in this order
# country, description, designation, points, price, province, region_1, region_2,
# taster_name, taster_twitter_handle, title, variety, winery

wine_df = read.csv('data/winemag-data-130k-v2.csv')

# definitely unwanted variables for the modelling 
wine_df = within(wine_df, remove('X', 'description', 'designation', 'taster_name', 'taster_twitter_handle', 'title'))

# these variables might be useful in the future, but ignorning them for now.
wine_df = within(wine_df, remove('province', 'region_2', 'winery'))

# currently the data frame contains the following columns "country, points, price and variety" in this order
# where X = {country, price and variety} and Y = points
View(wine_df)

# this filters the data as per Q1b, Italian Wine, Priced less than $20 with regions which have at least 4 reviews
wine_it_lt20 = filter(wine_df, country == 'Italy' & price < 20 & region_1 != "" & price != 0 & price != "")
wine_regs <- sqldf("SELECT region_1, count(*) FROM wine_it_lt20 GROUP BY region_1 HAVING count(*) >= 4")$region_1
wine_it_lt20 = wine_it_lt20[wine_it_lt20$region_1 %in% wine_regs, ]
wine_it_lt20 = na.omit(wine_it_lt20)
View(wine_it_lt20)

# this sqldf provides a list of unique region_1 in Italy and there count in the dataset.
region_count <- sqldf("SELECT region_1, count(*) FROM wine_it_lt20 GROUP BY region_1")
sum(region_count$`count(*)`)

ggplot(wine_it_lt20) + geom_boxplot(aes(x = reorder(region_1, points, median), points, 
                               fill = reorder(region_1, points, median)), show.legend=FALSE)

ggplot(wine_it_lt20, aes(x = reorder(region_1, region_1, length))) + stat_count()

ggplot(wine_it_lt20, aes(points)) + stat_bin()

ggplot(data.frame(size = tapply(wine_it_lt20$points, wine_it_lt20$region_1, length), 
                  mean_score = tapply(wine_it_lt20$points, wine_it_lt20$region_1, mean)), aes(size, mean_score)) + 
  geom_point() + xlab("Region sample size") + ylab("Mean Points") + 
  ggtitle("Effect size versus sample size")


compare_m_gibbs <- function(y, ind, mu0 = 50, tau0 = 1/400, 
                            a0 = 1, b0 = 50, alpha0 =1, beta0 = 50, maxiter = 5000)
{
  
  ### weakly informative priors
  a0 <- 1/2 ; b0 <- 50 ## tau_w hyperparameters
  alpha0 <-1/2 ; beta0 <- 50 ## tau_b hyperparameters
  mu0<-50 ; tau0 <- 1/25
  ###
  
  ### starting values
  m <- nlevels(ind)
  ybar <- theta <- tapply(y, ind, mean)
  tau_w <- mean(1 / tapply(y, ind, var)[tapply(y, ind, var) != 0]) ##within group precision
  mu <- mean(theta)
  tau_b <-var(theta) ##between group precision
  n_m <- tapply(y, ind, length)
  alphan <- alpha0 + sum(n_m)/2
  ###
  
  ### setup MCMC
  theta_mat <- matrix(0, nrow=maxiter, ncol=m)
  mat_store <- matrix(0, nrow=maxiter, ncol=3)
  ###
  
  ### MCMC algorithm
  for(s in 1:maxiter) 
  {
    
    # sample new values of the thetas
    for(j in 1:m) 
    {
      taun <- n_m[j] * tau_w + tau_b
      thetan <- (ybar[j] * n_m[j] * tau_w + mu * tau_b) / taun
      theta[j]<-rnorm(1, thetan, 1/sqrt(taun))
    }
    
    #sample new value of tau_w
    ss <- 0
    for(j in 1:m){
      ss <- ss + sum((y[ind == j] - theta[j])^2)
    }
    betan <- beta0 + ss/2
    tau_w <- rgamma(1, alphan, betan)
    
    #sample a new value of mu
    taum <- m * tau_b + tau0
    mum <- (mean(theta) * m * tau_b + mu0 * tau0) / taum
    mu <- rnorm(1, mum, 1/ sqrt(taum)) 
    
    # sample a new value of tau_b
    am <- a0 + m/2
    bm <- b0 + sum((theta - mu)^2) / 2
    tau_b <- rgamma(1, am, bm)
    
    #store results
    theta_mat[s,] <- theta
    mat_store[s, ] <- c(mu, tau_w, tau_b)
  }
  colnames(mat_store) <- c("mu", "tau_w", "tau_b")
  return(list(params = mat_store, theta = theta_mat))
}

fit2 <- compare_m_gibbs(wine_it_lt20$points, as.factor(as.numeric(wine_it_lt20$region_1)))

apply(fit2$params, 2, mean)

apply(fit2$params, 2, sd)

## within school standard variation
mean(1/sqrt(fit2$params[, 2]))

sd(1/sqrt(fit2$params[, 2]))

mean(1/sqrt(fit2$params[, 3]))

sd(1/sqrt(fit2$params[, 3]))

theta_hat <- apply(fit2$theta, 2, mean) ## get basic posterior summary
names(theta_hat) <- wine_regs ## keep track of different schools
sort(theta_hat, decreasing = TRUE) ## which schools did best and worst?

sort(theta_hat, decreasing = TRUE) > mean(wine_it_lt20$points)

theta_ci <- apply(fit2$theta, 2, quantile, prob = c(0.025, .975)) ## upper/lower bounds for thetas
df_error <- data.frame(lower = theta_ci[1, ], upper = theta_ci[2, ], mean = theta_hat, 
                       region_1 = factor(1:162))
ggplot(df_error, aes(x = reorder(region_1, mean), mean)) + geom_errorbar(aes(ymin = lower, ymax = upper))


## reformat samples for ggplot
theta_df <- data.frame(samples = as.numeric(fit2$theta), 
                       region_1 = rep(1:ncol(fit2$theta), each = nrow(fit2$theta))) 

ggplot(theta_df) + geom_boxplot(aes(x = reorder(region_1, samples, median), samples, 
                                    fill = reorder(region_1, samples, median)), show.legend=FALSE)

ggplot(data.frame(size = tapply(wine_it_lt20$points, as.factor(as.numeric(wine_it_lt20$region_1)), length), theta_hat = theta_hat), 
       aes(size, theta_hat)) + geom_point()

ggplot(data.frame(ybar = tapply(wine_it_lt20$points, as.factor(as.numeric(wine_it_lt20$region_1)), mean), theta_hat = theta_hat), 
       aes(ybar, theta_hat)) + geom_point()

