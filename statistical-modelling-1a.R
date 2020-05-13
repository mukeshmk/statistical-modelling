# library imports
library(ggplot2)
library(MCMCpack)
library(dplyr)

# dataset is taken from https://www.kaggle.com/zynicide/wine-reviews

# The winemag-data-130k-v2.csv dataset
# This dataset has the following colummns in this order
# country, description, designation, points, price, province, region_1, region_2,
# taster_name, taster_twitter_handle, title, variety, winery

wine_df = read.csv('data/winemag-data-130k-v2.csv')

# definitely unwanted variables for the modelling 
wine_df = within(wine_df, remove('X', 'description', 'designation', 'taster_name', 'taster_twitter_handle', 'title'))

# these variables might be useful in the future, but ignorning them for now.
wine_df = within(wine_df, remove('province', 'region_1', 'region_2', 'winery'))

# currently the data frame contains the following columns "country, points, price and variety" in this order
# where X = {country, price and variety} and Y = points
View(wine_df)

mean(wine_df$points)
sd(wine_df$points)

# this filters the data as per Q1, Sauvignon Blanc Wine from South Africa, Priced at $15
wine_sa_sb_15 = filter(wine_df, variety=='Sauvignon Blanc' & country == 'South Africa' & price == 15)
View(wine_sa_sb_15)

## set up ggplot for histogram and density plots
ggplot(wine_sa_sb_15, aes(x = points, ..density..)) +
## add histogram
  geom_histogram(bins = 5, aes(alpha = 0.5, colour = "blue"),  show.legend = FALSE) +
  geom_density(fill = "pink", aes(alpha = 0.5),  show.legend = FALSE) +
  xlab("Wine Points") + ylab("density") +
  ggtitle("Distribution of Points for Sauvignon Blanc")

# this filters the data as per Q1a, Chardonnay Wine from Chile, Priced at $15
wine_ch_ch_15 = filter(wine_df, variety=='Chardonnay' & country == 'Chile' & price == 15)
View(wine_ch_ch_15)

## set up ggplot for histogram and density plots
ggplot(wine_ch_ch_15, aes(x = points, ..density..)) +
## add histogram
  geom_histogram(bins = 5, aes(alpha = 0.5, colour = "blue"),  show.legend = FALSE) +
  geom_density(fill = "pink", aes(alpha = 0.5),  show.legend = FALSE) +
  xlab("Wine Points") + ylab("density") +
  ggtitle("Distribution of Points for Chardonnay")

# combining the 2 data frames
comb_wine = rbind(wine_ch_ch_15, wine_sa_sb_15)

ggplot(comb_wine) + geom_boxplot(aes(variety, points, fill = variety)) +
  geom_jitter(aes(variety, points, shape = variety))

# apllying the func "mean" to points sorted by variety
wine_means = tapply(comb_wine$points, comb_wine$variety, mean)
# removed the non-existent varities (don't know how they are still there)
wine_means[complete.cases(wine_means)]

# apllying the func "median" to points sorted by variety
wine_median = tapply(comb_wine$points, comb_wine$variety, median)
wine_median[complete.cases(wine_median)]

# apllying the func "sd" to points sorted by variety
wine_sd = tapply(comb_wine$points, comb_wine$variety, sd)
wine_sd[complete.cases(wine_sd)]

t.test(points ~ variety, data=comb_wine, var.equal = TRUE)

ts = replicate(1000,t.test(points ~ variety, data=comb_wine, var.equal = TRUE)$statistic)
range(ts)
pts = seq(-4.5,4.5,length=100)
plot(pts,dt(pts,df=18),col='red',type='l')
lines(density(ts))

compare_2_gibbs <- function(y, ind, mu0 = 88.44714, tau0 = 1/(3.03973^2), del0 = 0, gamma0 = 1/(3.03973^2),
                            a0 = 1, b0 = 50, maxiter = 10000)
{
  y1 <- y[ind == 'Chardonnay']
  y2 <- y[ind == 'Sauvignon Blanc']
  
  n1 <- length(y1) 
  n2 <- length(y2)
  
  ##### starting values
  mu <- (mean(y1) + mean(y2)) / 2
  del <- (mean(y1) - mean(y2)) / 2
  
  mat_store <- matrix(0, nrow = maxiter, ncol = 3)
  #####
  
  ##### Gibbs sampler
  an <- a0 + (n1 + n2)/2
  
  for(s in 1 : maxiter) 
  {
    
    ##update tau
    bn <- b0 + 0.5 * (sum((y1 - mu - del) ^ 2) + sum((y2 - mu + del) ^ 2))
    tau <- rgamma(1, an, bn)
    ##
    
    ##update mu
    taun <-  tau0 + tau * (n1 + n2)
    mun <- (tau0 * mu0 + tau * (sum(y1 - del) + sum(y2 + del))) / taun
    mu <- rnorm(1, mun, sqrt(1/taun))
    ##
    
    ##update del
    gamman <-  tau0 + tau*(n1 + n2)
    deln <- ( del0 * tau0 + tau * (sum(y1 - mu) - sum(y2 - mu))) / gamman
    del<-rnorm(1, deln, sqrt(1/gamman))
    ##
    
    ## store parameter values
    mat_store[s, ] <- c(mu, del, tau)
  }
  colnames(mat_store) <- c("mu", "del", "tau")
  return(mat_store)
}

# contains 5000 rows of 3 columns mu, del and tau (5000 x 3 matrix)
fit1 <- compare_2_gibbs(comb_wine$points, as.factor(comb_wine$variety), maxiter = 20000)
fit <- fit1[ 4 * (1 : 5000), ]

plot(as.mcmc(fit))

acf(fit)

raftery.diag(as.mcmc(fit))

# applying the func "mean" on each columns of the resultant matrix in fit
apply(fit, 2, mean)
# applying the func "sd" on each columns of the resultant matrix in fit
apply(fit, 2, sd)

## easier to interpret standard deviation than precision
mean(1/sqrt(fit[, 3])) 
sd(1/sqrt(fit[, 3]))


y1_sim <- rnorm(5000, mean = fit[, 1] + fit[, 2], sd = 1/sqrt(fit[, 3]))
y2_sim <- rnorm(5000, mean = fit[, 1] - fit[, 2], sd = 1/sqrt(fit[, 3]))

ggplot(data.frame(y_sim_diff = y1_sim - y2_sim)) + stat_bin(aes(y_sim_diff), binwidth = 1)

mean(y1_sim > y2_sim)
mean(y2_sim > y1_sim)

ggplot(data.frame(y1_sim, y2_sim)) + geom_point(aes(y1_sim, y2_sim), alpha = 0.3) + 
  geom_abline(slope = 1, intercept = 0)
