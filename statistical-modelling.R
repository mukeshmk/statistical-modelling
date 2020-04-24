rm(list = ls())

# library imports
library(ggplot2)
library(MCMCpack)

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

# The winemag-data_first150k.csv
# This dataset has the following colummns in this order
# country, description, designation, points, price, province,
# region_1, region_2, variety, winery

wine2_df = read.csv('data/winemag-data_first150k.csv')

# definitely unwanted variables for the modelling
wine2_df = within(wine2_df, remove('X', 'description', 'designation'))

# these variables might be useful in the future, but ignorning them for now.
wine2_df = within(wine2_df, remove('province', 'region_1', 'region_2', 'winery'))

# currently the data frame contains the following columns "country, points, price and variety" in this order
# where X = {country, price and variety} and Y = points
View(wine2_df)

# combining both the data frames
comb_wine_df <- rbind(wine_df, wine2_df)
View(comb_wine_df)

# this filters the data as per Q1, Sauvignon Blanc Wine from South Africa, Priced at $15
wine_sa_sb_15 <- comb_wine_df[comb_wine_df$country == "South Africa", ]
wine_sa_sb_15 <- wine_sa_sb_15[wine_sa_sb_15$variety == "Sauvignon Blanc", ]
wine_sa_sb_15 <- wine_sa_sb_15[wine_sa_sb_15$price == 15, ]
wine_sa_sb_15 = within(wine_sa_sb_15, remove('country', 'price'))
wine_sa_sb_15 = wine_sa_sb_15[rowSums(is.na(wine_sa_sb_15)) == 0, ]
View(wine_sa_sb_15)

# this filters the data as per Q1, Chardonnay Wine from Chile, Priced at $15
wine_ch_ch_15 <- comb_wine_df[comb_wine_df$country == "Chile", ]
wine_ch_ch_15 <- wine_ch_ch_15[wine_ch_ch_15$variety == "Chardonnay", ]
wine_ch_ch_15 <- wine_ch_ch_15[wine_ch_ch_15$price == 15, ]
wine_ch_ch_15 = within(wine_ch_ch_15, remove('country', 'price'))
wine_ch_ch_15 = wine_ch_ch_15[complete.cases(wine_ch_ch_15), ]
View(wine_ch_ch_15)

# combining the 2 data frames
comb_wine = rbind(wine_ch_ch_15, wine_sa_sb_15)

ggplot(comb_wine) + geom_boxplot(aes(variety, points, fill = variety)) +
  geom_jitter(aes(variety, points, shape = variety))


tapply(comb_wine$points, comb_wine$variety, mean)
tapply(comb_wine$points, comb_wine$variety, median)
tapply(comb_wine$points, comb_wine$variety, sd)

t.test(points ~ variety, data=comb_wine, var.equal = TRUE)


compare_2_gibbs <- function(y, ind, mu0 = 50, tau0 = 1/400, del0 = 0, gamma0 = 1/400, 
                            a0 = 1, b0 = 50, maxiter = 5000)
{
  y1 <- y[ind == 1]
  y2 <- y[ind == 2]
  
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

fit <- compare_2_gibbs(comb_wine$points, as.factor(comb_wine$variety))

plot(as.mcmc(fit))

raftery.diag(as.mcmc(fit))

apply(fit, 2, mean)
apply(fit, 2, sd)
## easier to interpret standard deviation than precision
mean(1/sqrt(fit[, 3])) 
sd(1/sqrt(fit[, 3]))


y1_sim <- rnorm(5000, fit[, 1] + fit[, 2], sd = 1/sqrt(fit[, 3]))
y2_sim <- rnorm(5000, fit[, 1] - fit[, 2], sd = 1/sqrt(fit[, 3]))

ggplot(data.frame(y_sim_diff = y1_sim - y2_sim)) + stat_bin(aes(y_sim_diff))

mean(y1_sim > y2_sim)
mean(y2_sim > y1_sim)

ggplot(data.frame(y1_sim, y2_sim)) + geom_point(aes(y1_sim, y2_sim), alpha = 0.3) + 
  geom_abline(slope = 1, intercept = 0)
