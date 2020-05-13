# library imports
library(mclust)
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
wine_df = within(wine_df, remove('province', 'region_1', 'region_2', 'winery', 'variety'))

# currently the data frame contains the following columns "country, points and price" in this order
# where X = {country and price} and Y = points
View(wine_df)

# this filters the data as per Q2-b - US Wine
# additionally filtering it to price less than 400 so as to avoid outliers
wine_us = filter(wine_df, country == 'US' & price < 100)
wine_us = na.omit(wine_us)
wine_us = within(wine_us, remove('country'))
View(wine_us)

wine_us <- wine_us[c("price", "points")]

# not required
# wine_us$price<-log(wine_us$price)
# wine_us$points<-wine_us$points/wine_us$price

# plot "points vs price" for wine in US
plot(wine_us)

fit1 <- Mclust(wine_us)

print(fit1)
summary(fit1)
plot(fit1, what = "classification")
plot(fit1, what = "BIC")
fit1$BIC
fit1$parameters$mean
fit1$parameters$pro

toy_data <- wine_us
toy_data <- toy_data+rnorm(n = nrow(toy_data), sd = 0.1)
plot(toy_data)

fit1_1 <- Mclust(toy_data)
plot(fit1_1, what = "classification")
fit1_1$parameters$mean
fit1_1$parameters$pro

BIC <- mclustBIC(wine_us)
plot(BIC)
summary(BIC)

fit2 <- Mclust(wine_us, G = 9, modelNames = "VVE")
print(fit2)
summary(fit2)
plot(fit2, what = "classification")
plot(fit2, what = "uncertainty")

fit3 <- Mclust(wine_us, G = 8, modelNames = "VVV")
print(fit3)
summary(fit3)
plot(fit3, what = "classification")
plot(fit3, what = "uncertainty")

fit4 <- Mclust(wine_us, G = 6, modelNames = "VVV")
print(fit4)
summary(fit4)
plot(fit4, what = "classification")
plot(fit4, what = "uncertainty")

fit5 <- Mclust(wine_us, G = 6, modelNames = "VVV", prior = priorControl())
print(fit5)
summary(fit5)
plot(fit5, what = "classification")
plot(fit5, what = "uncertainty")
