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

# currently the data frame contains the following columns "country, points, price and variety" in this order
# where X = {country, price and variety} and Y = points
View(wine_df)

# this filters the data as per Q1b, Italian Wine, Priced less than $20 with regions which have at least 4 reviews
wine_us = filter(wine_df, country == 'US' & price < 1000)
wine_us = na.omit(wine_us)
wine_us = within(wine_us, remove('country'))
View(wine_us)

plot(wine_us)

fit <- Mclust(wine_us)

print(fit)
summary(fit)
plot(fit, what = "classification")
