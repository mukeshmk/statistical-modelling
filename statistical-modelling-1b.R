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

# this sqldf provides a list of unique region_1 in Italy and there count in the dataset.
region_count <- sqldf("SELECT region_1, count(*) FROM wine_it_lt20 GROUP BY region_1")
sum(region_count$`count(*)`)

# this filters the data as per Q1b, Italian Wine, Priced less than $20 with regions which have at least 4 reviews
wine_it_lt20 = filter(wine_df, country == 'Italy' & price < 20 & region_1 != "")
wine_regs <- sqldf("SELECT region_1, count(*) FROM wine_it_lt20 GROUP BY region_1 HAVING count(*) > 4")$region_1
wine_it_lt20 = wine_it_lt20[wine_it_lt20$region_1 %in% wine_regs, ]
wine_it_lt20 = na.omit(wine_it_lt20)
View(wine_it_lt20)

total_wine_avg <- mean(wine_it_lt20$points)

wine_it_lt20$region_1 <- factor(wine_it_lt20$region_1)
region_split<-split(wine_it_lt20, wine_it_lt20$region_1)

mat_wine_reg <- matrix(0, nrow = 141, ncol = 2)

i <- 0
for(reg in region_split) {
  mat_wine_reg[i, ] <- c(levels(reg[1,4])[i], mean(reg[,2]))
  i <- i + 1
}
colnames(mat_wine_reg) <- c("region_1", "avg_points")
View(mat_wine_reg)

wine_gr_avg <- tibble(region = "", mean = "")
for(i in 1:141) {
  if (mat_wine_reg[i,2] > total_wine_avg) {
    wine_gr_avg <- rbind(wine_gr_avg, c(mat_wine_reg[i,1], mat_wine_reg[i,2]))
  }
}
View(wine_gr_avg)
