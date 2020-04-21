# dataset is taken from https://www.kaggle.com/zynicide/wine-reviews
# The dataset has the following colummns in this order
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
