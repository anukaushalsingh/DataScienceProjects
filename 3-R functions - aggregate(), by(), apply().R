###################################
#  ### READING AND VIEWING A DATASET ###
############


############

#1) Read the data using read.csv
store.df <- read.csv(paste("3-StoreData.csv", sep=""))

#2) View the data frame in R
View(store.df)
############

#8) Aggregate() and by()

# Total Sales of Product 2 by Country
aggregate(store.df$p2sales, by=list(country=store.df$country), sum)

# Average Sales of Project 1 by Store
aggregate(store.df$p1sales, by=list(StoreID = store.df$storeNum), mean)

# Average Sales of Project 1 by Store
by(store.df$p1sales, store.df$storeNum, mean)

# Average Sales of Project 1 by Store and Year (2001, 2002)
by(store.df$p1sales, list(store.df$storeNum, store.df$Year), mean)

#9) apply()
# Average of store.df columns 2-9
apply(store.df[, 2:9], MARGIN=2, FUN=mean)
apply(store.df[, 2:9], 2, mean)

# Standard Deviation of store.df columns 2-9
apply(store.df[, 2:9], 2, sd)

# Applying a User-defined function
apply(store.df[, 2:9], 2, function(x) { mean(x) - median(x) } )




