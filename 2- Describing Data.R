###################################
#  ### DESCRIBING DATA ###
############

#1) Read the data using read.csv
store.df <- read.csv(paste("3-StoreData.csv", sep=""))

#2) View the data frame in R
View(store.df)
############

#3a) Summarize the data using summary()
summary(store.df)

#3b) Summarize the data using describe()
library(psych)
describe(store.df)
describe(store.df$p1sales)
describe(store.df[ , c(2, 4:9)])

#4) Viewing a subset of a data frame
head(store.df)
tail(store.df)
library(car) 
some(store.df) #requires package car 

#5) Dimensions of a data frame
dim(store.df)

#6a) Summarizing a Discrete Variable
table(store.df$p1price)
table(store.df$p1prom)

#6b) 2-way Contingency Table for Discrete Variables
table(store.df$p1price, store.df$p1prom)

#7) Summarizing a Continuous Variable
min(store.df$p1sales)
max(store.df$p1sales)
mean(store.df$p1sales)
sd(store.df$p1sales)
summary(store.df$p1sales)






