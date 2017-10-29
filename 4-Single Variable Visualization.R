###################################
#  ### SINGLE VARIABLE VISUALIZATION ###
############


############

#1) Read the data using read.csv
store.df <- read.csv(paste("3-StoreData.csv", sep=""))

#2) View the data frame in R
View(store.df)
############

############
# SINGLE VARIABLE VISUALIZATION
############

# 10) HISTOGRAMS

# 10a) Draw a Histogram
hist(store.df$p1sales)

# 10a) Add Labels to a Histogram
hist(store.df$p1sales, 
     main="Product 1 Weekly Sales Frequencies, All Stores",    # add labels
     xlab="Product 1 Sales (Units)",
     ylab="Count" )           

# 10b) Choose how many breaks
hist(store.df$p1sales, 
     main="Product 1 Weekly Sales Frequencies, All Stores",
     xlab="Product 1 Sales (Units)",
     ylab="Count",
     breaks=10,             # more columns 
     col="peachpuff")       # color the bars

# 10c) Choosing colors
colors()

# 10d) Add limits to the axes
hist(store.df$p1sales, 
     main="Product 1 Weekly Sales Frequencies, All Stores",
     xlab="Product 1 Sales (Units)",
     ylab="Count",
     xlim=c(50,200), ylim=c(0,500),  # add limits to the axes
     breaks=10,             # more columns 
     col="lightblue")       # color the bars

# 10e) Density
hist(store.df$p1sales, 
     main="Product 1 Weekly Sales Frequencies, All Stores",
     xlab="Product 1 Sales", ylab="Relative frequency",
     breaks=30, col="lightblue", freq=FALSE)

lines(density(store.df$p1sales, bw=10),  # bw = smoothing
      type="l", col="darkred", lwd=2)    # lwd = line width


      
############
# 11) BOXPLOTS

# 11a) Draw a BoxPlot
boxplot(store.df$p2sales)

# 11b) Add Labels
boxplot(store.df$p2sales, xlab="Weekly sales", ylab="P2",
        main="Weekly sales of P2, All stores", horizontal=TRUE)

# 11c) Generate Box Plots for each store branch
boxplot(store.df$p2sales ~ store.df$storeNum, horizontal=TRUE,
     ylab="Store", xlab="Weekly unit sales", las=1,
     main="Weekly Sales of P2 by Store")

# 11d) Colors
boxplot(store.df$p2sales ~ store.df$storeNum, horizontal=TRUE,
        ylab="Store", xlab="Weekly unit sales", las=1,
        main="Weekly Sales of P2 by Store",
        col=c("red","blue","green","yellow")
        )

# 11e) Do in-store promotions increase sales?
boxplot(p2sales ~ p2prom, data=store.df, horizontal=TRUE, yaxt="n", 
     ylab="P2 promoted in store?", xlab="Weekly sales",
     main="Weekly sales of P2 with and without promotion")
axis(side=2, at=c(1,2), labels=c("No", "Yes"))





