###################################
# 5-CF  ### COMPARING GROUPS: TABLES AND VISUALIZATION ###
############
# Read the data


#1 Load the Cable TV subscription dataset
# ==========
seg.df <- read.csv(paste("5-CableTVSubscribersData.csv", sep=""))
#2 Review the CRM dataset
# ==========
View(seg.df)
str(seg.df)
#3 Summary Statistics of the  Cable TV subscription dataset
# ==========
summary(seg.df)
library(psych)
describe(seg.df)
attach(seg.df)
############
#FINDING DESCRIPTIVES BY GROUP
############
#1 Descriptives: Selecting by group
# ==========
mean(seg.df$income[seg.df$Segment=="Moving up"])
mean(seg.df$income[seg.df$Segment == "Moving up" & 
                     seg.df$subscribe=="subNo"])

#2a Tables
# FREQUENCY or COUNT of factor level occurence by factor
# ==========
table(seg.df$Segment, seg.df$ownHome)
with(seg.df, table(Segment, ownHome))

#2b prop.table()
# ==========
prop.table(table(seg.df$Segment, seg.df$ownHome))
with(seg.df, prop.table(table(Segment, ownHome)))

## Adding a new column to a dataframe
#3a Add a column that has the segment mean income to the dataset
# ==========
seg.income.mean <- aggregate(seg.df$income, list(seg.df$Segment), mean)
seg.income.mean

#3b aggregate(): returns a data frame
# ==========
agg.data <- aggregate(income ~ Segment + ownHome, 
                      data=seg.df, mean)
View(agg.data)
str(agg.data)
agg.data[2, ]
agg.data[2, 3]

#4b Descriptives: apply a function by group using by()
# ==========
by(seg.df$income, seg.df$Segment, mean)
by(seg.df$income, list(seg.df$Segment, seg.df$subscribe), mean)
#5a Aggregate: use a formula!
# ==========
aggregate(income ~ Segment, data=seg.df, mean)
aggregate(income ~ Segment + ownHome, data=seg.df, mean)


# Create a new column called "segIncome" 
seg.df$segIncome <- seg.income.mean[seg.df$Segment, 2]
#6b Check the modified dataset
# ==========
library(car)
some(seg.df)
#6c Clear that variable
# ==========
seg.df$segIncome <- NULL
some(seg.df)







#8a Visualization of a Discrete Variable: Counts by Group
# ==========
library(lattice)
histogram(~subscribe | Segment, data=seg.df)
#8b A better Histogram
# ==========
histogram(~subscribe | Segment, data=seg.df, 
          type="count", 
          layout=c(4,1), 
          col=c("burlywood", "darkolivegreen"))
#8c Histograms by 2 factors
# ==========
# histogram of counts
histogram(~subscribe | Segment + ownHome, data=seg.df,
          type="count", 
          layout=c(4,2), 
          col=c("burlywood", "darkolivegreen"))
# histogram of percentages
histogram(~subscribe | Segment + ownHome, data=seg.df,
#          type="count", 
          layout=c(4,2), 
          col=c("burlywood", "darkolivegreen"))
#9a Continuous Data: "Spreadsheet" style
# ==========
seg.mean <- aggregate(income ~ Segment, data=seg.df, mean)
seg.mean
barchart(income ~ Segment, data=seg.mean, col="grey")
#9b Continuous data by two factors
# ==========
# split it by home ownership
seg.agg <- aggregate(income ~ Segment + ownHome, data=seg.df, mean)
seg.agg
barchart(income ~ Segment, data=seg.agg, 
         groups=ownHome, auto.key=TRUE,
         par.settings = simpleTheme(col=c("gray95", "gray50")) )
#9c Boxplots - Continuous Data: "Statistics" style
# ==========
library(lattice)
bwplot(Segment ~ income, data=seg.df, horizontal=TRUE, 
       xlab = "Income")
#9d Boxplots with two way grouping
# ==========
bwplot(Segment ~ income | ownHome, data=seg.df, 
       horizontal=TRUE, xlab="Income")


