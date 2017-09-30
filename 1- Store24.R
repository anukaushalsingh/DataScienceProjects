### ----read----------------------------------------------------------------
##R being in memory we need to get the data in data frame
# Read the data
store.df <- read.csv(paste("Store24.csv", sep=""))
View(store.df)

## ----summarize-----------------------------------------------------------
# Summarize the data
attach(store.df)
library(psych)
describe(store.df)


## ----data types----------------------------------------------------------
# Data Types
str(store.df)

## ----describe Profit MTenure CTenure-------------------------------------
library(psych)
describe(store.df$Profit)
describe(store.df$MTenure)
describe(store.df$CTenure)

## ----mean sd-------------------------------------------------------------
mean(store.df$Profit)
sd(store.df$Profit)
mean(store.df$MTenure)
sd(store.df$MTenure)
mean(store.df$CTenure)
sd(store.df$CTenure)
##-Sort data---
sorteddata <- store.df[order(Profit),]
head(sorteddata[c("store", "Sales","Profit", "MTenure","CTenure")],n=10)
tail(sorteddata[c("store", "Sales","Profit", "MTenure","CTenure")],n=10)
## ----histograms, echo=TRUE-----------------------------------------------
hist(store.df$MTenure, 
     breaks=18, 
     col="gray", 
     xlab="MTenure", 
     main="Manager Tenure")

hist(store.df$CTenure, 
     breaks=18, 
     col="gray", 
     xlab="CTenure", 
     main="Crew Tenure")

## ----scatterplots, echo=TRUE, warning=FALSE------------------------------
## => 3a.  Draw a scatter plot of Profit vs. MTenure

library(car)
scatterplot(Profit ~ MTenure, data=store.df,
            spread=FALSE, smoother.args=list(lty=2), pch=19,
            main="Scatterplot of Profit vs. MTenure",
            xlab="MTenure",
            ylab="Profit")

scatterplot(Profit ~ CTenure, data=store.df,
            spread=FALSE, smoother.args=list(lty=2), pch=19,
            main="Scatterplot of Profit vs. CTenure",
            xlab="CTenure",
            ylab="Profit")

## ------------------------------------------------------------------------
# => 4. What is the correlation of Profit with MTenure; What is the correlation of Profit with CTenure 

options(digits=2)
cor(store.df$Profit, store.df$MTenure)
cor(store.df$Profit, store.df$CTenure)

## ------------------------------------------------------------------------
cor.test(store.df[,"Profit"], store.df[,"MTenure"])
cor.test(store.df[,"Profit"], store.df[,"CTenure"])

## ------------------------------------------------------------------------
# => 4. Construct a Correlation Matrix for all variables in the dataset
options(digits=2)
cor(store.df, use="complete.obs", method="kendall") 

## ------------------------------------------------------------------------
#library(Hmisc)
#rcorr(store.df, type="pearson") # type can be pearson or spearman

## ---- warning=FALSE------------------------------------------------------
library(car)
scatterplotMatrix(store.df[,c("Profit","MTenure","CTenure")], 
                  spread=FALSE, smoother.args=list(lty=2),
                  main="Scatter Plot Matrix")

## ---- warning=FALSE------------------------------------------------------
library(car)
scatterplotMatrix(store.df[,c("Profit","MTenure","CTenure","Comp","Pop","PedCount")], 
                  spread=FALSE, smoother.args=list(lty=2),
                  main="Scatter Plot Matrix")

## ------------------------------------------------------------------------
install.packages("corrgram")
library(corrgram)
corrgram(store.df, order=FALSE, 
         lower.panel=panel.shade,
         upper.panel=panel.pie, 
         diag.panel=panel.minmax,
         text.panel=panel.txt,
         main="Corrgram of store.df intercorrelations")

## ------------------------------------------------------------------------
m1 <- lm(Profit ~ 
           MTenure 
         + CTenure 
         + Pop
         + PedCount 
         + Res
         + Visibility
         + Hours24 
         + Comp, 
         data=store.df)
summary(m1)

m1$coefficients

## ------------------------------------------------------------------------
# beta coefficients
m1$coefficients

## ------------------------------------------------------------------------
# confidence intervals
confint(m1)

## ------------------------------------------------------------------------
# Visualize 
library(coefplot)
# 1. MTenure and CTenure are statistically significant
coefplot(m1, predictors=c("MTenure","CTenure"))

# 2. Visibility is NOT statistically significant.
# We infer this since its confidence interval includes zero within it.
coefplot(m1, predictors=c("Visibility"))

# 3. The other factors are statistically significant
coefplot(m1, predictors=c("Pop", "PedCount", "Res","Hours24","Comp"))

## ------------------------------------------------------------------------
# Compare the Profit with the fitted values 

# Here is the actual profit
store.df$Profit

## ------------------------------------------------------------------------
# Here is the Profit, as predicted by the OLS model
fitted(m1)

# Compare profit predicted by the model with the actual profit given in the data
predictedProfit = data.frame(fitted(m1)) 
actualProfit = data.frame(store.df$Profit)
profitComparison = cbind(actualProfit, predictedProfit)
View(profitComparison)


## ------------------------------------------------------------------------

# See the current data types
str(store.df)

# convert PedCount into factor variable 
store.df$PedCount <- factor(store.df$PedCount)

# convert Visibility into factor variable 
store.df$Visibility <- factor(store.df$Visibility)

# convert Hours24 into factor variable 
store.df$Hours24 <- factor(store.df$Hours24)

# convert Res into factor variable 
# firstly, replace the int values 0 and 1 with the text values 'Ind' and 'Res'
store.df$Res[store.df$Res == 0] <- 'Industrial'
store.df$Res[store.df$Res == 1] <- 'Residential'
# secondly, convert them into a Factor
store.df$Res <- factor(store.df$Res)

# check that the data types have changed to factor
str(store.df)

## ------------------------------------------------------------------------
# Res       : Factor w/ 2 levels "Industrial","Residential"
table(store.df$Res)

# Hours24   : Factor w/ 2 levels "0","1"
table(store.df$Hours24)

# PedCount  : Factor w/ 5 levels "1","2","3","4","5"
table(store.df$PedCount)

# Visibility: Factor w/ 4 levels "2","3","4","5"
table(store.df$Visibility)

## ------------------------------------------------------------------------
m2 <- lm(Profit ~ 
           MTenure 
         + CTenure 
         + Pop
         + PedCount 
         + Res
         + Visibility
         + Hours24 
         + Comp, 
         data=store.df)
summary(m2)

## ------------------------------------------------------------------------
m1$coefficients
m2$coefficients

