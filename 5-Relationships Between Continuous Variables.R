############################################
#  ### CONTINUOUS VARIABLES (a), (b), (c)###
#  ### Reference: R for Marketing Research and Analytics by Chapman and Feit, Chapter 4 ###
############################################


### (a) CONTINUOUS VARIABLES ###

############
#1 Load the CRM dataset
# ==========
cust.df <- read.csv(paste("4-CRMData.csv", sep=""))
#2 Review the CRM dataset
# ==========
View(cust.df)
str(cust.df)
#3 Summary Statistics of the CRM dataset
# ==========
summary(cust.df)
library(psych)
describe(cust.df)

#4 Converting cust.id to factors
# ==========
str(cust.df$cust.id)
cust.df$cust.id <- factor(cust.df$cust.id)
str(cust.df$cust.id)
str(cust.df)
help(factor)

#5a. Create a Basic Scatterplot using plot()
# ==========
plot(x=cust.df$age, y=cust.df$credit.score)

#5b. A better Scatterplot of credit.score versus age
# ==========
plot(cust.df$age, cust.df$credit.score, 
     col="blue",
     xlim=c(15, 55), ylim=c(500, 900), 
     main="Active Customers as of June 2014",
     xlab="Customer Age (years)", ylab="Credit Score ")

#5c. Add the sample means of credit.score and age to the Scatterplot
# ==========
abline(h=mean(cust.df$credit.score), col="dark blue", lty="dotted")
abline(v=mean(cust.df$age), col="dark blue", lty="dotted")

#5d. Add a regression line
# ==========
abline(lm(cust.df$credit.score ~ cust.df$age))

#6a. Do Customers who buy more online, buy less in stores?
#   Scatterplot with skew
# ==========
plot(cust.df$store.spend, cust.df$online.spend, 
     main="Customers as of June 2014", 
     xlab="Prior 12 months in-store sales ($)", 
     ylab="Prior 12 months online sales ($)", 
     cex=0.6)

#6b. Review the skew using describe() and hist()
# ==========
describe(cust.df$store.spend)

hist(cust.df$store.spend, 
     main="Histogram of Store Spending",
     xlim=c(0,400), ylim=c(0,450),  # add limits to the axes
     breaks=(0:ceiling(max(cust.df$store.spend)/10))*10,
     col=c("red","blue","green","yellow"),
     xlab="Prior 12 months online sales ($)" )

#6c. Review the skew using summary() and boxplot()
# ==========
summary(cust.df$store.spend)

boxplot(cust.df$store.spend, 
     main="Boxplot of Store Spending",
      col=c("yellow"),
      horizontal=TRUE,
      xlab="Prior 12 months online sales ($)" )




### (b) CONTINUOUS VARIABLES ###

#6d. Using logarithmic axes
# ==========
plot(cust.df$store.spend+1, cust.df$online.spend+1, 
     main="Customers as of June 2014", 
     xlab="Prior 12 months in-store sales ($)", 
     ylab="Prior 12 months online sales ($)", 
     cex=0.6
     )

plot(cust.df$store.spend, cust.df$online.spend, 
     main="Customers as of June 2014", 
     xlab="Prior 12 months in-store sales ($)", 
     ylab="Prior 12 months online sales ($)", 
     cex=0.6,
     log="xy")
## log of zero or a negative number is not defined.

plot(cust.df$store.spend+1, cust.df$online.spend+1, 
     main="Customers as of June 2014", 
     xlab="Prior 12 months in-store sales ($)", 
     ylab="Prior 12 months online sales ($)", 
     cex=0.6,
     log="xy")

#6e. Multi-panel plots
# ==========
par(mfrow=c(1, 3))
with(cust.df, plot(distance.to.store, store.spend,cex=0.5))
with(cust.df, plot(distance.to.store, online.spend,cex=0.5))
with(cust.df, plot(distance.to.store, store.spend+1,cex=0.5, log="xy"))
with(cust.df, plot(distance.to.store, online.spend+1,cex=0.5, log="xy"))
par(mfrow=c(1, 1))


#7. Effect of Email Promotions? 
#  Color-coding points; Adding a Legend
# ==========
my.col <- c("black", "red") 
my.pch <- c(1, 19) # R's symbols for solid and open circles (see ?points)
plot(cust.df$store.spend, cust.df$online.spend,
     cex=0.7,
     col=my.col[cust.df$email], pch=my.pch[cust.df$email], 
     main="Customers as of June 2014", 
     xlab="Prior 12 months in-store sales ($)", 
     ylab="Prior 12 months online sales ($)" )
     legend(x="topright", legend=paste("email on file:", levels(cust.df$email)), 
       col=my.col, pch=my.pch)

plot(cust.df$store.spend, cust.df$online.spend,
     log="xy",cex=0.7,
          col=my.col[cust.df$email], pch=my.pch[cust.df$email], 
          main="Customers as of June 2014", 
          xlab="Prior 12 months in-store sales ($)", 
          ylab="Prior 12 months online sales ($)" )
     legend(x="topright", legend=paste("email on file:", levels(cust.df$email)), 
            col=my.col, pch=my.pch)

     
     

          
### (c) CONTINUOUS VARIABLES ###
          
#8a. Scatterplot matrix using pairs()
# ==========
pairs(formula = ~ age + credit.score + online.spend, cex=0.6,
                  data=cust.df)
     
pairs(formula = ~ age + credit.score + distance.to.store + 
                  online.spend + store.trans + store.spend, cex=0.6,
                  data=cust.df)
     
#8b. Scatterplot matrix using scatterplotMatrix() from package car
# ==========
library(car)
scatterplotMatrix(formula = ~ age + credit.score + online.spend, cex=0.6,
                       data=cust.df, diagonal="histogram")

scatterplotMatrix(formula = ~ age + credit.score + email +
                       online.visits + online.trans + online.spend, cex=0.6,
                       data=cust.df, diagonal="histogram")

help(scatterplotMatrix)
#9a. Correlation
# ==========
cor(cust.df$age, cust.df$credit.score)

#9b. Is the correlation statistically significant?
# ==========
cor.test(cust.df$age, cust.df$credit.score)

#9c. Correlation matrix
# ==========
cor(cust.df[, c(2, 3, 5:10)])  # only numeric cols

#9d. Visualize correlation matrix
# ==========
library(corrplot)    # install if needed
corrplot(corr=cor(cust.df[ , c(2, 3, 5:12)], use="complete.obs"), 
         method ="ellipse")

library(gplots)      # for color interpolation
par(mfrow=c(1, 1))
corrplot.mixed(corr=cor(cust.df[ , c(2, 3, 5:12)], use="complete.obs"), 
               upper="ellipse", tl.pos="lt", 
               col = colorpanel(50, "red", "gray60", "blue4"))

#10. Jitter
# ==========
plot(cust.df$sat.service, cust.df$sat.selection, 
     xlab="Sat, Service", ylab="Sat, Selection")

plot(jitter(cust.df$sat.service), jitter(cust.df$sat.selection), 
     xlab="Sat, Service", ylab="Sat, Selection")


#11. Miscellaneous Additional Plots and Tables
# ==========

boxplot(cust.df$age, main="Customer Age",
              col=c("yellow"),horizontal=TRUE,
              xlab="Age (Years)" )

boxplot(cust.df$credit.score, main="Customers' Credit Score",
        col=c("yellow"),horizontal=TRUE,
        xlab="Credit Score" )

hist(cust.df$distance.to.store, 
     main="Distance to Nearest STore",
     breaks=(0:ceiling(max(cust.df$distance.to.store)/15))*15,
     col=c("red","blue","green","yellow"),
     xlab="Distance (miles)" )

boxplot(cust.df$distance.to.store, 
     main="Distance to Nearest Offline Store",
     horizontal=TRUE,
     col=c("yellow"),
     xlab="Distance (miles)" )

boxplot(cust.df$online.visits, 
        main="Visits to Retailer's Website",
        horizontal=TRUE,
        col=c("yellow"),
        xlab="Number of Visits" )


summary(cust.df$online.trans)
nonzerot <- cust.df$online.trans[cust.df$online.trans>0]
summary(nonzerot)

summary(cust.df$online.spend)
table(cust.df$online.spend==0)
nonzeros <- cust.df$online.spend[cust.df$online.spend>0]
summary(nonzeros)


summary(cust.df$store.trans)
table(cust.df$store.trans==0)
nonzerost <- cust.df$store.trans[cust.df$store.trans>0]
summary(nonzerost)

summary(cust.df$store.spend)
table(cust.df$store.spend==0)
nonzeross <- cust.df$store.spend[cust.df$store.spend>0]
summary(nonzeross)




