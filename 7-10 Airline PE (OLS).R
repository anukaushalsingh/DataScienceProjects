## ------------------------------------------------------------------------
airline.df <- read.csv(paste("SixAirlinesDataV2.csv", sep=""))
attach(airline.df)

## ------------------------------------------------------------------------
Model1 <- PricePremium ~ PriceEconomy + PitchDifference + WidthDifference + PercentPremiumSeats + SeatsTotal + IsInternational + TravelMonth + FlightDuration + Aircraft
fit1 <- lm(Model1, data = airline.df)
summary(fit1)

## ------------------------------------------------------------------------
library(leaps)
leap1 <- regsubsets(Model1, data = airline.df, nbest=1)
# summary(leap1)
plot(leap1, scale="adjr2")

## ------------------------------------------------------------------------
Model2 <- PricePremium ~ PriceEconomy + PitchDifference + WidthDifference + PercentPremiumSeats + SeatsTotal + FlightDuration + IsInternational
fit2 <- lm(Model2, data = airline.df)
summary(fit2)

## ------------------------------------------------------------------------
library(coefplot)
coefplot(fit2, intercept= FALSE, outerCI=1.96,coefficients=c("PriceEconomy","PitchDifference", "WidthDifference", "PercentPremiumSeats", "SeatsTotal", "FlightDuration"))

## ------------------------------------------------------------------------
# the Adjusted R Squared for Model 2 is less than Model 1
summary(fit1)$adj.r.squared
summary(fit2)$adj.r.squared

# the AIC for Model 2 is less than Model 1
AIC(fit1)
AIC(fit2)


