
# LINEAR REGRESSION 
# EXAMPLE:  PREDICT A WOMAN'S WEIGHT FROM HER HEIGHT

# A dataset called "women", in built in R, contains the heights and weights of 15 women
# Suppose we want to construct a model that will predict weight from height. 

# Having an equation for predicting weight from height can help us identify 
# overweight or underweight women.

#a. Dataset
data(women)

#b. 
women.df <- as.data.frame(women)

#c.
library(psych)
describe(women.df)

#d.
attach(women.df)

#e.
plot(height, weight, xlab="height (inches)", ylab="weight (lbs)")

#f. THE lm() FUNCTION IN R
# In R, the basic funciton for fitting a linear model is lm()

# The format is   fit <- lm(formula, data)

#     formula: describes the model to be fit
#     data: the dataframe containing the data to be used in fitting the model
#     fit: a list that contains extensive information about the fitted model
#     The formula is typically written as    Y ~ X1 + X2 + .. Xk
#     Y: Dependent Variable, also called Response Variable
#     Xi : Independent Variable(s), also called Predictor Variable(s)
#     ~  separates the Response Variable (Y) from the Predictor variables (X1, X2..)

#g. SIMPLE LINEAR REGRESSION
#  When the Regression Model contains one independent variable X
#  Regression Model:    Y = b0 + b1*X + e
#     which describes a line with slope b1 and y-intercept b0. 
#  b0, b1: beta coefficients
#  X: Independent Variable; Y: Dependent Variable

#h. In an experimental context we may have data points which reflect such a relationship 
#   between Y and X, but only approximately. 
#  Say there are n such points and call them {(Xi, Yi), i = 1, ..., n}. 
#  We can describe the approximate relation which exists between Yi and Xi 
#  by introducing an error term e to capture the deviation of the data from the model:

# Yi = b0 + b1*Xi + ei
# This relationship between the parameters and the data points is called a linear regression model
# and the ei are called "residuals".

#i. PREDICTING weight FROM height
#  weight: Dependent variable 
#  height: Independent variable
#  Model:    weight = b0 + b1*height
# The lm() function in R gets (weight, height) as input
#      and returns beta coefficients {b0, b1} as output 

fit <- lm(weight ~ height, data = women)
summary(fit)

#j.  Model:    weight = b0 + b1*height
#   b0 = -87.52,  b1 = 3.45
#  Model:    weight = -87.52 + 3.45*height

#k. BETA COEFFICIENTS coefficients()
fit$coefficients

#l. CONFIDENCE INTERVALS (95%)
# If repeated samples were taken and the 95% confidence interval was computed for each sample, 
# 95% of the intervals would contain the population mean. 
confint(fit)

# weight
women.df$weight

#m.  fitted()   Lists the predicted values in a fitted model
fitted(fit)

#n.  residuals()  Lists the residual values in a fitted model
# residuals are the vertical distances between the data and the fitted line 
# The Ordinary Least Squares (OLS) method minimizes the residuals.
# In OLS, the accuracy of a line through the sample data points is measured by the 
# sum of squared residuals, 
# and the goal is to make this sum as small as possible. 
residuals(fit)

#o. Statistical Significance and p-values
summary(fit)
# The regression coefficient (3.45) is significantly dfferent from zero (p < 0.001)
# There is an expected increase of 3.45 lbs of weight for every 1 inch increases in height.

#p. MULTIPLE R-SQUARED
# The multiple R-squared indciates that the model accounts for 99.1% of the variance in weights.

#q. F-STATISTIC
# The F-statistic tests whether the predictor variables, taken together,
# predict the response variable above chance levels.


