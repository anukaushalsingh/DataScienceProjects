# Analysis of MBA SALARIES
# NAME: <Anurag Singh>
# EMAIL: <anukaushalsingh@gmail.com>
# COLLEGE / COMPANY: <Sir M.V.I.T / Wells Fargo Center Bangalore>

# R being in memory we first need to set the working directory.
setwd("C:/DataScienceInternship_IIMLSameerMathur/TasksProgress/MiniProject")
##for running in batch mode
##source("Anurag Singh Airline Code.R")
# Read the data in data frame
MBASal.df <- read.csv(paste("MBA Starting Salaries Data.csv", sep=""))
View(MBASal.df)
# direct output to a file 
sink("Anurag Singh MBASalaries Output.doc", append=TRUE, split=FALSE)
##sink()

# Summarize the data
attach(MBASal.df)
library(psych)
summary(MBASal.df)
## describe all columns
describe((MBASal.df))
## ----data types----------------------------------------------------------
# get the Data Types factor is data that is among fixedset of selections
str(MBASal.df)

library(car)
jpeg("Comparison of Salaries of Males and Females.jpg")	
boxplot(salary ~ sex, data=MBASal.df, horizontal=TRUE, ylab="Gender", xlab="Salary", main="Comparison of Salaries of Males and Females") 
## get the best fit line
abline(0,1)
dev.off()

jpeg("Salary vs Age.jpg")	
plot(~salary + age, main="Salary vs Age")
jpeg("Salary vs GMATScore.jpg")	
plot(~salary + gmat_tot, main="Salary vs GMATScore")
jpeg("Salary vs springavg.jpg")	
plot(~salary + s_avg, main="Salary vs springavg")
jpeg("Salary vs favg.jpg")	
plot(~salary + f_avg, main="Salary vs favg")
jpeg("Work Experience vs Salary.jpg")	
scatterplot(salary, work_yrs , main="Work Experience vs Salary", xlab="Salary", ylab="Work Experience")
jpeg("FirstLang vs Salary.jpg")	
scatterplot(salary, frstlang , main="FirstLang vs Salary", xlab="Salary", ylab="FirstLang")
jpeg("Age vs Salary.jpg")	
scatterplot(salary, age , main="Age vs Salary", xlab="Salary", ylab="Age")
jpeg("Spring Avg vs Salary.jpg")	
scatterplot(salary, s_avg , main="Spring Avg vs Salary", xlab="Salary", ylab="Spring Avg")
jpeg("Fall Avg vs Salary.jpg")	
scatterplot(salary, f_avg , main="Fall Avg vs Salary", xlab="Salary", ylab="Fall Avg")
dev.off()

library(Hmisc)
##sink("Anurag Singh Airline Output.doc", append=TRUE, split=FALSE)
colsalary <- c("salary","work_yrs","age","sex","gmat_tot","s_avg","f_avg")
corMatrix <- rcorr(as.matrix(MBASal.df[,colsalary]))
corMatrix

## ------------------------------------------------------------------------
library(Hmisc)
library(car)
library(corrgram)
colsalary <- c("salary","work_yrs","age","sex","gmat_tot","s_avg","f_avg")
jpeg("MBA Salary.jpg")
corrgram(MBASal.df[,colsalary], order=TRUE,
         main="MBA Salary",
         lower.panel=panel.pts, upper.panel=panel.pie,
         diag.panel=panel.minmax, text.panel=panel.txt)
dev.off()
##subset of people who actually got job considering salary not as 0
gotjob <- MBASal.df[ which(salary!=0) , ]
View(gotjob)



#creating models and regressions

## ------------------------------------------------------------------------
Model1 <- salary ~ work_yrs + age + s_avg + f_avg + gmat_tot + sex + frstlang
fit1 <- lm(Model1, data = gotjob)
summary(fit1)

library(leaps)
leap1 <- regsubsets(Model1, data = gotjob, nbest=1)
summary(leap1)
jpeg("MBASalariesLeap1.jpg")
plot(leap1, scale="adjr2")
dev.off()
Model2 <- salary ~ work_yrs + s_avg + f_avg + frstlang
fit2 <- lm(Model2, data = gotjob)
summary(fit2)

library(coefplot)
jpeg("MBASalariesCoefPlot.jpg")
coefplot(fit2, intercept= FALSE, outerCI=1.96,coefficients=c("salary","work_yrs", "s_avg", "f_avg", "frstlang"))
dev.off()

# the Adjusted R Squared for Model 2 is less than Model 1
summary(fit1)$adj.r.squared
summary(fit2)$adj.r.squared

# the AIC for Model 1 is less than Model 2
AIC(fit1)
AIC(fit2)

#T tests
t.test(salary,s_avg)

## ------------------------------------------------------------------------
t.test(salary, f_avg)

## ------------------------------------------------------------------------
t.test(salary, work_yrs)

t.test(salary, gmat_tot)

t.test(salary, frstlang)

t.test(salary, age)

sink()
