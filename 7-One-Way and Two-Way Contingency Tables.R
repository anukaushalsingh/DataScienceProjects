## ----Summarize-----------------------------------------------------------
library(vcd) #this package contains the Arthritis dataset used here
View(Arthritis) # view the Arthritis dataset
library(psych)
describe(Arthritis$Age) # Summary Statistics of the columns in the Arthritis dataset

## ----(i) One-Way Table of Frequency--------------------------------------
mytable <- with(Arthritis, table(Improved))
mytable  # frequencies

## ----(ii) Proportions----------------------------------------------------
prop.table(mytable) # proportions
prop.table(mytable)*100 # percentages

## ----(iii) xtabs---------------------------------------------------------
mytable <- xtabs(~ Treatment+Improved, data=Arthritis)
mytable # frequencies

## ----(iv) Proportions----------------------------------------------------
margin.table(mytable,1) #row sums
prop.table(mytable, 1) # row proportions

## ----(v) Proportions-----------------------------------------------------
margin.table(mytable, 2) # column sums
prop.table(mytable, 2) # column proportions

## ----(vi) Cell Proportions-----------------------------------------------
prop.table(mytable) # cell proportions

## ----(vii) Sums----------------------------------------------------------
addmargins(mytable) # add row and column sums to table

## ----(viii) Cell Proportions---------------------------------------------
addmargins(prop.table(mytable))

## ----(ix) CrossTable-----------------------------------------------------
library(gmodels) # ensure that this package is installed
CrossTable(Arthritis$Treatment, Arthritis$Improved)

