## ------------------------------------------------------------------------
library(vcd)

## ----(i) Arthritis dataset-----------------------------------------------
library (vcd)
head(Arthritis)

## ----(ii) 2-Way Table----------------------------------------------------
mytable <- xtabs(~Treatment+Improved, data=Arthritis)
addmargins(mytable)

## ----(iii) Pearson-------------------------------------------------------
chisq.test(mytable)

## ----(iv) 2-way table----------------------------------------------------
library(vcd)
mytable <- xtabs(~Improved+Sex, data=Arthritis)
addmargins(mytable)

## ----(v) Pearson---------------------------------------------------------
chisq.test(mytable)

