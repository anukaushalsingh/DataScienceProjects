## ------------------------------------------------------------------------
library(vcd)

## ----(vi) Fisher---------------------------------------------------------
mytable <- xtabs(~Treatment+Improved, data=Arthritis)
fisher.test(mytable)

## ----(vii) Measure of Association----------------------------------------
library(vcd)
mytable <- xtabs(~Treatment+Improved, data=Arthritis)
addmargins(mytable)
assocstats(mytable)

