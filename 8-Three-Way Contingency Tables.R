## ----(i) Arthritis dataset-----------------------------------------------
library(vcd)
View(Arthritis)

## ----(ii) 3-Way Table----------------------------------------------------
mytable <- xtabs(~ Treatment+Sex+Improved, data=Arthritis)
mytable

## ----(iii) Compact 3-Way Table-------------------------------------------
ftable(mytable) 

## ----(iv) 3-Way to 1-Way-------------------------------------------------
margin.table(mytable, 1)
margin.table(mytable, 2)
margin.table(mytable, 3)

## ----(v) 3-Way to 2-Way--------------------------------------------------
margin.table(mytable, c(1,3)) # 2-Way table of Treatment versus Improved
margin.table(mytable, c(2,3)) # 2-Way table of Sex versus Improved

## ----(vi) ftable() and prop.table()--------------------------------------
ftable(prop.table(mytable, c(1,2))) # Notice that each row in this table adds up to 1

## ----(vii)  ftable() and prop.table()------------------------------------
ftable(prop.table(mytable)) # Notice that all the cells in this table add up to 1

