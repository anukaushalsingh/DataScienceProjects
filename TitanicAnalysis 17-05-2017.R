### TITANIC SURVIVAL ANALYSIS

#1 Read the Titanic dataset and save it inside a data frame called "titanic"
titanic <- read.csv(paste("Titanic Data.csv", sep=""))

#2 Summary Statistics of the data
summary(titanic)
library(psych)
describe(titanic)
attach(titanic)

str(titanic)

#3 ANALYZE WHO AND HOW MANY SURVIVED

# 3a. Total Number of Passengers
dim(titanic)

# 3b. Number of Passengers who survived
survivedTable <- table(titanic$Survived)
survivedTable

# 3c. Percentage of Passengers who surivied
100*prop.table(survivedTable) # proportions
# 3c. Alternate soluton
summary(titanic$Survived)

# 3d.  Number of 1st Class Passengers Who Survived?
surviversByClass <- xtabs(~ Survived+Pclass, data=titanic)
surviversByClass # frequencies
addmargins(surviversByClass)

# 3e.  Percentage of 1st Class Passengers Who Survived?
prop.table(surviversByClass, 2) # column proportions

# 3f.  Percentage of 3rd Class Who Survived?
prop.table(surviversByClass, 2) # column proportions

# 3g. Number of Females from 1st Class who survived
myt <- xtabs(~ Survived+Pclass+Sex, data=titanic)
addmargins(myt)
ftable(myt) 


surviversBySex <- xtabs(~ Survived+Sex, data=titanic)
surviversBySex # frequencies
addmargins(surviversBySex)

# 3h. Percentage of Surivers who were Female
prop.table(surviversBySex,1)
# Ans: 67.9% 

# 3i. Percentage of total females on the Titanic who survived
prop.table(surviversBySex,2)
# Ans: 74.0%

# 3j. Chi Square Test : percentage of female survivers was higher than percentage of male survivers
chisq.test(surviversBySex)



