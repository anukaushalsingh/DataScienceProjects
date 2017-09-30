###################################
#  A DEAN'S DILEMMA: SELECTION OF STUDENTS FOR THE MBA PROGRAM
############
setwd("C:/DataScienceInternship_IIMLSameerMathur/TasksProgress/CapstoneProject")

### Read the data
mba <- read.csv(paste("deans_dilemma.csv", sep=""))
View(mba)

### Attach the dataframe
attach(mba)

### Summarize the data
summary(mba)
library(psych)
describe(mba)

### Q1. What is the median salary of all the students?
median(mba$Salary)

### Q2. What percentage of students were placed? Answer rounding off to 2 decimal places.
summary(mba$Placement)
100*mean(mba$Placement_B)


### Q3. Create a dataframe called "placed" having the subset of students who were placed
placed <- mba[ which(mba$Placement=='Placed') , ] 

### Q4. What is the median salary of students who were placed, excluding those who were not placed?
### Method 1:
median (mba$Salary[mba$Salary > 0] )
### Method 2:
median(placed$Salary)

### Q5. Considering placed students, create a table giving the mean salary of males and females?
### Method 1: Using aggregate()
aggregate(placed$Salary, by=list(Sex = placed$Gender), mean)
### Method 2: Using by()
by(placed$Salary, placed$Gender, mean)


### Q6. Create a histogram showing a breakup of the MBA performance of the 
### students who were placed
hist(placed$Percent_MBA, 
     main="MBA Performance of placed students",
     xlab="MBA Percentage",
     ylab="Count",
     breaks=2,        # more columns 
     col="grey")       # color the bars


### Q7. Create a dataframe called notplaced, containing only those students 
### who were not placed.    
notplaced <- mba[ which(mba$Placement=='Not Placed') , ] 


### Q8. Draw two histograms side-by-side, showing the MBA performance of Placed 
### and Not Placed students
par(mfrow=c(1, 2))
hist(placed$Percent_MBA, 
     main="MBA Performance of placed students",
     xlab="MBA Percentage",
     ylab="Count",
     breaks=2,        # more columns 
     col="grey")       # color the bars
hist(notplaced$Percent_MBA, 
     main="MBA Performance of not placed students",
     xlab="MBA Percentage",
     ylab="Count",
     breaks=2,        # more columns 
     col="grey")       # color the bars
par(mfrow=c(1, 1))



### Q9. Draw two boxplots, one below the other, comparing the salaries of males 
### and female MBAs who were placed

boxplot(placed$Salary ~ placed$Gender, data=placed, horizontal=TRUE, yaxt="n", 
        ylab="Gender", xlab="Salary",
        main="Comparison of Salaries of Males and Females")
axis(side=2, at=c(1,2), labels=c("Females", "Males"))


### Q10. Create a dataframe called placedET, representing students who were placed
### after the MBA and who also gave some MBA entrance test 
### before admission into the MBA program.  
#### Method 1:
placedET <- placed[ which(placed$S.TEST==1) , ] 
#### Method 2:
placedET <- placed[ which(placed$Entrance_Test != "None") , ] 
View(placedET)


### Q11. Draw a Scatter Plot Matrix for 3 variables 
### {Salary, Percent_MBA, Experience_Yrs} using placedET.
library(car)
scatterplotMatrix(
  placedET[
    ,c("Salary","Percent_MBA","Percentile_ET")], 
  spread=FALSE, smoother.args=list(lty=2),
  main="Scatter Plot Matrix")

