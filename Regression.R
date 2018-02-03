# Load required packages
install.packages("rpart")
install.packages("rpart.plot")
install.packages("ggplot2")
install.packages("plyr")
install.packages("reshape2")

library(rpart)
library(rpart.plot)
library(ggplot2)
library(plyr)
library(reshape2)

# Load data
adult <- read.csv("C:/ME/NYU/Fall Sem 2016 - I/Business Analytics/Assignments/Assignment 3/adult.data.csv", header = TRUE)
names(adult)
str(adult)
summary(adult$age)
summary(adult$education.number)
summary(adult$hours.per.week)
summary(adult)

# Classification Tree
# Income greater than or less than 50K
decision_tree <- rpart(salary ~ age + hours.per.week + education.number, method="class", data=adult)

# Detailed summary of the resulting decision tree
summary(decision_tree)

# Display the performance results
printcp(decision_tree)

# Visualize cross-validation results
plotcp(decision_tree)

# Print results
print(decision_tree)

# Output of the decision tree
rpart.plot(decision_tree, type = 1)

plot(decision_tree, uniform=TRUE, main="Classification Tree")
text(decision_tree, use.n=TRUE, all=TRUE, cex=.8)

rpart.plot(decision_tree)
