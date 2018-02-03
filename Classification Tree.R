# Load required packages
install.packages("rpart")
install.packages("rpart.plot")
install.packages("ggplot2")
install.packages("plyr")
install.packages("reshape2")
install.packages("tree")

library(rpart)
library(rpart.plot)
library(ggplot2)
library(plyr)
library(reshape2)
library(tree)

# Load data
math <- read.csv("C:/ME/NYU/Fall Sem 2016 - I/Business Analytics/Assignments/Assignment 3/Math Grades Portugal.csv", header = TRUE)
names(math)
str(math)
summary(math)

# Classification Tree for student success
decision_tree <- rpart(G3 ~ absences + studytime + activities + failures + G1 + G2, method="class", data=math)

# Output of the decision tree
plot(decision_tree, uniform=TRUE, main="Classification Tree")
text(decision_tree, use.n=TRUE, all=TRUE, cex=.8)

rpart.plot(decision_tree, type = 1)


# Detailed summary of the resulting decision tree
summary(decision_tree)

# Display the performance results
printcp(decision_tree)

# Visualize cross-validation results
plotcp(decision_tree)

# Print results
print(decision_tree)


# Regression Tree
tree_regression <- tree(G3 ~ absences + studytime + activities + G2 + G1, data=math)
plot(tree_regression)
text(tree_regression, cex = .75)
tree_regression
summary(tree_regression)
print(tree_regression)

#Regression Model
regression_model <- lm(G3 ~ absences + studytime + activities + G2 + G1, data=math)
summary(regression_model)
