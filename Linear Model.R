####### Assignment 2 ########
     ##### Part 2 #####
## Submitted by Sanchita Anil Ubale ##

#read csv file
Progresso <- read.csv("C:/ME/NYU/Fall Sem 2016 - I/Business Analytics/Assignments/Assignment 2/Progresso_Soup_Hwk.csv", header = TRUE)
str(Progresso)

#Months are treated as integers. we can create new factor variable and assign it values of the months
#wintermonths <- factor(Progresso$Month, levels = c(10,11,12,1,2), labels = c("Oct", "Nov", "Dec", "Jan", "Feb"))
#Progresso$Month <- wintermonths

#str(Progresso)
#View(Progresso)
#table(wintermonths)

# Create dummy variables for each of the winter months
Progresso$Oct <- as.numeric(0)
Progresso$Nov <- as.numeric(0)
Progresso$Dec <- as.numeric(0)
Progresso$Jan <- as.numeric(0)
Progresso$Feb <- as.numeric(0)
str(Progresso)

# IF condition
for (i in 1:nrow(Progresso)) {
  if(Progresso$Month[i] == 10)
    Progresso$Oct[i] <- as.numeric(1)
  else if(Progresso$Month[i] == 11)
    Progresso$Nov[i] <- as.numeric(1)
  else if(Progresso$Month[i] == 12)
    Progresso$Dec[i] <- as.numeric(1)
  else if(Progresso$Month[i] == 1)
    Progresso$Jan[i] <- as.numeric(1)
  else if(Progresso$Month[i] == 2)
    Progresso$Feb[i] <- as.numeric(1)
}

View(Progresso)   


# Compute Market Share (as a percentage of total sales)
# Formula used: Market Share = (Price.Progresso/Sales.Progresso) * 100

wintermonths.subset <- subset(Progresso, Month == c(10,11,12,1,2))
wintermonths.subset

nonwintermonths.subset <- subset(Progresso, Month == c(3,4,5,6,7,8,9))
nonwintermonths.subset

# Compute for winter months
totalprice.wintermonths <- sum(wintermonths.subset$Price.Progresso + wintermonths.subset$Price.Campbell + wintermonths.subset$Price.PL)
totalsales.wintermonths <- sum(wintermonths.subset$Sales.Progresso + wintermonths.subset$Category_Sales)
market.share.winter <- ((totalprice.wintermonths*100)/totalsales.wintermonths)*100 # assuming the Price.Progresso is given as per 100 value
market.share.winter

# Compute for non-winter months
totalprice.nonwintermonths <- sum(nonwintermonths.subset$Price.Progresso + nonwintermonths.subset$Price.Campbell + nonwintermonths.subset$Price.PL)
totalsales.nonwintermonths <- sum(nonwintermonths.subset$Sales.Progresso + nonwintermonths.subset$Category_Sales)
market.share.nonwinter <- ((totalprice.nonwintermonths*100)/totalsales.nonwintermonths)*100 # assuming the Price.Progresso is given as per 100 value
market.share.nonwinter

# Linear Regression Model
sales_model<- lm(Sales.Progresso ~ Price.Progresso, data=Progresso)
summary(sales_model)
