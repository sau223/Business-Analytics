####### Business Analytics Assignment ########
  ############ Homework 1 ################
## Submitted by: Sanchita Anil Ubale

             ##### Part 1 #####
      ##### Analytical Questions #####

# Loading the given data for Citibike usage
citi <- read.csv("C:/ME/NYU/Fall Sem 2016 - I/Business Analytics/Assignments/Week 1-2/CitiBike Data.csv", header = TRUE)

summary(citi$tripduration) # computing the summary statistics for tripduration

# Calculating the age from the birth year
date <- Sys.Date()
year <- format(date, format = "%Y")
citi.Age <- as.numeric(year) - citi$birth.year 

# Computing the summary statistics for age
summary(citi.Age)                  


citi.Tripduration.minutes <- citi$tripduration/60 # Converting the tripduration in minutes
summary(citi.Tripduration.minutes)                # Computing the summary statistics for tripduration in minutes


# Computing the correlation between age and tripduration
cor(citi.Age, citi.Tripduration.minutes) # when tripduration in minutes
cor(citi.Age, citi$tripduration)         # when tripduration in seconds


# plotting histogram and boxplot for tripduration for males
citi.male <- subset(citi, citi$gender == 1) # Creating subset having only males, assuming that the gender = 1 represents Male
par(mfrow = c(1,2))
hist(citi.male$tripduration, col = "red", 
     main = "Histogram displaying tripduration for Males", 
     xlab = "Tripduration") 
boxplot(citi.male$tripduration, col = "blue", 
        main = "Boxplot displaying tripduration for Males",
        xlab = "Tripduration", 
        ylab = "Frequency")



# plotting histogram and boxplot for tripduration for females
citi.female <- subset(citi, citi$gender == 2) # Creating subset having only females, assuming that the gender = 2 represents female
par(mfrow = c(1,2))
hist(citi.female$tripduration, col = "beige", 
     main = "Histogram displaying tripduration for Females", 
     xlab = "Tripduration") 
boxplot(citi.female$tripduration, col = "red", 
        main = "Boxplot displaying tripduration for Females",
        xlab = "Tripduration", 
        ylab = "Frequency")


# Calculating total revenue for users riding bikes from 0 to 45 minutes pay $3 per ride
tripduration.below.45.1 <- nrow(subset(citi, citi$tripduration/60 == 45)) # users riding for exact 45 minutes
tripduration.below.45.2 <- nrow(subset(citi, citi$tripduration/60 < 45)) # users riding for less than 45 minutes
tripduration.below.45 <- c (tripduration.below.45.1, tripduration.below.45.2) # concatenate the number of users
total.revenue.below.45 <- sum(tripduration.below.45) * 3 # sum the total number of users and multiply with the cost per ride
total.revenue.below.45 # display the total revenue


# Calculating total revenue for users exceeding 45 minutes pay an additional $2 per ride
tripduration.above.45 <- nrow(subset(citi, citi$tripduration/60 > 45)) # users riding for more than 45 minutes
total.revenue.above.45 <- tripduration.above.45 * 5 # multiply with the cost per ride that is $5
total.revenue.above.45 # display the total revenue


# Variance for tripduration
var(citi.Tripduration.minutes)
plot(citi.Tripduration.minutes)


# Average tripduration by gender
round(mean(citi.male$tripduration),2) # for male
round(mean(citi.female$tripduration),2) # for female


# Percentage of male and female in dataset
percent.male <- round(nrow(citi.male)/nrow(citi) * 100,2)
percent.male

percent.female <- round(nrow(citi.female)/nrow(citi) * 100,2)
percent.female


# Read energy usage of LA CSV file
energy <- read.csv("C:/ME/NYU/Fall Sem 2016 - I/Business Analytics/Assignments/Week 1-2/Average_monthly_residential_energy_usage_By_zip_code.csv",
                   header = TRUE)
summary(energy)

