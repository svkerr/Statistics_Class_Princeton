setwd("/Users/stuart/R_Files/Statistics Class")

#Lab goals
# Read a datafile into R
# Print summary statistics
# Conduct correlation analysis
# Conduct regression analyses, unstandardized
# Conduct regression analyses, standardized

# Example
# A correlational study investigating predictors of physical endurance in adults
#   Outcome variable (Y) is physical endurance
#   Predictors (X) are age and number of years actively engaged in exercise/sports
#   Sample size is N = 200

# If necessary install packages
install.packages("psych")
library(psych)
# Read data into a dataframe called PE (physical endurance)
PE = read.table("Stats1.13.Lab.04.txt", header=T)
# Summary statistics
describe(PE)

# Correlation Analysis
cor(PE[2:4])  # Omit col 1 because it contains participant id numbers
round(cor(PE[2:4]),2)   # Round off correlation to 2 decimal places

cor.test(PE$age, PE$activeyears)
cor.test(PE$endurance, PE$activeyears)
cor.test(PE$endurance, PE$age)

# Histograms - to kinda check sense of cor.tests
layout(matrix(c(1,2,3,4),2,2,byrow = TRUE))
hist(PE$age)
hist(PE$activeyears)
hist(PE$endurance)
layout(matrix(c(1,1),1,1, byrow = TRUE))

# Regression Analyses, unstandardized
model1 <- lm(PE$endurance ~ PE$age)
summary(model1)
plot(PE$endurance ~ PE$activeyears, main = "Scatterplot", ylab = "Endurance", xlab = "Active Years")
abline(lm(PE$endurance ~ PE$age), col="blue")