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
library(ggplot2)  # installing ggplot2 for my own plot compare and contrast

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

## Histograms - to kinda check sense of cor.tests
layout(matrix(c(1,2,3,4),2,2,byrow = TRUE))
hist(PE$age)
hist(PE$activeyears)
hist(PE$endurance)
layout(matrix(c(1,1),1,1, byrow = TRUE))

## Regression Analyses, UNSTANDARDIZED
model1 <- lm(PE$endurance ~ PE$age)
summary(model1)
plot(PE$endurance ~ PE$activeyears, main = "Scatterplot", ylab = "Endurance", xlab = "Age")
abline(lm(PE$endurance ~ PE$age), col="blue")

model2 <- lm(PE$endurance ~ PE$activeyears)
summary(model2)
plot(PE$endurance ~ PE$activeyears, main = "Scatterplot" , ylab = "Endurance", xlab = "Active Years")
abline(lm(PE$endurance ~ PE$activeyears), col="blue")
## use ggplot for comparison
sp <- ggplot(PE,aes(x=activeyears,y=endurance, colour=age))
sp + geom_point(size=2.5)
# now add a regression line
sp + geom_point(size=2.5) + stat_smooth(method=lm, level=0.80)

model3 <- lm(PE$endurance ~ PE$age + PE$activeyears) # technically this is a multiple regression
summary(model3)

# To visualize model3, save the predicted scores as a new variable and then plot with endurance
# Plotting predicted scores takes in the multiple axes for predictors that cannot be plotted individually
PE$predicted <- fitted(model3)
plot(PE$endurance ~ PE$predicted, main = "scatterplot",ylab="Endurance",xlab="Model3 Predicted Scores")
abline(lm(PE$endurance ~ PE$predicted),col="blue")
# Save the model residuals
PE$e <- resid(model3)
hist(PE$e)
plot(PE$predicted ~ PE$e, main = "Scatterplot",ylab="Model3 Predicted Scores",xlab = "Model 3 Residuals")
abline(lm(PE$predicted ~ PE$e),col="blue")
# Using ggplot2
sp <- ggplot(PE,aes(x=PE$e,y=PE$predicted, colour=age))
sp + geom_point(size=2.5)

## Regression analyses, STANDARDIZED
# In simple regression, the standardized regression coefficient will be same as the correlation coefficient
round(cor(PE[2:4]),2)

model1.z <- lm(scale(PE$endurance) ~ scale(PE$age))
summary(model1.z)

model2.z <- lm(scale(PE$endurance) ~ scale(PE$activeyears))
summary(model2.z)

model3.z <- lm(scale(PE$endurance) ~ scale(PE$age) + scale(PE$activeyears))
summary(model3.z)