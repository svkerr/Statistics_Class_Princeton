setwd("/Users/stuart/R_Files/Statistics Class")

# Lab goals
#   Read a datafile into R
#   Print summary statistics
#   Conduct correlation analyses, including NHSTs
#   Conduct regression analyses, emphasis on standard error, confidence intervals, and model comparison 
# Example:
#  A correlational study investigating predictors of physical endurance in adults
#  Outcome variable (Y) is physical endurance
#  Predictors (X) are age and number of years actively engaged in exercise/sports
#  Initial analyses assume a sample size of N = 200
#  Analyses are then repeated with a sample size of N = 20
#
# If necessary install packages:
# install.packages("psych")
# install.packages("ggplot2")
# Load packages
library(psych)
library(ggplot2)

# Read data into a dataframe called PE (physical endurance)
PE <- read.table("Stats1.13.Lab.05.txt", header = T)

# Get summary statistics:
describe(PE)

# Illustration of standard error calculation
# Standard error = SD/sqrt(N)
table1 <- describe(PE)  # NOTE: we'll be extracting elements from summary tables going forward
table1
age.sd <- table1[2,4]
age.sd
age.n <- table1[2,2]
age.n
age.se = age.sd/sqrt(age.n)
age.se

# Correlation Analysis
cor(PE[2:4])

# NHST for each correlation coefficient
cor.test(PE$age, PE$activeyears)
cor.test(PE$endurance, PE$activeyears)
cor.test(PE$endurance, PE$age)

# Save the correlations in a table to illustrate calculation of regression coefficients
table2 <- cor(PE[2:4])

# Regression analyses, unstandardized
model1 <- lm(PE$endurance ~ PE$age)
summary(model1)

# Illustration of calculation of regression coefficient
# Regression coefficient = corr coeff * (stand dev of Y/ stand dev of X)
# B = r * (sdy/sdx)
table2
table2[3,1]
table1
model1.B <- table2[3,1] * table1[4,4]/table1[2,4]
model1.B

# Illustration of calculation of standard error of the regression coefficient
# SE = sqrt( (Sums of Squares.Residual / (N-2) )/ (Sums of Squares.X) )
# se.B = sqrt( (SS.resid/(N-2) )/SS.X))
table3 <- anova(model1)
table3
SS.resid <- table3[2,2]
SS.resid
df <- table3[2,1]
df
SS.X <- table3[1,2] + table3[2,2]
SS.X
se.B = sqrt( (SS.resid/df) / SS.X)
se.B
# Print 95% confidence interval for the regression coefficient
# NOTE: if the conf interval spans 0, it's an indication that result is not significant
#       and we will retain the null hypothesis
confint(model1)

# Illustration of calculation of confidence interval
# Upper value = B + (tcrit * se.B)
# Lower value = B - (tcrit * se.B)
# NOTE: We'll rely on R to calculate confidence intervals from now on, using: confint()
tcrit <- qt(c(.025, .975),df = 198)
tcrit
interval <- -0.08772 + (tcrit*se.B)
interval

# Scatterplot with confidence interval around the regression line
ggplot(PE, aes(x = age, y = endurance)) + geom_smooth(method = 'lm') + geom_point()

model2 <- lm(PE$endurance ~ PE$activeyears)
summary(model2)  # Remember the R-Squared coefficient tells you how much of the variance is explained by model
confint(model2)  # Prints 95% confidence intervals for the regression coefficients

ggplot(PE, aes(x= activeyears, y = endurance)) + geom_smooth(method = 'lm') + geom_point()

model3 <- lm(PE$endurance ~ PE$age + PE$activeyears)
summary(model3)
confint(model3)

# To visualize model3 (multiple regression), save the predicted scores as a new variable and
# then plot with endurance
PE$predicted <- fitted(model3)
ggplot(PE, aes(x= predicted, y = endurance)) + geom_smooth(method = 'lm') + geom_point()

# Conduct a model comparison NHST to compare the fit of model2 to fit of model3
# Is there a significant difference (measure by Pr(>F) value) between Model1 and Model2
anova(model2,model3)

# Regression analyses, standardized
# Thing to notice is that the intercept coefficient is close to 0
model1.z <- lm(scale(PE$endurance) ~ scale(PE$age))
summary(model1.z)
confint(model1.z)

model2.z <- lm(scale(PE$endurance) ~ scale(PE$activeyears))
summary(model2.z)
confint(model2.z)

model3.z <- lm(scale(PE$endurance) ~ scale(PE$activeyears) + scale(PE$age))
summary(model3.z)
confint(model3.z)

# Conduct a model comparison NHST to cmpare the fit of model2.z to fit of model3.z
# Note: will get same thing as before: F value and p value are same from unstandardized
anova(model2.z, model3.z)
anova(model2, model3)

### Now take a random subset of the data such that N = 20
PE.20 <- PE[sample(nrow(PE),20),]
# Summary statistics
describe(PE.20)

# Correlation analysis
round(cor(PE.20[2:4]),2)
cor.test(PE.20$age, PE.20$activeyears)
cor.test(PE.20$endurance, PE.20$activeyears)
cor.test(PE.20$endurance, PE.20$age)

# Regression analyses, unstandardized
model1.20 <- lm(PE.20$endurance ~ PE.20$age)
summary(model1.20)
confint(model1.20)

model2.20 <- lm(PE.20$endurance ~ PE.20$activeyears)
summary(model2.20)
confint(model2.20)

model3.20 <- lm(PE.20$endurance ~ PE.20$age + PE.20$activeyears)
summary(model3.20)
confint(model3.20)

# Conduct a model comparison NHST to cmpare the fit of model 2.20 and model 3.20
anova(model2.20, model3.20)