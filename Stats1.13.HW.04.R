# Statistics One HW Lab 4
setwd("/Users/stuart/R_Files/Statistics Class")
library(psych)
library(ggplot2)  # installing ggplot2 for my own plot compare and contrast

# Read data into a dataframe called data 
data = read.table("Stats1.13.HW.04.txt", header=T)

# What is the correlation between salary and years of professional experience?
# What is the correlation between salary and courses completed?
cor(PE[2:4])  # Omit col 1 because it contains participant id numbers
round(cor(PE[2:4]),2)   # Round off correlation to 2 decimal places

# What is the percentage of variance explained in a regression model with salary as the outcome
# variable and professional experience as the predictor variable?
model1 <- lm(data$salary ~ data$years)
summary(model1)

model2 <- lm(data$salary ~ data$courses)
summary(model2)

# Now let's include both predictors (years of professional experience and courses completed) in a regression model with
# salary as the outcome. Now what is the percentage of variance explained?
model3 <- lm(data$salary ~ data$years + data$courses)
summary(model3)

# What is the standardized regression coefficient for years of professional experience, predicting salary?
model1.z <- lm(scale(data$salary) ~ scale(data$years))
summary(model1.z)

model2.z <- lm(scale(data$salary) ~ scale(data$courses))
summary(model2.z)
# or maybe the question is in regards to the multiple linear regression problem previously posed?
# I will go with this one
model3.z <- lm(scale(data$salary) ~ scale(data$years) + scale(data$courses))
summary(model3.z)

data$model3_pred <- fitted(model3)
data$model3_res <- resid(model3)

# What is the mean of the salary distribution predicted by the model including both years of professional experience and courses completed as predictors? (with 0 decimal places)
mean(data$model3_pred)
describe(data$model3_pred)

# What is the mean of the residual distribution for the model predicting salary from both years of professional experience and courses completed? (with 0 decimal places)
mean(data$model3_res)
describe(data$model3_res)

# Are the residuals from the regression model with both predictors normally distributed?
hist(data$model3_res)