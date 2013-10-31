# Stats1.13.HW.06.R
setwd("/Users/stuart/R_Files/Statistics Class")

# If necessary install packages:
# install.packages("psych")

# Load packages
library(psych)

# Read data into a dataframe called PE (physical endurance)
PS <- read.table("Stats1.13.HW.06.txt", header = T)
str(PS)
edit(FS)

# Summary statitics
describe(PS)
# Correlation Analysis
cor(PS[2:4])
# In a model predicting salary, what is the unstandardized regression coefficient for years, assuming years is the only predictor variable in the model?
model1 <- lm(PS$salary ~ PS$years)
summary(model1)

# In a model predicting salary, what is the 95% confidence interval for the unstandardized regression coefficient for years, assuming years is the only predictor variable in the model?
confint(model1)

# In a model predicting salary, what is the unstandardized regression coefficient for years, assuming years and courses are both included as predictor variables in the model?
model2 <- lm(PS$salary ~ PS$years + PS$courses)
summary(model2)
confint(model2)

# What is the predicted difference in salary between Doctors and Lawyers assuming an equal and average number of years and courses?
## We need to use dummy codes to represent the nominal variable (dept) as numeric
prof.code <- C(PS$profession, treatment)  # Note that Doctors becomes our reference due to alphabetical order
prof.code
model3 <- lm(PS$salary ~ PS$years + PS$courses + (prof.code))
summary(model3)

## What is the actual difference in mean salary between Doctors and Teachers?
tapply(PS$salary, PS$profession, mean)

# What combination of predictors represents the best model in terms of predicting salary?
model2 <- lm(PS$salary ~ PS$years + PS$courses)
model3 <- lm(PS$salary ~ PS$years + PS$courses + (prof.code))
model4 <- lm(PS$salary ~ PS$years + (prof.code))
model5 <- lm(PS$salary ~ PS$courses + (prof.code))
summary(model2)
summary(model3)
summary(model4)
summary(model5)

