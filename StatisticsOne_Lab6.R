# Statistics One Lab 6
setwd("/Users/stuart/R_Files/Statistics Class")

# Lab goals
#   Read a datafile into R
#   Print summary statistics
#   Conduct correlation analyses
#   Conduct regression analyses, emphasis on models that include a categorical predictor variable

# Example:
#  A correlational study investigating predictors of salary among University faculty members
#  Outcome variable (Y) is annual salary in US Dollars ($)
#  Predictors (X) are age and number of years as a faculty member, number of publications, and academic department (History, Psychology, Sociology)
#  Sample size of N = 100
#  Analyses are then repeated with a sample size of N = 20
#
# If necessary install packages:
# install.packages("psych")

# Load packages
library(psych)

# Read data into a dataframe called PE (physical endurance)
FS <- read.table("Stats1.13.Lab.06.txt", header = T)
str(FS)
edit(FS)

# Summary statitics
describe (FS)

# Correlation Analysis
cor(FS[1:4])

# Regression Analysis - Unstandardized
# Model 0 demonstrates that age and years are, for the most part redundant, so we'll only use years in the next set of models:
model0 <- lm(FS$salary ~ FS$years + FS$age)
summary(model0)

model1 <- lm(FS$salary ~ FS$years)
summary(model1)
confint(model1)

model2 <- lm(FS$salary ~ FS$pubs)
summary(model2)
confint(model2)

model3 <- lm(FS$salary ~ FS$years + FS$pubs)
summary(model3)
confint(model3)

# Compare Model3 to both Model1 and Model2 to determine if including both predictors is best
anova(model1, model3)
anova(model2, model3)

# Now let's conduct regression analysis that includes a categorical predictor
# We need to use dummy codes to represent the nominal variable (dept) as numeric
# In R, there are several ways to do this, the following is just one example, using the function C (for contrasts)
dept.code <- C(FS$dept, treatment)  # Note that History becomes our reference due to alphabetical order
dept.code

model4 <- lm(FS$salary ~ FS$years + FS$pubs + (dept.code))
summary(model4)
confint(model4)

# Compare Model4 to Model3 to determine if including dept improves the predictions of the model
anova(model3, model4)

# Let's examine the salary difference between History and Sociology
# To quickly view the means, use the tapply funtion
tapply(FS$salary, FS$dept, mean)

# The actual means are not that different, so why are the means predicted by the model so different?
# There must be differences across departments in years and/or pubs
# Let's look at years
tapply(FS$years, FS$dept, mean)

# Let's look at pubs
tapply(FS$pubs, FS$dept, mean)

# So, the actual salary for Sociology is not that different from the other departments BUT they have more years on the job and more publications, on average, than the other departmnets, so their PREDICTED salary, based on an AVERAGE number of years and pubs is lower, which is a more accurate reflection of the discrepancies across departments

# My own investigation and model here:
model5<- lm(FS$salary ~ (dept.code))
summary(model5)