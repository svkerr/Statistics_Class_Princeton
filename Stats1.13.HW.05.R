getwd()
setwd("/Users/stuart/R_Files/Statistics Class")
# If necessary install packages:
# install.packages("psych")
# install.packages("ggplot2")
# Load packages
library(psych)
library(ggplot2)

# Read data into a dataframe
data <- read.table("Stats1.13.HW.04.txt", header = T)

# Get summary statistics:
describe(data)
#Run a regression model with salary as the outcome variable and years of experience as the predictor variable. What is the 95% confidence interval for the regression coefficient? Type your answer exactly as it appears in R but include only two decimal places (for example, if the 95% confidence interval is -1 to +1 then type -1.00 1.00)

model1 <- lm(data$salary ~ data$years)
summary(model1)
confint(model1)

# Run a regression model with salary as the outcome variable and courses as the predictor variable. What is the 95% confidence interval for the regression coefficient?
model2 <- lm(data$salary ~ data$courses)
summary(model2)
confint(model2)

# Run a multiple regression model with both predictors and compare it with both the model from Question 1 and the model from Question 2. Is the model with both predictors significantly better than:
model3 <- lm(data$salary ~ data$years + data$courses)
summary(model3)
confint(model3)
anova(model1, model3)
anova(model2, model3)

# Run a standardized multiple regression model with both predictors. Do the confidence interval values differ from the corresponding unstandardized model?

model3.z <- lm(scale(data$salary) ~ scale(data$years) + scale(data$courses))
confint(model3.z)

# Run the following command in R: set.seed(1). Now take a random subset of the original data so that N=15. Is the correlation coefficient between salary and years of experience in this sample higher or lower than in the whole data set?

set.seed(1)
data.15 <- data[sample(nrow(data),15),]
describe(data.15)
cor.test(data.15$salary, data.15$years)
cor.test(data$salary, data$years)

#Take a subset of the original data from row 51 to 70. What is the percentage of variance explained by a multiple regression model with both predictors (Provide your result with no decimal place)

data.51to70 <- subset(data, ID >= 51 & ID <= 70)
summary(data.51to70)
model51to70 <- lm(data.51to70$salary ~ data.51to70$years + data.51to70$courses)
summary(model51to70)

# Using model comparison, which model provides the best fit for the subsetted data from Question 7?
model1.subset <- lm(data.51to70$salary ~ data.51to70$years)
model2.subset <- lm(data.51to70$salary ~ data.51to70$courses)
model3.subset <- lm(data.51to70$salary ~ data.51to70$years + data.51to70$courses)
anova(model1.subset,model2.subset)
anova(model1.subset,model3.subset)
anova(model2.subset,model3.subset)

# What is the correlation between the salary values predicted by the multiple regression model and the actual salary scores in the subsetted data? (Provide your result rounded 
model51to70Pred <- fitted(model51to70)
cor.test(model51to70Pred, data.51to70$salary)

#Compute the correlation between the scores predicted by the multiple regression model and the residuals from the same model. Is the correlation statistically significant?
model51to70Res <- resid(model51to70)
cor.test(model51to70Pred, model51to70Res)