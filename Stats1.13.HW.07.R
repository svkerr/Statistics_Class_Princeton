# Stats1.13.HW.07.R
setwd("/Users/stuart/R_Files/Statistics Class")

# If necessary, install packages
# install.packages("psych")
# install.packages("ggplot2")
install.packages("multilevel")

# Load packages
library(psych)
library(ggplot2)
library(multilevel)

# Read data into a dataframe called data
data <- read.table("Stats1.13.HW.07.txt", header = T)
str(data)

# What is the correlation between extraversion and happiness?
# i.e., correlation analysis
cor(data[2:3])

# What is the correlation between extraversion and diversity of life experience?
cor(data[1:3])

# What percentage of variance in happiness is explained by extraversion?
# i.e., do regression analysis and look at R-Squared
model.he <- lm(data$happy ~ data$extra)
summary(model.he)

# What percentage of variance in happiness is explained by a model with both extraversion and diversity of life experience as predictors?
model.hed <- lm(data$happy ~ data$extra + data$diverse)
summary(model.hed)

# What is the 95% confidence interval for the regression coefficient for extraversion when it is the only predictor of happiness?
confint(model.he)

# What is the 95% confidence interval for the regression coefficient for extraversion when it and diversity of life experience are both predictors of happiness?
confint(model.hed)

# What is the unstandardized regression estimate of the indirect effect?
model.de <- lm(data$diverse ~ data$extra)
summary(model.de)

# What is the z-value of the Sobel test?
model.ALL <- sobel(data$extra, data$diverse, data$happy)
model.ALL


