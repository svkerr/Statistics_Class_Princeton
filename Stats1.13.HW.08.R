# Statistics One, Homework 8

# Welcome to week 8 assignment! 
# We now return to our cognitive training example. In this week's dataset, we compare the impact of 
# three training conditions (1. Working Memory training, 2. Physical Exercise, and 3. Designed Sport) 
# Dependent Variable: Spatial Reasoning (SR), 
# Measured before (pre) and after (post) training.

# Check your working directory
getwd()
# If necessary, set your working directory
setwd("/Users/stuart/R_Files/Statistics Class")

# If necessary, install packages
# install.packages("psych")
install.packages("car")

# Load packages
library(psych)
library(car)
library(lsr)
library(ggplot2)
library(reshape)

# Read data into a dataframe called wm
data <- read.table("Stats1.13.HW.08.txt", header = T)
str(data)

# Summary statistics by all groups (WM training, Physical Exercise, Designed Sport)
describeBy(data,data$condition)

# Question 1: Using a dependent t-test, is the difference between pre and post-test scores significant?
# # Create two subsets of data: One for the pretest group and another for the posttest groups
pre <- subset(data, data$time == 'pre')
post <- subset(data, data$time == 'post')

t.test(post$SR,pre$SR, paired = T)

# Question #2: Create subsets for each training condition (WM, PE, DS). Which group shows no difference between pre and post-test scores
wm <- subset(data, condition == 'WM')
ds <- subset(data, condition == 'DS')
pe <- subset(data, condition == 'PE')

wm.pre <- subset(wm, wm$time == 'pre')
wm.post <- subset(wm, wm$time == 'post')

ds.pre <- subset(ds, ds$time == 'pre')
ds.post <- subset(ds, ds$time == 'post')

pe.pre <- subset(pe, pe$time == 'pre')
pe.post <- subset(pe, pe$time == 'post')

t.test(wm.post$SR, wm.pre$SR, paired = T)
t.test(ds.post$SR, ds.pre$SR, paired = T)
t.test(pe.post$SR, pe.pre$SR, paired = T)

# Question #3: Which training group shows the largest effect size for the difference pre-test to post-test?
# Let's use the cohensD test

cohensD(wm.post$SR, wm.pre$SR, method = "paired")
cohensD(ds.post$SR, ds.pre$SR, method = "paired")
cohensD(pe.post$SR, pe.pre$SR, method = "paired")

# Question #4: Reshape the data into a wide format, and create a new variable for gain score. Now subset the new dataframe based on the training conditions. Which comparison between training conditions does not show a significant difference?
# Note: research on reshape library is required here.
data.wide <- cast(data, subject + condition ~ time)
data.wide
data.wider <- transform(data.wide, gain=post-pre)
data.wider

wm.w <- subset(data.wider, condition == 'WM')
ds.w <- subset(data.wider, condition == 'DS')
pe.w <- subset(data.wider, condition == 'PE')

t.test(wm.w$gain,pe.w$gain, var.equal= T)
t.test(pe.w$gain, ds.w$gain, var.equal = T)
t.test(ds.w$gain, wm.w$gain, var.equal = T)

#Question #5: To compare the gain scores across all groups, we now turn to ANOVA. Is the homogeneity of variance assumption violated?
leveneTest(data.wider$gain, data.wider$condition)
leveneTest(data.wider$gain, data.wider$condition, center="mean")

#Question #6: Run an ANOVA model on the gain scores as a function of training condition. Is the effect of condition significant?
aov.model <- aov(data.wider$gain ~ data.wider$cond)
summary(aov.model)

# Question #7: What is the corresponding eta-squared value? (round to 2 decimal places)
# etasq = proportion of variance in gain accounted for by our treatment
etaSquared(aov.model, anova=T)

# Question #8: Are the eta-squared and partial eta-squared value different in this case? 
# See results from etaSquared() from Question #7

# Question #9:et's now run post-hoc comparisons (Tukey HSD). Which two groups do not significantly differ from one another when considering gain scores?
TukeyHSD(aov.model)
