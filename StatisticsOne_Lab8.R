# Statistics One, 2013, Lab 8

# Lab goals
#   Conduct group comparisons
#     Dependent t-tests
#     Independent t-tests
#     Analysis of Variance (ANOVA)

# Example
#  Working memory training experiment (N = 120)
#  The dependent variable (DV) is number of items answered correctly on an intelligence test
#  There are three independent variables:
#    Time (2 levels): pre and post training
#    Training (2 levels): training (1) and control (0) (n.training = 80, n.control = 40)
#    Training sessions (4 levels): 8, 12, 17, 19 (for each, n = 20)

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
wm <- read.table("Stats1.13.Lab.08.txt", header = T)
str(wm)

# Summary statistics by all groups (control, 8 sessions, 12, 17, 19)
describeBy(wm,wm$cond)

# Create two subsets of data: One for the control group and another for the training groups
wm.c <- subset(wm, wm$train == "0")
wm.t <- subset(wm, wm$train == "1")

# Save summary statistics in tables to illustrate calculation of effect size
wm.c.out <- describe(wm.c)
wm.c.out
wm.t.out <- describe(wm.t)
wm.t.out

### Dependent t-tests

# First, compare pre and post scores in the control group (note - same group == dep t-test)
t.test(wm.c$post, wm.c$pre, paired = T) # recall paired=T means a dep t-test

# Next, compare pre and post scores in the training groups
t.test(wm.t$post, wm.t$pre, paired = T)

# Let's look at the effect size since we have two different sample sizes for the control and trained groups
# Cohen's d for dependent t-tests
# d = Mean of difference scores / Standard dev of difference scores

d.c <- (wm.c.out[4,3]/wm.c.out[4,4])
d.c
# or cohensD:
cohensD(wm.c$post, wm.c$pre, method = "paired")

d.t <- (wm.t.out[4,3])/wm.t.out[4,4]
d.t
cohensD(wm.t$post, wm.t$pre, method = "paired")

# Boxplots
long.wm <- melt(wm, id=c("cond", "train", "gain"))
ggplot(long.wm, aes(x=cond, y=value, color=variable)) + geom_boxplot() + guides(fill=FALSE)

### Independent t-test
# Compare the gain scores in the control and training groups
# Recall that when assuming homogeniety of variance, var.equal = T
t.test(wm$gain ~ wm$train, var.equal = T) 

# Cohen's d for independent t-tests
# d = (M1 - M2)/Pooled Standard Deviation

df.c <- wm.c.out[1,2] - 1
df.c
df.t <- wm.t.out[1,2] -1
df.t
df.tot <- df.c + df.t
df.tot

pooled.sd <- ((df.t/df.tot) * wm.t.out[4,4]) + ((df.c/df.tot) * wm.c.out[4,4])

d.ct <- (wm.t.out[4,3] - wm.c.out[4,3])/pooled.sd
d.ct
# or:
cohensD(wm$gain ~ wm$train, method = "pooled")

# To compare the gain scores across all groups, use ANOVA
# First check the homogeniety of variance assumption (before doing ANOVA for many groups)
# leveneTest(DV, IV, ...)
# We find not a sign't difference of variance across groups 
leveneTest(wm.t$gain, wm.t$cond, center="mean")
leveneTest(wm.t$gain, wm.t$cond)

# aov(DV, IV)
aov.model <- aov(wm.t$gain ~ wm.t$cond)
summary(aov.model)

# save results in a table to illustrate calculation of effect size

