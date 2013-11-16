# Statistics One, 2013, Lab 7

# Lab goals
#   Conduct moderation and mediation analyses

# Segment 1
#   Moderation analysis
#     Example
#     An experimental research investigation of the effects of stereotype threat on intelligence testing 
#       Dependent variable (Y) is score on an intelligence test (IQ)
#       Independent variable (X) is the treatment condition (3 levels: control, explicit threat, implicit threat)
#       Moderator variable is score on a working memory task
#       Sample size of N = 150 (n = 50)

# Segment 2
#   Mediation analysis
#     Example
#       An experimental research investigation of the effects of stereotype threat on intelligence testing 
#         Dependent variable (Y) is score on an intelligence test (IQ)
#         Independent variable (X) is the treatment condition (2 levels: control, threat)
#         Mediator variable (M) is score on a working memory task
#         Sample size of N = 100 (n = 50)

# Check your working directory
getwd()
# If necessary, set your working directory
setwd("/Users/stuart/R_Files/Statistics Class")

# If necessary, install packages
# install.packages("psych")
# install.packages("ggplot2")
install.packages("multilevel")

# Load packages
library(psych)
library(ggplot2)
library(multilevel)

### Segment 1 (Moderation analysis)

# Read data into a dataframe called MOD
MOD <- read.table("Stats1.13.Lab.07.txt", header = T)
str(MOD)

# Summary Statistics
describeBy(MOD, MOD$condition)

# As a starter, is there an effect of stereotype threat?
model0 <- lm(MOD$IQ ~ MOD$D1 + MOD$D2)
summary(model0)
confint(model0)

# We could also use the aov function (Analysis of Variance) followed by the TukeyHSD fucntion (Tukey's test of pairwise comparisons, which adjusts the p value to prevent inflation of Type I error rate)
model0a <- aov(MOD$IQ ~ MOD$condition)
summary(model0a)
TukeyHSD(model0a)

# Moderation analysis (uncentered): model1 tests for "first-order effects"; model2 tests for moderation
# Recall: WM is our 'Z' variable from lecture video
model1 <- lm(MOD$IQ ~ MOD$WM + MOD$D1 + MOD$D2)
summary(model1)

ggplot(MOD, aes(x = WM, y = IQ)) + geom_smooth(method = "lm") + geom_point()

# Create new predictor variables
MOD$WM.D1 <- (MOD$WM * MOD$D1)
MOD$WM.D2 <- (MOD$WM * MOD$D2)

model2 <- lm(MOD$IQ ~ MOD$WM + MOD$D1 + MOD$D2 + MOD$WM.D1 + MOD$WM.D2)
summary(model2)
anova(model1,model2)

# Scatter plot by group and all in one
WM.control <- MOD$WM[1:50]
IQ.control <- MOD$IQ[1:50]
WM.threat1 <- MOD$WM[51:100]
IQ.threat1 <- MOD$IQ[51:100]
WM.threat2 <- MOD$WM[101:150]
IQ.threat2 <- MOD$IQ[101:150]

ggplot(MOD, aes(x = WM.control, y = IQ.control)) + geom_smooth(method = "lm") + geom_point()
ggplot(MOD, aes(x = WM.threat1, y = IQ.threat1)) + geom_smooth(method = "lm") + geom_point()
ggplot(MOD, aes(x = WM.threat2, y = IQ.threat2)) + geom_smooth(method = "lm") + geom_point()

color <- c("red","green","blue")
ggplot(MOD, aes(x=WM,y=IQ)) + stat_smooth(method = "lm",se=F) + geom_point(aes(color=condition))
ggplot(MOD, aes(x=WM,y=IQ)) + 
  geom_smooth(aes(group=condition), method="lm",se=T, color="black", fullrange=T) +
  geom_point(aes(color=condition))

### Segment 2 (Mediation Analysis)

# read data into a dataframe called MED



