# Statistics One, 2013, Lab 9

# Lab Goals
#    Conduct a between groups factorial ANOVA
#    Example
#    A randomized controlled experiment designed to investigate the effects of talking on a cell phone while driving
#      DV = Number of driving errors
#      Two IVs
#         (A) Conversation difficulty (3 levels): Control, Easy, Difficult
#         (B) Driving difficulty (2 levels): Easy, Difficult

# If necessary, set your working directory
setwd("/Users/stuart/R_Files/Statistics Class")

library(psych)
library(car)
library(lsr)

# Read data into a dataframe called AB
AB <- read.table("Stats1.13.Lab.09.txt", header = T)
str(AB)
head(AB, n = 30L)

# Before conducting ANOVA, check for homogeniety of variance (homoscedasticity)
# Note the different form aov(DV ~ IV1 * IV2)  due to two IVs
# Since p-value is > 0.05 we don't reject the null hypoth and confirm that there is not a significant difference of variances between groups
leveneTest(AB$errors ~ AB$driving * AB$conversation)

# Conduct the factorial ANOVA
AB.model <- aov(AB$errors ~ AB$driving * AB$conversation)
summary(AB.model)

# Conduct simple effects analysis (of Conversation at each level of Difficulty)
# This is post analysis - we'v seen a big effect, let's probe deeper
# Treat conversation as an IV and fo a one way ANOVA
# We find there is still a signficant effect of conversation, even in easy driving case
AB1 <- subset(AB, AB$driving == 'Easy')
AB2 <- subset(AB, AB$driving == 'Difficult')

aov.AB1 <- aov(AB1$errors ~ AB1$conversation)
aov.AB2 <- aov(AB2$errors ~ AB2$conversation)
summary(aov.AB1)
summary(aov.AB2)

# We note that both levels of conversation have an impact on errors in both easy and difficult driving conditions, so why would we get interaction. Let's look at effect sizes:

etaSquared(aov.AB1, anova = T)
etaSquared(aov.AB2, anova = T)

# So, looking at era.sq.part we see that under easy driving conditions, conversation accounts for about 15% of the variance and when driving conditions are difficult, conversation accounts for 58% of variance.

# Finally, let's look at pairwise comparisons for the simple effects
TukeyHSD(aov.AB1)
TukeyHSD(aov.AB2)





