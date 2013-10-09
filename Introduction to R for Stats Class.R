3 + 4
5 * 5
12 /7
5 ^ 5
# Create a vector
v = c(1,3,5,7)
# Create a list - a vector with different classes of objects
l = c("Blue",2,5,"Red")
m = matrix(1:6,2,3)
# create a matrix by binding columns or rows
x=1:6
y=5:10
cbind(x,y)
rbind(x,y)
df = data.frame(subjectID=1:5,gender=c("M","F","F","M","F"),score=c(8,3,6,5,5))
## Statistics Lab 2
# Lab Goals
# Read a datafile into R
# Learn about object types
# Print summary statistics
# Examine distributions using Histograms

# Check working directory
getwd()
# Set working directory
setwd("/Users/stuart/R_Files/Statistics Class")
# If necessary, install packages:
install.packages("sm")
install.packages("psych")
# Load packages:
library(psych)
library(sm)
# To identify loaded packages:
search()
installed.packages()
# Read data into a dataframe called "impact"
# Note: go to course website to download datafiles
impact <- read.table("stats1-datafiles-Stats1.13.Lab.02.txt", header = T)

## Get dimensions of impact dataframe
str(impact)
dim(impact)
nrow(impact)
ncol(impact)

## Object types
class(impact)
names(impact)

class(impact$visual_memory_baseline)
class(impact$reaction_time_baseline)
class(impact$subject)
## reclassify subject from integer to factor
impact$subject <- factor(impact$subject)
class(impact$subject)

## summary statistics
mean(impact$verbal_memory_baseline)
sd(impact$verbal_memory_baseline)
describe(impact)  ## very nice summary statistics function
describeBy(impact,impact$condition)

## getting subsets of data (subsetting)
control <- subset(impact,impact[, 2] == "control")
concussed <- subset(impact,impact[, 2] == "concussed")

## Histogram of control group at baseline
par(mfrow = c(2,3)) ## to view 6 histograms on one page
hist(control[,3], xlab = "Verbal memory", main = "")
hist(control[,4], xlab = "Visual memory")
hist(control[,5], xlab = "Visual motor speed")
hist(control[,6], xlab = "Reaction time")
hist(control[,7], xlab = "Impulse time")
hist(control[,8], xlab = "Total symptom score")

## Can repeat the above for the concussed at baseline
## and both control and concussed at retest

## To demonstrate that there is more than 1 way to do histograms
par(mfrow = c(1,2))  ## to view 2 histograms on one page
hist(control[,3], xlab = "Verbal memory", main = "")
hist(control$verbal_memory_baseline, xlab = "Verbal memory", main = "")

## Density Plots
par(mfrow = c(1,2))
hist(concussed[,14], xlab = "Total symptom score", main = "")
plot(density(concussed[,14]), xlab = "Total symptom score", main = "")

## Compare density plots
par(mfrow = c(1,1))
sm.density.compare(impact$total_symptom_retest, impact$condition, 
                   xlab = "Total symptom score")

################
## Assignment #2
memory <- read.table("Stats1.13.HW.02.txt", header = T)
str(memory)
mean(memory$SR)
variance <- mean(memory$SR)**2
variance
# What is the mean of SR for all subjects at pretest?
pretest <- subset(memory,memory[,3]== "pre")
pretest$time
mean(pretest$SR)
# What is the standard deviation of SR for all subjects at posttest?
postest <- subset(memory,memory[,3]== "post")
postest$time
sd(postest$SR)
# What is the median of SR for all subjects at posttest?
describe(postest)
# Which group (WM, PE or DS) has the highest mean at posttest?
# Assuming this has to do with SR
describeBy(postest,postest$condition)
# Which one best approximates a normal distribution?
# Need to create subsets of my subsets
pre_ds <- subset(pretest,pretest[,2] == "DS")
pre_pe <- subset(pretest,pretest[,2] == "PE")
pre_wm <- subset(pretest,pretest[,2] == "WM")
post_ds <- subset(postest,postest[,2] == "DS")
post_pe <- subset(postest,postest[,2] == "PE")
post_wm <- subset(postest,postest[,2] == "WM")

par(mfrow = c(2,3)) ## to view 6 histograms on one page
hist(pre_ds[,4], xlab = "pre_ds", main = "")
hist(pre_pe[,4], xlab = "pre_pe", main = "")
hist(pre_wm[,4], xlab = "pre_wm", main = "")
hist(post_ds[,4], xlab = "post_ds", main = "")
hist(post_pe[,4], xlab = "post_pe", main = "")
hist(post_wm[,4], xlab = "post_wm", main = "")

#  Which group showed the biggest gains in SR?
sum(post_ds[,4]) - sum(pre_ds[,4])
sum(post_pe[,4]) - sum(pre_pe[,4])
sum(post_wm[,4]) - sum(pre_wm[,4])
############################
## End Assignment #2
########################################################
## Begin Assignment #3)
#########################################################
cogsport <- read.table("Stats1.13.HW.03.txt", header = T)
str(cogsport)
# What is the correlation between S1 and S2 pre-training?
# Note: S1.pre is col 3 and S2.pre is col 4
cor(cogsport$S1.pre,cogsport$S2.pre)
# What is the correlation V1between V1 and V2 pre-training
cor(cogsport$V1.pre, cogsport$V2.pre)
# Correlations from the control group could be used to estimate test/retest reliability
# If so, which test is most reliable
aerobic <- subset(cogsport,cogsport[,2] == "aer")
cor(cogsport$S1.pre,cogsport$S1.post)
cor(cogsport$S2.pre,cogsport$S2.post)
cor(cogsport$V1.pre,cogsport$V1.post)
cor(cogsport$V2.pre,cogsport$V2.post)
# Does there appear to be a correlation between spatial reasoning before training and the amount
# of improvement in spatial reasoning?
cogsport$S_improve <- ((cogsport$S1.post - cogsport$S1.pre) + (cogsport$S2.post - cogsport$S2.pre))/2
cogsport$S_preavg <- (cogsport$S1.pre + cogsport$S2.pre)/2
cor(cogsport$S_preavg, cogsport$S_improve)
# Does there appear to be a correlation between verbal reasoning before training and the amount
# of improvement in verbal reasoning?
cogsport$V_improve <- ((cogsport$V1.post - cogsport$V1.pre) + (cogsport$V2.post - cogsport$V2.pre))/2
cogsport$V_preavg <- (cogsport$V1.pre + cogsport$V2.pre)/2
cor(cogsport$V_preavg, cogsport$V_improve)
# Which group exhibited more improvement in spatial reasoning
aerobic <- subset(cogsport,cogsport[,2] == "aer")
design <- subset(cogsport,cogsport[,2] == "des")
aerobic_S_improvement <- ((aerobic$S1.post - aerobic$S1.pre) + (aerobic$S2.post - aerobic$S2.pre))/2
design_S_improvement <- ((design$S1.post - design$S1.pre) + (design$S2.post - design$S2.pre))/2
sum(aerobic_S_improvement)
sum(design_S_improvement)
# Create a color scatterplot of all 4 measures at pre-test. Do the scatterplots suggest 2 reliable and valid constructs?
base <- subset(cogsport, select=c(S1.pre,S2.pre,V1.pre,V2.pre))
base.r <- abs(cor(base))
base.color <- dmat.color(base.r)
base.order <- order.single(base.r)
cpairs(base, base.order, panel.colors = base.color, gap = .5, main = "Variables Ordered and Colored by Correlation")
# Create a color scatterplot of all 4 measures at post-test. Do the scatterplots suggest 2 reliable and valid constructs?
base <- subset(cogsport, select=c(S1.post,S2.post,V1.post,V2.post))
base.r <- abs(cor(base))
base.color <- dmat.color(base.r)
base.order <- order.single(base.r)
cpairs(base, base.order, panel.colors = base.color, gap = .5, main = "Variables Post Test Ordered and Colored by Correlation")
#### what follows are some extra scatter plots using ggplot2 i was messsing with
sp <- ggplot(cogsport,aes(x = V2.pre,y=V2.post,colour=cond,shape=cond))
sp + geom_point(size=2.5, position=position_jitter(width=0.5,height=0))
# now add some regression lines
sp + geom_point(size=2.5) + stat_smooth(method=lm, level=0.95)
# or a different curve: loess = locally weighted polynomial
sp + geom_point(size=2.5) + stat_smooth(method=lm)
# some histograms
ggplot(cogsport,aes(x=S2.pre)) + geom_histogram()
ggplot(cogsport,aes(x=V2.pre)) + geom_histogram()
ggplot(cogsport,aes(x=V2.post)) + geom_histogram()
####################################
End of Assignment #3
#####################################