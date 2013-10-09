# Lab Goals
#   Read a data file into R
#   Print summary statistics
#   Conduct correlational analyses
#   Examine relationships amont variables using scatterplots
# Example
#   Investigating the effects of sports-related concussion
#   Simulated data are based on an online assessment tool called IMPACT (http://www.impacttest.com)
#   IMPACT provides 6 main measures, listed here:
#     Memory composite verbal (vermem)
#     Memory composite visual (vismem)
#     Visual motor speed composite (vms)
#     Reaction time composite (rt)
#     Impulse control composite (ic)
#     Total symptom score (sym)

# Check working directory
getwd()
setwd("/Users/stuart/R_Files/Statistics Class")

# If necessary, install packages:
install.packages("psych")
install.packages("gclus")
install.packages("rgl")
# Load packages
library(psych)
library(gclus)
library(rgl)
# Read data file into a dataframe called impact
impact <- read.table("stats1datafilesStats1.13.Lab.03.txt", header = T)
# Edit impact
edit(impact)
# Summary statistics
describe(impact)
describeBy(impact,impact$condition)
# Correlation analysis of baseline measures (contained in cols 3:8)
cor(impact[3:8])
# Round correlation r to 2 decimal places
round(cor(impact[3:8]),2)
# Create 2 subsets: control and concussed
control <- subset(impact,impact[,2] == "control")
concussed <- subset(impact,impact[,2] == "concussed")
# Do correlation analysis of the control group, ALL measures
round(cor(control[3:14]),2)
# Do correlation analysis of the concussed group, ALL measures
round(cor(concussed[3:14]),2)
# Question: Does baseline impulse control (ic1) predict memory imparement after a concussion?
# To answer, let's create a composite memory impaired variable
concussed$verbal.impair <- concussed$vermem1 - concussed$vermem2
concussed$visual.impair <- concussed$vismem1 - concussed$vismem2
concussed$memory.impair <- (concussed$verbal.impair + concussed$visual.impair)/2  ## our composite memory variable
cor(concussed$memory.impair, concussed$ic1)

# Scatterplots
# Scatterplots functions are available in many packages and offer advanced features. Encouraged to explore these...
plot(impact$vermem1 ~ impact$vismem1)
# Standard scatterplot with regression line
abline(lm(impact$vermem1 ~ impact$vismem1), col = "green")
# Scatterplot matrix
pairs(~impact$vermem1 + impact$vismem1 + impact$vms1 + impact$rt1 + impact$sym1, cex.labels = 1.2)
# Color scatterplot matrix, colored and ordered by magnitude of r
base <- impact[3:8]
base.r <- abs(cor(base))
base.color <- dmat.color(base.r)
base.order <- order.single(base.r)
cpairs(base, base.order, panel.colors = base.color, gap = .5, main = "Variables Ordered and Colored by Correlation")
# ScatterPlots in 3D
plot3d(impact$vismem1, impact$sym1, impact$vermem1, main = "3D Plot")
plot3d(impact$vismem2, impact$sym2, impact$vermem2, main = "3D Plot")