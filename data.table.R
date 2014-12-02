# Experiments with data.table package
setwd("/Users/stuart/R_Files")

library(data.table)

# Create your first data.table 
my_first_data_table = data.table(x = c("a","b","c","d","e"),y = c(1,2,3,4,5))
my_first_data_table

DT = data.table(a = c(1,2), b = c("A", "B","C","D")) 
DT
DT[3]
DT[2:3]
