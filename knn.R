###################
### Running KNN ###
###################

source("util.R")
library("class") #for use with k nearest neighbors. 
library("permute")

shuffled <- trump_nums[sample.int(nrow(trump_nums)),]
train <- shuffled[1:1500, ]
test <- shuffled[1501:nrow(shuffled), ]

out <- knn(subset(train, select = -c(votes)), subset(test, select = -c(votes)), train$votes, k = 2)

