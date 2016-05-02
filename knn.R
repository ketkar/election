###################
### Running KNN ###
###################

source("util.R")
library("FNN")

shuffled <- trump_nums[sample.int(nrow(trump_nums)),]
train <- shuffled[1:1500, ]
test <- shuffled[1501:nrow(shuffled), ]

out <- knn(subset(train, select = -c(votes)), subset(test, select = -c(votes)), train$votes, k = 2)


calculate_distance <- function(vec1, vec2){
  #Calculates Euclidian distance between two vectors
  return(sqrt(sum((vec1 - vec2)^2)))
}

knn <- function(cand, data, num = 5){
  #Returns num nearest neighbors to cand in dataset 
  distances <- apply(data, 1, calculate_distance, cand)
  
  return(sort(distances)[1:num])
}

t <- knn(trump_nums[1, ], trump_nums)
