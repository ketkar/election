###################
### Running KNN ###
###################

source("util.R")
library("FNN")


#Partitioning data 
shuffled <- trump_nums[sample.int(nrow(trump_nums)),]
train <- shuffled[1:1500, ]
test <- shuffled[1501:nrow(shuffled), ]

# DEPRECATED out <- knn(subset(train, select = -c(votes)), subset(test, select = -c(votes)), train$votes, k = 2)



#Rewriting K Nearest Neighbors 
calculate_distance <- function(vec1, vec2){
  #Calculates Euclidian distance between two vectors
  return(sqrt(sum((vec1 - vec2)^2)))
}
knn <- function(cand, data, num = 5){
  #Returns num nearest neighbors to cand in dataset 
  distances <- apply(data, 1, calculate_distance, cand)
  return(sort.int(distances, index.return = T)$ix[1:num])
}

#t <- knn(trump_nums[1, ], trump_nums, 10)


#Cleaning some more, with labels for win/losses relative to each other. 
#Assumes two people races 
trump_wins <- as.numeric(trump_nums$votes - cruz_nums$votes > 0)
sanders_wins <- as.numeric(sanders_nums$votes - clinton_nums$votes > 0)


t <- knn(trump_nums[1, ], trump_nums, 10)
