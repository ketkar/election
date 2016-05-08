###################
### Running KNN ###
###################

source("util.R")

#Cleaning some more, with labels for win/losses relative to each other. 
#Assumes two people races 
trump_wins <- as.numeric(trump_nums$votes - cruz_nums$votes > 0)
sanders_wins <- as.numeric(sanders_nums$votes - clinton_nums$votes > 0)

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

#Packaging it up 
knn_pred <- function(cand_nums, cand_wins, cand, state = "Idaho", k=20, features = c(30, 10, 13, 11, 16)){
  #Returns proportion of KNN with that label for that state (predictions)
  ##USAGE EXAMPLE## : test <- knn_pred(trump_nums, trump_wins, trump, k=20)
  cand_cut <- cand_nums[features]
  cand_idaho_train <- cand_cut[which(cand$state == state),]
  cand_idaho_labels <- cand_wins[which(cand$state == state)]
  cand_train <- cand_cut[which(cand$state != state), ]
  cand_train_labels <- cand_wins[which(cand$state != state)]
  
  cand_confidence <- numeric()
  
  for (i in 1:nrow(cand_idaho_train)){
    cand_confidence[[i]] <- mean(cand_train_labels[knn(cand_idaho_train[i, ], cand_train, k)[1:k]])
  }
  correct <- 1 - sum(abs(cand_idaho_labels - as.numeric(cand_confidence > 0.5)))/length(cand_confidence)
  if (correct < 0.5){
    correct <- 1 - correct # :) binary prediction shortcut
  }
  print(paste("Correct rate is,", correct))
  df <- data.frame(indexes = which(cand$state == state)[1:length(cand_confidence)], confidence = cand_confidence) 
  return(df)
}

