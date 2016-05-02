#Testing 
trump_cut <- trump_nums[c(30, 10, 13, 11, 16)]
trump_idaho_train <- trump_cut[which(trump$state == "Idaho"),]
trump_idaho_labels <- trump_wins[which(trump$state == "Idaho")]
trump_train <- trump_cut[which(trump$state != "Idaho"), ]
trump_train_labels <- trump_wins[which(trump$state != "Idaho")]

trump_confidence <- numeric()
for (i in 1:nrow(trump_idaho_train[1:5])){
  trump_confidence[[i]] <- mean(trump_train_labels[knn(trump_idaho_train[i, ], trump_train, 15)[2:15]]) #start at 2 to avoid same sample? 
}

correct <- 1 - sum(abs(trump_idaho_labels - as.numeric(trump_confidence > 0.5)))/length(trump_confidence)

#Time for Cross validation! 
#Partitioning data 
shuffled_indexes <- sample.int(nrow(trump_nums))
shuffled <- trump_nums[shuffled_indexes,]
train <- shuffled[1:1500, ]
train_indexes <- shuffled_indexes[1:1500]
test_indexes <- shuffled_indexes[1501:length(shuffled_indexes)]
test <- shuffled[1501:nrow(shuffled), ]
test_labels <- trump_wins[test_indexes]
train_labels <- trump_wins[train_indexes]