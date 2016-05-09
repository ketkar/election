#install.packages("maps")
#install.packages("ggplot2")
library(ggplot2)
library(choroplethr)
library(mapproj)

source("knn.R")
map_data <- function(cand_nums, cand_wins, cand, opponent, k = 20, state = "idaho"){
  
  State <- paste(toupper(substr(state, 1, 1)), substr(state, 2, nchar(state)), sep = "")

  #cand_confidence <- knn_pred(cand_nums, cand_wins, cand, State, k=k)
  #saveRDS(cand_confidence, paste(state, ".rds", sep=""))
  cand_confidence <- readRDS(paste(state, ".rds", sep=""))
  
  cand_confidence$value = cand_confidence$confidence
  cand_confidence$region <- cand[which(cand$state == State),]$fips
  print(State)
  
  
  cand_idaho_labels <- as.numeric(cand$votes - opponent$votes > 0)[which(cand$state == State)]
  true_values <- data.frame(value = cand_idaho_labels, region = cand_confidence$region)
  a <- county_choropleth(cand_confidence, 
                    state_zoom = state,
                    title      = "Predicted Election Results",
                    legend = c("Cand Win (1), Opponent Win (0)"),
                    num_colors = 1)
  print(true_values$value)
  b <- county_choropleth(true_values, 
                    state_zoom = state,
                    title      = "Actual Election Results",
                    legend = c("Cand Win (1), Opponent Win (0)"),
                    num_colors = 1)

  plot(a) 
  plot(b)
}
map_data(trump_nums, trump_wins, trump, cruz,  k = 20, state = "idaho")


