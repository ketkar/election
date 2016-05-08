#install.packages("maps")
#install.packages("ggplot2")
library(ggplot2)
library(maps)
source("knn.R")

# data is a data frame that has the columns: percentages, candidate, and the region.
map_data <- function(data, color1, color2, state="ohio") {
  states = map_data("county", regions=state)
  data <- data %>% mutate(percentages = (candidate == "Sanders") * percentages + (candidate == "Clinton") * (-percentages))
  names(states) <- tolower(names(data))
  
  combined <- merge(states, data, sort=FALSE, by="region")
  data <- data %>% mutate()
  ggplot() + geom_polygon(data=combined, aes(x=long, y=lat, group=group, color=percentages)) 
  + scale_colour_gradient2(low = muted(color1), mid="white", high = muted(color2), guide="colorbar") 
  + theme_bw()  + labs(fill = "Candidate", title = "Winning Candidate by State", x="", y="")
}



library(choroplethr)
library(mapproj)

source("knn.R")
map_data <- function(cand_nums, cand_wins, cand, opponent, k = 20, state = "idaho"){
  
  State <- paste(toupper(substr(state, 1, 1)), substr(state, 2, nchar(state)), sep = "")

  cand_confidence <- knn_pred(cand_nums, cand_wins, cand, State, k=20)
  saveRDS(cand_confidence, paste(state, ".rds", sep=""))
  #cand_confidence <- readRDS("debug4.rds")
  
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
  print(cut2(true_values$value, g = 1))
  b <- county_choropleth(true_values, 
                    state_zoom = state,
                    title      = "Actual Election Results",
                    legend = c("Cand Win (1), Opponent Win (0)"),
                    num_colors = 1)

  plot(a) 
  plot(b)
}
map_data(sanders_nums, sanders_wins, sanders, clinton,  k = 20, state = "georgia")


