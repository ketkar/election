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

trump_confidence <- knn_pred(trump_nums, trump_wins, trump, k=20)

trump_confidence$value = trump_confidence$confidence
trump_confidence$region <- trump[which(trump$state == "Idaho"),]$fips
county_choropleth(trump_confidence, 
                  state_zoom = "idaho",
                  title      = "Trump Win Probability",
                  num_colors = 9, color = "red")

