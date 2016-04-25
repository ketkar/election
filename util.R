############
## Util ####
############

# Use source("util.R") to import these methods. 

library("dplyr")
library("class") #for use with k nearest neighbors. 

county_features <- read.csv("data/county_facts_dictionary.csv")
county_data <- read.csv("data/county_facts.csv")
results <- read.csv("data/primary_results.csv")

data_with_results <- left_join(results, county_data, by = c("fips" = "fips"))

trump <- filter(data_with_results, candidate == "Donald Trump")
sanders <- filter(data_with_results, candidate == "Bernie Sanders")
clinton <- filter(data_with_results, candidate == "Hillary Clinton")
cruz <- filter(data_with_results, candidate == "Ted Cruz")

trump_nums <- subset(trump, select = - c(state:candidate, fraction_votes:state_abbreviation.y))
sanders_nums <- subset(sanders, select = - c(state:candidate, fraction_votes:state_abbreviation.y))
clinton_nums <- subset(clinton, select = - c(state:candidate, fraction_votes:state_abbreviation.y))
cruz_nums <- subset(cruz, select = - c(state:candidate, fraction_votes:state_abbreviation.y))

i <- match(names(clinton_nums), county_features$column_name)
names(trump_nums) <- county_features$description[i]
names(trump_nums)[[1]] <- "votes"
names(clinton_nums) <- county_features$description[i]
names(clinton_nums)[[1]] <- "votes"
names(cruz_nums) <- county_features$description[i]
names(cruz_nums)[[1]] <- "votes"
names(sanders_nums) <- county_features$description[i]
names(sanders_nums)[[1]] <- "votes"

trump_model <- lm(votes ~ ., trump_nums)
sanders_model <- lm(votes ~ ., sanders_nums)
clinton_model <- lm(votes ~ ., clinton_nums)
cruz_model <- lm(votes ~ ., cruz_nums)

#Example: Top weighted features for Sanders: 
head(sort(abs(sanders_model$coefficients), decreasing = T), 10)
#Least important features: 
head(sort(abs(sanders_model$coefficients), decreasing = F), 10)

