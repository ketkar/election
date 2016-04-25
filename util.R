############
## Util ####
############

# Use source("util.R") to import these methods. 

library("dplyr")
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


trump_model <- lm(votes ~ ., trump_nums)
sanders_model <- lm(votes ~ ., sanders_nums)
clinton_model <- lm(votes ~ ., clinton_nums)
cruz_model <- lm(votes ~ ., cruz_nums)

