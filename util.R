############
## Util ####
############

# Use source("util.R") to import these methods. 


library("dplyr")
county_features <- read.csv("data/county_facts_dictionary.csv")
county_data <- read.csv("data/county_facts.csv")
results <- read.csv("data/primary_results.csv")

data_with_results <- left_join(results, county_data, by = c("fips" = "fips"))
