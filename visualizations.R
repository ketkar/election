library(DataComputing)
source("util.R")
#create scatter plots of white voter percentage vs proportion of votes
trump_winloss <- trump_nums
trump_names <- names(trump_winloss)
trump_names[7] = "Young.Percentage"
trump_names[10] = "White.Percentage"
trump_names[22] = "College.Percentage"
trump_names[30] = "Persons.Per.Household"
names(trump_winloss) <- trump_names
trump_winloss$won <- as.logical(trump$votes >cruz$votes)
trump_winloss$fraction_votes <- trump$fraction_votes

Trump.White.Profile <- ggplot(trump_winloss, aes(x=White.Percentage, y=fraction_votes, col=won)) +
  geom_point() +
  labs(title = "Trump Voter Profile",
       x = "Percent White",
       y = "Proportion of Votes")

cruz_winloss <- cruz_nums
cruz_names <- names(cruz_winloss)
cruz_names[7] = "Young.Percentage"
cruz_names[10] = "White.Percentage"
cruz_names[22] = "College.Percentage"
cruz_names[30] = "Persons.Per.Household"
names(cruz_winloss) <- cruz_names
cruz_winloss$won <- as.logical(cruz$votes >trump$votes)
cruz_winloss$fraction_votes <- cruz$fraction_votes
Cruz.White.Profile <- ggplot(cruz_winloss, aes(x=White.Percentage, y=fraction_votes, col=won)) +
  geom_point() +
  labs(title = "Cruz Voter Profile",
       x = "Percent White",
       y = "Proportion of Votes")

sanders_winloss <- sanders_nums
sanders_names <- names(sanders_winloss)
sanders_names[7] = "Young.Percentage"
sanders_names[10] = "White.Percentage"
sanders_names[22] = "College.Percentage"
sanders_names[30] = "Persons.Per.Household"
names(sanders_winloss) <- sanders_names
sanders_winloss$won <- as.logical(sanders$votes > clinton$votes)
sanders_winloss$fraction_votes <- sanders$fraction_votes
Sanders.White.Profile <- ggplot(sanders_winloss, aes(x=White.Percentage, y=fraction_votes, col=won)) +
  geom_point() +
  labs(title = "Sanders Voter Profile",
       x = "Percent White",
       y = "Proportion of Votes")

clinton_winloss <- clinton_nums
clinton_names <- names(clinton_winloss)
clinton_names[7] = "Young.Percentage"
clinton_names[10] = "White.Percentage"
clinton_names[22] = "College.Percentage"
clinton_names[30] = "Persons.Per.Household"
names(clinton_winloss) <- clinton_names
clinton_winloss$won <- as.logical(clinton$votes > sanders$votes)
clinton_winloss$fraction_votes <- clinton$fraction_votes
Clinton.White.Profile <- ggplot(clinton_winloss, aes(x=White.Percentage, y=fraction_votes, col=won)) +
  geom_point() +
  labs(title = "Clinton Voter Profile",
       x = "Percent White",
       y = "Proportion of Votes")

Trump.White.Profile
Cruz.White.Profile
Clinton.White.Profile
Sanders.White.Profile

# histogram of people per household, facetted by win/loss
Trump.Num.People.Profile <- ggplot(trump_winloss, aes(x = Persons.Per.Household, y=fraction_votes, col=won)) +
  geom_point() +
  labs(title = "Trump - Voter Household Profile",
       x = "Number of People per Household",
       y = "Count")

Cruz.Num.People.Profile <- ggplot(cruz_winloss, aes(x = Persons.Per.Household, y=fraction_votes, col=won)) +
  geom_point() +
  labs(title = "Cruz - Voter Household Profile",
       x = "Number of People per Household",
       y = "Count")

Sanders.Num.People.Profile <- ggplot(sanders_winloss, aes(x = Persons.Per.Household,y=fraction_votes, col=won)) +
  geom_point() +
  labs(title = "Sanders - Voter Household Profile",
       x = "Number of People per Household",
       y = "Count")

Clinton.Num.People.Profile <- ggplot(clinton_winloss, aes(x = Persons.Per.Household,y=fraction_votes, col=won)) +
  geom_point() +
  labs(title = "Clinton - Voter Household Profile",
       x = "Number of People per Household",
       y = "Count")

Trump.Num.People.Profile
Cruz.Num.People.Profile
Sanders.Num.People.Profile
Clinton.Num.People.Profile

# show initial county turnouts (who is winning)
Republican.Pie.Chart <- ggplot(trump_winloss, aes(x = factor(1), fill=won)) +
  geom_bar(width=1) +
  coord_polar(theta="y") +
  scale_fill_discrete(name="Candidate",
                      breaks=c("FALSE", "TRUE"),
                      labels=c("Cruz", "Trump")) +
  labs(title = "Republican Primary Race Results")

Democrat.Pie.Chart <- ggplot(clinton_winloss, aes(x = factor(1), fill=won)) +
  geom_bar(width=1) +
  coord_polar(theta="y") +
  scale_fill_discrete(name="Candidate",
                      breaks=c("FALSE", "TRUE"),
                      labels=c("Sanders", "Clinton")) +
  labs(title = "Democrat Primary Race Results")


Republican.Pie.Chart
Democrat.Pie.Chart

#Plot of age voter profile
Trump.Age.Profile <- ggplot(trump_winloss, aes(x=Young.Percentage, y=fraction_votes, col=won)) +
  geom_point() +
  labs(title = "Trump Age Profile",
       x = "Percent 18 and Under",
       y = "Proportion of Votes")
Trump.Age.Profile

Cruz.Age.Profile <- ggplot(cruz_winloss, aes(x=Young.Percentage, y=fraction_votes, col=won)) +
  geom_point() +
  labs(title = "Cruz Age Profile",
       x = "Percent 18 and Under",
       y = "Proportion of Votes")
Cruz.Age.Profile

Sanders.Age.Profile <- ggplot(sanders_winloss, aes(x=Young.Percentage, y=fraction_votes, col=won)) +
  geom_point() +
  labs(title = "Sanders Age Profile",
       x = "Percent 18 and Under",
       y = "Proportion of Votes")
Sanders.Age.Profile

Clinton.Age.Profile <- ggplot(clinton_winloss, aes(x=Young.Percentage, y=fraction_votes, col=won)) +
  geom_point() +
  labs(title = "Clinton Age Profile",
       x = "Percent 18 and Under",
       y = "Proportion of Votes")
Clinton.Age.Profile
