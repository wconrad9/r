install.packages("openintro")

library(openintro)
library(tidyverse)
marioKart


#to answer question 1, we find that the least expensive set with one wheel was 33 dollars
marioKart %>%
  group_by(wheels) %>%
  filter(wheels > 0) %>%
  arrange(totalPr)


#question 2: UPS 3 day ground had the smallest shipping range at $0, parcel delivery has the
#greatest out of all 8 methods with $25.5
marioKart %>%
  group_by(shipSp) %>%
  summarize(range = max(shipPr) - min(shipPr), n = n())


#question 3: new mean: 53.8, new median: 54.0, used mean: 47.1, used median: 42.8
#Used games are cheaper by approximately 7 dollars when comparing means, but by 12 when
#comparing median prices. One imagines that certain expensive used games, perhaps barely
#used, are driving the mean price for used games up.
marioKart %>%
  group_by(cond) %>%
  summarize(mean = mean(totalPr), median = median(totalPr), n = n())


#question 4: for one thing, there are two used sets that sold at very expensive rates.
#removing these two outliers brings the mean and median for used games much closer together.
#I chose to remove any prices less than 100 because the two data points were so far from the
#normal spread of data that they skewed the values for mean and median. They were significantly
#more than any of the new sets sold for. Looking at the titles for the outliers,
#these sales were bundled with a variety of other products that drove the prices
#of the auctions up.
#after removing points: mean 42.9, median 42.4

marioKart %>%
  group_by(cond) %>%
  filter(cond == "used") %>%
  filter(totalPr < 100) %>%
  arrange(totalPr) %>%
  summarize(mean = mean(totalPr), median = median(totalPr))


marioKart %>%
  ggplot(mapping = aes(totalPr)) +
  geom_histogram()


#question 5: See above.

marioKart %>%
  group_by(cond) %>%
  filter(cond == "used") %>%
  filter(totalPr > 100) %>%
  select(title)

#question 6: # of steering wheels -> median
marioKart %>%
  group_by(cond, wheels) %>%
  summarize(median = median(totalPr), n = n())



#having more wheels increases totalPr
marioKart %>%
  group_by(cond) %>%
  ggplot(mapping = aes(x = wheels,
                       y = totalPr)) +
  geom_point()


#question 7: proportion of games less than $50
marioKart %>%
  mutate(indicator = ifelse(totalPr<50, 1, 0)) %>%
  group_by(cond) %>%
  summarize(proportion = mean(indicator))

#question 8:

library(ggplot2)
library(tidyverse)

marioKart %>%
  arrange(nBids) %>%
  select(totalPr, nBids)

#number of bids didn't seem to affect totalPr... we actually can't determine this
marioKart %>%
  ggplot(mapping = aes(x = nBids,
                     y = totalPr)) +
  geom_point()


#start price doesn't seem to have any overall effect on totalPr
marioKart %>%
  ggplot(mapping = aes(x = startPr,
                       y = totalPr)) +
  geom_point()

marioKart %>%
  group_by(startPr) %>%
  summarize(price = mean(totalPr), n = n())


#duration doesn't seem to have any overall effect on totalPr
marioKart %>%
  ggplot(mapping = aes(x = duration,
                       y = totalPr)) +
  geom_point() +
  xlim(0,7.5) +
  ylim(0,100)


marioKart %>%
  group_by(duration) %>%
  summarize(price = mean(totalPr), n = n())


#shipping method doesn't seem to have any overall effect on totalPr
marioKart %>%
  ggplot(mapping = aes(x = shipSp,
                       y = totalPr)) +
  geom_point()+
  ylim(25,75)


marioKart %>%
  group_by(shipSp) %>%
  summarize(price = mean(totalPr), n = n())



#stockPhoto or not... interesting finding about new vs. used without stock photo
marioKart %>%
  group_by(stockPhoto, cond) %>%
  summarize(price = mean(totalPr), n = n())




