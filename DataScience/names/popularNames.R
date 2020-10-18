
library(babynames)
library(openintro)
library(tidyverse)
library(nycflights13)

#for question 1
babynames %>%
  group_by(name) %>%
  summarize(number = sum(n)) %>%
  arrange(-number)


#question 2: look at the help documentation
babynames %>%
  group_by(year) %>%
  summarize(births = sum(n)) %>%
  ggplot(mapping = aes(x = year,
                       y = births)) +
  geom_point(aes(color = births))


#question 3: 4.06 million from government site, 3.78 million from this data
babynames %>%
  group_by(year) %>%
  filter(year == "2000") %>%
  summarize(births = sum(n))

?babynames


#question 4: Christopher and Maximiliano
babynames %>%
  filter(n >1000) %>%
  filter(name != "Christopher") %>%
  group_by(name) %>%
  mutate(name.length = nchar(name, type = "chars")) %>%
  arrange(desc(name.length))
  

#question 5:
babynames %>%
  group_by(year) %>%
  mutate(name.length = nchar(name, type = "chars")) %>%
  summarize(avg.name.length = mean(name.length)) %>%
  ggplot(mapping = aes(x = year,
                       y = avg.name.length)) +
  geom_point(aes(color = avg.name.length))


#question 6:
setup <- babynames %>%
  group_by(name, year) %>%
  #filter(name %in% c("Donnie", "Leslie", "Robbie")) %>%
  mutate(gender.assoc = ifelse(sex == "M", 1, -1)) %>%
  mutate(indicator = (gender.assoc*n)) %>%
  mutate(prop1 = mean(indicator)) %>%
  mutate(prop2 = mean(n)) %>%
  mutate(proportion = mean(prop1/prop2)) %>%
  mutate(test = max(proportion)-min(proportion))

setup %>%
  group_by(name) %>%
  summarize(math = max(proportion) - min(proportion))

setup %>%
  ggplot(mapping = aes(x = year,
                       y = proportion)) +
  geom_line(aes(color = name))
  

#####
setup %>%
  filter(year %in% c(1880, 2017)) %>%
  filter(sex == "F") %>%
  group_by(name) %>%
  summarize(some.math = max(proportion) - min(proportion)) %>%
  arrange(-some.math)
#####
