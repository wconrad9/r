library(rvest)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(textdata)
library(gridExtra)
library(data.table)
library(stringr)

#Question 1
years <- c(2012, 2013, 2014, 2015, 2016)
consultant.salaries <- data.frame(years,
                                  Salary = c(22000,
                                             35000,
                                             37000,
                                             46500,
                                             63000),
                                  Occupation = c("Consultant",
                                                 "Consultant",
                                                 "Consultant",
                                                 "Consultant",
                                                 "Consultant"))
Assistant.salaries <- data.frame(years,
                                Salary = c(22000,
                                          24000,
                                           23500,
                                           23000,
                                           26500),
                                Occupation = c("Research Assistant",
                                               "Research Assistant",
                                               "Research Assistant",
                                               "Research Assistant",
                                               "Research Assistant"))
Instructor.salaries <- data.frame(years,
                                 Salary = c(22000,
                                            29500,
                                            32000,
                                            31750,
                                            33500),
                                 Occupation = c("Instructor",
                                                "Instructor",
                                                "Instructor",
                                                "Instructor",
                                                "Instructor"))

merge1 <- full_join(consultant.salaries,
                    Instructor.salaries,
                    by = c("years",
                           'Occupation',
                           'Salary'))
merge2 <- full_join(merge1,
                    Assistant.salaries,
                    by = c("years",
                           'Occupation',
                           'Salary'))


frame.years <- data.frame(years)
frame.consultant.salary <- data.frame(consultant.salary)

consultant.test <- merge(frame.years,
                         frame.consultant.salary,
                         by=c("ID"))


merge2$Occupation = factor(merge2$Occupation,
                           levels = c("Consultant",
                                      "Instructor",
                                      "Research Assistant"))

merge2 %>%
  group_by(years) %>%
  ggplot(aes(x = as.numeric(years),
             y = Salary,
             fill = Occupation)) +
  geom_area(position = 'identity') +
  xlab("Year") +
  ylab("Income($)") +
  scale_fill_manual(values = c("#51b3eb",
                               "#f1e62a",
                               "#ce3b0c")) +
  ggtitle("Alex's Income in Various Jobs") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position="bottom", legend.title = element_blank()) +
  theme(plot.title = element_text(hjust=.5)) +
  theme(text = element_text(size = 10))


#Question 2
reddit.data <- fread(file.choose())

test1000 <- reddit.data %>%
  head(1000)

test1000 %>%
  mutate(digits = ifelse(str_detect(test1000$author, "(?<![:digit:])[:digit:]{2}(?![:digit:])"),
                         str_extract(test1000$author, "(?<![:digit:])[:digit:]{2}(?![:digit:])"),
                          0)) %>%
  filter(digits > 10 & digits <= 99) %>%
  group_by(digits) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = digits,
             y = count)) +
  geom_bar(stat = 'identity') +
  coord_flip()

reddit.data %>%
  mutate(digits = ifelse(str_detect(reddit.data$author, "(?<![:digit:])[:digit:]{2}(?![:digit:])"),
                         str_extract(reddit.data$author, "(?<![:digit:])[:digit:]{2}(?![:digit:])"),
                         0)) %>%
  filter(digits > 10 & digits <= 99) %>%
  group_by(digits) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = digits,
             y = count)) +
  geom_bar(stat = 'identity') +
  coord_flip()


#Question 3

marriage.url <- 'https://en.wikipedia.org/wiki/Marriage_age_in_the_United_States'

marriage.data <- marriage.url %>%
  read_html() %>%             #reads the websites html code
  html_nodes("table") %>%     #scrapes all table nodes from url
  .[[1]] %>%
  html_table(fill = TRUE)

view(marriage.data)

colnames(marriage.data) <- marriage.data[1,]

marriage.data.copy <- marriage.data

marriage.data.formatted <- marriage.data.copy[-1,]

census.data <- fread(file.choose())
census.data2 <- census.data
census.data3 <- census.data

census.data %>%
  group_by(STATE) %>%
  filter(NAME == STATE)

census.data2$POPULATION <- as.numeric(census.data$POPULATION)

census.data2[c(1:10),]

census.data2 %>%
  group_by(STATE) %>%
  na.omit() %>%
  filter(NAME != STATE) %>%
  count(POPULATION) %>%
  mutate(real.pop = n * POPULATION) %>%
  summarize(total.pop = sum(real.pop))

census.simplified <- census.data3 %>%
  group_by(STATE) %>%
  filter(NAME == STATE)

marriage.final <- marriage.data.formatted %>%
  mutate(test = ifelse(str_detect(marriage.data.formatted$`Minimum statutory age after all exceptions[6]`, "(?<![:punct:])[:digit:]{2}(?![:digit:])"),
                       str_extract(marriage.data.formatted$`Minimum statutory age after all exceptions[6]`, "(?<![:digit:])[:digit:]{2}(?![:digit:])"),
                       0)) %>%
  mutate(proportion = ifelse(test != 0,
                             ((as.numeric(test) - 14)/10 * (22213952 + 21137826) + 31255995 + 29919938)/309300000,
                             "Technically, everyone.")) %>%
  mutate(categorize = as.numeric(test) - 14) %>%
  mutate(NAMES1 = ifelse(str_detect(marriage.final$Name, "[:punct:]"),
                        str_sub(marriage.final$Name, 0, -5),
                        Name)) %>%
  mutate(NAMES2 = ifelse(str_detect(marriage.final$NAMES1, "[:punct:]"),
                         str_sub(marriage.final$NAMES1, 0, -5),
                         NAMES1))

merged <- left_join(census.simplified,
          marriage.final,
          by = c("NAME" = "NAMES2"))

#US POP 2010: 309,347,261
merged %>%
  ungroup() %>%
  summarize(US.pop = sum(as.numeric(POPULATION)))

merged %>%
  mutate(age = ifelse(STATE == "District of Columbia",
                      toString(16),
                      test)) %>%
  group_by(age) %>%
  summarize(proportion.by.age = sum(as.numeric(POPULATION))/309347261)

'0–14 years: 18.62% (male 31,255,995/female 29,919,938)'
'15–24 years: 13.12% (male 22,213,952/female 21,137,826)'

eighteen.or.under <- (31255995 + 29919938 + 4/10 * (22213952 + 21137826))

seventeen.or.under <- (31255995 + 29919938 + 3/10 * (22213952 + 21137826))

sixteen.or.under <- (31255995 + 29919938 + 2/10 * (22213952 + 21137826))

fifteen.or.under <- (31255995 + 29919938 + 1/10 * (22213952 + 21137826))

fourteen.or.under <- (31255995 + 29919938)
          

state.marriage.categories <- c(18, 17, 16, 15, 14)
eighteen.states <- NULL
seventeen.states <- NULL

for(i in 1:52) {
  
  ifelse(marriage.final$test[i] == 18,
        eighteen.states[i] <- marriage.final$Name[i],
        marriage.final$test[i] == marriage.final$test[i])
  
print(i)
}

eighteen.states.data <- data.frame(eighteen.states)

left_join(census.simplified,
          eighteen.states.data,
          by = c("NAME" = "eighteen.states"))


for(i in 1:52) {
  
  ifelse(marriage.final$test[i] == 17,
         seventeen.states[i] <- marriage.final$Name[i],
         marriage.final$test[i] == marriage.final$test[i])
  
  print(i)
}

?append


