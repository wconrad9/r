##Which populations are most susceptible to contracting the virus? Why?
library(tidyverse)
library(stringr)

county_cases <- read_csv('./county_case_data.csv')
county_health_data <- read_csv('./county_health_rankings.csv')
county_svi <- read_csv('./social_vulnerability_index.csv')
ny_tests <- read_csv('./ny_testing.csv')

view(county_cases)

view(county_health_data)

colnames(county_health_data)

aian_indexes <- str_detect(colnames(county_health_data), 'aian')
black_indexes <- str_detect(colnames(county_health_data), 'black')
hispanic_indexes <- str_detect(colnames(county_health_data), 'hispanic')
white_indexes <- str_detect(colnames(county_health_data), 'white')
asian_indexes <- str_detect(colnames(county_health_data), 'asian')
percent_indexes <- str_detect(colnames(county_health_data), '95percent')

indexes <- NULL

for(i in 1:507) {
  if(percent_indexes[i] == TRUE ||
     asian_indexes[i] == TRUE ||
     aian_indexes[i] == TRUE ||
     black_indexes[i] == TRUE ||
     hispanic_indexes[i] == TRUE ||
     white_indexes[i] == TRUE) {
      
      indexes[i] = i
    }
  else{
    indexes[i] = 9
  }
}

county_health_data_clean <- county_health_data[,-c(unique(indexes))]

health_and_cases <- left_join(county_cases,
                              county_health_data_clean,
                              by = c('fips'))

health_and_cases_subset <- health_and_cases %>%
  head(1000) %>%
  group_by(state.x,county.x) %>%
  view()

view(health_and_cases_subset)

view(county_svi)

county_svi <- county_svi[,-c(1:4)]


M_indexes <- str_detect(colnames(county_svi), 'M_')
MP_indexes <- str_detect(colnames(county_svi), 'MP_')

indexes2 <- NULL

for (i in 1:119) {
  
  if(M_indexes[i] == TRUE ||
     MP_indexes[i] == TRUE) {
    indexes2[i] = i
  }
  else{
    indexes2[i] = 5
  }
}


county_svi_subset <- county_svi[,-c(unique(indexes2))]

colnames(county_svi_subset)[1] = 'fips'

county_svi_subset$fips <- as.numeric(county_svi_subset$fips) 

view(county_svi_subset)

county_overall <- left_join(health_and_cases,
                           county_svi_subset,
                           by = c('fips'))

county_overall_subset <- county_overall %>%
  head(1000) %>%
  #filter(county.y == 'Snohomish') %>%
  filter(date == '2020-03-09')

county_overall_subset <- county_overall_subset[,-c(18,147,148)]

county_overall_subset <- county_overall_subset %>%
  na.omit()

county_overall_subset

county_overall_subset <- county_overall_subset[,-c(7,8)]

county_overall_subset_ranger <- county_overall_subset[,-c(1:4)]

library(caret)

ranger1 <- train(cases ~.,
                 data = county_overall_subset_ranger,
                 method = 'ranger')

### let's try to add number of tests as a training variable, using data from NY


ny_overall <- county_overall %>%
  filter(state.x == 'New York')


ny_overall_testing %>%
  filter(date == '2020-4-27') %>%
  select(cumulative_number_of_tests_performed)

ny_overall_testing <- left_join(ny_overall,
                                ny_tests,
                                by = c('county.x' = 'county',
                                       'date' = 'test_date'))

ny_overall_testing[, c('county.x', 'E_TOTPOP')]

ny_overall_testing <- ny_overall_testing %>%
  filter(county.x != 'New York City') %>%
  mutate(case_pop_proportion = cases / E_TOTPOP) %>%
  mutate(death_pop_proportion = deaths / E_TOTPOP) %>%
  mutate(pop_density = E_TOTPOP / AREA_SQMI) %>%
  filter(date == '2020-04-26')

ny_overall_testing_daterange <- ny_overall_testing %>%
  filter(county.x != 'New York City') %>%
  mutate(case_pop_proportion = cases / E_TOTPOP) %>%
  mutate(death_pop_proportion = deaths / E_TOTPOP) %>%
  mutate(pop_density = E_TOTPOP / AREA_SQMI) %>%
  filter(date == '2020-04-26' |
           date == '2020-04-25' |
           date == '2020-04-24' |
           date == '2020-04-23' |
          date == '2020-04-22' |
         date == '2020-04-21' |
           date == '2020-04-20')

ny_overall_testing_daterange %>%
  select(cases, county.x, percent_rural) %>%
  view()


#verify the join worked
ny_overall_testing[,c(1:5, 250:254)] %>%
  filter(county.x == 'Westchester') %>%
  view()



lm1 <- lm(case_pop_proportion ~ cumulative_number_of_tests_performed + pop_density,
          data = ny_overall_testing)

lm2 <- lm(case_pop_proportion ~ pop_density + E_TOTPOP + cumulative_number_of_tests_performed,
          data = ny_overall_testing)

summary(lm2)


summary(lm1)


ny_overall <- ny_overall %>%
  na.omit()


view(ny_overall)

#let's try some unsupervised learning, just for new york
#important columns: 5,6,12,14,16,21,23,27,31,43,45,46,53,54,61,64,76,77,97,99,100


ny_overall_testing_day <- ny_overall_testing %>%
  filter(date == '2020-04-26')


ny_hclust_summary %>%
  mutate(cluster_test = ifelse(county.x %in% c('Rockland','Westchester', 'Suffolk', 'Nassau'), 1, 0)) %>%
  group_by(cluster_test) %>%
  summarize_all(mean) %>%
  view()

ny_hclust_summary <- ny_overall_testing[,c(2,5,12,14,16,21,23,27,31,43,45,46,53,54,61,64,76,77,97,99,100,170)]

ny_numbers <- ny_overall_testing_day[,c(12,14,16,21,23,27,31,43,45,46,53,54,61,64,76,77,97,99,100,170)]

ny_numbers_scaled <- scale(ny_numbers)

ny_numbers_dist <- dist(ny_numbers_scaled)

hc1 <- hclust(ny_numbers_dist)

plot(hc1)

library(dendextend)


hc1 %>%
  as.dendrogram() %>%
  place_labels(ny_overall_testing_day$county.x) %>%
  set('labels_cex', .3) %>%
  color_branches(k = 6) %>%
  color_unique_labels() %>%
  plot()

hc1 %>%
  as.dendrogram() %>%
  place_labels(ny_overall_testing_day$cases) %>%
  set('labels_cex', .3) %>%
  color_branches(k = 3) %>%
  color_unique_labels() %>%
  plot()



#heirarchical clustering hinted that overall population / population density could be a good metric to
#train a linear model by


#let's try kmeans clustering

km1 <- kmeans(ny_numbers_scaled,
              4)


ny_clusters <- cbind(ny_overall_testing, km1$cluster)

ny_clusters %>%
  group_by(km1$cluster) %>%
  summarize(average_cases = mean(cases),
            average_deaths = mean(deaths),
            average_pop = mean(E_TOTPOP))

ny_clusters %>%
  filter(km1$cluster == 4) %>%
  select(county.x)




tot <- NULL
for(i in 1:10){
  km <- kmeans(ny_numbers_scaled,
               i)
  
  tot[i] <- km$tot.withinss/i
}

plot(tot)

ny_numbers_scaled %>%
  as.data.frame() %>%
  mutate(cluster = km1$cluster) %>%
  ggplot(aes(x = E_TOTPOP,
             y = cases)) +
  geom_bar(aes(color = factor(cluster)))


km1$cluster == 2

ny_overall_testing_day$county.x

###
ny_pca_numbers <- ny_overall_testing[,c(5,97,99,158,167,170)]

#ny_pca_numbers <- cbind(ny_pca_numbers, ny_overall_testing_day$county.x)

row.names(ny_pca_numbers) <- ny_overall_testing_day$county.x

#ok let's try to reduce our variables using PCA
pca1 <- prcomp(ny_pca_numbers,
               scale. = TRUE)

library(pls)

biplot(pca1)

cases_numbers <- cbind(ny_pca_numbers, ny_overall_testing_day$cases)

row.names(ny_pca_numbers) <- ny_overall_testing_day$county.x

view(ny_pca_numbers)

pca2 <- prcomp(ny_pca_numbers,
              scale = TRUE)

validationplot(pca2)

biplot(pca2)






